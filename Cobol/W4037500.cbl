000100 PROCESS DYNAM                                                            
000200*                                                                         
000300******************************************************************        
000400*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0131      *        
000500******************************************************************        
000600*                                                                         
000700 ID DIVISION.                                                             
000800 PROGRAM-ID.     W4037500.                                                
000900 AUTHOR.         ROGER OLSSON.                                            
001000 DATE-WRITTEN.   90/05/10.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*LDC21+25 VERSION                                                         
001400*    FUNKTION.                                                            
001500*        LÄSER BESTÄLLD PLOCKSATS.                                        
001600*        BEHANDLAR ALLA ORDERDELAR SOM INGÅR I PLOCKSATSEN, OCH           
001700*        SKAPAR FÄRDIGA PLE & PU-RADER.                                   
001800*        PROGRAMMET STARTAS OM EFTER ETT ANTAL BEHANDLADE RADER.          
001900*        NÄR ALLA ORDERDELAR OCH DESS RADER HAR BEHANDLATS                
002000*        STARTAS ETIKETTPGM (W4037600) & PU-PROGRAM (W4037700).           
002100*                                                                         
002200*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T375U                                             
002600*                     W4T375X VID OMSTART                                 
002700*                     W4T375Y MED LÄGRE PRIORITET                         
002800*        MID:         W4I37501                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        TRANSAKTION: W4T375X (GÄLLER VID OMSTART AV PGM)                 
003200*        TRANSAKTION: W4T376U                                             
003300*        TRANSAKTION: W4T377U                                             
003400                                                                          
003500* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
003600* ETRACK 3872743 KDARTURS FOR OVR(OVERLEVERANS)                           
003700* ETRACK 5444132 SPÅRA PRISFRÅGOR                                         
003800* ETRACK 4823800 LDC ROLL-OUT WWDC99                                      
003900* ETRACK 2218613 KAMPANJORDER SEPARAT PRC.                                
004000* ETRACK 7450328  2008-HÖST  VOHF                                         
004100* E-TRACKER:7898645 ADDITION OF NEW FIELDS TO WDGX4004                    
004200*                   4004-IDMSG3IV 4004-IDSNO3IV 4004-ADDISPXTRA           
004300*                                                                         
004400*                   ADDITION OF NEW FIELDS TO WDGX4008                    
004500*                   4008-IDMSG3IV 4008-IDSNO3IV 4008-ADDISPXTRA           
004600*                                                                         
004700* E-TRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                       
004800*                                                                         
004900* E-trACKER: 10130993 2015-04-22                                          
005000*            REDUCE NUMBER OF DELIVERY SCHEDULES                          
005100* ETRACK 10254592 2015  decomission vohf                                  
005200* I samband med KINA-EXP 2016 hittade vi fel i section                    
005300* CEBB-UPPDAT-WDK7-K9                                                     
005400* Positionen på wdk7 sönderläst av S04C-EV-LARM-2191-MID-CN               
005500* före REPL                                                               
005600* Vi flyttar larmet till efter replace och byter samtidigt                
005700* namn på sectionen till S04E-LARM-2191-MID-CN-US                         
005800*                                                                         
005900*  E-TRACKER: 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2        
006000*                                                                         
006100*  E-TRACKER: 10302687 2017 LOCAL SOURCING NA-US                          
006200*                                                                         
006300     SKIP3                                                                
006400 ENVIRONMENT DIVISION.                                                    
006500     EJECT                                                                
006600 DATA DIVISION.                                                           
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900*    -COPY WY2000W4                                                       
007000     SKIP3                                                                
007100 77  IDPGM                       PIC X(08)   VALUE 'W4037500'.            
007200                                                                          
007300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007400 77  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
007500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007600 77  FILLER                      PIC X(08)   VALUE 'PGMPOS :'.            
007700 77  PGMPOS                      PIC X(24) VALUE SPACE.                   
007800 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
007900 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
008000 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
008100 77  CURRENT-IMS-SECTION         PIC X(24)  VALUE SPACE.                  
008200                                                                          
008300 77  JA                          PIC X       VALUE 'J'.                   
008400 77  YES                         PIC X       VALUE 'Y'.                   
008500 77  NEJ                         PIC X       VALUE 'N'.                   
008600 77  DEF-IDROLL                  PIC X(5)    VALUE 'VOR99'.               
008700 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
008800 77  WS-FLODELUT                 PIC X       VALUE 'N'.                   
008900                                                                          
009000 77  SW-AENDRA-LAGOMR            PIC X       VALUE 'N'.                   
009100     88  AENDRA-LAGOMR                       VALUE 'J'.                   
009200                                                                          
009300 77  SW-RO-LARM                  PIC X       VALUE 'N'.                   
009400     88  SKAPA-RO-LARM                       VALUE 'J'.                   
009500                                                                          
009510 77  SW-LYNK-NON-API             PIC X       VALUE 'N'.                   
009520     88  LYNK-NON-API                        VALUE 'J'.                   
009530                                                                          
009540 77  SW-VOR                      PIC X       VALUE 'N'.                   
009550     88  VOR                                 VALUE 'J'.                   
009560                                                                          
009600*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
009700 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
009800 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
009900                                                                          
010000 77  FILLER                      PIC X(08)  VALUE 'AAAAAAAA'.             
010100 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
010200 77  IX2                         PIC S9(9)  VALUE +0    COMP SYNC.        
010300 77  IX-CD-OMR                   PIC S9(9)  VALUE ZERO  COMP-3.           
010400 77  FILLER                      PIC X(08)  VALUE 'BBBBBBBB'.             
010500 77  BIPA-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
010600 77  ORAD-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
010700 77  FILLER                      PIC X(08)  VALUE 'CCCCCCCC'.             
010800 77  HFAK-TAB-IX                 PIC S9(9)  VALUE +0    COMP SYNC.        
010900 77  FILLER                      PIC X(08)  VALUE 'DDDDDDDD'.             
011000 77  OUTPUT-MSG-IX               PIC S9(9)  VALUE +0    COMP SYNC.        
011100 77  TECKEN-IX                   PIC S9(9)  VALUE +0    COMP SYNC.        
011200 77  FILLER                      PIC X(08)  VALUE 'EEEEEEEE'.             
011300 77  MAX-BIPA                    PIC S9(9)  VALUE +13   COMP SYNC.        
011400 77  FILLER                      PIC X(08)  VALUE 'FFFFFFFF'.             
011500 77  MAX-ORAD                    PIC S9(9)  VALUE +60   COMP SYNC.        
011600 77  MAX-LAGOMR                  PIC S9(9)  VALUE +99   COMP SYNC.        
011700 77  FILLER                      PIC X(08)  VALUE 'GGGGGGGG'.             
011800 77  MAX-AVSR                    PIC S9(9)  VALUE +100  COMP SYNC.        
011900 77  MAX-ANT-OUTPUT-MSG          PIC S9(9)  VALUE +45   COMP SYNC.        
012000 77  INITIERA-LDC-TABELL         PIC X      VALUE SPACE.                  
012100                                                                          
012200 77  RKOD-RET-RO                 PIC S9(4)  VALUE +90   COMP SYNC.        
012300 77  FILLER                      PIC X(08)  VALUE 'HHHHHHHH'.             
012400 77  WS-IDARTNR                  PIC  9(9).                               
012500 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
012600 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
012700 77  IDMSGVER                    PIC  9(3)  VALUE 001.                    
012800                                                                          
012900 01      WS-KLOCKAN.                                                      
013000   03    WS-TIHHMMSS             PIC 9(6).                                
013100   03    FILLER                  PIC X(2).                                
013200                                                                          
013300 01      WS-KLOCKAN-LOK.                                                  
013400   03    WS-TIHHMMSS-LOK         PIC 9(6).                                
013500   03    FILLER                  PIC X(2).                                
013600                                                                          
013700 01  WS-TIMESTAMP.                                                        
013800   03 WS-AAAAMMDD-E              PIC 9(8)    VALUE ZERO.                  
013900   03 WS-TTMMSSTH-E              PIC 9(8)    VALUE ZERO.                  
014000   03 FILLER                     PIC X(10)   VALUE SPACE.                 
014100                                                                          
014200 77      WS-VOR-TID-BRIST        PIC 9(9)   VALUE ZERO.                   
014300 77      WS-DIRLEV-PRC           PIC X(4)   VALUE '9998'.                 
014400 77      WS-SEKELTAL-19          PIC X(2)   VALUE '19'.                   
014500 77      WS-SEKELTAL-20          PIC X(2)   VALUE '20'.                   
014600 77      WS-DATUM                PIC 9(6).                                
014700 77      WS-DATUM-Y2K            PIC 9(8).                                
014800 77      WS-DATUM-LOK            PIC 9(6).                                
014900 77      WS-DATUM-9KOMPL         PIC 9(8).                                
015000 77      WS-DATUM-AADDD          PIC 9(5).                                
015100 77      WS-DATUM-AADDD-LOK      PIC 9(5).                                
015200 77      WS-REGDATUM-AADDD       PIC 9(5).                                
015300 77      WS-ANTAL-DAGAR          PIC 9(5).                                
015400                                                                          
015500 77      WS-PRC-KVRADER          PIC S9(5)      COMP-3.                   
015600 77      WS-PRC-VKORDNTO         PIC S9(6)V9(1) COMP-3.                   
015700 77      WS-PRC-VLORDNTO         PIC S9(4)V9(3) COMP-3.                   
015800 77      WS-PRC-SPLITGRANS       PIC S9(7)V9(3) COMP-3.                   
015900 77      WS-PRC-RESPLIT          PIC S9(1)V9(2) COMP-3.                   
016000                                                                          
016100 77  WS-KVSEMBRA                 PIC S9(3)      COMP-3.                   
016200 77  WS-IDRADNR-SISTA            PIC S9(5)      COMP-3.                   
016300 77  WS-IDPLKLST-SISTA           PIC S9(3)      COMP-3.                   
016400 77      WS-SPLIT-IDARTNR        PIC S9(9)      COMP-3 VALUE 0.           
016500 77      WS-ANTRADER             PIC S9(5)      COMP-3.                   
016600 77      WS-ADLAGOMR             PIC S9(3)      COMP-3.                   
016700 77      WS-ADGANG               PIC S9(3)      COMP-3.                   
016800 77      WS-ADPLATS              PIC S9(5)      COMP-3.                   
016900 77      WS-VKORDNTO             PIC S9(6)V9(1) COMP-3.                   
017000 77      WS-VLORDNTO             PIC S9(4)V9(3) COMP-3.                   
017100 77      WS-SUORDV               PIC S9(9)V9(2) COMP-3.                   
017200 77      WS-SUORDV-LOC           PIC S9(9)V9(2) COMP-3.                   
017300 77      WS-SUORDV-LOCPREL       PIC S9(9)V9(2) COMP-3.                   
017400 77      WS-ODEL-KVRADER         PIC S9(5)      COMP-3.                   
017500 77      WS-ODEL-VKORDNTO        PIC S9(6)V9(1) COMP-3.                   
017600 77      WS-ODEL-VLORDNTO        PIC S9(4)V9(3) COMP-3.                   
017700 77      WS-ODEL-SUORDV          PIC S9(9)V9(2) COMP-3.                   
017800 77      WS-ODEL-SUORDV-LOC      PIC S9(9)V9(2) COMP-3.                   
017900 77      WS-ODEL-SUORDV-LOCPREL  PIC S9(9)V9(2) COMP-3.                   
018000 77      WS-ORDERVIKT-PER-RAD    PIC S9(6)V9(1) COMP-3.                   
018100 77      WS-ORDERVOLYM-PER-RAD   PIC S9(4)V9(3) COMP-3.                   
018200 77      WS-ORDERVARDE-PER-RAD   PIC S9(7)V9(2) COMP-3.                   
018300 77      WS-ORDERVARDE-PER-RAD-LOC       PIC S9(7)V9(2) COMP-3.           
018400 77      WS-ORDERVARDE-PER-RAD-LOCPREL   PIC S9(7)V9(2) COMP-3.           
018500                                                                          
018600 77      WS-KDARTHNT             PIC S9(7)      COMP-3.                   
018700 77      WS-KDROO                PIC S9(1)      COMP-3.                   
018800 77      WS-KDRAPRIO             PIC S9(3)      COMP-3.                   
018900 77      WS-KDORDSTA-CX          PIC X(2)  VALUE SPACE.                   
019000 77      WS-ANTOBKR              PIC S9(3)      COMP-3.                   
019100 77      FILLER                  PIC  X(8) VALUE 'IIIIIIII'.              
019200 77      WS-KVROS                PIC S9(7)      COMP-3.                   
019300 77      WS-KVLS                 PIC S9(7)      COMP-3.                   
019400 77      WS-KVEFRS               PIC S9(7)      COMP-3.                   
019500 77      WS-KVRESS               PIC S9(7)      COMP-3.                   
019600 77      WS-KART-KVRESS-ART      PIC S9(7)      COMP-3.                   
019700 77      WS-KVAVBART             PIC S9(7)      COMP-3.                   
019800 77      FILLER                  PIC  X(8) VALUE 'JJJJJJJJ'.              
019900 77      WS-IDPGM                PIC X(8).                                
020000 77      WS-KDORDBEK             PIC 9(2).                                
020100     88  VOR-KON                 VALUE 51 52 53 54 55 57 67 92.           
020200 77      WS-IDPRQUES             PIC S9(7)   VALUE ZERO.                  
020300*                                                                         
020400 01  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
020500   88 TRAFF-VORKO                            VALUE 'J'.                   
020600 77      WS-BEART                PIC X(25).                               
020700 77      WS-KDEMBAL              PIC X(1).                                
020800 77      WS-IDKONTO              PIC 9(11).                               
020900 77      WS-IDDISTR-NUM4         PIC 9(4).                                
021000 77      WS-IDKUNDNR-NUM6        PIC 9(6).                                
021100 77      WI-ADPLATS              PIC 9(5)    VALUE ZERO.                  
021200 77      WU-ADPLATS              PIC 9(5)    VALUE ZERO.                  
021300 77      WS-IDDISTR-NUM5         PIC 9(5).                                
021400 77      WS-GMT-IDZON            PIC X(2).                                
021500 77      WS-GMT-FLCOD            PIC X(1).                                
021600 77      WS-KDARTURS             PIC X(2).                                
021700 77      WS-IDPSN                PIC 9(3)    VALUE ZERO.                  
021800 77      WS-HFAK-REF-X10         PIC X(10).                               
021900 77      WS-KV402                PIC S9(4)   VALUE ZERO.                  
022000 77    S28-IDARTNR               PIC 9(9)    VALUE ZERO COMP-3.           
022100 77    S28-IDDC                  PIC X(2)    VALUE SPACE.                 
022200 77    S28-KVVORKO               PIC S9(7)   VALUE ZERO COMP-3.           
022300 77    S28-IDANSK                PIC 9(3)    VALUE ZERO COMP-3.           
022400 77    S28-IDLEVNR               PIC X(5)    VALUE SPACE.                 
022500 77    WS-4002-IDUSER            PIC X(8)    VALUE SPACE.                 
022600 77    WS-KDMFSFOR               PIC 9(1)    VALUE ZERO.                  
022700 77    WS-IDPRODNR               PIC S9(7)   VALUE ZERO COMP-3.           
022800 77    WS-DCS-IDFTG              PIC  X(2)   VALUE SPACE.                 
022900*                                                                         
023000 77  S27-IDDC                    PIC X(2)    VALUE SPACE.                 
023100 77  S27-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
023200*                                                                         
023300 77  WS-PARTNER                  PIC X(1)    VALUE SPACE.                 
023400*                                                                         
023500 01  S27-IDANSK-X.                                                        
023600     03 S27-IDANSK               PIC 9(3).                                
023700                                                                          
023800 01  S27-IDARTNR-X.                                                       
023900     03 S27-IDARTNR              PIC 9(9).                                
024000                                                                          
024100 01  S27-IDDISTR-X.                                                       
024200     03 S27-IDDISTR              PIC 9(4).                                
024300                                                                          
024400 01  S27-IDKUNDNR-X.                                                      
024500     03 S27-IDKUNDNR             PIC 9(6).                                
024600     SKIP2                                                                
024700 01  WS-IDUSER.                                                           
024800     03  FILLER                  PIC X(3)   VALUE SPACES.                 
024900     03  WS-IDANSTNR             PIC X(5)   VALUE SPACES.                 
025000                                                                          
025100 01  WS-DCUSER.                                                           
025200     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
025300     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
025400     03 FILLER                   PIC X(1)   VALUE SPACE.                  
025500                                                                          
025600 01  WS-IDSYSTEM.                                                         
025700     03 WS-IDSYST-1-3            PIC X(3)    VALUE SPACE.                 
025800     03 WS-IDSYST-4              PIC X(1)    VALUE SPACE.                 
025900                                                                          
026000 01  W-2203-X.                                                            
026100     05  W-IDHTYP        PIC X(4)    VALUE '2203'.                        
026200     05  W-IDDC-2203     PIC X(2)    VALUE SPACE.                         
026300     05  FILLER          PIC X(24)   VALUE LOW-VALUE.                     
026400                                                                          
026500                                                                          
026600 01 DB2-LASNING.                                                          
026700     03 FILLER                   PIC X(16)   VALUE                        
026800                                             'WS-DB2-SEKTION'.            
026900     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
027000                                                                          
027100     EJECT                                                                
027200 01 NYCKLAR-TP4TRAN.                                                      
027300     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
027400                                                                          
027500                                                                          
027600 01  TEST-IDDISTR                PIC 9(5)  VALUE ZERO COMP-3.             
027700*    ----DIST20-EMBALLAGE-DISTRIKT----                                    
027800*01  FILLER   -COPY WWDIST20     -RED TEST-IDDISTR.                       
027900                                                                          
028000*    ----DIST79-DEALER-PRICE----                                          
028100*01  FILLER   -COPY WWDIST79     -RED TEST-IDDISTR.                       
028200                                                                          
028300 01  FILLER                      PIC X(8)    VALUE ALL 'C'.               
028400*      --- VALID IDDC CODES                                               
028500*                                                                         
028600*01    -COPY WWDC99                                                       
028700*01    -COPY WWDCKONS                                                     
028800       EJECT                                                              
028900*----> TABELL FÖR ATT ÖVERSÄTTA HF-AK-PLOCK                               
029000                                                                          
029100*   -COPY W413WHFA                                                        
029200     EJECT                                                                
029300 77  ALLT-SW                     PIC X.                                   
029400     88  ALLT-OK                             VALUE 'J'.                   
029500     88  ALLT-FEL                            VALUE 'N'.                   
029600                                                                          
029700 77  LAGOMR-SW                   PIC X.                                   
029800     88  NYTT-LAGEROMRADE                    VALUE 'J'.                   
029900                                                                          
030000 77  ARTIKEL-SW                  PIC X.                                   
030100     88  ARTIKEL-OK                          VALUE 'J'.                   
030200     88  ARTIKEL-FEL                         VALUE 'N'.                   
030300                                                                          
030400 77  PRC-SW                      PIC X.                                   
030500     88  PRC-EJ-OK                           VALUE 'N'.                   
030600     88  PRC-OK                              VALUE 'J'.                   
030700                                                                          
030800 77  ORAD-SW                     PIC X.                                   
030900     88  BIPACKNING                          VALUE '1'.                   
031000     88  WDQ4-RAD                            VALUE '2'.                   
031100                                                                          
031200 77  ODEL-SW                     PIC X.                                   
031300     88  NY-ORDERDEL                         VALUE 'J'.                   
031400                                                                          
031500 77  PLOCK-SW                    PIC X.                                   
031600     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
031700                                                                          
031800 77  SPLIT-SW                    PIC X.                                   
031900     88  SPLITGRANS-OK                       VALUE 'J'.                   
032000                                                                          
032100 77  PLOCKSATS-ETIK-SW           PIC X.                                   
032200     88  PLOCKSATS-ETIK-FINNS                VALUE 'J'.                   
032300     88  PLOCKSATS-ETIK-SAKNAS               VALUE 'N'.                   
032400                                                                          
032500 77  PLOCKSATS-PU-SW             PIC X.                                   
032600     88  PLOCKSATS-PU-FINNS                  VALUE 'J'.                   
032700     88  PLOCKSATS-PU-SAKNAS                 VALUE 'N'.                   
032800                                                                          
032900 77  HF-AK-SW                    PIC X.                                   
033000     88  HF-AK-PLOCK                         VALUE 'J'.                   
033100                                                                          
033200 77  BIPA-SPARR-SW               PIC X.                                   
033300     88  BIPA-SPARR                          VALUE 'J'.                   
033400                                                                          
033500 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
033600     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
033700*                                                                         
033800 77  RENOVA-SW                   PIC X       VALUE 'N'.                   
033900     88  RENOVA                              VALUE 'J'.                   
034000*                                                                         
034100*    PARAMETRAR FÖR EVENT                                                 
034200 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
034300     88 EVENT-OK                             VALUE 'LYNB'                 
034400                                                   'LYNV'                 
034500                                                   'LYND'                 
034600                                                   'LYNK'                 
034700                                                   'TADB'                 
034800                                                   'TADV'                 
034900                                                   'TADD'                 
035000                                                   'TAD '                 
035100                                                   'POLE'                 
035200                                                   'ACC '                 
035300                                                   'APA '                 
035400                                                   'APB '                 
035500                                                   'APC '                 
035600                                                   'APD '                 
035700                                                   'APE '                 
035800                                                   'APF '                 
035900                                                   'APG '                 
036000                                                   'APH '                 
036100                                                   'API '                 
036200                                                   'APJ '                 
036300                                                   'ECOM'.                
036400                                                                          
036500 77  CREATE-EVENT-SW              PIC X(1)   VALUE 'N'.                   
036600     88 CREATE-EVENT                         VALUE 'J'.                   
036700*                                                                         
036800 01  WS-IDEVENTORDREF.                                                    
036900     03 WS-IDDISTR-EVENT         PIC 9(4).                                
037000     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
037100     03 WS-IDORDNR7-EVENT        PIC 9(7).                                
037200     03 WS-TIREGDAT-EVENT        PIC 9(6).                                
037300*                                                                         
037400 01 FILLER                      PIC  X(16) VALUE 'LDC-TAB'.               
037500******************************************************************        
037600*TABELL FÖR ATT SÄTTA TILL LDC WIP-ORDER.                                 
037700******************************************************************        
037800*LDC-GB                                                                   
037900 01 LDC-TABELL.                                                           
038000    03 LDC-TAB-WIP-ORDER  OCCURS 100.                                     
038100       05   LDC-TAB-WIPID               PIC  X(10) VALUE SPACE.           
038200*                                                                         
038300 01    LDC-TAB-IX                       PIC S9(3)   COMP-3.               
038400 01    LDC-TAB-IX-MAX                PIC S9(3) COMP-3 VALUE +100.         
038500******************************************************************        
038600 01  WS-LOPNR-WIPID.                                                      
038700     03 WS-WIP-LOPNR              PIC  9(2).                              
038800     03 WS-WIP-STRECK             PIC  X(1)  VALUE '-'.                   
038900     03 WS-WIPID                  PIC  X(7).                              
039000     EJECT                                                                
039100*                                                                         
039200                                                                          
039300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
039400     88  BILD-4353                           VALUE '4353'.                
039500     88  BILD-4351-4352                      VALUE '4351' '4352'.         
039600     88  GODK-MID                            VALUE '4293' '4351'          
039700                                                   '4352' '4353'          
039800                                                   '4375' '435A'          
039900                                                   '435B'.                
040000     EJECT                                                                
040100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
040200 01  GENERELLA-SUBPROGRAM.                                                
040300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
040400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
040500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
040600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
040700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
040800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
040900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
041000     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
041100 01  ABENDKODER.                                                          
041200     03  FILLER                  PIC X(16) VALUE 'ABENDKODER'.            
041300     03  RKOD-ABEND-WITHOUT-DUMP PIC S9(4) COMP SYNC VALUE   +16.         
041400     03  RKOD-ABEND-WITH-DUMP    PIC S9(4) COMP SYNC VALUE   +33.         
041500     03  RKOD-FELTEXT            PIC X(32) VALUE SPACE.                   
041600                                                                          
041700     EJECT                                                                
041800 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
041900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
042000*01 -COPY WMSGINIT                                                        
042100     EJECT                                                                
042200*    --- PARAMETERS TO W009WAIT                                           
042300 77  1-SECOND                    PIC S9(9)   COMP VALUE +100.             
042400     EJECT                                                                
042500                                                                          
042600 01  GEMENSAMMA-SUBPROGRAM.                                               
042700     03  W411BIPA                PIC X(8)    VALUE 'W411BIPA'.            
042800*        BIPACKNING                                                       
042900     03  W411DEAV                PIC X(8)    VALUE 'W411DEAV'.            
043000*        DEFINITIV AVBOKNING                                              
043100     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
043200*        RANSONERING                                                      
043300     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
043400*        SPÄRR-KONTROLL                                                   
043500     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
043600*        LÄSNING ARTIKELREGISTRET                                         
043700     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
043800*        AVSLUT ORDERRAD                                                  
043900     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
044000*        OMVANDLA LAGEROMRÅDE PLATS                                       
044100     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
044200*        PRISFRÅGA                                                        
044300     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
044400*        PRISFRÅGA                                                        
044500     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
044600*        KONTROLL AV ENHETSLAST                                           
044700     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
044800*        CROSSINDEX ARTILEL                                               
044900     SKIP3                                                                
045000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
045100*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
045200*                                                                         
045300 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
045400 01  FILLER                      PIC X(16)  VALUE 'WDATKONV '.            
045500*   -COPY WDATAREA                                                        
045600     EJECT                                                                
045700 01  FILLER                      PIC X(16)  VALUE 'W411BIPA '.            
045800*   -COPY W411BIPA                                                        
045900     EJECT                                                                
046000 01  FILLER                      PIC X(16)  VALUE 'W411DEAV '.            
046100*   -COPY W411DEAV                                                        
046200     EJECT                                                                
046300 01  FILLER                      PIC X(16)  VALUE 'W411RANS '.            
046400*   -COPY W411RANS                                                        
046500     EJECT                                                                
046600 01  FILLER                      PIC X(16)  VALUE 'W411SPAR '.            
046700*   -COPY W411SPAR                                                        
046800     EJECT                                                                
046900 01  FILLER                      PIC X(16)  VALUE 'W411AREG '.            
047000*   -COPY W411AREG                                                        
047100     EJECT                                                                
047200 01  FILLER                      PIC X(16)  VALUE 'W413AVSR '.            
047300*   -COPY W413AVSR                                                        
047400     EJECT                                                                
047500 01  FILLER                      PIC X(16)  VALUE 'W413ADRS '.            
047600*   -COPY W413ADRS                                                        
047700     EJECT                                                                
047800 01 FILLER                       PIC X(8)   VALUE 'W335PRNO'.             
047900*   -COPY W335PRNO                                                        
048000     EJECT                                                                
048100 01 FILLER                       PIC X(8)   VALUE 'W335PRQU'.             
048200*   -COPY W335PRQU                                                        
048300     EJECT                                                                
048400 01  FILLER                      PIC X(08)   VALUE 'W411LAST'.            
048500*01 -COPY W411LAST                                                        
048600     EJECT                                                                
048700     EJECT                                                                
048800 01  FILLER                      PIC X(16)   VALUE 'W009CIA  '.           
048900*01  -COPY W009CIA                                                        
049000     EJECT                                                                
049100 01  FILLER                      PIC X(16)  VALUE 'SKROTDISTR'.           
049200*   -COPY WWDIST18                                                        
049300 01  FILLER                      PIC X(16)  VALUE 'SDC-DISTRIKT'.         
049400*01 -COPY WWDIST34                                                        
049500 01  FILLER                      PIC X(16)  VALUE 'REFILLDISTR'.          
049600*   -COPY WWDIST35                                                        
049700 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
049800*   -COPY WWDIST57                                                        
049900     EJECT                                                                
050000*    --- MID-AREA                                                         
050100*                                                                         
050200 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
050300                                                                          
050400*01  -COPY W4I37501                                                       
050500     EJECT                                                                
050600*    --- AREOR FÖR MSG-HANTERING                                          
050700*                                                                         
050800 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
050900     SKIP3                                                                
051000*01  -COPY WMSGAREA                                                       
051100     EJECT                                                                
051200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
051300 01  P-TO-P-SW1.                                                          
051400     03  PTOP1-LL                PIC S9(4)   VALUE 28 COMP SYNC.          
051500     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
051600     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
051700     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T375X'.             
051800     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
051900     03  PTOP1-IDTRANS           PIC  X(4)   VALUE '4375'.                
052000     03  PTOP1-KDMFSFOR          PIC  X(1).                               
052100*    03  -COPY W4I37501  -PRE PTOP1-                                      
052200     EJECT                                                                
052300 01  P-TO-P-SW2.                                                          
052400     03  PTOP2-LL                PIC S9(4)   VALUE 36 COMP SYNC.          
052500     03  PTOP2-Z1                PIC  X(1)   VALUE LOW-VALUE.             
052600     03  PTOP2-Z2                PIC  X(1)   VALUE LOW-VALUE.             
052700     03  PTOP2-TRANSKOD          PIC  X(7)   VALUE 'W4T376X'.             
052800     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
052900     03  FILLER                  PIC  X(4)   VALUE '4375'.                
053000     03  PTOP2-KDMFSFOR          PIC  X(1).                               
053100*    03  -COPY W4I37601  -PRE PTOP2-                                      
053200     EJECT                                                                
053300 01  P-TO-P-SW3.                                                          
053400     03  PTOP3-LL                PIC S9(4)   VALUE 34 COMP SYNC.          
053500     03  PTOP3-Z1                PIC  X(1)   VALUE LOW-VALUE.             
053600     03  PTOP3-Z2                PIC  X(1)   VALUE LOW-VALUE.             
053700     03  PTOP3-TRANSKOD          PIC  X(7)   VALUE 'W4T377X'.             
053800     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
053900     03  FILLER                  PIC  X(4)   VALUE '4375'.                
054000     03  PTOP3-KDMFSFOR          PIC  X(1).                               
054100*    03  -COPY W4I37701  -PRE PTOP3-                                      
054200     EJECT                                                                
054300 01  P-TO-P-SW4.                                                          
054400     03  PTOP4-LL                PIC S9(4)   VALUE 44 COMP SYNC.          
054500     03  PTOP4-Z1                PIC  X(1)   VALUE LOW-VALUE.             
054600     03  PTOP4-Z2                PIC  X(1)   VALUE LOW-VALUE.             
054700     03  PTOP4-TRANSKOD          PIC  X(7)   VALUE SPACE.                 
054800     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
054900     03  FILLER                  PIC  X(4)   VALUE '4375'.                
055000     03  PTOP4-KDMFSFOR          PIC  X(1).                               
055100*    03  -COPY W4I37A01  -PRE PTOP4-                                      
055200     EJECT                                                                
055300 01  FILLER                      PIC X(16)   VALUE 'TRANS-AREA '.         
055400 01  4397-TRANSAREA.                                                      
055500     03  4397-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
055600     03  4397-IDANSTNR           PIC 9(5)    VALUE ZERO.                  
055700     03  4397-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
055800     03  4397-IDPURAD            PIC 9(5)    VALUE ZERO.                  
055900     03  FILLER                  PIC X(80)   VALUE SPACE.                 
056000     EJECT                                                                
056100 01  FILLER                      PIC X(16)   VALUE '4397-IO-AREA'.        
056200 01  4397-IO-AREA.                                                        
056300     03  4397-LL                 PIC S9(4)   COMP  SYNC.                  
056400     03  4397-Z1                 PIC X(1).                                
056500     03  4397-Z2                 PIC X(1).                                
056600     03  4397-TRANSKOD           PIC X(8)    VALUE SPACE.                 
056700     03  4397-IDTRANS            PIC X(4)    VALUE SPACE.                 
056800     03  4397-KDMFSFOR           PIC X(1)    VALUE SPACE.                 
056900     03  4397-AREA               PIC X(100)  VALUE SPACE.                 
057000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
057100*                                                                         
057200                                                                          
057300*    --- AREOR FÖR HANTERING AV API                                       
057400*01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-WMSG'.         
057500 01  KOM-IO-AREA.                                                         
057600     03  -COPY WMSGKOM                                                    
057700                                                                          
057800*01  FILLER                      PIC X(16)   VALUE 'Z430-REQU-A '.        
057900*01  -COPY WZ0430I1  -PRE Z430-                                           
058000*    03  -COPY WAPIORD  -RED Z430-REQU-EVENT-DATA -PRE Z430-              
058100                                                                          
058200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
058300     SKIP3                                                                
058400 01  NYCKLAR-TILL-DLI.                                                    
058500                                                                          
058600*----> DIREKTNYCKEL TILL ORDERHUVUD.                                      
058700                                                                          
058800     03  W-IDORDER-X.                                                     
058900         05  W-IDORDER               PIC S9(7)  COMP-3.                   
059000                                                                          
059100*----> SEKUNDÄR INDEX C TILL ORDERHUVUD.                                  
059200                                                                          
059300   03  W-WDQ2CSEQ-X.                                                      
059400       05  W-WDQ2CSEQ-IDGMTREF.                                           
059500         07 W-WDQ2CSEQ-IDDISTR    PIC S9(5) COMP-3 VALUE +0.              
059600         07 W-WDQ2CSEQ-IDKUNDNR   PIC S9(7) COMP-3 VALUE +0.              
059700         07 W-WDQ2CSEQ-IDKUNDRF   PIC X(10)    VALUE SPACE.               
059800*                                                                         
059900   03  W-WDQ2CSEQ.                                                        
060000     05  W-IDDISTR-CSEQ          PIC S9(5)   VALUE ZERO COMP-3.           
060100     05  W-IDKUNDNR-CSEQ         PIC S9(7)   VALUE ZERO COMP-3.           
060200     05  FILLER                  PIC 9(2)    VALUE ZERO.                  
060300     05  W-IDORDNR5-CSEQ         PIC 9(5).                                
060400     05  FILLER                  PIC X(3)    VALUE SPACE.                 
060500*                                                                         
060600*----> DIREKTNYCKEL TILL ORDERHUVUD ARBETSTABELL.                         
060700                                                                          
060800     03  W-IDDC-X.                                                        
060900         05  W-IDDC                  PIC X(2).                            
061000                                                                          
061100*----> DIREKTNYCKEL TILL ORDERDEL.                                        
061200                                                                          
061300     03  W-WDQ301KY-X.                                                    
061400         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
061500         05  W-Q301KY-IDDC           PIC X(2).                            
061600         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
061700         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
061800                                                                          
061900     03  W-WDQ301KY-MIN.                                                  
062000         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
062100         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
062200         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
062300                                                                          
062400     03  W-WDQ301KY-MAX.                                                  
062500         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
062600         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
062700         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
062800                                                                          
062900     03  W-KDODELST                  PIC X.                               
063000                                                                          
063100*----> DIREKTNYCKEL TILL ORDERRAD.                                        
063200                                                                          
063300     03  W-WDQ401KY-MIN-X.                                                
063400         05  W-Q401KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
063500         05  W-Q401KY-MIN-IDDC       PIC X(2).                            
063600         05  W-Q401KY-MIN-ADLAGOMR   PIC S9(3)  COMP-3.                   
063700         05  W-Q401KY-MIN-ADGANG     PIC S9(3)  COMP-3.                   
063800         05  W-Q401KY-MIN-ADPLATS    PIC S9(5)  COMP-3.                   
063900         05  W-Q401KY-MIN-IDARTNR    PIC S9(9)  COMP-3.                   
064000         05  W-Q401KY-MIN-IDLOPNR    PIC S9(3)  COMP-3.                   
064100                                                                          
064200     03  W-WDQ401KY-MAX-X.                                                
064300         05  W-Q401KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
064400         05  W-Q401KY-MAX-IDDC       PIC X(2).                            
064500         05  W-Q401KY-MAX-ADLAGOMR   PIC S9(3)  COMP-3.                   
064600         05  W-Q401KY-MAX-ADGANG     PIC S9(3)  COMP-3.                   
064700         05  W-Q401KY-MAX-ADPLATS    PIC S9(5)  COMP-3.                   
064800         05  W-Q401KY-MAX-IDARTNR    PIC S9(9)  COMP-3.                   
064900         05  W-Q401KY-MAX-IDLOPNR    PIC S9(3)  COMP-3.                   
065000                                                                          
065100*----> PRC-KANALEN.                                                       
065200                                                                          
065300     03  W-4447-IDHTYP-X.                                                 
065400         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
065500         05  W-4447-IDDC         PIC  X(02).                              
065600         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
065700                                                                          
065800     03  W-4448-IDPRC-X.                                                  
065900         05  W-4448-IDPRC        PIC  X(04).                              
066000         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
066100                                                                          
066200*----> PRC-PRINTERTABELL.                                                 
066300                                                                          
066400     03  W-4453-IDHTYP-X.                                                 
066500         05  W-4453-IDHTYP       PIC  X(04) VALUE '4453'.                 
066600         05  W-4453-IDDC         PIC  X(02).                              
066700         05  W-4453-IDPRC        PIC  X(04).                              
066800         05  W-4453-LOW-VALUE    PIC  X(20) VALUE LOW-VALUE.              
066900                                                                          
067000     03  W-4454-KDSEGKEY-X       PIC  X(01) VALUE '1'.                    
067100                                                                          
067200*----> PLOCKSATS FRÅN 4351-4353.                                          
067300                                                                          
067400     03  W-4001-IDHTYP-X.                                                 
067500         05  W-4001-IDHTYP       PIC  X(04) VALUE '4001'.                 
067600         05  W-4001-IDPRODNR     PIC  9(07).                              
067700         05  W-4001-IDPLKLST     PIC  9(03).                              
067800         05  FILLER              PIC  X(16) VALUE LOW-VALUE.              
067900                                                                          
068000*----> PLOCKSATS TILL ETIKETTPROGRAM.                                     
068100                                                                          
068200     03  W-4003-ETIK-IDHTYP-X.                                            
068300         05  W-4003-ETIK-IDHTYP   PIC  X(04) VALUE '4003'.                
068400         05  W-4003-ETIK-IDPRODNR PIC  9(07).                             
068500         05  W-4003-ETIK-IDPLKLST PIC  9(03).                             
068600         05  FILLER               PIC  X(16) VALUE LOW-VALUE.             
068700                                                                          
068800*----> PLOCKSATS TILL PACKUNDERLAGSPROGRAM.                               
068900                                                                          
069000     03  W-4007-PU-IDHTYP-X.                                              
069100         05  W-4007-PU-IDHTYP     PIC  X(04) VALUE '4007'.                
069200         05  W-4007-PU-IDPRODNR   PIC  9(07).                             
069300         05  W-4007-PU-IDPLKLST   PIC  9(03).                             
069400         05  FILLER               PIC  X(16) VALUE LOW-VALUE.             
069500                                                                          
069600*----> ARTIKELREGISTER WDK6 ,K7 & K9                                      
069700                                                                          
069800     03  W-IDARTNR-X.                                                     
069900         05  W-IDARTNR           PIC  S9(9) COMP-3.                       
070000                                                                          
070100     03  W-DASKROT-X.                                                     
070200         05  W-DASKROT           PIC   9(8).                              
070300                                                                          
070400     03  W-IDLAND-X.                                                      
070500         05  W-IDLAND            PIC   X(2).                              
070600                                                                          
070700*----> ARTIKELREGISTER WDK6 CDC                                           
070800                                                                          
070900     03  W-WDK611-KDSEGKEY-X.                                             
071000         05  W-WDK611-KDSEGKEY   PIC   X      VALUE '1'.                  
071100                                                                          
071200*----> ARTIKELREGISTER WDD5                                               
071300                                                                          
071400     03  W-ART-IDARTNR-X.                                                 
071500         05  W-ART-IDARTNR       PIC  S9(9) COMP-3.                       
071600                                                                          
071700*----> ORDERBEKRÄFTELSE                                                   
071800                                                                          
071900     03  W-WDQ101KY-MIN-X.                                                
072000         05  W-Q101KY-MIN-IDORDER  PIC  S9(7) COMP-3.                     
072100         05  W-Q101KY-MIN-IDARTNR  PIC  S9(9) COMP-3.                     
072200         05  W-Q101KY-MIN-IDLOPNR  PIC  S9(3) COMP-3.                     
072300         05  W-Q101KY-MIN-IDSEKVNR PIC  S9(3) COMP-3.                     
072400         05  W-Q101KY-MIN-FILLER   PIC   X(4) VALUE LOW-VALUE.            
072500                                                                          
072600     03  W-WDQ101KY-MAX-X.                                                
072700         05  W-Q101KY-MAX-IDORDER  PIC  S9(7) COMP-3.                     
072800         05  W-Q101KY-MAX-IDARTNR  PIC  S9(9) COMP-3.                     
072900         05  W-Q101KY-MAX-IDLOPNR  PIC  S9(3) COMP-3.                     
073000         05  W-Q101KY-MAX-IDSEKVNR PIC  S9(3) COMP-3.                     
073100         05  W-Q101KY-MAX-FILLER   PIC   X(4) VALUE HIGH-VALUE.           
073200                                                                          
073300*----> RESTORDER                                                          
073400*                                                                         
073500     03  W-WDA501KY-X.                                                    
073600         05  W-A501KY-IDDISTR    PIC  S9(5) COMP-3.                       
073700         05  W-A501KY-IDKUNDNR   PIC  S9(7) COMP-3.                       
073800         05  W-A501KY-IDKUNDRF   PIC  X(10).                              
073900         05  W-A501KY-IDARTNR    PIC  S9(9) COMP-3.                       
074000         05  W-A501KY-IDLOPNR    PIC  S9(3) COMP-3.                       
074100*                                                                         
074200   03  W-WDA501KY-A5-MIN-X.                                               
074300       05  W-IDDISTR-A5-MIN          PIC S9(5) VALUE ZERO COMP-3.         
074400       05  W-IDKUNDNR-A5-MIN         PIC S9(7) VALUE ZERO COMP-3.         
074500       05  FILLER                    PIC X(17) VALUE LOW-VALUE.           
074600                                                                          
074700   03  W-WDA501KY-A5-MAX-X.                                               
074800       05  W-IDDISTR-A5-MAX          PIC S9(5) VALUE ZERO COMP-3.         
074900       05  W-IDKUNDNR-A5-MAX         PIC S9(7) VALUE ZERO COMP-3.         
075000       05  FILLER                    PIC X(17) VALUE HIGH-VALUE.          
075100*                                                                         
075200   03  W-KDORDKL-X.                                                       
075300       05  W-KDORDKL                 PIC S9    VALUE ZERO COMP-3.         
075400                                                                          
075500   03    W-IDKUNDRF-LEV-X.                                                
075600     05  W-IDKUNDRF-LEV              PIC X(10)   VALUE SPACE.             
075700*                                                                         
075800                                                                          
075900*----> PRIORITETSSTYRNING FÖR RESTORDER                                   
076000                                                                          
076100     03  W-4511-IDHTYP-X.                                                 
076200         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
076300         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
076400                                                                          
076500     03  W-4512-KDTPOTYP-X.                                               
076600         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
076700     03  W-4512-KDORDKL-X.                                                
076800         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
076900     03  W-4512-IDDISTR-FOM-X.                                            
077000         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
077100     03  W-4512-IDDISTR-TOM-X.                                            
077200         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
077300                                                                          
077400*----> SEKUNDÄR INDEX ARTIKELBENÄMNING                                    
077500                                                                          
077600     03  FILLER                  PIC X(8)    VALUE ALL 'B'.               
077700     03  W-WDD3BSEQ-X.                                                    
077800         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
077900                                                                          
078000     03  W-IDSKYLT-X             PIC X(3).                                
078100                                                                          
078200*----> FÖRRÅDSDATATEXT/EMBALLAGEKOD                                       
078300                                                                          
078400     03  W-4535-IDHTYP-X.                                                 
078500         05  W-4535-IDHTYP       PIC  X(4)  VALUE '4535'.                 
078600         05  W-4535-KDFDKRAV     PIC S9(3)  COMP-3.                       
078700         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
078800                                                                          
078900     03  FILLER                  PIC X(8)    VALUE ALL 'C'.               
079000     03  W-4536-IDSKYLT-X.                                                
079100         05  W-4536-IDSKYLT      PIC  X(3).                               
079200         05  W-4536-LOW-VALUE    PIC  X(2)  VALUE LOW-VALUE.              
079300                                                                          
079400*----> RESERVERAT/KAMPANJ                                                 
079500                                                                          
079600     03  W-WDM201-X.                                                      
079700         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
079800         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
079900                                                                          
080000     03  W-WDM211-X.                                                      
080100         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
080200                                                                          
080300     03  W-WDM221-X.                                                      
080400         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
080500         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
080600         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
080700         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
080800                                                                          
080900*----> LAGRA LDC-GB WIPID-TABELL                                          
081000                                                                          
081100     03  W-4017-IDHTYP-X.                                                 
081200         05  W-4017-IDHTYP       PIC  X(4)  VALUE '4017'.                 
081300         05  W-4017-IDPRODNR     PIC  9(7).                               
081400         05  W-4017-IDPLKLST     PIC  9(3).                               
081500         05  W-4017-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
081600                                                                          
081700     03  W-4018-KDSEGKEY-X.                                               
081800         05  W-4018-KDSEGKEY     PIC   X      VALUE '1'.                  
081900                                                                          
082000*----> KUND-REG                                                           
082100                                                                          
082200     03  W-IDGMT-X.                                                       
082300         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
082400         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
082500*                                                                         
082600     03  W-IDGMT-MIN-X.                                                   
082700         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
082800         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
082900*                                                                         
083000     03  W-IDGMT-MAX-X.                                                   
083100         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
083200         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
083300                                                                          
083400     03  W-WDB101KY-X.                                                    
083500         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
083600         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
083700     EJECT                                                                
083800     SKIP2                                                                
083900   03    W-WDA601KY-MIN-X.                                                
084000     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
084100     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
084200     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
084300     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
084400     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
084500     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
084600     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
084700     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
084800     SKIP2                                                                
084900   03    W-WDA601KY-MAX-X.                                                
085000     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
085100     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
085200     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
085300     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
085400     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
085500     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
085600     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
085700     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
085800*                                                                         
085900   03    W-WDA6BSEQ-MIN-X.                                                
086000     05    W-A6BSEQ-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
086100     05    W-A6BSEQ-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
086200     05    W-A6BSEQ-MIN-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
086300     05    W-A6BSEQ-MIN-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
086400     05    W-A6BSEQ-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
086500     05    FILLER                    PIC X(14) VALUE SPACE.               
086600     SKIP2                                                                
086700   03    W-WDA6BSEQ-MAX-X.                                                
086800     05    W-A6BSEQ-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
086900     05    W-A6BSEQ-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
087000     05    W-A6BSEQ-MAX-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
087100     05    W-A6BSEQ-MAX-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
087200     05    W-A6BSEQ-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
087300     05    FILLER                    PIC X(14) VALUE SPACE.               
087400     SKIP2                                                                
087500   03  W-IDDC-B6-X.                                                       
087600     05    W-IDDC-B6                 PIC X(2).                            
087700                                                                          
087800   03  W-IDPRC-X.                                                         
087900     05    W-IDPRC                   PIC X(4).                            
088000                                                                          
088100   03  W-ADLAGOMR-X.                                                      
088200     05  W-ADLAGOMR          PIC  S9(3)    COMP-3.                        
088300                                                                          
088400   03  W-IDDISTR-P4-X.                                                    
088500     05   W-IDDISTR-P4             PIC S9(5)   VALUE ZERO  COMP-3.        
088600*                                                                         
088700                                                                          
088800 01  FILLER                     PIC X(16) VALUE 'ALT5-IO-AREA'.           
088900 01  ALT5-IO-AREA.                                                        
089000                                                                          
089100  03     ALT5-LL                 PIC S9(4) COMP SYNC.                     
089200  03     ALT5-Z1                 PIC X(1)  VALUE LOW-VALUE.               
089300  03     ALT5-Z2                 PIC X(1)  VALUE LOW-VALUE.               
089400  03     ALT5-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
089500  03     ALT5-IDTRANS            PIC X(4)  VALUE '4375'.                  
089600  03     ALT5-SPRAK              PIC X(1).                                
089700* 03     MID -COPY W2I19101   -PRE ALT5-                                  
089800     EJECT                                                                
089900     03  W-4541-IDHTYP-X.                                                 
090000         05  W-4541-IDHTYP       PIC  X(4)  VALUE '4541'.                 
090100         05  W-4541-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
090200                                                                          
090300*    --- STATUS-KOD FRÅN IMS                                              
090400 01  STATUS-WS-Q221              PIC XX.                                  
090500     88  Q221-SEG-FINNS          VALUE '  '.                              
090600     88  Q221-SEG-SAKNAS         VALUE 'GE'.                              
090700                                                                          
090800 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
090900                                                                          
091000 01  STATUS-WS                   PIC  X(02).                              
091100     88  SEGMENT-FINNS                       VALUE '  '.                  
091200     88  ISRT-OK                             VALUE '  '.                  
091300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
091400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
091500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
091600     88  END-OF-DATA                         VALUE 'GB'.                  
091700     SKIP2                                                                
091800 01  GODK-STATUSKODER.                                                    
091900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
092000     SKIP3                                                                
092100 01  SSA1                        PIC X(200).                              
092200 01  SSA2                        PIC X(128).                              
092300 01  SSA3                        PIC X(128).                              
092400 01  SSA4                        PIC X(128).                              
092500     EJECT                                                                
092600*************************                                                 
092700*  ARBETSAREA RYE-TRANS *                                                 
092800*************************                                                 
092900 01  W-RYEPOST.                                                           
093000*    03  -COPY WDGZRYE    -PRE W-                                         
093100     EJECT                                                                
093200 01  W-SORTPOST.                                                          
093300*    03  -COPY WDGZRYES   -PRE W-                                         
093400     EJECT                                                                
093500*************************                                                 
093600*  ARBETSAREA RYX-TRANS *                                                 
093700*************************                                                 
093800 01  W-RYXPOST.                                                           
093900*    03  -COPY WDGZRYX    -PRE W-                                         
094000     EJECT                                                                
094100 01  W-RYX-SORTPOST.                                                      
094200*    03  -COPY WDGZRYXS   -PRE W-                                         
094300     EJECT                                                                
094400*************************                                                 
094500*  ARBETSAREA RYK-TRANS *                                                 
094600*************************                                                 
094700 01  W-RYKPOST.                                                           
094800*    03  -COPY WDGZRYK    -PRE W-                                         
094900     EJECT                                                                
095000***************************                                               
095100*  ARBETSAREA ORDERDELEN  *                                               
095200***************************                                               
095300 01  W-ORDERDEL.                                                          
095400*    03  -COPY WDQ301     -PRE W-                                         
095500     EJECT                                                                
095600******************************                                            
095700*  SPAR-AREA PLOCKSATS ETIK  *                                            
095800******************************                                            
095900 01  W-ETIK-PLOCKSATS.                                                    
096000*    03  -COPY WDGX4004   -PRE W-ETIK-                                    
096100     EJECT                                                                
096200*                            DB2 FUNKTIONSKODER                           
096300 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
096400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
096500                                                                          
096600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
096700 01  DB2-WS.                                                              
096800     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
096900         88  CURSOR-OK                       VALUE 000.                   
097000         88  RADER-FINNS                     VALUE 000.                   
097100         88  RADER-SAKNAS                    VALUE 100.                   
097200         88  ATKOMST-FEL                     VALUE 904.                   
097300     03  GODK-SQLCODEKODER.                                               
097400         05  GODK-SQLCODE OCCURS 5                                        
097500             INDEXED BY SQLCODE-IX PIC 9(3).                              
097600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
097700     EJECT                                                                
097800*    --- IMS FUNKTIONSKODER                                               
097900*01  -COPY W0003                                                          
098000     EJECT                                                                
098100*    ---  DLI INPUT-OUTPUT AREA                                           
098200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
098300     SKIP3                                                                
098400 01  DLI-IO-AREA.                                                         
098500     03  IO-AREA                 PIC X(1500) VALUE SPACE.                 
098600     SKIP3                                                                
098700     03  WLORQA01 REDEFINES IO-AREA.                                      
098800*        05  -COPY WDQ301                                                 
098900     EJECT                                                                
099000     03  WLXXKH01 REDEFINES IO-AREA.                                      
099100*        05  -COPY WDGX4447                                               
099200     EJECT                                                                
099300     03  WLXXKH11 REDEFINES IO-AREA.                                      
099400*        05  -COPY WDGX4448                                               
099500     EJECT                                                                
099600     03  WLORQM01 REDEFINES IO-AREA.                                      
099700*        05  -COPY WDQ101                                                 
099800     EJECT                                                                
099900     03  WLORDP01 REDEFINES IO-AREA.                                      
100000*        05  -COPY WDA501                                                 
100100     EJECT                                                                
100200     03  WDK611 REDEFINES IO-AREA.                                        
100300*        05  -COPY WDK611                                                 
100400     EJECT                                                                
100500     03  WDK627 REDEFINES IO-AREA.                                        
100600*        05  -COPY WDK627                                                 
100700     EJECT                                                                
100800     03  WLXXJN11 REDEFINES IO-AREA.                                      
100900*        05  -COPY WDGX4512                                               
101000     EJECT                                                                
101100     03  WLBENA11 REDEFINES IO-AREA.                                      
101200*        05  -COPY WDD311                                                 
101300     EJECT                                                                
101400     03  WLXXKU01 REDEFINES IO-AREA.                                      
101500*        05  -COPY WDGX4535                                               
101600     EJECT                                                                
101700     03  WLXXKU11 REDEFINES IO-AREA.                                      
101800*        05  -COPY WDGX4536                                               
101900     EJECT                                                                
102000     03  WLZZAC01 REDEFINES IO-AREA.                                      
102100*        05  -COPY WDG601                                                 
102200     EJECT                                                                
102300     03  WL454111 REDEFINES IO-AREA.                                      
102400*        05  -COPY WDGX4542                                               
102500     EJECT                                                                
102600     03  WLARTM01 REDEFINES IO-AREA.                                      
102700*        05  -COPY WDK901                                                 
102800     EJECT                                                                
102900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
103000     SKIP3                                                                
103100 01  DLI-IO-AREA1.                                                        
103200*    03  -COPY WDQ201                                                     
103300     EJECT                                                                
103400*    03  -COPY WDQ212                                                     
103500     EJECT                                                                
103600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ212'.         
103700     SKIP3                                                                
103800 01  DLI-IO-WDQ212.                                                       
103900*    03  -COPY WDQ212     -PRE Q212-                                      
104000     EJECT                                                                
104100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ221'.         
104200 01  DLI-IO-WDQ221.                                                       
104300*    03 -COPY WDQ221                                                      
104400     EJECT                                                                
104500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
104600 01  DLI-IO-WDM211.                                                       
104700*    03 -COPY WDM211                                                      
104800     EJECT                                                                
104900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
105000 01  DLI-IO-WDM221.                                                       
105100*    03 -COPY WDM221                                                      
105200     EJECT                                                                
105300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
105400 01  DLI-IO-AREA4.                                                        
105500*    03  -COPY WDQ401                                                     
105600     EJECT                                                                
105700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
105800 01  DLI-IO-AREA5.                                                        
105900*    03  -COPY WDGX4454                                                   
106000     EJECT                                                                
106100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
106200 01  DLI-IO-AREA6.                                                        
106300     03  IO-AREA6                PIC X(1440).                             
106400     03  WL400101 REDEFINES IO-AREA6.                                     
106500*        05  -COPY WDGX4001                                               
106600     EJECT                                                                
106700     03  WL400111 REDEFINES IO-AREA6.                                     
106800*        05  -COPY WDGX4002                                               
106900     EJECT                                                                
107000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
107100 01  DLI-IO-AREA7.                                                        
107200     03  WL400311.                                                        
107300*        05  -COPY WDGX4004                                               
107400     EJECT                                                                
107500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
107600 01  DLI-IO-AREA8.                                                        
107700     03 IO-AREA8                 PIC X(160).                              
107800     03  WL400301 REDEFINES IO-AREA8.                                     
107900*        05  -COPY WDGX4003  -PRE ETIK-                                   
108000     EJECT                                                                
108100     03  WL400321 REDEFINES IO-AREA8.                                     
108200*        05  -COPY WDGX4006                                               
108300     EJECT                                                                
108400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA9'.        
108500 01  DLI-IO-AREA9.                                                        
108600     03  WL400711.                                                        
108700*        05  -COPY WDGX4008                                               
108800     EJECT                                                                
108900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA10'.        
109000 01  DLI-IO-AREA10.                                                       
109100     03 IO-AREA10                PIC X(999).                              
109200     03  WL400701 REDEFINES IO-AREA10.                                    
109300*        05  -COPY WDGX4007  -PRE PU-                                     
109400     EJECT                                                                
109500     03  WL400721 REDEFINES IO-AREA10.                                    
109600*        05  -COPY WDGX4010                                               
109700     EJECT                                                                
109800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA11'.        
109900 01  DLI-IO-AREA11.                                                       
110000*    03  -COPY WDQ201        -PRE CSQ-                                    
110100     EJECT                                                                
110200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA12'.        
110300 01  DLI-IO-AREA12.                                                       
110400*    03  -COPY WDD501                                                     
110500     EJECT                                                                
110600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA13'.        
110700 01  DLI-IO-AREA13.                                                       
110800*    03  -COPY WDK711                                                     
110900     EJECT                                                                
111000 01  DLI-IO-WDK712.                                                       
111100*    03  -COPY WDK712                                                     
111200     EJECT                                                                
111300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK722'.        
111400 01  DLI-IO-WDK722.                                                       
111500*    03  -COPY WDK722                                                     
111600     EJECT                                                                
111700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA17'.        
111800 01    DLI-IO-AREA17.                                                     
111900*    03  -COPY WDA601                                                     
112000     EJECT                                                                
112100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDG302'.        
112200 01  DLI-IO-WDG302.                                                       
112300*    03  -COPY WDGX2204                                                   
112400     EJECT                                                                
112500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA18'.        
112600 01    DLI-IO-AREA18.                                                     
112700*    03  -COPY WDK601                                                     
112800     EJECT                                                                
112900 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
113000 01  DLI-IO-WLLOGA01.                                                     
113100*    03  WLLOGA01  -COPY WDL901                                           
113200     EJECT                                                                
113300 01  FILLER                      PIC X(16)  VALUE '4017-AREA'.            
113400*01  -COPY WDGX4017                                                       
113500     EJECT                                                                
113600 01  FILLER                      PIC X(16)  VALUE '4018-AREA'.            
113700*01  -COPY WDGX4018                                                       
113800     EJECT                                                                
113900 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
114000 01  DLI-IO-AREA-WDB201.                                                  
114100     03  WLGMTA01.                                                        
114200*        05  -COPY WDB201                                                 
114300     EJECT                                                                
114400 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
114500 01  DLI-IO-AREA-WDB101.                                                  
114600     03  WLBETC01.                                                        
114700         05  -COPY WDB101                                                 
114800                                                                          
114900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
115000 01   DLI-IO-AREA-B601.                                                   
115100*     03  -COPY WDB601                                                    
115200                                                                          
115300 01  FILLER               PIC X(16)   VALUE 'WDP4A1 AREA'.                
115400 01   DLI-IO-AREA-WDP4A1.                                                 
115500*     03  -COPY WDP4A1                                                    
115600     EJECT                                                                
115700 01  FILLER                  PIC X(24) VALUE 'SEND-CONTROL-PRQU'.         
115800     SKIP3                                                                
115900 01  SEND-AREA-CONTROL-PRQU.                                              
116000*    03  -COPY WZ01SEND  -PRE PRQU-                                       
116100     SKIP3                                                                
116200 01  FILLER                  PIC X(24) VALUE 'SEND-AREA-PRQU'.            
116300 01  SEND-AREA-PRQU.                                                      
116400*    03  -COPY WZ01REQU  -PRE PRQU-                                       
116500*    03  -COPY W30391I1  -PRE 3039-                                       
116600     EJECT                                                                
116700 01  FILLER                  PIC X(24) VALUE 'SEND-CONTROL-TACD'.         
116800 01  SEND-AREA-CONTROL-TACD.                                              
116900*    03  -COPY WZ01SEND  -PRE TACD-                                       
117000     SKIP3                                                                
117100 01  FILLER                  PIC X(16) VALUE 'SEND-AREA-TACD'.            
117200******************************                                            
117300*  AREA OBKR INFO TILL TACDIS*                                            
117400******************************                                            
117500*01  FILLER                      PIC X(16)   VALUE '*W402TACD**'.         
117600 01  SEND-AREA-TACD.                                                      
117700*    03  -COPY W402TACD                                                   
117800     EJECT                                                                
117900 01  FILLER                  PIC X(16) VALUE 'HDR-AREA-TACD'.             
118000 01  HDR-AREA-TACD.                                                       
118100*    03  -COPY WZ01REQU  -PRE TACD-                                       
118200*    03  -COPY WZ04HDR   -PRE TACD-                                       
118300                                                                          
118400 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
118500                                                                          
118600*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
118700     EJECT                                                                
118800     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
118900     EJECT                                                                
119000 LINKAGE SECTION.                                                         
119100                                                                          
119200*01  -COPY W0009      -PRE MSG-                                           
119300     EJECT                                                                
119400 01  0693-PCB          PIC X.                                             
119500     EJECT                                                                
119600*01  -COPY W0009      -PRE ALT1-                                          
119700     EJECT                                                                
119800*01  -COPY W0009      -PRE ALT2-                                          
119900     EJECT                                                                
120000*01  -COPY W0009      -PRE ALT3-                                          
120100     EJECT                                                                
120200 01  AVSR-ALT4-PCB               PIC X.                                   
120300     EJECT                                                                
120400*01  -COPY W0009      -PRE ALT5-                                          
120500     EJECT                                                                
120600*01  -COPY W0009      -PRE ALT6-                                          
120700     EJECT                                                                
120800*01  -COPY W0009      -PRE ALT7-                                          
120900     EJECT                                                                
121000*01  -COPY W0009      -PRE 4397-                                          
121100     EJECT                                                                
121200*01  -COPY W0009      -PRE PRQRY-                                         
121300     05  FILLER                  PIC X.                                   
121400*01  -COPY W0009      -PRE DISTRDOC-                                      
121500     05  FILLER                  PIC X.                                   
121600     SKIP2                                                                
121700*01  -COPY W0009      -PRE USEA-                                          
121800     EJECT                                                                
121900*01  -COPY W0008      -PRE ORQA-                                          
122000     05  FILLER                  PIC X.                                   
122100     EJECT                                                                
122200*01  -COPY W0008      -PRE ORQI2-                                         
122300     05  FILLER                  PIC X.                                   
122400     EJECT                                                                
122500*01  -COPY W0008      -PRE ORQI-                                          
122600     05  FILLER                  PIC X.                                   
122700     EJECT                                                                
122800*01  -COPY W0008      -PRE XXKH-                                          
122900     05  FILLER                  PIC X.                                   
123000     EJECT                                                                
123100*01  -COPY W0008      -PRE XXKL-                                          
123200     05  FILLER                  PIC X.                                   
123300     EJECT                                                                
123400*01  -COPY W0008      -PRE 4001-                                          
123500     05  FILLER                  PIC X.                                   
123600     EJECT                                                                
123700*01  -COPY W0008      -PRE ORQF-                                          
123800     05  FILLER                  PIC X.                                   
123900     EJECT                                                                
124000*01  -COPY W0008      -PRE ORQM-                                          
124100     05  FILLER                  PIC X.                                   
124200     EJECT                                                                
124300*01  -COPY W0008      -PRE ORDP-                                          
124400     05  FILLER                  PIC X.                                   
124500     EJECT                                                                
124600*01  -COPY W0008      -PRE XXJN-                                          
124700     05  FILLER                  PIC X.                                   
124800     EJECT                                                                
124900*01  -COPY W0008      -PRE WDK6-                                          
125000     05  FILLER                  PIC X.                                   
125100     EJECT                                                                
125200*01  -COPY W0008      -PRE BENA-                                          
125300     05  FILLER                  PIC X.                                   
125400     EJECT                                                                
125500*01  -COPY W0008      -PRE XXKU-                                          
125600     05  FILLER                  PIC X.                                   
125700     EJECT                                                                
125800*01  -COPY W0008      -PRE ZZAC-                                          
125900     05  FILLER                  PIC X.                                   
126000     EJECT                                                                
126100*01  -COPY W0008      -PRE WDM2-                                          
126200     05  FILLER                  PIC X.                                   
126300     EJECT                                                                
126400*01  -COPY W0008      -PRE 4541-                                          
126500     05  FILLER                  PIC X.                                   
126600     EJECT                                                                
126700*01  -COPY W0008      -PRE ARTM-                                          
126800     05  FILLER                  PIC X.                                   
126900     EJECT                                                                
127000*01  -COPY W0008      -PRE 4003-                                          
127100     05  FILLER                  PIC X.                                   
127200     EJECT                                                                
127300*01  -COPY W0008      -PRE 4007-                                          
127400     05  FILLER                  PIC X.                                   
127500     EJECT                                                                
127600*01  -COPY W0008      -PRE ORQICSQ-                                       
127700     05  FILLER                  PIC X.                                   
127800     EJECT                                                                
127900*01  -COPY W0008      -PRE ARTN-                                          
128000     05  FILLER                  PIC X.                                   
128100     EJECT                                                                
128200*01  -COPY W0008      -PRE GMTA-                                          
128300     05  FILLER                  PIC X.                                   
128400     EJECT                                                                
128500*01  -COPY W0008      -PRE WDK7-                                          
128600     05  FILLER                  PIC X.                                   
128700     EJECT                                                                
128800*01  -COPY W0008  -PRE WLLOGA-                                            
128900     05  FILLER                  PIC X.                                   
129000     EJECT                                                                
129100*01  -COPY W0008  -PRE 4017-                                              
129200     05  FILLER                  PIC X.                                   
129300     EJECT                                                                
129400*01  -COPY W0008      -PRE WDB2-                                          
129500     05  FILLER                  PIC X.                                   
129600     EJECT                                                                
129700*01  -COPY W0008      -PRE WDB1-                                          
129800     05  FILLER                  PIC X.                                   
129900     EJECT                                                                
130000*01  -COPY W0008      -PRE WDB6-                                          
130100     05  FILLER                  PIC X.                                   
130200     EJECT                                                                
130300*01  -COPY W0008      -PRE WDK6-2-                                        
130400     05  FILLER                  PIC X.                                   
130500     EJECT                                                                
130600*01  -COPY W0008      -PRE WDA6B-                                         
130700     05  FILLER                  PIC X.                                   
130800     EJECT                                                                
130900*01  -COPY W0008      -PRE WDA6-                                          
131000     05  FILLER                  PIC X.                                   
131100     EJECT                                                                
131200*01  -COPY W0008      -PRE WDP4A-                                         
131300     05  FILLER                  PIC X.                                   
131400     EJECT                                                                
131500*01  -COPY W0008      -PRE WDA5-                                          
131600     05  FILLER                  PIC X.                                   
131700*                                                                         
131800*01  -COPY W0008      -PRE WDQ2-                                          
131900     05  FILLER                  PIC X.                                   
132000*                                                                         
132100                                                                          
132200*-------  PROGRAM W411BIPA.                                               
132300                                                                          
132400 01  BIPA-ORDP-PCB               PIC X(1).                                
132500 01  BIPA-WDB6-PCB               PIC X(1).                                
132600                                                                          
132700 01  BIPA-WDK6-PCB               PIC X(1).                                
132800                                                                          
132900 01  BIPA-LEVF-PCB               PIC X(1).                                
133000 01  BIPA-LEVG-PCB               PIC X(1).                                
133100 01  BIPA-WDF8-PCB               PIC X(1).                                
133200 01  BIPA-WDF8A-PCB              PIC X(1).                                
133300 01  BIPA-LEVA-PCB               PIC X(1).                                
133400 01  BIPA-ARTS2-PCB              PIC X(1).                                
133500     EJECT                                                                
133600*----> SUBPROGRAM W411DEAV.                                               
133700                                                                          
133800 01  DEAV-ARTM-PCB               PIC X(1).                                
133900                                                                          
134000 01  DEAV-WDK7-PCB               PIC X(1).                                
134100                                                                          
134200 01  DEAV-WDB6-PCB               PIC X(1).                                
134300                                                                          
134400 01  KVAN-WDB2-PCB               PIC X(1).                                
134500                                                                          
134600 01  DEAV-WDL7-PCB               PIC X(1).                                
134700                                                                          
134800 01  DEAV-WDK72-PCB              PIC X(1).                                
134900                                                                          
135000 01  DEAV-WDR2-PCB               PIC X(1).                                
135100                                                                          
135200 01  DEAV-WDR5-PCB               PIC X(1).                                
135300                                                                          
135400 01  DEAV-WDC1-PCB               PIC X(1).                                
135500                                                                          
135600     EJECT                                                                
135700*----> SUBPROGRAM W411RANS.                                               
135800                                                                          
135900 01  RANS-XXKM-PCB               PIC X(1).                                
136000                                                                          
136100 01  RANS-ARTM-PCB               PIC X(1).                                
136200                                                                          
136300 01  RANS-ARTS-PCB               PIC X(1).                                
136400     EJECT                                                                
136500*----> SUBPROGRAM W411AREG.                                               
136600                                                                          
136700 01  AREG-WDK6-PCB               PIC X(1).                                
136800 01  AREG-WDK7-PCB               PIC X(1).                                
136900     EJECT                                                                
137000*----> SUBPROGRAM W413AVSR.                                               
137100                                                                          
137200 01  AVSR-ORQI-PCB               PIC X(1).                                
137300 01  AVSR-GMTB-PCB               PIC X(1).                                
137400 01  AVSR-GMTC-PCB               PIC X(1).                                
137500 01  AVSR-WDB2-PCB               PIC X(1).                                
137600 01  AVSR-WDB6-PCB               PIC X(1).                                
137700 01  TRAN-XXKB-PCB               PIC X(1).                                
137800                                                                          
137900     EJECT                                                                
138000*----> SUBPROGRAM W413SPAR.                                               
138100                                                                          
138200 01  SPAR-WDF8-PCB               PIC X(1).                                
138300 01  SPAR-WDF8A-PCB              PIC X(1).                                
138400 01  SPAR-WDK6-PCB               PIC X(1).                                
138500                                                                          
138600*----> SUBPROGRAM W335PRNO.                                               
138700                                                                          
138800 01  PRNO-3107-PCB               PIC X.                                   
138900                                                                          
139000*----> SUBPROGRAM W335PRQU.                                               
139100 01  PRQU-WDG2-PCB               PIC X.                                   
139200 01  PRQU-WDC7-PCB               PIC X.                                   
139300 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
139400*                                                                         
139500 01  KOM-WDP8-PCB                PIC X.                                   
139600*                                                                         
139700     EJECT                                                                
139800 PROCEDURE DIVISION  USING MSG-PCB  0693-PCB ALT1-PCB ALT2-PCB            
139900                           ALT3-PCB  AVSR-ALT4-PCB ALT5-PCB               
140000                           ALT6-PCB  ALT7-PCB      4397-PCB               
140100                           DISTRDOC-PCB  PRQRY-PCB                        
140200                           USEA-PCB                                       
140300                           ORQA-PCB ORQI2-PCB ORQI-PCB XXKH-PCB           
140400                           XXKL-PCB  4001-PCB      ORQF-PCB               
140500                           ORQM-PCB  ORDP-PCB      XXJN-PCB               
140600                           WDK6-PCB  BENA-PCB      XXKU-PCB               
140700                           ZZAC-PCB  WDM2-PCB      4541-PCB               
140800                           ARTM-PCB  4003-PCB                             
140900                           4007-PCB  ORQICSQ-PCB   ARTN-PCB               
141000                           GMTA-PCB  WDK7-PCB                             
141100                           WLLOGA-PCB 4017-PCB                            
141200                           WDB2-PCB WDB1-PCB WDB6-PCB                     
141300                           WDK6-2-PCB WDA6B-PCB WDA6-PCB                  
141400                           WDP4A-PCB WDA5-PCB   WDQ2-PCB                  
141500                           BIPA-ORDP-PCB                                  
141600                           BIPA-WDB6-PCB                                  
141700                           BIPA-WDK6-PCB                                  
141800                           BIPA-LEVF-PCB                                  
141900                           BIPA-LEVG-PCB                                  
142000                           BIPA-WDF8-PCB                                  
142100                           BIPA-WDF8A-PCB                                 
142200                           BIPA-LEVA-PCB                                  
142300                           BIPA-ARTS2-PCB                                 
142400                           DEAV-ARTM-PCB  DEAV-WDK7-PCB                   
142500                           DEAV-WDB6-PCB  KVAN-WDB2-PCB                   
142600                           DEAV-WDL7-PCB  DEAV-WDK72-PCB                  
142700                           DEAV-WDR2-PCB  DEAV-WDR5-PCB                   
142800                           DEAV-WDC1-PCB                                  
142900                           RANS-XXKM-PCB  RANS-ARTM-PCB                   
143000                           RANS-ARTS-PCB                                  
143100                           AREG-WDK6-PCB  AREG-WDK7-PCB                   
143200                           AVSR-ORQI-PCB                                  
143300                           AVSR-GMTB-PCB  AVSR-GMTC-PCB                   
143400                           AVSR-WDB2-PCB  AVSR-WDB6-PCB                   
143500                           TRAN-XXKB-PCB                                  
143600                           SPAR-WDF8-PCB SPAR-WDF8A-PCB                   
143700                           SPAR-WDK6-PCB                                  
143800                           PRNO-3107-PCB                                  
143900                           PRQU-WDG2-PCB                                  
144000                           PRQU-WDC7-PCB                                  
144100                           PRQU-SJKO-WDK6-PCB                             
144200                           KOM-WDP8-PCB.                                  
144300                                                                          
144400  MAIN SECTION.                                                           
144500     ENTRY 'DLITCBL' USING MSG-PCB  0693-PCB ALT1-PCB ALT2-PCB            
144600                           ALT3-PCB  AVSR-ALT4-PCB ALT5-PCB               
144700                           ALT6-PCB  ALT7-PCB      4397-PCB               
144800                           DISTRDOC-PCB  PRQRY-PCB                        
144900                           USEA-PCB                                       
145000                           ORQA-PCB ORQI2-PCB ORQI-PCB XXKH-PCB           
145100                           XXKL-PCB  4001-PCB      ORQF-PCB               
145200                           ORQM-PCB  ORDP-PCB      XXJN-PCB               
145300                           WDK6-PCB  BENA-PCB      XXKU-PCB               
145400                           ZZAC-PCB  WDM2-PCB      4541-PCB               
145500                           ARTM-PCB  4003-PCB                             
145600                           4007-PCB  ORQICSQ-PCB   ARTN-PCB               
145700                           GMTA-PCB  WDK7-PCB                             
145800                           WLLOGA-PCB 4017-PCB                            
145900                           WDB2-PCB WDB1-PCB WDB6-PCB                     
146000                           WDK6-2-PCB WDA6B-PCB WDA6-PCB                  
146100                           WDP4A-PCB WDA5-PCB WDQ2-PCB                    
146200                           BIPA-ORDP-PCB                                  
146300                           BIPA-WDB6-PCB                                  
146400                           BIPA-WDK6-PCB                                  
146500                           BIPA-LEVF-PCB                                  
146600                           BIPA-LEVG-PCB                                  
146700                           BIPA-WDF8-PCB                                  
146800                           BIPA-WDF8A-PCB                                 
146900                           BIPA-LEVA-PCB                                  
147000                           BIPA-ARTS2-PCB                                 
147100                           DEAV-ARTM-PCB  DEAV-WDK7-PCB                   
147200                           DEAV-WDB6-PCB  KVAN-WDB2-PCB                   
147300                           DEAV-WDL7-PCB  DEAV-WDK72-PCB                  
147400                           DEAV-WDR2-PCB  DEAV-WDR5-PCB                   
147500                           DEAV-WDC1-PCB                                  
147600                           RANS-XXKM-PCB  RANS-ARTM-PCB                   
147700                           RANS-ARTS-PCB                                  
147800                           AREG-WDK6-PCB  AREG-WDK7-PCB                   
147900                           AVSR-ORQI-PCB                                  
148000                           AVSR-GMTB-PCB  AVSR-GMTC-PCB                   
148100                           AVSR-WDB2-PCB  AVSR-WDB6-PCB                   
148200                           TRAN-XXKB-PCB                                  
148300                           SPAR-WDF8-PCB SPAR-WDF8A-PCB                   
148400                           SPAR-WDK6-PCB                                  
148500                           PRNO-3107-PCB                                  
148600                           PRQU-WDG2-PCB                                  
148700                           PRQU-WDC7-PCB                                  
148800                           PRQU-SJKO-WDK6-PCB                             
148900                           KOM-WDP8-PCB.                                  
149000                                                                          
149100     EJECT                                                                
149200     PERFORM IMS-GU-MSG                                                   
149300     IF SEGMENT-FINNS                                                     
149400        PERFORM A-INIT                                                    
149500        IF ALLT-OK                                                        
149600           PERFORM B-LAES-PLOCKSATS                                       
149700           IF ALLT-OK                                                     
149800              PERFORM C-BEHANDLA-ORDERDELAR                               
149900              PERFORM D-UPPDAT-PLOCKSATS                                  
150000              PERFORM E-SKICKA-IMSTRANSAR                                 
150100           END-IF                                                         
150200        END-IF                                                            
150300     END-IF                                                               
150400                                                                          
150500     MOVE ZERO TO RETURN-CODE                                             
150600     GOBACK                                                               
150700     .                                                                    
150800     EJECT                                                                
150900 A-INIT SECTION.                                                          
151000     MOVE 'STA A-INIT-'          TO PGMPOS                                
151100                                                                          
151200     MOVE '0' TO ORAD-SW                                                  
151300*                                                                         
151400     MOVE JA  TO ALLT-SW                                                  
151500                 INITIERA-LDC-TABELL                                      
151600     MOVE NEJ TO PRC-SW                                                   
151700                 PLOCK-SW                                                 
151800                 SPLIT-SW                                                 
151900                 PLOCKSATS-ETIK-SW                                        
152000                 PLOCKSATS-PU-SW                                          
152100                 RENOVA-SW                                                
152200                 SW-AENDRA-LAGOMR                                         
152210                 SW-LYNK-NON-API                                          
152220                 SW-VOR                                                   
152300                                                                          
152400     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
152500     IF NOT GODK-MID                                                      
152600        MOVE NEJ TO ALLT-SW                                               
152700     END-IF                                                               
152800                                                                          
152900     IF ALLT-OK                                                           
153000        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I37501                  
153100        MOVE MSG-KDMFSFOR-1              TO PTOP1-KDMFSFOR                
153200                                            PTOP2-KDMFSFOR                
153300                                            PTOP3-KDMFSFOR                
153400                                            PTOP4-KDMFSFOR                
153500                                            WS-KDMFSFOR                   
153600     END-IF                                                               
153700     MOVE 1                   TO W-WDK611-KDSEGKEY                        
153800                                                                          
153900     ACCEPT WS-DATUM         FROM DATE                                    
154000     ACCEPT WS-KLOCKAN       FROM TIME                                    
154100     ACCEPT WS-VOR-TID-BRIST FROM TIME                                    
154200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM-Y2K                     
154300                                                                          
154400     MOVE WS-DATUM TO DAT-I-TIDATUM                                       
154500     PERFORM S14-ANROP-WDATKONV                                           
154600     MOVE DAT-TIAADDD TO WS-DATUM-AADDD                                   
154700                                                                          
154800     MOVE 'END A-INIT-'          TO PGMPOS                                
154900*                                                                         
155000     MOVE SPACE                 TO EVENT-SW                               
155100*                                                                         
155200*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
155300*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
155400     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
155500     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
155600     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
155700     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
155800     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
155900     MOVE 'W4037500'                 TO MSG-KOM-IDSNDJOB                  
156000     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
156100*    -- THIS IS THE START VALUE                                           
156200     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
156300                                        WS-TTMMSSTH-E                     
156400                                                                          
156500     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-AAAAMMDD-E                     
156600                                                                          
156700*    -- THIS IS THE START VALUE                                           
156800*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
156900     MOVE LOW-VALUE                  TO MSG-KDZ1                          
157000     MOVE LOW-VALUE                  TO MSG-KDZ2                          
157100                                                                          
157200     .                                                                    
157300     EJECT                                                                
157400 B-LAES-PLOCKSATS SECTION.                                                
157500     MOVE 'STA B-LAES-'          TO PGMPOS                                
157600                                                                          
157700     MOVE MID-IDPRODNR  TO W-4001-IDPRODNR                                
157800     MOVE MID-IDPLKLST  TO W-4001-IDPLKLST                                
157900                                                                          
158000     PERFORM IMS-GHU-4001-WL400111                                        
158100                                                                          
158200     IF MID-FLSVAR = NEJ                                                  
158300        PERFORM BA-INIT-PLOCKSATS-ETIK                                    
158400        PERFORM BB-INIT-PLOCKSATS-PU                                      
158500     ELSE                                                                 
158600        PERFORM BC-LAES-PLOCKSATS-ETIK                                    
158700        IF PLOCKSATS-ETIK-SAKNAS                                          
158800           PERFORM BA-INIT-PLOCKSATS-ETIK                                 
158900        END-IF                                                            
159000                                                                          
159100        PERFORM BD-LAES-PLOCKSATS-PU                                      
159200        IF PLOCKSATS-PU-SAKNAS                                            
159300           PERFORM BB-INIT-PLOCKSATS-PU                                   
159400        END-IF                                                            
159500                                                                          
159600     END-IF                                                               
159700                                                                          
159800     MOVE '011'                TO MSGI-KDCALL                             
159900*TÄNK PÅ ATT OM FLERA ANV. SKRIVER UT SAMTIDIGT MED SAMMA ID              
160000*FÖRSÖKER W005INIT UPPDATERA SAMMA ID SAMTIDIGT......                     
160100*DÄRFÖR SKALL ANV. ANVÄNDA OLIKA ID VID PÅLOGGNING I IMS.                 
160200     IF BILD-4353                                                         
160300       MOVE WS-IDDC            TO WS-DCUSER-IDDC                          
160400       MOVE WS-DCUSER          TO MSGI-IDUSER                             
160500     ELSE                                                                 
160600       MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                             
160700                                  MSGI-IDLTERM-USER                       
160800     END-IF                                                               
160900     MOVE WS-DATUM             TO MSGI-TILOKDAT                           
161000     MOVE WS-TIHHMMSS(1:4)     TO MSGI-TILOKTID                           
161100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
161200     MOVE MSGI-TILOKDAT        TO WS-DATUM-LOK                            
161300     MOVE WS-TIHHMMSS          TO WS-TIHHMMSS-LOK                         
161400     MOVE MSGI-TILOKTID        TO WS-TIHHMMSS-LOK(1:4)                    
161500                                                                          
161600     MOVE WS-DATUM-LOK TO DAT-I-TIDATUM                                   
161700     PERFORM S14-ANROP-WDATKONV                                           
161800     MOVE DAT-TIAADDD TO WS-DATUM-AADDD-LOK                               
161900     MOVE 'END B-LAES-'          TO PGMPOS                                
162000     .                                                                    
162100     EJECT                                                                
162200 BA-INIT-PLOCKSATS-ETIK SECTION.                                          
162300     MOVE 'STA BA-INIT-'          TO PGMPOS                               
162400                                                                          
162500     MOVE 1          TO 4004-KDSEGKEY                                     
162600     MOVE SPACE      TO 4004-KDPRT-PLE                                    
162700                                                                          
162800     IF W-IDTRANS = '435A'                                                
162900     OR W-IDTRANS = '435B'                                                
163000       MOVE 4002-IDMSG3IV   TO 4004-IDMSG3IV                              
163100       MOVE 4002-IDSNO3IV   TO 4004-IDSNO3IV                              
163200       MOVE 4002-ADDISPXTRA TO 4004-ADDISPXTRA                            
163300       MOVE 4002-IDUSER     TO WS-4002-IDUSER                             
163400     END-IF                                                               
163500                                                                          
163600     MOVE LOW-VALUE  TO 4004-NYCKEL-GRP                                   
163700     MOVE 1          TO IX1                                               
163800                                                                          
163900     PERFORM UNTIL IX1 > 101                                              
164000        MOVE 0 TO 4004-KVRADER (IX1)                                      
164100        ADD  1 TO IX1                                                     
164200     END-PERFORM                                                          
164300     MOVE 'END BA-INIT-'          TO PGMPOS                               
164400     .                                                                    
164500     EJECT                                                                
164600 BB-INIT-PLOCKSATS-PU SECTION.                                            
164700     MOVE 'STA BB-INIT-'          TO PGMPOS                               
164800                                                                          
164900     MOVE 1                 TO 4008-KDSEGKEY                              
165000     MOVE 4002-IDLOPNR-PL   TO 4008-IDLOPNR-PL                            
165100     MOVE SPACE             TO 4008-IDPRC                                 
165200                               4008-KDPRT-PU                              
165300     IF W-IDTRANS = '435A'                                                
165400     OR W-IDTRANS = '435B'                                                
165500       MOVE 4002-IDMSG3IV   TO 4008-IDMSG3IV                              
165600       MOVE 4002-IDSNO3IV   TO 4008-IDSNO3IV                              
165700       MOVE 4002-ADDISPXTRA TO 4008-ADDISPXTRA                            
165800     END-IF                                                               
165900                                                                          
166000     MOVE 0                 TO 4008-IDSID                                 
166100     MOVE 4002-IDDC     (1) TO 4008-IDDC                                  
166200                               WS-IDDC                                    
166300                                                                          
166400     MOVE 0                 TO 4008-KVRADER  (1)                          
166500                               4008-KVRADER  (2)                          
166600                               4008-KVRADER  (3)                          
166700                               4008-VKORDNTO (1)                          
166800                               4008-VKORDNTO (2)                          
166900                               4008-VKORDNTO (3)                          
167000                               4008-VLORDNTO (1)                          
167100                               4008-VLORDNTO (2)                          
167200                               4008-VLORDNTO (3)                          
167300     MOVE LOW-VALUE         TO 4008-NYCKEL-GRP                            
167400     MOVE 1                 TO IX1                                        
167500                                                                          
167600     PERFORM UNTIL IX1 > 101                                              
167700        MOVE 0 TO 4008-KVRADER-GRP (IX1)                                  
167800        ADD  1 TO IX1                                                     
167900     END-PERFORM                                                          
168000     MOVE 'END BB-INIT-'          TO PGMPOS                               
168100     .                                                                    
168200     EJECT                                                                
168300 BC-LAES-PLOCKSATS-ETIK SECTION.                                          
168400     MOVE 'STA BC-LAES-'          TO PGMPOS                               
168500                                                                          
168600     MOVE MID-IDPRODNR TO W-4003-ETIK-IDPRODNR                            
168700     MOVE MID-IDPLKLST TO W-4003-ETIK-IDPLKLST                            
168800                                                                          
168900     PERFORM IMS-GU-4003-WL400311                                         
169000                                                                          
169100     IF SEGMENT-FINNS                                                     
169200        MOVE JA TO PLOCKSATS-ETIK-SW                                      
169300     END-IF                                                               
169400     MOVE 'STA BC-LAES-'          TO PGMPOS                               
169500     .                                                                    
169600     EJECT                                                                
169700 BD-LAES-PLOCKSATS-PU SECTION.                                            
169800     MOVE 'STA BD-LAES-'          TO PGMPOS                               
169900                                                                          
170000     MOVE MID-IDPRODNR TO W-4007-PU-IDPRODNR                              
170100     MOVE MID-IDPLKLST TO W-4007-PU-IDPLKLST                              
170200                                                                          
170300     PERFORM IMS-GU-4007-WL400711                                         
170400                                                                          
170500     IF SEGMENT-FINNS                                                     
170600        MOVE JA TO PLOCKSATS-PU-SW                                        
170700        MOVE 4008-IDDC TO WS-IDDC                                         
170800     END-IF                                                               
170900     MOVE 'END BD-LAES-'          TO PGMPOS                               
171000     .                                                                    
171100     EJECT                                                                
171200 C-BEHANDLA-ORDERDELAR SECTION.                                           
171300     MOVE 'STA C-BEHAN'          TO PGMPOS                                
171400                                                                          
171500     MOVE 1 TO ORAD-IX                                                    
171600     MOVE ZERO                 TO OUTPUT-MSG-IX                           
171700                                                                          
171800     PERFORM UNTIL ORAD-IX  > MAX-ORAD                                    
171900                                                                          
172000        MOVE JA TO ALLT-SW                                                
172100        PERFORM CA-LAES-ORDERDEL                                          
172200        IF ALLT-OK                                                        
172300           PERFORM CB-LAES-ORDERHUVUD                                     
172400           IF ALLT-OK                                                     
172500              PERFORM CC-LAES-PRC-PRINTERTABELL                           
172600              IF ALLT-OK                                                  
172700                 IF CDC OR NDC                                            
172800                                                                          
172900                   PERFORM CD-BEHANDLA-BIPACKNING                         
173000                 END-IF                                                   
173100                 PERFORM CE-BEHANDLA-ORDERRADER                           
173200                 PERFORM CF-UPPDAT-ORDERDEL                               
173300              END-IF                                                      
173400           END-IF                                                         
173500        END-IF                                                            
173600     END-PERFORM                                                          
173700     .                                                                    
173800     EJECT                                                                
173900 CA-LAES-ORDERDEL SECTION.                                                
174000     MOVE 'STA CA-LAES-'          TO PGMPOS                               
174100*********************************************************                 
174200*  OM ORDERDELEN HAR PRC 9998 SKALL DEN LÄGGAS TILLBAKA *                 
174300*  PÅ KÖN EFTERSOM DET ÄR DIREKTLEVERANS.               *                 
174400*  DETTA KAN INTRÄFFA OM BAKGRUNDSMPP:N SOM SKRIVER UT  *                 
174500*  DIREKTLEVERANS ÄR STOPPAD AV NÅGON ANLEDNING.        *                 
174600*********************************************************                 
174700                                                                          
174800     MOVE 'GE' TO STATUS-WS                                               
174900     MOVE JA   TO BIPA-SPARR-SW                                           
175000                                                                          
175100     IF 4002-IXHEL = 0                                                    
175200        MOVE 1 TO 4002-IXHEL                                              
175300     END-IF                                                               
175400                                                                          
175500     PERFORM UNTIL SEGMENT-FINNS       OR                                 
175600                   4002-IXHEL > 99     OR                                 
175700                   4002-ORDDEL (4002-IXHEL) = LOW-VALUE                   
175800        MOVE 4002-IDORDER  (4002-IXHEL) TO W-Q301KY-IDORDER               
175900                                           W-Q301KY-MIN-IDORDER           
176000                                           W-Q301KY-MAX-IDORDER           
176100        MOVE 4002-IDDC (4002-IXHEL)     TO W-Q301KY-IDDC                  
176200                                           W-Q301KY-MIN-IDDC              
176300                                           W-Q301KY-MAX-IDDC              
176400        MOVE 4002-IDPRODNR (4002-IXHEL) TO W-Q301KY-IDPRODNR              
176500                                           WS-IDPRODNR                    
176600        MOVE 4002-IDPLKLST (4002-IXHEL) TO W-Q301KY-IDPLKLST              
176700                                                                          
176800        PERFORM IMS-GHU-ORQA-WLORQA01                                     
176900        IF SEGMENT-FINNS                                                  
177000           IF ODEL-IDPRC = WS-DIRLEV-PRC                                  
177100*** FIX START FÖR ATT ÅTERSTÄLLA VALDA ORDERDELAR TILL STATUS "R"         
177200*             OR 4002-IDORDER (4002-IXHEL) = +2133                        
177300*** FIX SLUT                                                              
177400              PERFORM S13-ATERSTALL-ORDERDEL                              
177500              MOVE 'GE'      TO STATUS-WS                                 
177600           END-IF                                                         
177700        END-IF                                                            
177800                                                                          
177900        IF SEGMENT-SAKNAS                                                 
178000           ADD 1 TO 4002-IXHEL                                            
178100        END-IF                                                            
178200     END-PERFORM                                                          
178300                                                                          
178400     IF SEGMENT-FINNS                                                     
178500        MOVE WLORQA01 TO W-ORDERDEL                                       
178600        MOVE JA       TO ODEL-SW                                          
178700        PERFORM CAA-LAES-EMBALLAGEKOD                                     
178800        IF 4008-IDPRC = SPACE                                             
178900           MOVE W-ODEL-IDPRC TO 4008-IDPRC                                
179000        END-IF                                                            
179100     END-IF                                                               
179200                                                                          
179300     IF 4002-IXHEL > 99   OR                                              
179400        4002-ORDDEL (4002-IXHEL) = LOW-VALUE                              
179500        MOVE JA   TO PLOCK-SW                                             
179600        MOVE NEJ  TO ALLT-SW                                              
179700        MOVE 1000 TO ORAD-IX                                              
179800     END-IF                                                               
179900     MOVE 'END CA-LAES-'          TO PGMPOS                               
180000     .                                                                    
180100     EJECT                                                                
180200 CAA-LAES-EMBALLAGEKOD SECTION.                                           
180300     MOVE 'STA CAA-LAES-'          TO PGMPOS                              
180400                                                                          
180500     MOVE SPACE           TO WS-KDEMBAL                                   
180600                                                                          
180700     MOVE W-ODEL-KDFDKRAV TO W-4535-KDFDKRAV                              
180800                                                                          
180900     MOVE WS-IDDC TO W-IDDC-B6                                            
181000     PERFORM IMS-GU-WDB601                                                
181100     MOVE DCS-IDFTG               TO WS-DCS-IDFTG                         
181200     IF DCS-SWEDEN                                                        
181300        MOVE 'S  ' TO W-4536-IDSKYLT                                      
181400     ELSE                                                                 
181500        MOVE 'GB ' TO W-4536-IDSKYLT                                      
181600     END-IF                                                               
181700                                                                          
181800     PERFORM IMS-GU-XXKU-WLXXKU11                                         
181900     IF SEGMENT-FINNS                                                     
182000        MOVE 4536-KDEMBAL TO WS-KDEMBAL                                   
182100     END-IF                                                               
182200     MOVE 'END CAA-LAES-'          TO PGMPOS                              
182300     .                                                                    
182400     EJECT                                                                
182500 CB-LAES-ORDERHUVUD SECTION.                                              
182600     MOVE 'STA CB-LAES-'          TO PGMPOS                               
182700                                                                          
182800     MOVE W-ODEL-IDORDER  TO W-IDORDER                                    
182900     MOVE W-ODEL-IDDC     TO W-IDDC                                       
183000                                                                          
183100     PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                               
183200     PERFORM IMS-GU-WDQ212                                                
183300     IF OHUV-FLKLAR NOT = JA                                              
183400        MOVE NEJ  TO ALLT-SW                                              
183500     ELSE                                                                 
183600       IF SEGMENT-FINNS                                                   
183700          PERFORM S17-DIST-KUND-LDC                                       
183800       END-IF                                                             
183900     END-IF                                                               
184000                                                                          
184100     IF ALLT-OK                                                           
184200                                                                          
184300        IF OHUV-KDORDKL = 3 OR 4                                          
184400           PERFORM CBB-KOLLA-BIPA-SPARR                                   
184500        END-IF                                                            
184600*LDC-GB                                                                   
184700        MOVE OHUV-IDDISTR     TO DIST34-IDDISTR                           
184800        IF LDC-GB-3A                                                      
184900        OR SDC-NL OR SDC-IT                                               
185000        IF DIST34-ENGLAND-SDC AND                                         
185100           GMT-FLLDCKND = JA                                              
185200          IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                        
185300          OR  OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                       
185400                                                                          
185500            IF INITIERA-LDC-TABELL = JA                                   
185600                                                                          
185700              MOVE HIGH-VALUE TO LDC-TABELL                               
185800                                                                          
185900              MOVE NEJ          TO INITIERA-LDC-TABELL                    
186000              MOVE '4017'       TO W-4017-IDHTYP                          
186100              MOVE MID-IDPRODNR TO W-4017-IDPRODNR                        
186200              MOVE MID-IDPLKLST TO W-4017-IDPLKLST                        
186300              MOVE LOW-VALUE    TO W-4017-LOW-VALUE                       
186400                                                                          
186500              PERFORM IMS-GHU-WDGX4017                                    
186600              IF SEGMENT-FINNS                                            
186700                                                                          
186800                PERFORM IMS-GHU-WDGX4018                                  
186900                IF SEGMENT-FINNS                                          
187000                  MOVE +1 TO LDC-TAB-IX                                   
187100                  MOVE +100 TO LDC-TAB-IX-MAX                             
187200                  PERFORM UNTIL LDC-TAB-IX > LDC-TAB-IX-MAX               
187300                                                                          
187400                    MOVE 4018-BERADREF(LDC-TAB-IX)                        
187500                      TO LDC-TAB-WIPID (LDC-TAB-IX)                       
187600                                                                          
187700                    ADD +1 TO LDC-TAB-IX                                  
187800                  END-PERFORM                                             
187900                END-IF                                                    
188000              END-IF                                                      
188100            END-IF                                                        
188200          END-IF                                                          
188300        END-IF                                                            
188400        END-IF                                                            
188500     ELSE                                                                 
188600        MOVE W-ORDERDEL TO WLORQA01                                       
188700        PERFORM S13-ATERSTALL-ORDERDEL                                    
188800        ADD 1 TO 4002-IXHEL                                               
188900     END-IF                                                               
189000     MOVE 'END CB-LAES-'          TO PGMPOS                               
189100     .                                                                    
189200     EJECT                                                                
189300 CBB-KOLLA-BIPA-SPARR SECTION.                                            
189400     MOVE 'STA CBB-kolla-'          TO PGMPOS                             
189500                                                                          
189600     IF W-ODEL-TIREGDAT = WS-DATUM-LOK                                    
189700        MOVE NEJ TO BIPA-SPARR-SW                                         
189800     ELSE                                                                 
189900        MOVE W-ODEL-TIREGDAT TO DAT-I-TIDATUM                             
190000        PERFORM S14-ANROP-WDATKONV                                        
190100        MOVE DAT-TIAADDD TO WS-REGDATUM-AADDD                             
190200                                                                          
190300        MOVE WS-DATUM-AADDD-LOK TO TMP1-YYDDD                             
190400        MOVE WS-REGDATUM-AADDD  TO TMP2-YYDDD                             
190500        PERFORM WY2000P4                                                  
190600                                                                          
190700        COMPUTE WS-ANTAL-DAGAR = TMP1-YYDDD -                             
190800                                 TMP2-YYDDD                               
190900                                                                          
191000        IF WS-ANTAL-DAGAR < 2                                             
191100           MOVE NEJ TO BIPA-SPARR-SW                                      
191200        END-IF                                                            
191300     END-IF                                                               
191400     MOVE 'END CBB-KOLLA'          TO PGMPOS                              
191500     .                                                                    
191600     EJECT                                                                
191700 CC-LAES-PRC-PRINTERTABELL SECTION.                                       
191800     MOVE 'STA CC-LAES- '          TO PGMPOS                              
191900                                                                          
192000     IF PRC-EJ-OK                                                         
192100        MOVE 4008-IDPRC      TO W-4448-IDPRC                              
192200                                W-4453-IDPRC                              
192300        MOVE W-ODEL-IDDC     TO W-4447-IDDC                               
192400                                W-4453-IDDC                               
192500        PERFORM IMS-GU-XXKH-WLXXKH11                                      
192600        IF SEGMENT-SAKNAS                                                 
192700           MOVE 9999 TO W-4448-IDPRC                                      
192800                        W-4453-IDPRC                                      
192900                                                                          
193000           PERFORM IMS-GU-XXKH-WLXXKH11                                   
193100           IF SEGMENT-SAKNAS                                              
193200              MOVE NEJ TO ALLT-SW                                         
193300           END-IF                                                         
193400        END-IF                                                            
193500                                                                          
193600        IF SEGMENT-FINNS                                                  
193700           MOVE 4448-KVRADER  TO WS-PRC-KVRADER                           
193800           MOVE 4448-VKORDNTO TO WS-PRC-VKORDNTO                          
193900           MOVE 4448-VLORDNTO TO WS-PRC-VLORDNTO                          
194000           MOVE 4448-RESPLIT  TO WS-PRC-RESPLIT                           
194100                                                                          
194200           PERFORM IMS-GU-XXKL-WLXXKL11                                   
194300           IF SEGMENT-SAKNAS                                              
194400              MOVE NEJ  TO ALLT-SW                                        
194500           END-IF                                                         
194600        END-IF                                                            
194700                                                                          
194800        IF ALLT-OK                                                        
194900           MOVE JA TO PRC-SW                                              
195000           IF 4002-KDPRT-PU = SPACE                                       
195100              MOVE 4454-KDPRTGEN-PU  TO 4008-KDPRT-PU                     
195200           ELSE                                                           
195300              MOVE 4002-KDPRT-PU     TO 4008-KDPRT-PU                     
195400           END-IF                                                         
195500                                                                          
195600           IF 4002-KDPRT-PLE = SPACE                                      
195700              MOVE 4454-KDPRTGEN-PLE TO 4004-KDPRT-PLE                    
195800           ELSE                                                           
195900              MOVE 4002-KDPRT-PLE    TO 4004-KDPRT-PLE                    
196000           END-IF                                                         
196100        END-IF                                                            
196200     END-IF                                                               
196300                                                                          
196400     IF ALLT-FEL                                                          
196500        MOVE W-ORDERDEL TO WLORQA01                                       
196600        PERFORM S13-ATERSTALL-ORDERDEL                                    
196700        MOVE SPACE      TO 4008-IDPRC                                     
196800        ADD 1           TO 4002-IXHEL                                     
196900     END-IF                                                               
197000     MOVE 'END CC-LAES- '          TO PGMPOS                              
197100     .                                                                    
197200     EJECT                                                                
197300 CD-BEHANDLA-BIPACKNING SECTION.                                          
197400     MOVE 'STA CD-BEHAND'          TO PGMPOS                              
197500                                                                          
197600     IF BIPA-SPARR AND 4002-KVRADER = 0                                   
197700        PERFORM CDA-LAES-BIPACKNING                                       
197800                                                                          
197900        MOVE 1 TO BIPA-IX                                                 
198000                                                                          
198100        IF BIPA-IDARTNR (BIPA-IX) > 0                                     
198200           MOVE '1' TO ORAD-SW                                            
198300           PERFORM CDB-INIT-LANKAREA-W413AVSR                             
198400        END-IF                                                            
198500                                                                          
198600        PERFORM UNTIL BIPA-IX > MAX-BIPA                                  
198700           IF BIPA-IDARTNR (BIPA-IX) > 0                                  
198800              PERFORM S01-LAES-ART-REG-WDD3                               
198900              PERFORM CDC-FLYTTA-TILL-ARBETSAREA                          
199000              PERFORM CDG-JUSTERA-LAGEROMR-PLATS                          
199100              PERFORM CDD-FYLL-I-LANKAREA-W413AVSR                        
199200              PERFORM CDE-ISRT-WDQ4                                       
199300              PERFORM CDF-KOMPLETTERA-ORDERDEL                            
199400              MOVE 0  TO WS-ANTOBKR                                       
199500                         WS-KVLS                                          
199600                         WS-KVROS                                         
199700                         WS-KVAVBART                                      
199800              MOVE 10 TO WS-KDORDBEK                                      
199900              MOVE IDPGM TO WS-IDPGM                                      
200000              PERFORM S02-ORDERBEKRAFTELSE                                
200100              PERFORM S08-SKAPA-RYETRANS                                  
200200           END-IF                                                         
200300           ADD 1 TO BIPA-IX                                               
200400        END-PERFORM                                                       
200500                                                                          
200600        IF BIPACKNING                                                     
200700           CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT4-PCB                
200800                               AVSR-ORQI-PCB AVSR-GMTB-PCB                
200900                               AVSR-GMTC-PCB AVSR-WDB2-PCB                
201000                               AVSR-WDB6-PCB TRAN-XXKB-PCB                
201100           PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                         
201200           IF SEGMENT-FINNS                                               
201300               PERFORM S17-DIST-KUND-LDC                                  
201400           END-IF                                                         
201500        END-IF                                                            
201600     END-IF                                                               
201700     MOVE 'END CD-BEHAND'          TO PGMPOS                              
201800     .                                                                    
201900     EJECT                                                                
202000 CDA-LAES-BIPACKNING SECTION.                                             
202100     MOVE 'STA CDA-LAES-'          TO PGMPOS                              
202200                                                                          
202300     MOVE 7                    TO BIPA-KDORDBEH                           
202400     MOVE W-ODEL-IDDISTR       TO BIPA-IDDISTR                            
202500     MOVE W-ODEL-IDKUNDNR      TO BIPA-IDKUNDNR                           
202600     MOVE W-ODEL-IDKUNDRF (3:5)                                           
202700                               TO BIPA-IDKUNDRF                           
202800     MOVE OHUV-KDTPOTYP        TO BIPA-KDTPOTYP                           
202900     MOVE OHUV-KDORDKL         TO BIPA-KDORDKL                            
203000     MOVE OHUV-KDFAKTYP        TO BIPA-KDFAKTYP                           
203100     MOVE OHUV-IDSYSTEM        TO BIPA-IDSYSTEM                           
203200     MOVE OHUV-IDKAMPRF        TO BIPA-IDKAMPRF                           
203300     MOVE OHUV-IDKONTO         TO BIPA-IDKONTO                            
203400     MOVE OHUV-IDKST           TO BIPA-IDKST                              
203500     MOVE OHUV-IDANALYS        TO BIPA-IDANALYS                           
203600     MOVE W-ODEL-IDPRC         TO BIPA-IDPRC-RAD                          
203700     MOVE 13                   TO BIPA-KVBIPACK                           
203800     MOVE OHUV-FLFORBI         TO BIPA-FLFORBI                            
203900     MOVE OHUV-FLORDSPE        TO BIPA-FLORDSPE                           
204000     MOVE OHUV-BEVARREF        TO BIPA-BEVARREF                           
204100     MOVE OHUV-FLOVRLEV        TO BIPA-FLOVRLEV                           
204200     MOVE OHUV-IDBIPREF        TO BIPA-IDBIPREF                           
204300                                                                          
204400     IF (ARB-KDROPACK = '5' OR '8' OR 'L')                                
204500        MOVE OHUV-IDBIPREF     TO BIPA-IDBIPREF                           
204600     END-IF                                                               
204700                                                                          
204800     MOVE ARB-KDROPACK         TO BIPA-KDROPACK                           
204900     MOVE ARB-KDFRAKT          TO BIPA-KDFRAKT                            
205000     MOVE ARB-IDDC             TO BIPA-IDDC                               
205100     PERFORM IMS-GNP-WDQ221                                               
205200     PERFORM UNTIL Q221-SEG-SAKNAS                                        
205300        MOVE LOR-IDPRC         TO BIPA-IDPRC-LAGOMR (LOR-ADLAGOMR)        
205400        PERFORM IMS-GNP-WDQ221                                            
205500     END-PERFORM                                                          
205600                                                                          
205700     CALL W411BIPA USING BIPA-W411BIPA BIPA-ORDP-PCB                      
205800                                       BIPA-WDB6-PCB                      
205900                                       BIPA-WDK6-PCB                      
206000                                       AREG-WDK7-PCB                      
206100                                       BIPA-LEVF-PCB                      
206200                                       BIPA-LEVG-PCB                      
206300                                       BIPA-WDF8-PCB                      
206400                                       BIPA-WDF8A-PCB                     
206500                                       BIPA-LEVA-PCB                      
206600                                       BIPA-ARTS2-PCB                     
206700     MOVE 'END CDA-LAES-'          TO PGMPOS                              
206800     .                                                                    
206900     EJECT                                                                
207000 CDB-INIT-LANKAREA-W413AVSR SECTION.                                      
207100     MOVE 'STA CDB-INIT-'          TO PGMPOS                              
207200                                                                          
207300     MOVE 1 TO IX1                                                        
207400                                                                          
207500     PERFORM UNTIL IX1 > MAX-AVSR                                         
207600        MOVE SPACE TO AVSR-KDORDSTA         (IX1)                         
207700                      AVSR-IDLEVNR          (IX1)                         
207800        INITIALIZE    AVSR-DEAL-PR-LINE     (IX1)                         
207900        MOVE ZERO  TO AVSR-ADLAGOMR         (IX1)                         
208000                      AVSR-IDDC             (IX1)                         
208100                      AVSR-KDSPEEMB         (IX1)                         
208200                      AVSR-KVANNANT         (IX1)                         
208300                      AVSR-KVBEART-Q        (IX1)                         
208400                      AVSR-PRARTNTO         (IX1)                         
208500                      AVSR-PRARTNTO-LOC     (IX1)                         
208600                      AVSR-PRARTNTO-LOCPREL (IX1)                         
208700                      AVSR-PRAVCOST         (IX1)                         
208800                      AVSR-VKART            (IX1)                         
208900                      AVSR-VLARTNTO         (IX1)                         
209000                      AVSR-KDVIA            (IX1)                         
209100                      AVSR-KVDAGAR-DIFF     (IX1)                         
209200                      AVSR-TISKEPPN-DDC     (IX1)                         
209300                      AVSR-KDFARLIG         (IX1)                         
209400                      AVSR-KDVSOP           (IX1)                         
209500        ADD 1      TO IX1                                                 
209600     END-PERFORM                                                          
209700     MOVE 'END CDB-INIT-'          TO PGMPOS                              
209800     .                                                                    
209900     EJECT                                                                
210000 CDC-FLYTTA-TILL-ARBETSAREA SECTION.                                      
210100     MOVE 'STA CDC-FLYTT'          TO PGMPOS                              
210200                                                                          
210300     MOVE OHUV-IDORDER                   TO ORAD-IDORDER                  
210400     MOVE BIPA-IDDC-UT      (BIPA-IX)    TO ORAD-IDDC                     
210500     MOVE BIPA-IDDC-RO      (BIPA-IX)    TO ORAD-IDDC-RO                  
210600     MOVE BIPA-IDARTNR      (BIPA-IX)    TO ORAD-IDARTNR                  
210700     MOVE ORAD-IDDC         TO WS-IDDC                                    
210800                                                                          
210900     IF  NDC                                                              
211000         MOVE ORAD-IDARTNR               TO W-IDARTNR                     
211100         MOVE ORAD-IDDC                  TO W-IDDC                        
211200         PERFORM IMS-GU-WDK711                                            
211300         MOVE SLAG-ADLAGOMR              TO ORAD-ADLAGOMR                 
211400         MOVE SLAG-ADGANG                TO ORAD-ADGANG                   
211500         MOVE SLAG-ADPLATS               TO ORAD-ADPLATS                  
211600     ELSE                                                                 
211700         MOVE AREG-ADLAGOMR              TO ORAD-ADLAGOMR                 
211800         MOVE AREG-ADGANG                TO ORAD-ADGANG                   
211900         MOVE AREG-ADPLATS               TO ORAD-ADPLATS                  
212000     END-IF                                                               
212100     MOVE BIPA-IDLOPNR      (BIPA-IX)    TO ORAD-IDLOPNR                  
212200     MOVE BIPA-BERADREF     (BIPA-IX)    TO ORAD-BERADREF                 
212300     MOVE BIPA-BEVOLREF     (BIPA-IX)    TO ORAD-BEVOLREF                 
212400     MOVE NEJ                            TO ORAD-FLAKPLOC                 
212500     MOVE BIPA-FLINVEST     (BIPA-IX)    TO ORAD-FLINVEST                 
212600     MOVE OHUV-FLOBTRAN                  TO ORAD-FLOBTRAN                 
212700     MOVE BIPA-FLPRTILL     (BIPA-IX)    TO ORAD-FLPRTILL                 
212800     MOVE OHUV-FLRESTN                   TO ORAD-FLRESTN                  
212900     MOVE NEJ                            TO ORAD-FLTILLK                  
213000     MOVE NEJ                            TO ORAD-FLSDCLEV                 
213100     MOVE BIPA-IDDISTR                   TO ORAD-IDDISTR                  
213200     MOVE BIPA-IDKUNDNR                  TO ORAD-IDKUNDNR                 
213300     MOVE '0000000   '                   TO ORAD-IDKUNDRF                 
213400     MOVE BIPA-IDKUNDRF     (1:5)        TO ORAD-IDKUNDRF (3:5)           
213500     MOVE BIPA-IDKAMPRF-UT  (BIPA-IX)    TO ORAD-IDKAMPRF                 
213600     MOVE BIPA-IDLEVNR      (BIPA-IX)    TO ORAD-IDLEVNR                  
213700     MOVE BIPA-IDLOPNR      (BIPA-IX)    TO ORAD-IDLOPNR-RO               
213800     MOVE '0000000   '                   TO ORAD-IDKUNDRF-RO              
213900     MOVE BIPA-IDKUNDRF-UT  (BIPA-IX) (1:5)                               
214000                                 TO ORAD-IDKUNDRF-RO    (3:5)             
214100     MOVE 0                              TO ORAD-IDSPECEMB                
214200     MOVE BIPA-IDSYSTEM-UT  (BIPA-IX)    TO ORAD-IDSYSTEM                 
214300*FÖLJANDE REGEL FINNS ÄVEN I SEKTION S06AA-HAMTA-KDARTURS.                
214400     IF CDC                                                               
214500       MOVE AREG-KDARTURS                TO ORAD-KDARTURS                 
214600       MOVE AREG-VKART                   TO ORAD-VKART                    
214700       MOVE AREG-VKART-NTO               TO ORAD-VKART-NTO                
214800       MOVE AREG-VLARTNTO                TO ORAD-VLARTNTO                 
214900     ELSE                                                                 
215000       MOVE ORAD-IDARTNR                 TO W-IDARTNR                     
215100       MOVE ORAD-IDDC                    TO W-IDDC                        
215200       PERFORM IMS-GU-WDK711                                              
215300       IF (SLAG-IDLEVNR = '1441 ' OR SLAG-IDLEVNR = 'BP2TW')              
215400         MOVE AREG-KDARTURS              TO ORAD-KDARTURS                 
215500         MOVE AREG-VKART                 TO ORAD-VKART                    
215600         MOVE AREG-VKART-NTO             TO ORAD-VKART-NTO                
215700         MOVE AREG-VLARTNTO              TO ORAD-VLARTNTO                 
215800       ELSE                                                               
215900         IF SLAG-IDLEVNR NOT = SPACE                                      
216000           IF ORAD-IDDC NOT = DCS-IDDC                                    
216100              MOVE ORAD-IDDC             TO W-IDDC-B6                     
216200              PERFORM IMS-GU-WDB601                                       
216300           END-IF                                                         
216400           MOVE DCS-IDLANDX2             TO W-IDLAND                      
216500           IF DCS-NDC                                                     
216600              PERFORM IMS-GU-WDK712                                       
216700              IF SEGMENT-FINNS                                            
216800                IF LART-KDARTURS > SPACE                                  
216900                   MOVE LART-KDARTURS    TO ORAD-KDARTURS                 
217000                ELSE                                                      
217100                   MOVE AREG-KDARTURS    TO ORAD-KDARTURS                 
217200                END-IF                                                    
217300                IF LART-VKART > ZERO AND                                  
217400                   LART-VKART NOT = AREG-VKART                            
217500                  MOVE LART-VKART        TO ORAD-VKART                    
217600                                            ORAD-VKART-NTO                
217700                ELSE                                                      
217800                  MOVE AREG-VKART        TO ORAD-VKART                    
217900                  MOVE AREG-VKART-NTO    TO ORAD-VKART-NTO                
218000                END-IF                                                    
218100                IF LART-VLARTNTO > ZERO                                   
218200                  MOVE LART-VLARTNTO     TO ORAD-VLARTNTO                 
218300                ELSE                                                      
218400                  MOVE AREG-VLARTNTO     TO ORAD-VLARTNTO                 
218500                END-IF                                                    
218600              ELSE                                                        
218700                MOVE AREG-KDARTURS       TO ORAD-KDARTURS                 
218800                MOVE AREG-VKART          TO ORAD-VKART                    
218900                MOVE AREG-VKART-NTO      TO ORAD-VKART-NTO                
219000                MOVE AREG-VLARTNTO       TO ORAD-VLARTNTO                 
219100              END-IF                                                      
219200           ELSE                                                           
219300             MOVE AREG-KDARTURS          TO ORAD-KDARTURS                 
219400             MOVE AREG-VKART             TO ORAD-VKART                    
219500             MOVE AREG-VKART-NTO         TO ORAD-VKART-NTO                
219600             MOVE AREG-VLARTNTO          TO ORAD-VLARTNTO                 
219700           END-IF                                                         
219800         ELSE                                                             
219900           MOVE AREG-KDARTURS            TO ORAD-KDARTURS                 
220000           MOVE AREG-VKART               TO ORAD-VKART                    
220100           MOVE AREG-VKART-NTO           TO ORAD-VKART-NTO                
220200           MOVE AREG-VLARTNTO            TO ORAD-VLARTNTO                 
220300         END-IF                                                           
220400       END-IF                                                             
220500     END-IF                                                               
220600     MOVE BIPA-KDDSP        (BIPA-IX)    TO ORAD-KDDSP                    
220700     MOVE AREG-KDFARLIG                  TO ORAD-KDFARLIG                 
220800     MOVE BIPA-KDOI         (BIPA-IX)    TO ORAD-KDOI                     
220900     MOVE BIPA-CLEARGROUP   (BIPA-IX)    TO ORAD-CLEARGROUP               
221000     MOVE BIPA-KDKVBRYT     (BIPA-IX)    TO ORAD-KDKVBRYT                 
221100     MOVE BIPA-KDORDING     (BIPA-IX)    TO ORAD-KDORDING                 
221200     MOVE JA                             TO ORAD-FLORDING                 
221300     MOVE BIPA-KDORDKL-UT   (BIPA-IX)    TO ORAD-KDORDKL                  
221400     MOVE BIPA-KDPRODSL     (BIPA-IX)    TO ORAD-KDPRODSL                 
221500     MOVE BIPA-KDPRTYP      (BIPA-IX)    TO ORAD-KDPRTYP                  
221600     MOVE AREG-KDSPEEMB                  TO ORAD-KDSPEEMB                 
221700     MOVE BIPA-KDTPOTYP-UT  (BIPA-IX)    TO ORAD-KDTPOTYP                 
221800     MOVE BIPA-KDVRINFO     (BIPA-IX)    TO ORAD-KDVRINFO                 
221900     MOVE BIPA-KVART        (BIPA-IX)    TO ORAD-KVBEART                  
222000                                            ORAD-KVBEART-Q                
222100     MOVE 0                              TO ORAD-KVPREAVB                 
222200     MOVE 0                              TO ORAD-KVPRERO                  
222300     MOVE 0                              TO ORAD-KVOKS-PREL               
222400     MOVE 0                              TO ORAD-KVSLATT                  
222500     MOVE BIPA-PRARTNTO     (BIPA-IX)    TO ORAD-PRARTNTO                 
222600     MOVE BIPA-PRAVCOST     (BIPA-IX)    TO ORAD-PRAVCOST                 
222700     MOVE BIPA-KDVALISO     (BIPA-IX)    TO ORAD-KDVALISO                 
222800     MOVE BIPA-DEAL-PR-LINE (BIPA-IX)    TO ORAD-DEAL-PR-LINE             
222900***SKAPAR EN PRISFRÅGA PÅ DE SOM ÄNNU INTE HAR FÅTT EN SÅDAN.             
223000     MOVE BIPA-IDDISTR                   TO TEST-IDDISTR                  
223100     IF DIST79-DEALER-PRICE AND                                           
223200        (BIPA-PRARTNTO-LOC (BIPA-IX) = +0                                 
223300         AND BIPA-PRARTNTO-LOCPREL (BIPA-IX) = +0)                        
223400        PERFORM S18-ADD-PRICE-Q-LINE                                      
223500     END-IF                                                               
223600     MOVE 0                              TO ORAD-PRBPRIS                  
223700     MOVE BIPA-REKSIFFR     (BIPA-IX)    TO ORAD-REKSIFFR                 
223800     MOVE 1.0000                         TO ORAD-RERF-RAD                 
223900     MOVE 0                              TO ORAD-TIPRIS                   
224000     MOVE BIPA-TIREGDAT     (BIPA-IX)    TO ORAD-TIREGDAT                 
224100     MOVE OHUV-TIREGTID                  TO ORAD-TIREGTID                 
224200     MOVE BIPA-TIRODAT      (BIPA-IX)    TO ORAD-TIRODAT                  
224300     MOVE BIPA-TITPO        (BIPA-IX)    TO ORAD-TITPO                    
224400     MOVE SPACE                          TO ORAD-IDBIL                    
224500                                            ORAD-IDKLIENT                 
224600                                            ORAD-IDARBREF                 
224700                                            ORAD-IDVIN                    
224800     MOVE BIPA-IDKUNDRF-WIP (BIPA-IX)    TO ORAD-IDKUNDRF-WIP             
224900     MOVE 'END CDC-FLYTT'          TO PGMPOS                              
225000     .                                                                    
225100     EJECT                                                                
225200 CDD-FYLL-I-LANKAREA-W413AVSR SECTION.                                    
225300     MOVE 'STA CDD-FYLL-'          TO PGMPOS                              
225400                                                                          
225500     MOVE 1                 TO AVSR-KDCALL                                
225600     MOVE OHUV-IDORDER      TO AVSR-IDORDER                               
225700     MOVE OHUV-KDORDKL      TO AVSR-KDORDKL                               
225800     MOVE ZERO              TO AVSR-KDFRAKT                               
225900                               AVSR-KDROPACK                              
226000     MOVE MSGI-TILOKDAT     TO AVSR-TIREGDAT                              
226100     MOVE MSGI-TILOKTID     TO AVSR-TIHHMM                                
226200                                                                          
226300     MOVE ORAD-ADLAGOMR     TO AVSR-ADLAGOMR  (BIPA-IX)                   
226400     MOVE ORAD-IDLEVNR      TO AVSR-IDLEVNR   (BIPA-IX)                   
226500     MOVE ORAD-IDDC         TO AVSR-IDDC      (BIPA-IX)                   
226600     MOVE ORAD-KDSPEEMB     TO AVSR-KDSPEEMB  (BIPA-IX)                   
226700     MOVE ORAD-KVBEART-Q    TO AVSR-KVBEART-Q (BIPA-IX)                   
226800     MOVE ORAD-PRARTNTO     TO AVSR-PRARTNTO  (BIPA-IX)                   
226900     MOVE ORAD-PRAVCOST     TO AVSR-PRAVCOST  (BIPA-IX)                   
227000     MOVE ORAD-DEAL-PR-LINE TO AVSR-DEAL-PR-LINE (BIPA-IX)                
227100     MOVE ORAD-VKART        TO AVSR-VKART     (BIPA-IX)                   
227200     MOVE ORAD-VLARTNTO     TO AVSR-VLARTNTO  (BIPA-IX)                   
227300     MOVE AREG-KDFARLIG     TO AVSR-KDFARLIG  (BIPA-IX)                   
227400     MOVE AREG-KDVSOP       TO AVSR-KDVSOP    (BIPA-IX)                   
227500     MOVE 'END CDD-FYLL-'          TO PGMPOS                              
227600     .                                                                    
227700     EJECT                                                                
227800 CDE-ISRT-WDQ4 SECTION.                                                   
227900     MOVE 'STA CDE-ISRT-'          TO PGMPOS                              
228000                                                                          
228100     PERFORM IMS-ISRT-ORQF-WLORQF01                                       
228200                                                                          
228300     PERFORM UNTIL SEGMENT-FINNS                                          
228400        ADD 1 TO ORAD-IDLOPNR                                             
228500        PERFORM IMS-ISRT-ORQF-WLORQF01                                    
228600     END-PERFORM                                                          
228700     MOVE 'END CDE-ISRT-'          TO PGMPOS                              
228800     .                                                                    
228900     EJECT                                                                
229000 CDF-KOMPLETTERA-ORDERDEL SECTION.                                        
229100     MOVE 'STA CDF-KOMPL'          TO PGMPOS                              
229200                                                                          
229300     IF ORAD-PRAVCOST > ZERO                                              
229400        COMPUTE WS-ORDERVARDE-PER-RAD =                                   
229500                ORAD-PRAVCOST * ORAD-KVBEART-Q                            
229600     ELSE                                                                 
229700        COMPUTE WS-ORDERVARDE-PER-RAD =                                   
229800                ORAD-PRARTNTO * ORAD-KVBEART-Q                            
229900     END-IF                                                               
230000     ADD WS-ORDERVARDE-PER-RAD TO W-ODEL-SUORDV                           
230100                                                                          
230200     COMPUTE WS-ORDERVARDE-PER-RAD-LOC =                                  
230300             ORAD-PRARTNTO-LOC * ORAD-KVBEART-Q                           
230400     ADD WS-ORDERVARDE-PER-RAD-LOC TO W-ODEL-SUORDV-LOC                   
230500                                                                          
230600     COMPUTE WS-ORDERVARDE-PER-RAD-LOCPREL =                              
230700             ORAD-PRARTNTO-LOCPREL * ORAD-KVBEART-Q                       
230800     ADD WS-ORDERVARDE-PER-RAD-LOCPREL TO W-ODEL-SUORDV-LOCPREL           
230900                                                                          
231000     COMPUTE WS-ORDERVIKT-PER-RAD =                                       
231100           ((ORAD-KVBEART-Q * ORAD-VKART) / 1000)                         
231200     ADD WS-ORDERVIKT-PER-RAD  TO W-ODEL-VKORDNTO                         
231300                                                                          
231400     COMPUTE WS-ORDERVOLYM-PER-RAD =                                      
231500           ((ORAD-KVBEART-Q * ORAD-VLARTNTO) / 1000000)                   
231600     ADD WS-ORDERVOLYM-PER-RAD TO W-ODEL-VLORDNTO                         
231700                                                                          
231800     ADD 1                     TO W-ODEL-KVRADER                          
231900     MOVE 'END CDF-KOMPL'          TO PGMPOS                              
232000     .                                                                    
232100     EJECT                                                                
232200 CDG-JUSTERA-LAGEROMR-PLATS SECTION.                                      
232300     MOVE 'STA CDG-JUSTE'          TO PGMPOS                              
232400                                                                          
232500     MOVE ORAD-ADLAGOMR     TO ADRS-ADLAGOMR-IN                           
232600     MOVE ORAD-ADPLATS      TO ADRS-ADPLATS-IN                            
232700                                                                          
232800     IF OHUV-BEVARREF = SPACE                                             
232900       MOVE ORAD-BERADREF   TO ADRS-BEVARREF-IN                           
233000     ELSE                                                                 
233100       MOVE OHUV-BEVARREF   TO WS-HFAK-REF-X10                            
233200       PERFORM S15-KOLLA-I-HFAK-TAB                                       
233300       IF BEVARREF-I-HFAK-TAB                                             
233400         MOVE OHUV-BEVARREF TO ADRS-BEVARREF-IN                           
233500       ELSE                                                               
233600         MOVE ORAD-BERADREF TO ADRS-BEVARREF-IN                           
233700       END-IF                                                             
233800     END-IF                                                               
233900                                                                          
234000     PERFORM S16-KOLLA-OM-RENOVA                                          
234100     IF RENOVA                                                            
234200       MOVE SPACE         TO ADRS-BEVARREF-IN                             
234300     END-IF                                                               
234400                                                                          
234500     MOVE OHUV-FLFORBI      TO ADRS-FLFORBI-IN                            
234600     MOVE OHUV-IDDISTR      TO ADRS-IDDISTR-IN                            
234700     MOVE +25     TO W-ADLAGOMR                                           
234800     PERFORM IMS-GNP-WDQ221-ADLAG25                                       
234900     IF Q221-SEG-FINNS                                                    
235000        MOVE 1              TO ADRS-KDCALL-IN                             
235100     ELSE                                                                 
235200        MOVE 2              TO ADRS-KDCALL-IN                             
235300     END-IF                                                               
235400                                                                          
235500     MOVE ORAD-IDDC         TO ADRS-IDDC-IN                               
235600     MOVE OHUV-KDORDKL      TO ADRS-KDORDKL-IN                            
235700     MOVE ORAD-KVBEART-Q    TO ADRS-KVBEART-Q-IN                          
235800     MOVE ORAD-VLARTNTO     TO ADRS-VLARTNTO-IN                           
235900                                                                          
236000     CALL W413ADRS USING ADRS-W413ADRS                                    
236100                                                                          
236200     MOVE ADRS-ADLAGOMR-UT  TO ORAD-ADLAGOMR                              
236300     MOVE ADRS-ADPLATS-UT   TO ORAD-ADPLATS                               
236400     MOVE 'END CDG-JUSTE'          TO PGMPOS                              
236500     .                                                                    
236600     EJECT                                                                
236700 CE-BEHANDLA-ORDERRADER SECTION.                                          
236800     MOVE 'STA CE-BEHANDL'         TO PGMPOS                              
236900                                                                          
237000     MOVE '2'             TO ORAD-SW                                      
237100                                                                          
237200     MOVE LOW-VALUE       TO W-WDQ401KY-MIN-X                             
237300     MOVE HIGH-VALUE      TO W-WDQ401KY-MAX-X                             
237400     MOVE ARB-KVSEMBRA        TO WS-KVSEMBRA                              
237500     MOVE ARB-IDRADNR-SISTA   TO WS-IDRADNR-SISTA                         
237600                                                                          
237700     MOVE W-ODEL-IDORDER  TO W-Q401KY-MIN-IDORDER                         
237800                             W-Q401KY-MAX-IDORDER                         
237900     MOVE W-ODEL-IDDC     TO W-Q401KY-MIN-IDDC                            
238000                             W-Q401KY-MAX-IDDC                            
238100     MOVE W-ODEL-IDPRC    TO W-IDPRC                                      
238200                                                                          
238300     PERFORM IMS-GHNP-WDQ221-FIRST                                        
238400                                                                          
238500     PERFORM UNTIL ORAD-IX  > MAX-ORAD                                    
238600                   OR Q221-SEG-SAKNAS                                     
238700                                                                          
238800       IF  4454-IDPRC(LOR-ADLAGOMR) = SPACE                               
238900         MOVE NEJ          TO LAGOMR-SW                                   
239000         MOVE LOR-ADLAGOMR TO W-Q401KY-MIN-ADLAGOMR                       
239100                              W-Q401KY-MAX-ADLAGOMR                       
239200                                                                          
239300         PERFORM UNTIL ORAD-IX  > MAX-ORAD                                
239400                       OR NYTT-LAGEROMRADE                                
239500                                                                          
239600           IF NY-ORDERDEL                                                 
239700              PERFORM CEI-LAES-EV-KUNDREG                                 
239800              PERFORM IMS-GHU-ORQF-WLORQF01                               
239900              MOVE NEJ TO ODEL-SW                                         
240000           ELSE                                                           
240100              IF NOT SPLITGRANS-OK                                        
240200                 PERFORM IMS-GHN-ORQF-WLORQF01                            
240300              END-IF                                                      
240400           END-IF                                                         
240600           IF SEGMENT-FINNS                                               
240700              MOVE JA       TO ARTIKEL-SW                                 
240800              MOVE ZERO     TO WS-KVROS                                   
240900                               WS-KVLS                                    
241000                               WS-KVEFRS                                  
241100                               WS-KVRESS                                  
241200                               WS-KART-KVRESS-ART                         
241300                               WS-KVAVBART                                
241400                               WS-ANTOBKR                                 
241500              PERFORM S20-LAES-ART-REG-WDD3                               
241600              PERFORM CEB-KOMPLETTERA-SPARRAR                             
241700              PERFORM CEC-AVROP-ARBETSTABELL                              
241800              PERFORM CED-KOMPLETTERA-RANSONERING                         
241900              PERFORM CEE-DEFINITIV-AVBOKNING                             
242000*LDC-GB                                                                   
242100              PERFORM CEK-EV-SPARA-I-LDC-TAB                              
242200              PERFORM S06-UPPLAGG-PU-PLE                                  
242300*POLE                                                                     
242400*SHOULD EVENT 151 BE SENT FOR BACKORDER?                                  
242500**            IF WS-KDORDBEK NOT  = 80                                    
242600**            OR WS-KDORDBEK NOT  = 90                                    
242700**            OR WS-KDORDBEK NOT  = 91                                    
242800                PERFORM CEL-CREATE-EVENT-151                              
242900**            END-IF                                                      
243000                                                                          
243100              IF NOT  DCS-NDC AND                                         
243200                 NOT (DCS-SDC AND DCS-CHINA)                              
243300*----- FÖR NDC OCH KINA-LDC HAR ALLT DETTA GJORTS I W411DEAV              
243400                PERFORM S04-UPPDAT-ART-REG-WDK6                           
243500              END-IF                                                      
243600              PERFORM S07-BORTTAG-ORDERRAD                                
243700              PERFORM CEF-KONTROLL-SPLITGRANS                             
243800              PERFORM CEG-JUSTERA-VARDE-PA-ORDERDEL                       
243900              ADD 1 TO ORAD-IX                                            
244540           END-IF                                                         
244550           IF LOR-KVRADER = 0                                             
244560              PERFORM IMS-DLET-WDQ221                                     
244570              PERFORM IMS-GHNP-WDQ221                                     
244580              MOVE JA TO LAGOMR-SW                                        
244593           END-IF                                                         
244600         END-PERFORM                                                      
244800       ELSE                                                               
244900         PERFORM IMS-GHNP-WDQ221                                          
245000       END-IF                                                             
245100     END-PERFORM                                                          
245260                                                                          
245300     IF Q221-SEG-SAKNAS                                                   
245400        IF 4002-FLORDSPL = JA                                             
245500           MOVE JA TO SPLIT-SW                                            
245600        END-IF                                                            
245700     END-IF                                                               
245800     MOVE 'END CE-BEHANDL'         TO PGMPOS                              
245900     .                                                                    
246000     EJECT                                                                
246100 CEK-EV-SPARA-I-LDC-TAB          SECTION.                                 
246200     MOVE 'STA CEK-EV-SPAR'         TO PGMPOS                             
246300*LDC-GB                                                                   
246400     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
246500     IF NOT DCS-CDC                                                       
246600     IF DIST34-ENGLAND-SDC AND                                            
246700        GMT-FLLDCKND = JA                                                 
246800       IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                           
246900       OR  OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                          
247000                                                                          
247100         MOVE +1        TO LDC-TAB-IX                                     
247200         MOVE +100      TO LDC-TAB-IX-MAX                                 
247300         PERFORM UNTIL ORAD-BERADREF = LDC-TAB-WIPID (LDC-TAB-IX)         
247400                    OR LDC-TAB-WIPID (LDC-TAB-IX) = HIGH-VALUE            
247500                    OR LDC-TAB-IX = LDC-TAB-IX-MAX                        
247600           ADD 1         TO LDC-TAB-IX                                    
247700         END-PERFORM                                                      
247800                                                                          
247900         IF ORAD-BERADREF = LDC-TAB-WIPID (LDC-TAB-IX)                    
248000            MOVE LDC-TAB-IX          TO WS-WIP-LOPNR                      
248100            MOVE '-'                 TO WS-WIP-STRECK                     
248200            MOVE ORAD-BERADREF       TO WS-WIPID                          
248300         ELSE                                                             
248400            MOVE LDC-TAB-IX     TO WS-WIP-LOPNR                           
248500            MOVE '-'            TO WS-WIP-STRECK                          
248600            MOVE ORAD-BERADREF  TO LDC-TAB-WIPID  (LDC-TAB-IX)            
248700                                   WS-WIPID                               
248800         END-IF                                                           
248900       END-IF                                                             
249000     END-IF                                                               
249100     END-IF                                                               
249200     MOVE 'END CEK-EV-SPAR'         TO PGMPOS                             
249300     .                                                                    
249400     EJECT                                                                
249500                                                                          
249600 CEB-KOMPLETTERA-SPARRAR SECTION.                                         
249700     MOVE 'STA CEB-KOMPLET'         TO PGMPOS                             
249800                                                                          
249900     MOVE ORAD-BERADREF          TO SPAR-BERADREF                         
250000     MOVE OHUV-BEKUNDRF          TO SPAR-BEKUNDRF                         
250100     MOVE AREG-FLAVRART          TO SPAR-FLAVRART                         
250200     MOVE OHUV-FLEMBORD          TO SPAR-FLEMBORD                         
250300     MOVE OHUV-FLOVRLEV          TO SPAR-FLOVRLEV                         
250400     MOVE OHUV-FLORDSPE          TO SPAR-FLORDSPE                         
250500     MOVE OHUV-FLFORBI           TO SPAR-FLFORBI                          
250600     MOVE AREG-FLIART            TO SPAR-FLIART                           
250700     MOVE AREG-FLMARKSP          TO SPAR-FLMARKSP                         
250800     MOVE AREG-FLLSRDEL          TO SPAR-FLLSRDEL                         
250900     MOVE AREG-FLRADREF          TO SPAR-FLRADREF                         
251000     MOVE ORAD-FLRESTN           TO SPAR-FLRESTN                          
251100     MOVE ORAD-IDARTNR           TO SPAR-IDARTNR                          
251200     MOVE ORAD-IDDISTR           TO SPAR-IDDISTR                          
251300     MOVE ORAD-IDKUNDNR          TO SPAR-IDKUNDNR                         
251400     MOVE ORAD-IDKUNDRF-RO       TO SPAR-IDKUNDRF-RO                      
251500     MOVE ORAD-IDDC              TO SPAR-IDDC                             
251600     MOVE ORAD-IDSYSTEM          TO SPAR-IDSYSTEM                         
251700     MOVE AREG-KDERS-UTG         TO SPAR-KDERS-UTG                        
251800     MOVE AREG-KDERS             TO SPAR-KDERS                            
251900     MOVE OHUV-KDFAKTYP          TO SPAR-KDFAKTYP                         
252000     MOVE AREG-KDLEVSP           TO SPAR-KDLEVSP                          
252100     MOVE 7                      TO SPAR-KDORDBEH                         
252200     MOVE OHUV-KDORDKL           TO SPAR-KDORDKL                          
252300     MOVE AREG-KDPRODSL          TO SPAR-KDPRODSL                         
252400     MOVE AREG-KDSORT            TO SPAR-KDSORT                           
252500     MOVE ORAD-KDPRTYP           TO SPAR-KDPRTYP                          
252600     MOVE ORAD-KDTPOTYP          TO SPAR-KDTPOTYP                         
252700     MOVE AREG-KDUART            TO SPAR-KDUART                           
252800     MOVE AREG-PRARTSTD          TO SPAR-PRARTSTD                         
252900     MOVE AREG-TIFINLV           TO SPAR-TIFINLV                          
253000     MOVE ORAD-TIRODAT           TO SPAR-TIRODAT                          
253100     MOVE ORAD-TITPO             TO SPAR-TITPO                            
253200     MOVE ORAD-FLSDCLEV          TO SPAR-FLSDCLEV                         
253300     MOVE OHUV-TIREPDAT          TO SPAR-TIREPDAT                         
253400                                                                          
253500     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
253600                                       SPAR-WDF8A-PCB                     
253700                                       SPAR-WDK6-PCB                      
253800                                                                          
253900                                                                          
254000     MOVE ORAD-IDDC              TO WS-IDDC                               
254100                                                                          
254200                                                                          
254300*ONE PART WHERE DATE ON CDC IS TO FAR AWAY BUT ITS OK DATE ON NDC.        
254400*FLPUBCDC IS SET IN W411SPAR                                              
254500                                                                          
254600     IF SPAR-FLPUBCDC = YES                                               
254700       MOVE 0                    TO SPAR-KDORDBEK                         
254800     END-IF                                                               
254900                                                                          
255000     IF SPAR-KDORDBEK > 0                                                 
255100                                                                          
255200        MOVE SPAR-KDORDBEK                   TO WS-KDORDBEK               
255300        MOVE '4375SPAR'                      TO WS-IDPGM                  
255400        MOVE ORAD-KVBEART-Q                  TO WS-KVROS                  
255500        PERFORM S02-ORDERBEKRAFTELSE                                      
255600        PERFORM S08-SKAPA-RYETRANS                                        
255700                                                                          
255800        IF CDC                   AND                                      
255900           ORAD-IDLEVNR    = SPACE AND                                    
256000           ORAD-TIRODAT    = 0   AND                                      
256100           ORAD-IDKAMPRF   = 0   AND                                      
256200           OHUV-FLORDSPE = NEJ   AND                                      
256300           OHUV-FLOVRLEV = NEJ                                            
256400                                                                          
256500           PERFORM CEBA-UPPDAT-WDK9                                       
256600        END-IF                                                            
256700                                                                          
256800        MOVE WS-IDDC TO W-IDDC-B6                                         
256900        PERFORM IMS-GU-WDB601                                             
257000        IF (DCS-SDC OR  DCS-NDC)                                          
257100        AND OHUV-FLORDSPE = NEJ                                           
257200        AND OHUV-FLOVRLEV = NEJ                                           
257300                                                                          
257400           PERFORM CEBB-UPPDAT-WDK7-K9                                    
257500        END-IF                                                            
257600                                                                          
257700        IF (DCS-CDC OR DCS-CDC-TR) AND                                    
257800           (ORAD-TIRODAT    > 0    OR                                     
257900            ORAD-IDKAMPRF   > 0)                                          
258000                                                                          
258100           PERFORM CEBC-MINSKA-KVRESS-WDK6                                
258200        END-IF                                                            
258300                                                                          
258400        IF (DCS-CDC OR DCS-CDC-TR)                                        
258500        OR  DCS-NDC                                                       
258600        OR (DCS-SDC and DCS-CHINA)                                        
258700          PERFORM CEBD-EV-UPPDAT-WDK7-REFILL                              
258800        END-IF                                                            
258900                                                                          
259000        IF WS-KDORDBEK = '90' OR '91'                                     
259100*           90 = RESTNOTERAD, 91 = RESTNOTERAD IGEN.                      
259200                                                                          
259300           IF ORAD-IDKUNDRF-RO NOT = '00000     '                         
259400              AND                                                         
259500              ORAD-IDKUNDRF-RO NOT = '0000000   '                         
259600                                                                          
259700              IF ORAD-TIRODAT = ZERO                                      
259800                PERFORM S12-SKAPA-RYKTRANS                                
259900              END-IF                                                      
260000              PERFORM S10-UPPDAT-BEFINTLIG-RESTORDER                      
260100           ELSE                                                           
260200              MOVE 2 TO WS-KDROO                                          
260300              PERFORM S03-NYUPPLAGG-RESTORDER                             
260400              PERFORM S12-SKAPA-RYKTRANS                                  
260500           END-IF                                                         
260600           IF NOT  DCS-NDC  AND                                           
260700              NOT (DCS-SDC AND DCS-CHINA)                                 
260800*------ FÖR NDC OCH KINA-LDC HAR ALLT DETTA GJORTS I W411DEAV             
260900             PERFORM S04-UPPDAT-ART-REG-WDK6                              
261000           END-IF                                                         
261100        END-IF                                                            
261200                                                                          
261300        IF ORAD-KDORDKL = 0                                               
261400        AND VOR-KON                                                       
261500            IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                        
261600                PERFORM S11-UPPDAT-VOR                                    
261700            ELSE                                                          
261800                PERFORM S11D-UPDATE-VORKONY                               
261900            END-IF                                                        
262000            PERFORM S23-DELETE-PRICE-Q-LINE                               
262100        END-IF                                                            
262200                                                                          
262300        PERFORM S07-BORTTAG-ORDERRAD                                      
262400        MOVE NEJ TO ARTIKEL-SW                                            
262500     END-IF                                                               
262600     MOVE 'END CEB-KOMPLET'         TO PGMPOS                             
262700     .                                                                    
262800     EJECT                                                                
262900 CEBA-UPPDAT-WDK9 SECTION.                                                
263000     MOVE 'STA CEBA-UPPDAT'         TO PGMPOS                             
263100                                                                          
263200     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
263300                                                                          
263400     PERFORM IMS-GHU-ARTM-WLARTM01                                        
263500                                                                          
263600     IF SEGMENT-FINNS                                                     
263700        IF ORAD-KDORDKL = 0                                               
263800                                                                          
263900           IF VOR-KON                                                     
264000              CONTINUE                                                    
264100           ELSE                                                           
264200              COMPUTE ART-KVOKS-VOR =                                     
264300                      ART-KVOKS-VOR - ORAD-KVBEART-Q                      
264400              END-COMPUTE                                                 
264500           END-IF                                                         
264600                                                                          
264700           COMPUTE ART-KVPREAVB-VOR =                                     
264800                   ART-KVPREAVB-VOR - ORAD-KVPREAVB                       
264900           END-COMPUTE                                                    
265000        ELSE                                                              
265100           IF ORAD-KDORDKL = 1                                            
265200              COMPUTE ART-KVOKS-DAG =                                     
265300                      ART-KVOKS-DAG - ORAD-KVBEART-Q                      
265400              END-COMPUTE                                                 
265500                                                                          
265600              COMPUTE ART-KVPREAVB-DAG =                                  
265700                      ART-KVPREAVB-DAG - ORAD-KVPREAVB                    
265800              END-COMPUTE                                                 
265900                                                                          
266000              COMPUTE ART-KVPRERO-DAG =                                   
266100                      ART-KVPRERO-DAG - ORAD-KVPRERO                      
266200              END-COMPUTE                                                 
266300           ELSE                                                           
266400              COMPUTE ART-KVOKS-BULK =                                    
266500                      ART-KVOKS-BULK - ORAD-KVBEART-Q                     
266600              END-COMPUTE                                                 
266700                                                                          
266800              COMPUTE ART-KVPREAVB-BULK =                                 
266900                      ART-KVPREAVB-BULK - ORAD-KVPREAVB                   
267000              END-COMPUTE                                                 
267100                                                                          
267200              COMPUTE ART-KVPRERO-BULK =                                  
267300                      ART-KVPRERO-BULK - ORAD-KVPRERO                     
267400              END-COMPUTE                                                 
267500           END-IF                                                         
267600        END-IF                                                            
267700        PERFORM IMS-REPL-ARTM-WLARTM01                                    
267800     END-IF                                                               
267900     MOVE 'END CEBA-UPPDAT'         TO PGMPOS                             
268000     .                                                                    
268100     EJECT                                                                
268200 CEBB-UPPDAT-WDK7-K9 SECTION.                                             
268300     MOVE 'STA CEBB-UPPDAT'         TO PGMPOS                             
268400                                                                          
268500     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
268600     MOVE ORAD-IDDC    TO W-IDDC                                          
268700                                                                          
268800     PERFORM IMS-GHU-WDK711                                               
268900     IF  ORAD-KDORDKL > +1                                                
269000         SUBTRACT ORAD-KVBEART-Q        FROM SLAG-KVOKS-BULK              
269100     ELSE                                                                 
269200         SUBTRACT ORAD-KVBEART-Q        FROM SLAG-KVOKS-DAG               
269300     END-IF                                                               
269400                                                                          
269500*--- GÄLLER NDC OCH KINA-LDC                                              
269600     IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                               
269700       IF  ORAD-TIRODAT > 0                                               
269800           SUBTRACT ORAD-KVBEART-Q    FROM SLAG-KVRESS                    
269900       END-IF                                                             
270000                                                                          
270100       IF (WS-KDORDBEK = '90'                                             
270200       OR  WS-KDORDBEK = '91')                                            
270300       AND ORAD-IDDC = ORAD-IDDC-RO                                       
270400       AND WS-KVROS > 0                                                   
270500         IF (DCS-NDC-CN OR (DCS-NDC-NA AND DCS-USA))                      
270600         AND SLAG-IDDC-REF = SPACE                                        
270700         AND OUTPUT-MSG-IX < MAX-ANT-OUTPUT-MSG                           
270800         AND SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                   
270900         AND SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                    
271000            MOVE JA  TO SW-RO-LARM                                        
271100         ELSE                                                             
271200            MOVE NEJ TO SW-RO-LARM                                        
271300         END-IF                                                           
271400         IF  ORAD-KDORDKL > +1                                            
271500           ADD WS-KVROS                 TO SLAG-KVROS-BULK                
271600         ELSE                                                             
271700           ADD WS-KVROS                 TO SLAG-KVROS-DAG                 
271800         END-IF                                                           
271900       END-IF                                                             
272000                                                                          
272100       IF ORAD-KDORDKL = 0                                                
272200       AND VOR-KON                                                        
272300         ADD ORAD-KVBEART-Q             TO SLAG-KVOKS-DAG                 
272400       END-IF                                                             
272500     END-IF                                                               
272600                                                                          
272700     PERFORM IMS-REPL-WDK7                                                
272800                                                                          
272900     IF SKAPA-RO-LARM                                                     
273000        PERFORM S04E-LARM-2191-MID-CN-US                                  
273100     END-IF                                                               
273200                                                                          
273300*------- RESTNOTERING PÅ ANNAT NDC                                        
273400     IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                               
273500       IF (WS-KDORDBEK = '90'                                             
273600       OR  WS-KDORDBEK = '91')                                            
273700       AND ORAD-IDDC NOT = ORAD-IDDC-RO                                   
273800       AND WS-KVROS > 0                                                   
273900           MOVE ORAD-IDDC-RO          TO W-IDDC                           
274000           PERFORM IMS-GHU-WDK711                                         
274100                                                                          
274200           IF (DCS-NDC-CN OR (DCS-NDC-NA AND DCS-USA))                    
274300           AND SLAG-IDDC-REF = SPACE                                      
274400           AND OUTPUT-MSG-IX < MAX-ANT-OUTPUT-MSG                         
274500           AND SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                 
274600           AND SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                  
274700              MOVE JA  TO SW-RO-LARM                                      
274800           ELSE                                                           
274900              MOVE NEJ TO SW-RO-LARM                                      
275000           END-IF                                                         
275100                                                                          
275200           IF  ORAD-KDORDKL > +1                                          
275300             ADD WS-KVROS                 TO SLAG-KVROS-BULK              
275400           ELSE                                                           
275500             ADD WS-KVROS                 TO SLAG-KVROS-DAG               
275600           END-IF                                                         
275700           PERFORM IMS-REPL-WDK7                                          
275800                                                                          
275900           IF SKAPA-RO-LARM                                               
276000              PERFORM S04E-LARM-2191-MID-CN-US                            
276100           END-IF                                                         
276200       END-IF                                                             
276300     END-IF                                                               
276400                                                                          
276500*--- GÄLLER SDC FÖRUTOM KINA-LDC                                          
276600     IF  ORAD-KDORDKL = 0                                                 
276700     AND VOR-KON                                                          
276800     AND (DCS-SDC AND NOT DCS-CHINA)                                      
276900       PERFORM IMS-GHU-ARTM-WLARTM01                                      
277000       COMPUTE ART-KVOKS-VOR =                                            
277100               ART-KVOKS-VOR + ORAD-KVBEART-Q                             
277200                                                                          
277300       PERFORM IMS-REPL-ARTM-WLARTM01                                     
277400     END-IF                                                               
277500     MOVE 'END CEBB-UPPDAT'         TO PGMPOS                             
277600     .                                                                    
277700     EJECT                                                                
277800 CEBC-MINSKA-KVRESS-WDK6 SECTION.                                         
277900     MOVE 'STA CEBC-MINSKA'         TO PGMPOS                             
278000                                                                          
278100     MOVE ORAD-IDARTNR              TO W-IDARTNR                          
278200     PERFORM IMS-GHU-WDK611                                               
278300     IF SEGMENT-FINNS                                                     
278400        SUBTRACT ORAD-KVBEART-Q     FROM CLAG-KVRESS                      
278500        PERFORM IMS-REPL-WDK6                                             
278600     END-IF                                                               
278700     MOVE 'END CEBC-MINSKA'         TO PGMPOS                             
278800     .                                                                    
278900     EJECT                                                                
279000 CEBD-EV-UPPDAT-WDK7-REFILL         SECTION.                              
279100     MOVE 'STA CEBD-EV-UPP'         TO PGMPOS                             
279200                                                                          
279300******************************************************************        
279400*                                                                         
279500*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
279600*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
279700*                                                                         
279800******************************************************************        
279900                                                                          
280000     MOVE OHUV-IDDISTR         TO W-TP4TRAN-IDDISTR                       
280100                                                                          
280200     PERFORM DB2-SELECT-TP4TRAN                                           
280300                                                                          
280400                                                                          
280500*REFILLORDERRAD TILL  MED SPÄRRAD ARTIKEL.                                
280600     MOVE OHUV-IDDISTR      TO DIST35-IDDISTR                             
280700     IF  (DIST35-REFILL                                                   
280800     OR   DIST35-REFILL-INOM-NDC                                          
280900     OR   DIST35-NA-TRANSFER                                              
281000     OR   DIST35-PACIFIC-TRANSFER                                         
281100     OR   DIST35-REFILL-INOM-JP                                           
281200     OR   DIST35-CN-TRANSFER                                              
281300     OR   DIST35-NA-NDC-RETURNS                                           
281400     OR   RADER-FINNS)                                                    
281500      AND WS-KDORDBEK NOT = '90'                                          
281600      AND WS-KDORDBEK NOT = '91'                                          
281700                                                                          
281800       IF RADER-FINNS                                                     
281900         MOVE TP4TRAN-IDDC-REC      TO W-IDDC                             
282000       ELSE                                                               
282100         MOVE OHUV-IDDISTR        TO WS-IDDISTR-NUM5                      
282200         SEARCH ALL DIST57-REFILL-DC                                      
282300            AT END                                                        
282400               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
282500                                TO FELTEXT                                
282600               CALL FELLOG                                                
282700            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR-NUM5          
282800               MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC              
282900         END-SEARCH                                                       
283000       END-IF                                                             
283100                                                                          
283200       MOVE ORAD-IDARTNR TO W-IDARTNR                                     
283300                                                                          
283400       PERFORM IMS-GHU-WDK711                                             
283500       SUBTRACT ORAD-KVBEART-Q      FROM SLAG-KVBEART                     
283600                                                                          
283700       PERFORM IMS-REPL-WDK7                                              
283800     ELSE                                                                 
283900        IF  DIST35-NONVCC-CDC-REFILL                                      
284000        AND WS-KDORDBEK NOT = '90'                                        
284100        AND WS-KDORDBEK NOT = '91'                                        
284200                                                                          
284300         MOVE OHUV-IDDISTR        TO WS-IDDISTR-NUM5                      
284400         SEARCH ALL DIST57-REFILL-DC                                      
284500            AT END                                                        
284600               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
284700                                TO FELTEXT                                
284800               CALL FELLOG                                                
284900            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR-NUM5          
285000               MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC              
285100         END-SEARCH                                                       
285200                                                                          
285300         MOVE ORAD-IDARTNR TO W-IDARTNR                                   
285400                                                                          
285500         PERFORM IMS-GHU-WDK611                                           
285600         SUBTRACT ORAD-KVBEART-Q      FROM CLAG-KVBEART                   
285700                                                                          
285800         PERFORM IMS-REPL-WDK6                                            
285900       END-IF                                                             
286000     END-IF                                                               
286100     MOVE 'END CEBD-EV-UPP'         TO PGMPOS                             
286200     .                                                                    
286300     SKIP2                                                                
286400 CEC-AVROP-ARBETSTABELL SECTION.                                          
286500     MOVE 'STA CEC-AVROP '         TO PGMPOS                              
286600                                                                          
286700     COMPUTE WS-ORDERVIKT-PER-RAD =                                       
286800            ((ORAD-KVBEART-Q * ORAD-VKART) / 1000)                        
286900                                                                          
287000     COMPUTE WS-ORDERVOLYM-PER-RAD =                                      
287100            ((ORAD-KVBEART-Q * ORAD-VLARTNTO) / 1000000)                  
287200                                                                          
287300     SUBTRACT 1                     FROM LOR-KVRADER                      
287400     SUBTRACT WS-ORDERVIKT-PER-RAD  FROM LOR-VKORDNTO                     
287500     SUBTRACT WS-ORDERVOLYM-PER-RAD FROM LOR-VLORDNTO                     
287600                                                                          
287700     PERFORM IMS-REPL-WDQ221                                              
287800                                                                          
287900     IF ORAD-KDSPEEMB > 0                                                 
288000        IF ORAD-IDSPECEMB = 0                                             
288100           IF WS-KVSEMBRA > 0                                             
288200              SUBTRACT 1 FROM WS-KVSEMBRA                                 
288300           END-IF                                                         
288400        END-IF                                                            
288500     END-IF                                                               
288600     MOVE 'END CEC-AVROP  '         TO PGMPOS                             
288700     .                                                                    
288800     EJECT                                                                
288900 CED-KOMPLETTERA-RANSONERING SECTION.                                     
289000     MOVE 'STA CED-KOMPLET'         TO PGMPOS                             
289100                                                                          
289200     IF ARTIKEL-OK AND CDC                                                
289300        MOVE ORAD-BERADREF        TO RANS-BERADREF                        
289400        MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                        
289500        MOVE OHUV-FLFORBI         TO RANS-FLFORBI                         
289600        MOVE OHUV-FLORDSPE        TO RANS-FLORDSPE                        
289700        MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                        
289800        MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                        
289900        MOVE ORAD-IDARTNR         TO RANS-IDARTNR                         
290000        MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                         
290100        MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                         
290200        MOVE ORAD-TIRODAT         TO RANS-TIRODAT                         
290300        MOVE 7                    TO RANS-KDORDBEH                        
290400        MOVE OHUV-KDORDKL         TO RANS-KDORDKL                         
290500        MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                       
290600        MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                        
290700        MOVE AREG-KDERS           TO RANS-KDERS                           
290800        MOVE AREG-KVLS            TO RANS-KVLS                            
290900        MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                       
291000        MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                        
291100        MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                        
291200        MOVE AREG-KVRESS          TO RANS-KVRESS                          
291300        MOVE AREG-KVSPANT         TO RANS-KVSPANT                         
291400        MOVE AREG-KVUTRS          TO RANS-KVUTRS                          
291500        MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                        
291600                                                                          
291700        MOVE ORAD-IDARTNR         TO W-IDARTNR                            
291800        PERFORM IMS-GHU-WDK611                                            
291900        IF  SEGMENT-FINNS                                                 
292000        AND CLAG-FLCDART = JA                                             
292100           MOVE ZERO              TO RANS-RERF-ART-UT                     
292200           MOVE 1                 TO RANS-RERF-RAD-UT                     
292300        ELSE                                                              
292400           IF AREG-KDPRODSL = 71 OR 72 OR 73 OR 74                        
292500             MOVE ZERO            TO RANS-RERF-ART-UT                     
292600             MOVE 1               TO RANS-RERF-RAD-UT                     
292700           ELSE                                                           
292800             CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB              
292900                                            RANS-ARTM-PCB                 
293000                                            RANS-ARTS-PCB                 
293100          END-IF                                                          
293200        END-IF                                                            
293300     END-IF                                                               
293400     MOVE 'END CED-KOMPLET'         TO PGMPOS                             
293500     .                                                                    
293600     EJECT                                                                
293700 CEE-DEFINITIV-AVBOKNING SECTION.                                         
293800     MOVE 'STA CEE-DEFINITIV'       TO PGMPOS                             
293900                                                                          
294000     IF ARTIKEL-OK                                                        
294100        IF (((OHUV-FLORDSPE = JA AND OHUV-IDSYSTEM NOT = 'W216')          
294200         OR  OHUV-FLOVRLEV = JA)                                          
294300         AND OHUV-FLLSBOK = JA)                                           
294400        OR   OHUV-FLLSBOK = NEJ                                           
294500        OR   ORAD-IDLEVNR NOT = SPACE                                     
294600            MOVE ORAD-KVBEART TO WS-KVLS                                  
294700                                 WS-KVEFRS                                
294800                                 WS-KVAVBART                              
294900            MOVE ZERO         TO WS-KDORDBEK                              
295000                                                                          
295100           MOVE OHUV-IDDISTR      TO DIST18-IDDISTR                       
295200           MOVE WS-IDDC TO W-IDDC-B6                                      
295300           PERFORM IMS-GU-WDB601                                          
295400           IF ((((DCS-CDC OR DCS-CDC-TR)                                  
295500           AND DIST18-SKROT-KVAL-CDC)                                     
295600           OR  DCS-SDC OR  DCS-NDC)                                       
295700           AND OHUV-FLLSBOK = JA)                                         
295800             PERFORM CEEA-DEAV-AVBOK                                      
295900           ELSE                                                           
296000*SVS FROG                                                                 
296100             MOVE SPACE          TO DEAV-FLAKPLOC-UT                      
296200             MOVE ZERO           TO DEAV-RERF-RAD-UT                      
296300                                    DEAV-KDORDBEK-UT                      
296400                                    DEAV-KVEFRS-UT                        
296500                                    DEAV-KVLS-UT                          
296600                                    DEAV-KVRESS-UT                        
296700                                    DEAV-KVROS-UT                         
296800                                    DEAV-KVAVBART-UT                      
296900                                    DEAV-KDROO-UT                         
297000                                    DEAV-KVBEART-Q-IN                     
297100           END-IF                                                         
297200                                                                          
297300           IF WS-KDORDBEK > 0                                             
297400                                                                          
297500              PERFORM S02-ORDERBEKRAFTELSE                                
297600              PERFORM S08-SKAPA-RYETRANS                                  
297700           END-IF                                                         
297800                                                                          
297900*------ TILLFÄLLIGT INLAGT FÖR FIL TILL LOGISTIK                          
298000*       OBS! LÄGG INGET MELLAN FÖREGÅENDE IF-SATS                         
298100*       OCH IF-SATSEN NEDAN.                                              
298200                                                                          
298300           IF (WS-KDORDBEK = 90                                           
298400           OR  WS-KDORDBEK = 91)                                          
298500           AND DEAV-IDDC-IN = 11                                          
298600             PERFORM S98-SKAPA-RYXTRANS                                   
298700           ELSE                                                           
298800             IF  DEAV-KVBEART-Q-IN > DEAV-KVAVBART-UT                     
298900             AND DEAV-KVAVBART-UT > 0                                     
299000             AND DEAV-IDDC-IN = 11                                        
299100               PERFORM S99-SKAPA-RYXTRANS                                 
299200             END-IF                                                       
299300           END-IF                                                         
299400*------ END                                                               
299500                                                                          
299600           IF WS-KVAVBART = 0                                             
299700              IF NOT  DCS-NDC AND                                         
299800                 NOT (DCS-SDC AND DCS-CHINA)                              
299900                PERFORM S04-UPPDAT-ART-REG-WDK6                           
300000              END-IF                                                      
300100              PERFORM S07-BORTTAG-ORDERRAD                                
300200              MOVE NEJ TO ARTIKEL-SW                                      
300300           END-IF                                                         
300400        ELSE                                                              
300500           PERFORM CEEA-DEAV-AVBOK                                        
300600                                                                          
300700           IF WS-KDORDBEK > 0                                             
300800                                                                          
300900              PERFORM S02-ORDERBEKRAFTELSE                                
301000              PERFORM S08-SKAPA-RYETRANS                                  
301100                                                                          
301200           END-IF                                                         
301300                                                                          
301400*------ TILLFÄLLIGT INLAGT FÖR FIL TILL LOGISTIK                          
301500*       OBS! LÄGG INGET MELLAN FÖREGÅENDE IF-SATS                         
301600*       OCH IF-SATSEN NEDAN.                                              
301700                                                                          
301800           IF (WS-KDORDBEK = 90                                           
301900           OR  WS-KDORDBEK = 91)                                          
302000           AND DEAV-IDDC-IN = 11                                          
302100             PERFORM S98-SKAPA-RYXTRANS                                   
302200           ELSE                                                           
302300             IF  DEAV-KVBEART-Q-IN > DEAV-KVAVBART-UT                     
302400             AND DEAV-KVAVBART-UT > 0                                     
302500             AND DEAV-IDDC-IN = 11                                        
302600               PERFORM S99-SKAPA-RYXTRANS                                 
302700             END-IF                                                       
302800           END-IF                                                         
302900*------ END                                                               
303000                                                                          
303100*                KDORDBEK 92 = VOR, RESTNOTERAD KVANT                     
303200           IF WS-KDORDBEK = 92                                            
303300              IF NDC OR LDC-CN                                            
303400                  PERFORM S11-UPPDAT-VOR                                  
303500              ELSE                                                        
303600                  PERFORM S11D-UPDATE-VORKONY                             
303700              END-IF                                                      
303800              PERFORM S23-DELETE-PRICE-Q-LINE                             
303900           END-IF                                                         
304000                                                                          
304100           IF WS-KVROS > 0                                                
304200                                                                          
304300              IF ORAD-IDKUNDRF-RO NOT = '00000     '                      
304400                 AND                                                      
304500                 ORAD-IDKUNDRF-RO NOT = '0000000   '                      
304600                                                                          
304700*START FIX OM WDA5 SAKNAS, WLORDP01                                       
304800*                IF  ORAD-IDDISTR           = +00076   AND                
304900*                    ORAD-IDKUNDNR          = +0037473 AND                
305000*                    ORAD-IDKUNDRF-RO (3:5) = 47519    AND                
305100*                    ORAD-IDARTNR           = +000272196                  
305200*                  PERFORM S03-NYUPPLAGG-RESTORDER                        
305300*                ELSE                                                     
305400                                                                          
305500                 IF ORAD-TIRODAT = ZERO                                   
305600                   PERFORM S12-SKAPA-RYKTRANS                             
305700                 END-IF                                                   
305800                                                                          
305900                 PERFORM S10-UPPDAT-BEFINTLIG-RESTORDER                   
306000*                END-IF                                                   
306100*END FIX                                                                  
306200              ELSE                                                        
306300                 PERFORM S03-NYUPPLAGG-RESTORDER                          
306400                 IF WS-KDORDBEK = 90                                      
306500*                      KDORDBEK 90 = RESTNOTERAD                          
306600                   PERFORM S12-SKAPA-RYKTRANS                             
306700                 END-IF                                                   
306800              END-IF                                                      
306900           END-IF                                                         
307000                                                                          
307100           IF WS-KVAVBART = 0                                             
307200              IF NOT  DCS-NDC  AND                                        
307300                 NOT (DCS-SDC AND DCS-CHINA)                              
307400                PERFORM S04-UPPDAT-ART-REG-WDK6                           
307500              END-IF                                                      
307600              PERFORM S07-BORTTAG-ORDERRAD                                
307700              MOVE NEJ TO ARTIKEL-SW                                      
307800           END-IF                                                         
307900        END-IF                                                            
308000     END-IF                                                               
308100     MOVE 'END CEE-DEFINITIV'       TO PGMPOS                             
308200     .                                                                    
308300     EJECT                                                                
308400 CEEA-DEAV-AVBOK SECTION.                                                 
308500     MOVE 'STA CEEA-DEAV'           TO PGMPOS                             
308600                                                                          
308700     MOVE ORAD-IDKAMPRF             TO DEAV-IDKAMPRF-IN                   
308800     MOVE OHUV-FLFORBI              TO DEAV-FLFORBI-IN                    
308900     MOVE ORAD-IDARTNR              TO DEAV-IDARTNR-IN                    
309000     MOVE AREG-IDANSK               TO DEAV-IDANSK-IN                     
309100     MOVE ORAD-TIRODAT              TO DEAV-TIRODAT-IN                    
309200     MOVE ORAD-KDORDKL              TO DEAV-KDORDKL-IN                    
309300     MOVE ORAD-IDDC                 TO DEAV-IDDC-IN                       
309400     MOVE ORAD-IDDC-RO              TO DEAV-IDDC-RO-IN                    
309500     MOVE ORAD-IDDISTR              TO DEAV-IDDISTR-IN                    
309600     MOVE AREG-KDLEVSP              TO DEAV-KDLEVSP-IN                    
309700     IF CDC                                                               
309800       MOVE AREG-KVAKS-CDC          TO DEAV-KVAKS-CDC-IN                  
309900       MOVE AREG-KVAKS-PAV          TO DEAV-KVAKS-PAV-IN                  
310000*SVS FROG                                                                 
310100       MOVE ORAD-IDDISTR            TO TEST-IDDISTR                       
310200       IF DIST20-EMBALLAGE-SVS                                            
310300       OR LOR-IDPRC = 2600                                                
310400         IF AREG-KVLS-SVS > zero                                          
310500           MOVE AREG-KVLS-SVS       TO DEAV-KVLS-IN                       
310600           MOVE ZERO                TO DEAV-KVRESS-IN                     
310700         ELSE                                                             
310800           MOVE AREG-KVLS           TO DEAV-KVLS-IN                       
310900           MOVE AREG-KVRESS         TO DEAV-KVRESS-IN                     
311000         END-IF                                                           
311100       ELSE                                                               
311200         MOVE AREG-KVLS             TO DEAV-KVLS-IN                       
311300         MOVE AREG-KVRESS           TO DEAV-KVRESS-IN                     
311400       END-IF                                                             
311500       MOVE AREG-KVSPANT            TO DEAV-KVSPANT-IN                    
311600       MOVE AREG-KVUTRS             TO DEAV-KVUTRS-IN                     
311700       MOVE AREG-KVQPACK-0          TO DEAV-KVQPACK-0-IN                  
311800*      MOVE AREG-KVQPACK-1          TO DEAV-KVQPACK-1-IN                  
311900       MOVE AREG-IDFKNGRP           TO DEAV-IDFKNGRP-IN                   
312000*      MOVE AREG-KDSORT             TO DEAV-KDSORT-IN                     
312100       MOVE AREG-KVSPARR-KVAL       TO DEAV-KVSPARR-KVAL-IN               
312200       MOVE RANS-RERF-ART-UT        TO DEAV-RERF-ART-IN                   
312300       MOVE RANS-RERF-RAD-UT        TO DEAV-RERF-RAD-NY-IN                
312400     ELSE                                                                 
312500       MOVE ZERO                    TO DEAV-KVAKS-CDC-IN                  
312600                                       DEAV-KVAKS-PAV-IN                  
312700                                       DEAV-KVLS-IN                       
312800                                       DEAV-KVRESS-IN                     
312900                                       DEAV-KVUTRS-IN                     
313000                                       DEAV-RERF-ART-IN                   
313100                                       DEAV-RERF-RAD-NY-IN                
313200*                                      DEAV-KVQPACK-1-IN                  
313300*                                      DEAV-KDSORT-IN                     
313400       IF DCS-CHINA                                                       
313500          MOVE AREG-KVSPANT         TO DEAV-KVSPANT-IN                    
313600       ELSE                                                               
313700          MOVE ZERO                 TO DEAV-KVSPANT-IN                    
313800       END-IF                                                             
313900     END-IF                                                               
314000*    we always move qpack and sort to W411deav                            
314100*    and then in W411deav if not cdc move zero                            
314200*    when calling W411kvan                                                
314300     MOVE AREG-KVQPACK-1            TO DEAV-KVQPACK-1-IN                  
314400     MOVE AREG-KDSORT               TO DEAV-KDSORT-IN                     
314500     MOVE ORAD-KDPRODSL             TO DEAV-KDPRODSL-IN                   
314600     MOVE ORAD-FLAKPLOC             TO DEAV-FLAKPLOC-IN                   
314700     MOVE ORAD-KVBEART-Q            TO DEAV-KVBEART-Q-IN                  
314800     MOVE ORAD-KVPREAVB             TO DEAV-KVPREAVB-IN                   
314900     MOVE ORAD-KVPRERO              TO DEAV-KVPRERO-IN                    
315000     MOVE ORAD-IDKUNDRF-RO          TO DEAV-IDKUNDRF-RO-IN                
315100     MOVE ORAD-RERF-RAD             TO DEAV-RERF-RAD-IN                   
315200     MOVE ORAD-IDLEVNR              TO DEAV-IDLEVNR-IN                    
315300     MOVE ORAD-KDKVBRYT             TO DEAV-KDKVBRYT-IN                   
315400     MOVE ORAD-FLRESTN              TO DEAV-FLRESTN-IN                    
315500     MOVE ORAD-KDTPOTYP             TO DEAV-KDTPOTYP-IN                   
315600     MOVE ORAD-IDSYSTEM             TO DEAV-IDSYSTEM-IN                   
315700     MOVE ORAD-ADLAGOMR             TO DEAV-ADLAGOMR-IN                   
315800     MOVE OHUV-FLORDSPE             TO DEAV-FLORDSPE-IN                   
315900     MOVE OHUV-FLOVRLEV             TO DEAV-FLOVRLEV-IN                   
316000     MOVE ORAD-IDKUNDNR             TO DEAV-IDKUNDNR-IN                   
316100     MOVE ORAD-IDORDNR7             TO DEAV-IDORDNR5-IN                   
316200     MOVE W-Q301KY-IDPRODNR         TO DEAV-IDPRODNR-IN                   
316300     MOVE W-Q301KY-IDPLKLST         TO DEAV-IDPLKLST-IN                   
316400     MOVE ORAD-BERADREF             TO DEAV-BERADREF-IN                   
316500     MOVE ORAD-FLSDCLEV             TO DEAV-FLSDCLEV-IN                   
316600                                                                          
316700     CALL W411DEAV USING DEAV-W411DEAV DEAV-ARTM-PCB                      
316800                                       DEAV-WDK7-PCB                      
316900                                       DEAV-WDB6-PCB                      
317000                                       WLLOGA-PCB                         
317100                                       WDK6-PCB                           
317200                                       KVAN-WDB2-PCB                      
317300                                       DEAV-WDL7-PCB                      
317400                                       DEAV-WDK72-PCB                     
317500                                       DEAV-WDR2-PCB                      
317600                                       DEAV-WDR5-PCB                      
317700                                       DEAV-WDC1-PCB                      
317800     MOVE DEAV-FLAKPLOC-UT          TO ORAD-FLAKPLOC                      
317900     MOVE DEAV-RERF-RAD-UT          TO ORAD-RERF-RAD                      
318000     MOVE DEAV-KDORDBEK-UT          TO WS-KDORDBEK                        
318100     MOVE '4375DEAV'                TO WS-IDPGM                           
318200     MOVE DEAV-KVEFRS-UT            TO WS-KVEFRS                          
318300     MOVE DEAV-KVLS-UT              TO WS-KVLS                            
318400     MOVE DEAV-KVRESS-UT            TO WS-KVRESS                          
318500     MOVE DEAV-KVROS-UT             TO WS-KVROS                           
318600     MOVE DEAV-KVAVBART-UT          TO WS-KVAVBART                        
318700     MOVE DEAV-KDROO-UT             TO WS-KDROO                           
318800     MOVE 'END CEEA-DEAV'           TO PGMPOS                             
318900                                                                          
319000     .                                                                    
319100     EJECT                                                                
319200 CEF-KONTROLL-SPLITGRANS SECTION.                                         
319300                                                                          
319400     MOVE 'STA CEF-KONT'           TO PGMPOS                              
319500     IF ARTIKEL-OK                                                        
319600        ADD 1                     TO 4002-KVRADER                         
319700        ADD WS-ORDERVIKT-PER-RAD  TO 4002-VKORDNTO                        
319800        ADD WS-ORDERVOLYM-PER-RAD TO 4002-VLORDNTO                        
319900                                                                          
320000        IF 4002-FLORDSPL = JA                                             
320100           IF W-ODEL-IDDC-EXP = SPACE OR                                  
320200              W-ODEL-IDDC-EXP = WC-CDC-SE                                 
320300*             *Bounce order with bounce dc=11 or                          
320400*             *just normal order                                          
320500              IF ORAD-PRAVCOST > ZERO                                     
320600                 COMPUTE WS-ORDERVARDE-PER-RAD =                          
320700                         ORAD-PRAVCOST * WS-KVAVBART                      
320800              ELSE                                                        
320900                 COMPUTE WS-ORDERVARDE-PER-RAD =                          
321000                         ORAD-PRARTNTO * WS-KVAVBART                      
321100              END-IF                                                      
321200           END-IF                                                         
321300           IF W-ODEL-IDDC-EXP NOT = SPACE AND                             
321400              W-ODEL-IDDC-EXP NOT = WC-CDC-SE                             
321500*             *Bounce order and bounce dc not = 11                        
321600*             *VOR order china/India                                      
321700               COMPUTE WS-ORDERVARDE-PER-RAD =                            
321800                       ORAD-PRARTNTO * WS-KVAVBART                        
321900           END-IF                                                         
322000           ADD WS-ORDERVARDE-PER-RAD TO 4002-SUORDV                       
322100                                                                          
322200           COMPUTE WS-ORDERVARDE-PER-RAD-LOC =                            
322300                   ORAD-PRARTNTO-LOC * WS-KVAVBART                        
322400           ADD WS-ORDERVARDE-PER-RAD-LOC TO 4002-SUORDV-LOC               
322500                                                                          
322600           COMPUTE WS-ORDERVARDE-PER-RAD-LOCPREL =                        
322700                   ORAD-PRARTNTO-LOCPREL * WS-KVAVBART                    
322800          ADD WS-ORDERVARDE-PER-RAD-LOCPREL TO 4002-SUORDV-LOCPREL        
322900                                                                          
323000           IF 4002-KDSORT = 'RA'                                          
323100              IF 4002-KVRADER >= 4002-KVORDSPL                            
323200                 MOVE JA   TO SPLIT-SW                                    
323300              END-IF                                                      
323400           END-IF                                                         
323500                                                                          
323600           IF 4002-KDSORT = 'KG'                                          
323700              IF 4002-VKORDNTO >= 4002-KVORDSPL                           
323800                 MOVE JA   TO SPLIT-SW                                    
323900              END-IF                                                      
324000           END-IF                                                         
324100                                                                          
324200           IF 4002-KDSORT = 'M3'                                          
324300              IF 4002-VLORDNTO >= 4002-KVORDSPL                           
324400                 MOVE JA   TO SPLIT-SW                                    
324500              END-IF                                                      
324600           END-IF                                                         
324700        END-IF                                                            
324800     END-IF                                                               
324900                                                                          
325000     IF SPLITGRANS-OK                                                     
325100        PERFORM CEFA-KONTROLL-ARTIKELNR                                   
325200     END-IF                                                               
325300     .                                                                    
325400     EJECT                                                                
325500 CEFA-KONTROLL-ARTIKELNR SECTION.                                         
325600                                                                          
325700     MOVE 'STA CEFA-KONT'           TO PGMPOS                             
325800     IF WS-SPLIT-IDARTNR = 0                                              
325900        MOVE ORAD-IDARTNR TO WS-SPLIT-IDARTNR                             
326000     END-IF                                                               
326100                                                                          
326500                                                                          
326600     PERFORM IMS-GHN-ORQF-WLORQF01                                        
326700     IF SEGMENT-FINNS                                                     
326800        IF ORAD-IDARTNR = WS-SPLIT-IDARTNR                                
326900           SUBTRACT 1 FROM ORAD-IX                                        
327000        ELSE                                                              
327100           MOVE 1000 TO ORAD-IX                                           
327200        END-IF                                                            
327300     ELSE                                                                 
327400        MOVE 1000 TO ORAD-IX                                              
327500     END-IF                                                               
327600     .                                                                    
327700     EJECT                                                                
327800 CEG-JUSTERA-VARDE-PA-ORDERDEL SECTION.                                   
327900                                                                          
328000     MOVE 'STA CEG-JUST'           TO PGMPOS                              
328100     IF ARTIKEL-FEL                                                       
328200        IF W-ODEL-IDDC-EXP = SPACE OR                                     
328300           W-ODEL-IDDC-EXP = WC-CDC-SE                                    
328400*         *Bounce order with bounce dc=11 or                              
328500*         *just normal order                                              
328600           IF ORAD-PRAVCOST > ZERO                                        
328700              COMPUTE WS-ORDERVARDE-PER-RAD =                             
328800                      ORAD-PRAVCOST * ORAD-KVBEART-Q                      
328900           ELSE                                                           
329000              COMPUTE WS-ORDERVARDE-PER-RAD =                             
329100                      ORAD-PRARTNTO * ORAD-KVBEART-Q                      
329200           END-IF                                                         
329300        END-IF                                                            
329400        IF W-ODEL-IDDC-EXP NOT = SPACE AND                                
329500           W-ODEL-IDDC-EXP NOT = WC-CDC-SE                                
329600*         *Bounce order and bounce dc not = 11                            
329700*         *VOR order china/India                                          
329800           COMPUTE WS-ORDERVARDE-PER-RAD =                                
329900                   ORAD-PRARTNTO * ORAD-KVBEART-Q                         
330000        END-IF                                                            
330100        COMPUTE WS-ORDERVARDE-PER-RAD-LOC =                               
330200                ORAD-PRARTNTO-LOC * ORAD-KVBEART-Q                        
330300        COMPUTE WS-ORDERVARDE-PER-RAD-LOCPREL =                           
330400                ORAD-PRARTNTO-LOCPREL * ORAD-KVBEART-Q                    
330500        SUBTRACT WS-ORDERVARDE-PER-RAD FROM W-ODEL-SUORDV                 
330600        SUBTRACT WS-ORDERVARDE-PER-RAD-LOC                                
330700                            FROM W-ODEL-SUORDV-LOC                        
330800        SUBTRACT WS-ORDERVARDE-PER-RAD-LOCPREL                            
330900                            FROM W-ODEL-SUORDV-LOCPREL                    
331000        SUBTRACT WS-ORDERVIKT-PER-RAD  FROM W-ODEL-VKORDNTO               
331100        SUBTRACT WS-ORDERVOLYM-PER-RAD FROM W-ODEL-VLORDNTO               
331200        SUBTRACT 1                     FROM W-ODEL-KVRADER                
331300                                                                          
331400        IF W-ODEL-SUORDV   NEGATIVE                                       
331500           MOVE 0                        TO W-ODEL-SUORDV                 
331600        END-IF                                                            
331700                                                                          
331800        IF W-ODEL-SUORDV-LOC     NEGATIVE                                 
331900           MOVE 0                        TO W-ODEL-SUORDV-LOC             
332000        END-IF                                                            
332100                                                                          
332200        IF W-ODEL-SUORDV-LOCPREL NEGATIVE                                 
332300           MOVE 0                        TO W-ODEL-SUORDV-LOCPREL         
332400        END-IF                                                            
332500                                                                          
332600        IF W-ODEL-VKORDNTO NEGATIVE                                       
332700           MOVE 0                        TO W-ODEL-VKORDNTO               
332800        END-IF                                                            
332900                                                                          
333000        IF W-ODEL-VLORDNTO NEGATIVE                                       
333100           MOVE 0                        TO W-ODEL-VLORDNTO               
333200        END-IF                                                            
333300                                                                          
333400        IF W-ODEL-KVRADER  NEGATIVE                                       
333500           MOVE 0                        TO W-ODEL-KVRADER                
333600        END-IF                                                            
333700                                                                          
333800        IF W-ODEL-KVRADER  = 0                                            
333900           MOVE 0                        TO W-ODEL-SUORDV                 
334000                                            W-ODEL-SUORDV-LOC             
334100                                            W-ODEL-SUORDV-LOCPREL         
334200                                            W-ODEL-VKORDNTO               
334300                                            W-ODEL-VLORDNTO               
334400        END-IF                                                            
334500     END-IF                                                               
334600     .                                                                    
334700     EJECT                                                                
334800 CEI-LAES-EV-KUNDREG   SECTION.                                           
334900                                                                          
335000     MOVE 'STA CEI-LAES'           TO PGMPOS                              
335100     IF NDC OR LDC-GB-3A                                                  
335200                                                                          
335300       IF  OHUV-IDDISTR   NOT = W-IDDISTR-WDB2                            
335400       OR  OHUV-IDKUNDNR  NOT = W-IDKUNDNR-WDB2                           
335500                                                                          
335600         MOVE OHUV-IDDISTR       TO W-IDDISTR-WDB2                        
335700         MOVE OHUV-IDKUNDNR      TO W-IDKUNDNR-WDB2                       
335800                                                                          
335900         PERFORM IMS-GU-GMTA-WDB201                                       
336000         MOVE GMT-IDZON         TO WS-GMT-IDZON                           
336100         MOVE GMT-FLCOD         TO WS-GMT-FLCOD                           
336200       END-IF                                                             
336300                                                                          
336400     END-IF                                                               
336500     .                                                                    
336600     EJECT                                                                
336700 CEL-CREATE-EVENT-151  SECTION.                                           
336800                                                                          
336810     IF LYNK-NON-API                                                      
336820       PERFORM S03D-CR-NON-API-EVENT                                      
336830     ELSE                                                                 
336900*NEW                                                                      
337000*EVENT HANDLING                                                           
337100*LYND = DÖSKALLE  WDQ2C                                                   
337200       IF OHUV-IDSYSTEM = 'LYND' OR 'TADD'                                
337300         MOVE OHUV-IDDISTR             TO W-IDDISTR-CSEQ                  
337400         MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-CSEQ                 
337500         MOVE ORAD-IDKUNDRF-RO(3:5)    TO W-IDORDNR5-CSEQ                 
337600         PERFORM IMS-GU-WDQ201-CSEQ-GE                                    
339700         IF SEGMENT-FINNS                                                 
339701           MOVE CSQ-OHUV-IDDISTR           TO WS-IDDISTR-EVENT            
339702           MOVE CSQ-OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT           
339703           MOVE CSQ-OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT           
339704           MOVE CSQ-OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT           
339705           MOVE JA                         TO CREATE-EVENT-SW             
340201         END-IF                                                           
340202       ELSE                                                               
340203*LYNV = VOR KUNDRF-LEV -A6                                                
340204         IF OHUV-IDSYSTEM = 'LYNV' OR 'TADV'                              
340205           MOVE LOW-VALUE              TO W-WDA6BSEQ-MIN-X                
340206           MOVE HIGH-VALUE             TO W-WDA6BSEQ-MAX-X                
340700                                                                          
340701           MOVE OHUV-IDDISTR           TO W-A6BSEQ-MIN-IDDISTR            
340702                                          W-A6BSEQ-MAX-IDDISTR            
340703           MOVE OHUV-IDKUNDNR          TO W-A6BSEQ-MIN-IDKUNDNR           
340704                                          W-A6BSEQ-MAX-IDKUNDNR           
340705           MOVE OHUV-IDKUNDRF         TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
340706                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
340707           PERFORM IMS-GU-SEQB-WDA601                                     
341600           IF SEGMENT-FINNS                                               
341601             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
341602             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
341603             MOVE VOR-IDKUNDRF(1:7)    TO WS-IDORDNR7-EVENT               
341604             MOVE VOR-TIREGDAT-URSP    TO WS-TIREGDAT-EVENT               
341605             MOVE JA                   TO CREATE-EVENT-SW                 
342101           END-IF                                                         
342102         ELSE                                                             
342103*LYNB = VERKSTADS/REPARATIONS-ORDER -A5                                   
342104           IF OHUV-IDSYSTEM = 'LYNB' OR 'TADB'                            
342105             MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                
342106                                          W-IDDISTR-A5-MAX                
342107             MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN               
342108                                          W-IDKUNDNR-A5-MAX               
342109             MOVE OHUV-KDORDKL         TO W-KDORDKL                       
342110             MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                  
342111*                                                                         
342112             PERFORM IMS-GU-WDA501                                        
342113             IF SEGMENT-FINNS                                             
342114               MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                
342115               MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT               
342116               MOVE  RAD-IDORDNR5      TO WS-IDORDNR7-EVENT               
342117               MOVE  RAD-TIREGDAT      TO WS-TIREGDAT-EVENT               
342118               MOVE JA                 TO CREATE-EVENT-SW                 
342200             END-IF                                                       
342300           ELSE                                                           
342400             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
342500             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
342600             MOVE OHUV-IDORDNR7        TO WS-IDORDNR7-EVENT               
342700             MOVE OHUV-TIREGDAT        TO WS-TIREGDAT-EVENT               
342800             MOVE JA                   TO CREATE-EVENT-SW                 
342900           END-IF                                                         
343000         END-IF                                                           
343100       END-IF                                                             
343110     END-IF                                                               
343200                                                                          
343300     MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3                             
343400                                                                          
343500     IF CREATE-EVENT                                                      
343600                                                                          
343700       MOVE OHUV-IDSYSTEM(1:4) TO EVENT-SW                                
343800       IF EVENT-OK OR LYNK-NON-API                                        
343900                                                                          
344000         IF (WS-IDSYST-1-3    = 'LYN') OR LYNK-NON-API                    
344100           MOVE 'L'            TO WS-PARTNER                              
344200         END-IF                                                           
344300                                                                          
344400         IF WS-IDSYST-1-3    = 'POL'                                      
344500           MOVE 'P'            TO WS-PARTNER                              
344600         END-IF                                                           
344700                                                                          
344800         IF WS-IDSYST-1-3    = 'ECO'                                      
344900           MOVE 'E'            TO WS-PARTNER                              
345000         END-IF                                                           
345100                                                                          
345200         IF WS-IDSYST-1-3    = 'TAD'                                      
345300           MOVE 'T'            TO WS-PARTNER                              
345400         END-IF                                                           
345500                                                                          
345600         IF WS-IDSYST-1-3    = 'ACC'                                      
345700           MOVE 'A'            TO WS-PARTNER                              
345800         END-IF                                                           
345900         IF WS-IDSYST-1-3    = 'APA'                                      
346000           MOVE 'K'            TO WS-PARTNER                              
346100         END-IF                                                           
346200         IF WS-IDSYST-1-3    = 'APB'                                      
346300           MOVE 'B'            TO WS-PARTNER                              
346400         END-IF                                                           
346500         IF WS-IDSYST-1-3    = 'APC'                                      
346600           MOVE 'C'            TO WS-PARTNER                              
346700         END-IF                                                           
346800         IF WS-IDSYST-1-3    = 'APD'                                      
346900           MOVE 'D'            TO WS-PARTNER                              
347000         END-IF                                                           
347100         IF WS-IDSYST-1-3    = 'APE'                                      
347200           MOVE 'M'            TO WS-PARTNER                              
347300         END-IF                                                           
347400         IF WS-IDSYST-1-3    = 'APF'                                      
347500           MOVE 'F'            TO WS-PARTNER                              
347600         END-IF                                                           
347700         IF WS-IDSYST-1-3    = 'APG'                                      
347800           MOVE 'G'            TO WS-PARTNER                              
347900         END-IF                                                           
348000         IF WS-IDSYST-1-3    = 'APH'                                      
348100           MOVE 'H'            TO WS-PARTNER                              
348200         END-IF                                                           
348300         IF WS-IDSYST-1-3    = 'API'                                      
348400           MOVE 'I'            TO WS-PARTNER                              
348500         END-IF                                                           
348600         IF WS-IDSYST-1-3    = 'APJ'                                      
348700           MOVE 'J'            TO WS-PARTNER                              
348800         END-IF                                                           
348900                                                                          
349000         MOVE SPACE            TO Z430-REQU-TIMESTAMP                     
349100         PERFORM CELA-CREATE-EVENT-151                                    
349200                                                                          
349300       END-IF                                                             
349400     END-IF                                                               
349500     .                                                                    
349600     EJECT                                                                
349700 CELA-CREATE-EVENT-151 SECTION.                                           
349800     MOVE 'STA CELA-CREATE-EVENT'   TO PGMPOS                             
349900                                                                          
350010     IF LYNK-NON-API                                                      
350020        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
350030     ELSE                                                                 
350040        MOVE OHUV-IDSYSTEM      TO Z430-REQU-IDEVENTREC                   
350050     END-IF                                                               
350100                                                                          
350200     MOVE IDMSGVER              TO Z430-REQU-IDMSGVER                     
350300     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
350400     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
350500***  MOVE WS-TIMESTAMP          TO Z430-REQU-TIMESTAMP                    
350600     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
350700     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
350800     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
350900     MOVE '151'                 TO Z430-IDMSG                             
351000     MOVE 'ORDER PART/LINE IS PRINTED'                                    
351100                                TO Z430-TEMFSINF                          
351200                                                                          
351300*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
351400*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
351500     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
351600     MOVE 'Z430'               TO MSG-IDTRANS-1                           
351700     MOVE '1'                  TO MSG-KDMFSFOR-1                          
351800     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
351900     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
352000          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
352100                                                                          
352200     ADD  1                    TO MSG-KOM-TIKLOCK                         
352300     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
352400     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
352510                                                                          
352600     CALL W006KOM USING MSG-PCB                                           
352700                        0693-PCB                                          
352800                        KOM-WDP8-PCB                                      
352900                        MSG-KOM-WMSGKOM                                   
353000                        MSG-IO-AREA                                       
353100                                                                          
353200     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
353300        MOVE                                                              
353400        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
353500                                     TO FELTEXT                           
353600        CALL ABEND USING RKOD-ABEND-WITHOUT-DUMP                          
353700     END-IF                                                               
353800     .                                                                    
353900     EJECT                                                                
354000 CF-UPPDAT-ORDERDEL SECTION.                                              
354100                                                                          
354200     MOVE 'STA CF-UPPD'           TO PGMPOS                               
354300                                                                          
354400     MOVE ARB-IDPLKLST-SISTA  TO WS-IDPLKLST-SISTA                        
354500                                                                          
354600     IF 4002-FLORDSPL = NEJ                                               
354700        IF Q221-SEG-SAKNAS                                                
354800                                                                          
354900           PERFORM CFC-SKAPA-DUMMYPOST-PU                                 
355000           IF W-ODEL-KDODELSTA NOT = 'P'                                  
355100              MOVE 4002-KVRADER    TO W-ODEL-KVRADER                      
355200              MOVE 4002-VLORDNTO   TO W-ODEL-VLORDNTO                     
355300              MOVE 4002-VKORDNTO   TO W-ODEL-VKORDNTO                     
355400           END-IF                                                         
355500           MOVE W-ORDERDEL TO WLORQA01                                    
355600           PERFORM IMS-REPL-ORQA-WLORQA01                                 
355700                                                                          
355800           PERFORM CFA-KONTROLL-FORDROJD-PRINT                            
355900           MOVE JA TO WS-FLODELUT                                         
356000           COMPUTE IX1 = 4002-IXHEL + 1                                   
356100                                                                          
356200           IF IX1 > 99                                                    
356300              OR                                                          
356400              4002-ORDDEL (IX1) = LOW-VALUE                               
356500              MOVE JA TO PLOCK-SW                                         
356600           ELSE                                                           
356700              ADD 1 TO 4002-IXHEL                                         
356800           END-IF                                                         
356900        ELSE                                                              
357000           MOVE W-ORDERDEL TO WLORQA01                                    
357100           PERFORM IMS-REPL-ORQA-WLORQA01                                 
357200        END-IF                                                            
357300     END-IF                                                               
357400                                                                          
357500     IF 4002-FLORDSPL = JA                                                
357600        IF SPLITGRANS-OK                                                  
357700           MOVE W-ODEL-KVRADER        TO WS-ODEL-KVRADER                  
357800           MOVE W-ODEL-VKORDNTO       TO WS-ODEL-VKORDNTO                 
357900           MOVE W-ODEL-VLORDNTO       TO WS-ODEL-VLORDNTO                 
358000           MOVE W-ODEL-SUORDV         TO WS-ODEL-SUORDV                   
358100           MOVE W-ODEL-SUORDV-LOC     TO WS-ODEL-SUORDV-LOC               
358200           MOVE W-ODEL-SUORDV-LOCPREL TO WS-ODEL-SUORDV-LOCPREL           
358300           PERFORM CFC-SKAPA-DUMMYPOST-PU                                 
358400           PERFORM CFB-SPLIT-NY-ORDERDEL                                  
358500           MOVE JA TO WS-FLODELUT                                         
358600           MOVE JA TO PLOCK-SW                                            
358700        ELSE                                                              
358800           MOVE W-ORDERDEL TO WLORQA01                                    
358900           PERFORM IMS-REPL-ORQA-WLORQA01                                 
359000        END-IF                                                            
359100     END-IF                                                               
359200                                                                          
359300*                                                                         
359400*    START TRANSACTION W4T397 FOR DUMMY LINE                              
359500*    SETS CORRECT ORDER STATUS IN KOLLI-REG                               
359600*                                                                         
359700     IF  (4002-FLORDSPL    = NEJ                                          
359800     AND  Q221-SEG-SAKNAS )                                               
359900     OR  (4002-FLORDSPL    = JA                                           
360000     AND  SPLITGRANS-OK)                                                  
360100         IF 4002-KVRADER   = 0                                            
360200            PERFORM CFD-START-4397                                        
360300         ELSE                                                             
360400            CONTINUE                                                      
360500         END-IF                                                           
360600     END-IF                                                               
360700*                                                                         
360800     MOVE 'R'                     TO W-KDODELST                           
360900     PERFORM IMS-GU-ORQA-STATUS                                           
361000     IF SEGMENT-FINNS                                                     
361100        MOVE 'R*'                 TO WS-KDORDSTA-CX                       
361200     ELSE                                                                 
361300        MOVE 'U'                  TO W-KDODELST                           
361400        PERFORM IMS-GU-ORQA-STATUS                                        
361500        IF SEGMENT-FINNS                                                  
361600           MOVE 'P'               TO W-KDODELST                           
361700           PERFORM IMS-GU-ORQA-STATUS                                     
361800                                                                          
361900           IF SEGMENT-FINNS                                               
362000              MOVE 'U*'           TO WS-KDORDSTA-CX                       
362100           ELSE                                                           
362200              MOVE 'U '           TO WS-KDORDSTA-CX                       
362300           END-IF                                                         
362400        ELSE                                                              
362500           MOVE 'P '              TO WS-KDORDSTA-CX                       
362600        END-IF                                                            
362700     END-IF                                                               
362800                                                                          
362900     MOVE WS-KDORDSTA-CX     TO ARB-KDORDSTA                              
363000     MOVE WS-KVSEMBRA        TO ARB-KVSEMBRA                              
363100     MOVE WS-IDRADNR-SISTA   TO ARB-IDRADNR-SISTA                         
363200     MOVE WS-IDPLKLST-SISTA  TO ARB-IDPLKLST-SISTA                        
363300     IF ARB-FLODELUT  = 'N'                                               
363400       MOVE WS-FLODELUT  TO ARB-FLODELUT                                  
363500     END-IF                                                               
363600                                                                          
363700*    EFTER UTSKRIFT KAN ORDERN ALDRIG VARA AKTUELL                        
363800*    FÖR TVINGANDE TILLÄGG                                                
363900*    FÖLJANDE REPLACE UPPDATERAR BÅDE 01- OCH 12-SEGMENT                  
364000                                                                          
364100     MOVE NEJ TO OHUV-FLORDTIL                                            
364200     PERFORM IMS-REPL-ORQI-WLORQI12                                       
364300                                                                          
364400     IF PLOCKSATS-KLAR                                                    
364500        MOVE 1000 TO ORAD-IX                                              
364600     END-IF                                                               
364700                                                                          
364800     IF Q221-SEG-SAKNAS                                                   
364900     OR PLOCKSATS-KLAR                                                    
365000        MOVE 0    TO 4002-KVRADER                                         
365100                     4002-VKORDNTO                                        
365200                     4002-VLORDNTO                                        
365300     END-IF                                                               
365400                                                                          
365500                                                                          
365600     .                                                                    
365700     EJECT                                                                
365800 CFA-KONTROLL-FORDROJD-PRINT SECTION.                                     
365900                                                                          
366000     MOVE 'STA CFA-KONT'           TO PGMPOS                              
366100     MOVE W-ODEL-IDPRC TO W-IDPRC                                         
366200     PERFORM IMS-GHNP-WDQ221-FIRST                                        
366300                                                                          
366400     PERFORM UNTIL Q221-SEG-SAKNAS                                        
366500       IF  4454-IDPRC  (LOR-ADLAGOMR) NOT = SPACE                         
366600         MOVE LOR-KVRADER            TO ODEL-KVRADER                      
366700         MOVE LOR-VKORDNTO           TO ODEL-VKORDNTO                     
366800         MOVE LOR-VLORDNTO           TO ODEL-VLORDNTO                     
366900         MOVE LOR-SUORDV             TO ODEL-SUORDV                       
367000         MOVE LOR-SUORDV-LOC         TO ODEL-SUORDV-LOC                   
367100         MOVE LOR-SUORDV-LOCPREL     TO ODEL-SUORDV-LOCPREL               
367200         MOVE 'R'                    TO ODEL-KDODELSTA                    
367300         MOVE SPACE                  TO ODEL-IDUSER                       
367400                                        ODEL-IDBORD                       
367500         MOVE WS-DATUM-LOK           TO ODEL-DAUTSKR                      
367600         IF WS-DATUM-LOK NOT = ZERO                                       
367700           IF WS-DATUM-LOK < 500000                                       
367800             MOVE 20                 TO ODEL-DAUTSKR (1:2)                
367900           ELSE                                                           
368000             IF WS-DATUM-LOK < 999999                                     
368100               MOVE 19               TO ODEL-DAUTSKR (1:2)                
368200             ELSE                                                         
368300               MOVE 99999999         TO ODEL-DAUTSKR                      
368400             END-IF                                                       
368500           END-IF                                                         
368600         END-IF                                                           
368700         MOVE WS-TIHHMMSS-LOK        TO ODEL-TIUTSTID                     
368800                                                                          
368900         COMPUTE WS-IDPLKLST-SISTA = WS-IDPLKLST-SISTA + 1                
369000         MOVE WS-IDPLKLST-SISTA        TO ODEL-IDPLKLST                   
369100         MOVE 4454-IDPRC(LOR-ADLAGOMR) TO LOR-IDPRC                       
369200                                          ODEL-IDPRC                      
369300         MOVE SPACE                    TO ODEL-IDPRCPLK                   
369400         MOVE ZERO                     TO ODEL-IDLOTNR-PLK                
369500                                          ODEL-IDLOPNR-ORD                
369600         PERFORM IMS-ISRT-ORQA-WLORQA01                                   
369700         PERFORM IMS-REPL-WDQ221                                          
369800       END-IF                                                             
369900       PERFORM IMS-GHNP-WDQ221                                            
370000     END-PERFORM                                                          
370100     .                                                                    
370200     EJECT                                                                
370300 CFB-SPLIT-NY-ORDERDEL SECTION.                                           
370400************************************************************              
370500*  IDENTITETEN TILL DEN NYA ORDERDELEN KAN ANTINGEN LIGGA  *              
370600*  I ELEMENT 98 ELLER 99. OM DEN LIGGER I 98 INNEBÄR       *              
370700*  DET ATT DET REDAN FINNS EN PLOCKSATS FÖR DENNA ORDER-   *              
370800*  DEL. DENNA SKALL DÄRFÖR HA STATUS 'U'(FRÅN 4353).       *              
370900*  OM DEN LIGGER I 99 SKALL DEN HA STATUS 'R' FÖR ATT VARA *              
371000*  VALBAR FRÅN SKÄRMEN PÅ NYTT.                            *              
371100************************************************************              
371200                                                                          
371300*----> GAMLA ORDERDELEN                                                   
371400     MOVE 'STA CFB-SPLI'           TO PGMPOS                              
371500                                                                          
371600     MOVE W-ORDERDEL             TO WLORQA01                              
371700                                                                          
371800     IF W-ODEL-KDODELSTA NOT = 'P'                                        
371900        MOVE 4002-KVRADER        TO ODEL-KVRADER                          
372000        MOVE 4002-VKORDNTO       TO ODEL-VKORDNTO                         
372100        MOVE 4002-VLORDNTO       TO ODEL-VLORDNTO                         
372200        MOVE 4002-SUORDV         TO ODEL-SUORDV                           
372300        MOVE 4002-SUORDV-LOC     TO ODEL-SUORDV-LOC                       
372400        MOVE 4002-SUORDV-LOCPREL TO ODEL-SUORDV-LOCPREL                   
372500        MOVE 'U'                 TO ODEL-KDODELSTA                        
372600        MOVE WS-DATUM-LOK        TO ODEL-DAUTSKR                          
372700        IF WS-DATUM-LOK NOT = ZERO                                        
372800          IF WS-DATUM-LOK < 500000                                        
372900            MOVE 20              TO ODEL-DAUTSKR (1:2)                    
373000          ELSE                                                            
373100            IF WS-DATUM-LOK < 999999                                      
373200              MOVE 19            TO ODEL-DAUTSKR (1:2)                    
373300            ELSE                                                          
373400              MOVE 99999999      TO ODEL-DAUTSKR                          
373500            END-IF                                                        
373600          END-IF                                                          
373700        END-IF                                                            
373800        MOVE WS-TIHHMMSS-LOK     TO ODEL-TIUTSTID                         
373900        MOVE 4002-IDUSER         TO ODEL-IDUSER                           
374000        MOVE 4002-IDBORD         TO ODEL-IDBORD                           
374100     END-IF                                                               
374200                                                                          
374300     PERFORM IMS-REPL-ORQA-WLORQA01                                       
374400                                                                          
374500*----> NYA ORDERDELEN                                                     
374600                                                                          
374700     COMPUTE WS-ANTRADER   = WS-ODEL-KVRADER  - 4002-KVRADER              
374800     COMPUTE WS-VKORDNTO   = WS-ODEL-VKORDNTO - 4002-VKORDNTO             
374900     COMPUTE WS-VLORDNTO   = WS-ODEL-VLORDNTO - 4002-VLORDNTO             
375000     COMPUTE WS-SUORDV     = WS-ODEL-SUORDV   - 4002-SUORDV               
375100     COMPUTE WS-SUORDV-LOC = WS-ODEL-SUORDV-LOC - 4002-SUORDV-LOC         
375200     COMPUTE WS-SUORDV-LOCPREL =                                          
375300             WS-ODEL-SUORDV-LOCPREL - 4002-SUORDV-LOCPREL                 
375400                                                                          
375500     IF WS-ANTRADER > 0                                                   
375600        IF 4002-ORDDEL (98) NOT = LOW-VALUE                               
375700           MOVE 4002-IDPLKLST (98)   TO ODEL-IDPLKLST                     
375800           MOVE 'U'                  TO ODEL-KDODELSTA                    
375900           MOVE WS-DATUM-LOK         TO ODEL-DAUTSKR                      
376000           IF WS-DATUM-LOK NOT = ZERO                                     
376100             IF WS-DATUM-LOK < 500000                                     
376200               MOVE 20               TO ODEL-DAUTSKR (1:2)                
376300             ELSE                                                         
376400               IF WS-DATUM-LOK < 999999                                   
376500                 MOVE 19             TO ODEL-DAUTSKR (1:2)                
376600               ELSE                                                       
376700                 MOVE 99999999       TO ODEL-DAUTSKR                      
376800               END-IF                                                     
376900             END-IF                                                       
377000           END-IF                                                         
377100           MOVE WS-TIHHMMSS-LOK      TO ODEL-TIUTSTID                     
377200           MOVE 4002-IDUSER          TO ODEL-IDUSER                       
377300           MOVE 4002-IDBORD          TO ODEL-IDBORD                       
377400        ELSE                                                              
377500           MOVE 4002-IDPLKLST (99)   TO ODEL-IDPLKLST                     
377600           MOVE 'R'                  TO ODEL-KDODELSTA                    
377700           MOVE ZERO                 TO ODEL-DAUTSKR                      
377800                                        ODEL-TIUTSTID                     
377900           IF 4002-FLORDKNY = JA                                          
378000              MOVE 4002-IDUSER       TO ODEL-IDUSER                       
378100           ELSE                                                           
378200              MOVE SPACE             TO ODEL-IDUSER                       
378300           END-IF                                                         
378400           MOVE SPACE                TO ODEL-IDBORD                       
378500        END-IF                                                            
378600        MOVE WS-ANTRADER         TO ODEL-KVRADER                          
378700        MOVE WS-VKORDNTO         TO ODEL-VKORDNTO                         
378800        MOVE WS-VLORDNTO         TO ODEL-VLORDNTO                         
378900        MOVE WS-SUORDV           TO ODEL-SUORDV                           
379000        MOVE WS-SUORDV-LOC       TO ODEL-SUORDV-LOC                       
379100        MOVE WS-SUORDV-LOCPREL   TO ODEL-SUORDV-LOCPREL                   
379200        MOVE SPACE               TO ODEL-IDPRCPLK                         
379300        MOVE ZERO                TO ODEL-IDLOTNR-PLK                      
379400                                    ODEL-IDLOPNR-ORD                      
379500        PERFORM IMS-ISRT-ORQA-WLORQA01                                    
379600     END-IF                                                               
379700     .                                                                    
379800     EJECT                                                                
379900 CFC-SKAPA-DUMMYPOST-PU SECTION.                                          
380000*************************************************                         
380100*  ENDAST I PLOCKSATSER SOM INNEHÅLLER EN       *                         
380200*  ORDERDEL SKALL MAN SKAPA DUMMYPOST.          *                         
380300*  I ÖVRIGA FALL NOLLSTÄLLES ENDAST ORDERDELS-  *                         
380400*  INFO.                                        *                         
380500*************************************************                         
380600                                                                          
380700     MOVE 'STA CFC-SKAP'           TO PGMPOS                              
380800     IF 4002-KVRADER = 0                                                  
380900        PERFORM CFCA-NOLLSTALL-ODELINFO                                   
381000                                                                          
381100        IF 4002-ORDDEL (2) = LOW-VALUE                                    
381200           PERFORM S05-CREATE-PLOCKSATS-PU                                
381300           PERFORM CFCB-FYLL-I-DUMMYPOST                                  
381400           PERFORM IMS-ISRT-4007-WL400721                                 
381500                                                                          
381600           PERFORM UNTIL SEGMENT-FINNS                                    
381700              ADD 1 TO 4010-IDLOPNR                                       
381800              PERFORM IMS-ISRT-4007-WL400721                              
381900           END-PERFORM                                                    
382000        END-IF                                                            
382100     END-IF                                                               
382200     .                                                                    
382300     EJECT                                                                
382400 CFCA-NOLLSTALL-ODELINFO SECTION.                                         
382500                                                                          
382600     MOVE 'P'                 TO W-ODEL-KDODELSTA                         
382700     MOVE ZERO                TO W-ODEL-KVRADER                           
382800                                 W-ODEL-VKORDNTO                          
382900                                 W-ODEL-VLORDNTO                          
383000                                 W-ODEL-SUORDV                            
383100                                 W-ODEL-SUORDV-LOC                        
383200                                 W-ODEL-SUORDV-LOCPREL                    
383300     MOVE WS-DATUM-LOK        TO W-ODEL-DAUTSKR                           
383400     IF WS-DATUM-LOK NOT = ZERO                                           
383500       IF WS-DATUM-LOK < 500000                                           
383600         MOVE 20              TO W-ODEL-DAUTSKR (1:2)                     
383700       ELSE                                                               
383800         IF WS-DATUM-LOK < 999999                                         
383900           MOVE 19            TO W-ODEL-DAUTSKR (1:2)                     
384000         ELSE                                                             
384100           MOVE 99999999      TO W-ODEL-DAUTSKR                           
384200         END-IF                                                           
384300       END-IF                                                             
384400     END-IF                                                               
384500     MOVE WS-DATUM-LOK        TO W-ODEL-TIPACKN                           
384600     MOVE WS-TIHHMMSS-LOK     TO W-ODEL-TIUTSTID                          
384700                                 W-ODEL-TIPACTID                          
384800     MOVE 4002-IDUSER         TO W-ODEL-IDUSER                            
384900     MOVE 4002-IDBORD         TO W-ODEL-IDBORD                            
385000     .                                                                    
385100     EJECT                                                                
385200 CFCB-FYLL-I-DUMMYPOST SECTION.                                           
385300                                                                          
385400     IF 4002-KDPRT-PU NOT = SPACE                                         
385500        MOVE 4002-KDPRT-PU    TO 4010-KDPRT                               
385600     ELSE                                                                 
385700        MOVE 4454-KDPRTGEN-PU TO 4010-KDPRT                               
385800     END-IF                                                               
385900                                                                          
386000     MOVE OHUV-IDORDER        TO 4010-IDORDER                             
386100     MOVE OHUV-KDORDKL        TO 4010-KDORDKL                             
386200                                                                          
386300     INITIALIZE                  4010-DEAL-PR-LINE                        
386400     MOVE SPACE               TO 4010-KDSS-PU                             
386500                                 4010-FLAKPLOC                            
386600                                 4010-BERADREF                            
386700                                 4010-BEVOLREF                            
386800                                 4010-KDARTURS                            
386900                                 4010-KDOI                                
387000                                 4010-CLEARGROUP                          
387100                                 4010-KDPRTYP                             
387200                                 4010-IDBIL                               
387300                                 4010-IDVIN                               
387400                                 4010-IDARBREF                            
387500                                 4010-IDKLIENT                            
387600                                 4010-IDDC-RO                             
387700                                 4010-IDANALYS                            
387800                                 4010-IDZON                               
387900                                 4010-IDLEVNR                             
388000                                 4010-IDKST                               
388100                                                                          
388200     MOVE 'N'                 TO 4010-FLSDCLEV                            
388300                                                                          
388400     MOVE 0                   TO 4010-ADLAGOMR                            
388500                                 4010-ADGANG                              
388600                                 4010-ADPLATS                             
388700                                 4010-IDARTNR                             
388800                                 4010-IDLOPNR                             
388900                                 4010-ADLAGOMR-ORD                        
389000                                 4010-ADPLATS-ORD                         
389100                                 4010-IDPURAD                             
389200                                 4010-IDSPECEMB                           
389300                                 4010-KVAVBART                            
389400                                 4010-KVBEART-Q                           
389500                                 4010-KVHANTTI                            
389600                                 4010-REKSIFFR                            
389700                                 4010-VKORDNTO                            
389800                                 4010-VLORDNTO                            
389900                                 4010-IDLOPNR-RO                          
390000                                 4010-KDDSP                               
390100                                 4010-KDFARLIG                            
390200                                 4010-KDKVBRYT                            
390300                                 4010-KDPRODSL                            
390400                                 4010-KDORDING                            
390500                                 4010-KDVRINFO                            
390600                                 4010-KVSLATT                             
390700                                 4010-PRARTNTO                            
390800                                 4010-TIPRIS                              
390900                                 4010-TIRODAT                             
391000                                 4010-VKART                               
391100                                 4010-VKART-NTO                           
391200                                 4010-VLARTNTO                            
391300                                 4010-IDKAMPRF                            
391400                                 4010-IDKONTO                             
391500                                 4010-KVQPACK-3                           
391600                                                                          
391700     MOVE 4002-IDBORD         TO 4010-IDBORD                              
391800     IF DCS-SWEDEN                                                        
391900       MOVE 'INGA RADER'      TO 4010-BEART                               
392000     ELSE                                                                 
392100       MOVE 'NO LINES'        TO 4010-BEART                               
392200     END-IF                                                               
392300     MOVE W-ODEL-IDKUNDRF     TO 4010-IDKUNDRF                            
392400     MOVE '0000000   '        TO 4010-IDKUNDRF-RO                         
392500     MOVE 4002-IXHEL          TO 4010-IDLOPNR-ORD                         
392600     MOVE 4002-IDLOPNR-PL     TO 4010-IDLOPNR-PL                          
392700     MOVE W-ODEL-IDPLKLST     TO 4010-IDPLKLST                            
392800     MOVE W-ODEL-IDPRC        TO 4010-IDPRC                               
392900     MOVE W-ODEL-IDPRODNR     TO 4010-IDPRODNR                            
393000     MOVE 4002-IDUSER         TO 4010-IDUSER                              
393100     MOVE W-ODEL-IDDC         TO 4010-IDDC                                
393200     MOVE W-ODEL-KDFDKRAV     TO 4010-KDFDKRAV                            
393300     MOVE W-ODEL-DALSTORD (3:10) TO 4010-TILST                            
393400     MOVE W-ODEL-TIREGDAT     TO 4010-TIREGDAT                            
393500     MOVE W-ODEL-TIREGTID     TO 4010-TIREGTID                            
393600     MOVE W-ODEL-DARFS (3:10) TO 4010-TIRFS                               
393700     MOVE W-ODEL-DAUTSKR (3:6) TO 4010-TIUTSKR                            
393800     MOVE W-ODEL-TIUTSTID     TO 4010-TIUTSTID                            
393900     MOVE NEJ                 TO 4010-FLCOD                               
394000     MOVE NEJ                 TO 4010-FLINVEST                            
394100     MOVE NEJ                 TO 4010-FLPRTILL                            
394200     MOVE NEJ                 TO 4010-FLTILLK                             
394300     MOVE NEJ                 TO 4010-FLRESTN                             
394400     MOVE OHUV-IDSYSTEM       TO 4010-IDSYSTEM                            
394500     MOVE ZERO                TO 4010-IDPSN                               
394600                                 4010-VKART-FG                            
394700                                 4010-SUEQFG                              
394800                                 4010-VLFG                                
394900                                 4010-PRAVCOST                            
395000     .                                                                    
395100     EJECT                                                                
395200 CFD-START-4397     SECTION.                                              
395300                                                                          
395400     MOVE 'W4T397X '              TO 4397-TRANSKOD                        
395500                                     4397-LTERM-NAME                      
395600     MOVE WS-KDMFSFOR             TO 4397-KDMFSFOR                        
395700     MOVE '4375'                  TO 4397-IDTRANS                         
395800     MOVE +117                    TO 4397-LL                              
395900     MOVE 4002-IDUSER             TO WS-IDUSER                            
396000     MOVE WS-IDANSTNR             TO 4397-IDANSTNR                        
396100     MOVE W-ODEL-IDPRODNR         TO 4397-IDPRODNR                        
396200     MOVE ZERO                    TO 4397-IDPLKLST                        
396300                                     4397-IDPURAD                         
396400     MOVE 4397-TRANSAREA          TO 4397-AREA                            
396500     PERFORM IMS-INSERT-4397-TRANS                                        
396600     .                                                                    
396700     EJECT                                                                
396800                                                                          
396900 D-UPPDAT-PLOCKSATS SECTION.                                              
397000                                                                          
397100     MOVE 'STA D-UPPD'           TO PGMPOS                                
397200     IF PLOCKSATS-KLAR                                                    
397300        PERFORM IMS-GHU-4001-WL400101                                     
397400        PERFORM IMS-DLET-4001-WL400101                                    
397500     ELSE                                                                 
397600        PERFORM IMS-REPL-4001-WL400111                                    
397700     END-IF                                                               
397800                                                                          
397900     IF PLOCKSATS-ETIK-FINNS                                              
398000        MOVE WL400311         TO W-ETIK-PLOCKSATS                         
398100        PERFORM IMS-GHU-4003-WL400311                                     
398200        MOVE W-ETIK-PLOCKSATS TO WL400311                                 
398300        PERFORM IMS-REPL-4003-WL400311                                    
398400     END-IF                                                               
398500     .                                                                    
398600     EJECT                                                                
398700 E-SKICKA-IMSTRANSAR SECTION.                                             
398800     MOVE 'STA E-SKICKA'         TO PGMPOS                                
398900                                                                          
399000     IF PLOCKSATS-KLAR                                                    
399100       IF W-IDTRANS = '435A'                                              
399200         MOVE MID-W4I37501    TO PTOP4-MID-W4I37A01                       
399300         MOVE 4008-IDPRC      TO PTOP4-MID-IDPRC                          
399400         MOVE 4008-IDLOPNR-PL TO PTOP4-MID-IDLOPNR                        
399500         MOVE 4008-IDDC       TO PTOP4-MID-IDDC                           
399600         MOVE WS-4002-IDUSER  TO PTOP4-MID-IDUSER                         
399700         MOVE 'W4037AU'       TO PTOP4-TRANSKOD                           
399800         MOVE '1'             TO PTOP4-KDMFSFOR                           
399900         PERFORM IMS-ISRT-MSG-ALT6                                        
400000       ELSE                                                               
400100         IF W-IDTRANS = '435B'                                            
400200           MOVE MID-W4I37501    TO PTOP4-MID-W4I37A01                     
400300           MOVE 4008-IDPRC      TO PTOP4-MID-IDPRC                        
400400           MOVE 4008-IDLOPNR-PL TO PTOP4-MID-IDLOPNR                      
400500           MOVE 4008-IDDC       TO PTOP4-MID-IDDC                         
400600           MOVE WS-4002-IDUSER  TO PTOP4-MID-IDUSER                       
400700           MOVE 'W4037BU'       TO PTOP4-TRANSKOD                         
400800           MOVE '1'             TO PTOP4-KDMFSFOR                         
400900           PERFORM IMS-ISRT-MSG-ALT7                                      
401000         ELSE                                                             
401100           IF PLOCKSATS-ETIK-FINNS                                        
401200              MOVE MID-W4I37501    TO PTOP2-MID-W4I37601                  
401300              MOVE 4008-IDPRC      TO PTOP2-MID-IDPRC                     
401400              MOVE 4008-IDLOPNR-PL TO PTOP2-MID-IDLOPNR                   
401500              MOVE 4008-IDDC       TO PTOP2-MID-IDDC                      
401600              PERFORM IMS-ISRT-MSG-ALT2                                   
401700           END-IF                                                         
401800           IF PLOCKSATS-PU-FINNS                                          
401900              MOVE MID-W4I37501    TO PTOP3-MID-W4I37701                  
402000              MOVE 4008-IDPRC      TO PTOP3-MID-IDPRC                     
402100              MOVE 4008-IDLOPNR-PL TO PTOP3-MID-IDLOPNR                   
402200              PERFORM IMS-ISRT-MSG-ALT3                                   
402300           END-IF                                                         
402400         END-IF                                                           
402500       END-IF                                                             
402600     ELSE                                                                 
402700*LDC-GB                                                                   
402800       MOVE OHUV-IDDISTR         TO DIST34-IDDISTR                        
402810                                                                          
402900       IF LDC-GB-3A                                                       
403000         IF DIST34-ENGLAND-SDC AND                                        
403100            GMT-FLLDCKND = JA                                             
403200           IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                       
403300           OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                       
403400              MOVE '4017'     TO 4017-IDHTYP                              
403500              MOVE MID-IDPRODNR TO W-4017-IDPRODNR                        
403600              MOVE MID-IDPLKLST TO W-4017-IDPLKLST                        
403700              MOVE LOW-VALUE TO 4017-LOWVALUE                             
403800              PERFORM IMS-GHU-WDGX4017                                    
403900                                                                          
404000              IF SEGMENT-SAKNAS                                           
404100                MOVE '4017'     TO 4017-IDHTYP                            
404200                MOVE MID-IDPRODNR TO 4017-IDPRODNR                        
404300                MOVE MID-IDPLKLST TO 4017-IDPLKLST                        
404400                MOVE LOW-VALUE TO 4017-LOWVALUE                           
404500                PERFORM IMS-ISRT-WDGX4017                                 
404600              END-IF                                                      
404700                                                                          
404800              MOVE '1' TO W-4018-KDSEGKEY                                 
404900              PERFORM IMS-GHU-WDGX4018                                    
405000              MOVE +1 TO LDC-TAB-IX                                       
405100              MOVE +100 TO LDC-TAB-IX-MAX                                 
405200              PERFORM UNTIL LDC-TAB-IX > LDC-TAB-IX-MAX                   
405300                                                                          
405400                MOVE LDC-TAB-WIPID (LDC-TAB-IX)                           
405500                  TO 4018-BERADREF (LDC-TAB-IX)                           
405600                MOVE ZERO TO 4018-KVANTART (LDC-TAB-IX)                   
405700                ADD +1 TO LDC-TAB-IX                                      
405800              END-PERFORM                                                 
405900                                                                          
406000              MOVE '1' TO 4018-KDSEGKEY                                   
406100                            W-4018-KDSEGKEY                               
406200              IF SEGMENT-FINNS                                            
406300                PERFORM IMS-REPL-WDGX4018                                 
406400              ELSE                                                        
406500                PERFORM IMS-ISRT-WDGX4018                                 
406600              END-IF                                                      
406700            END-IF                                                        
406800          END-IF                                                          
406900        END-IF                                                            
407000                                                                          
407100*       -- PREPARE FOR RESTART                                            
407200        IF W-IDTRANS = '435A'                                             
407300*         -- PICK-BY-VOICE VCOM TANSACTION FLOW                           
407400          MOVE '435A' TO PTOP1-IDTRANS                                    
407500        ELSE                                                              
407600          IF W-IDTRANS = '435B'                                           
407700*           -- PICK-BY-VOICE MQ TANSACTION FLOW                           
407800            MOVE '435B' TO PTOP1-IDTRANS                                  
407900          ELSE                                                            
408000*           -- CLASSIC PULS TRANSACTION FLOW                              
408100            MOVE '4375' TO PTOP1-IDTRANS                                  
408200          END-IF                                                          
408300        END-IF                                                            
408400        MOVE JA           TO MID-FLSVAR                                   
408500        MOVE MID-W4I37501 TO PTOP1-MID-W4I37501                           
408600        PERFORM IMS-ISRT-MSG-ALT1                                         
408700        IF EVENT-OK OR LYNK-NON-API                                       
408800* before restarting own trans make a 1 second wait to avoid               
408900* conflict of time stamp on wdp8 that can override already used           
409000* time in previous trans with events                                      
409100          CALL W009WAIT USING 1-SECOND                                    
409200        END-IF                                                            
409300     END-IF                                                               
409400     MOVE 'END E-SKICKA'         TO PGMPOS                                
409500     .                                                                    
409600     EJECT                                                                
409700 S01-LAES-ART-REG-WDD3            SECTION.                                
409800     MOVE 'S01-LAES-ART-REG-WDD3'       TO PGMPOS                         
409900*****************************************                                 
410000*  OM KODEN I AREG-KDORDBEK = 58 EFTER  *                                 
410100*  SUBPROGRAMMET, INNEBÄR DET ETT       *                                 
410200*  ALLVARLIGT FEL OCH PROGRAMMET SKALL  *                                 
410300*  ABENDA.                              *                                 
410400*****************************************                                 
410500                                                                          
410600     IF BIPACKNING                                                        
410700        MOVE BIPA-IDARTNR     (BIPA-IX) TO AREG-IDARTNR                   
410800        MOVE BIPA-IDDC                  TO AREG-IDDC                      
410900     ELSE                                                                 
411000        MOVE ORAD-IDARTNR               TO AREG-IDARTNR                   
411100        MOVE ORAD-IDDC                  TO AREG-IDDC                      
411200     END-IF                                                               
411300                                                                          
411400     CALL W411AREG USING AREG-W411AREG                                    
411500                         AREG-WDK6-PCB                                    
411600                         AREG-WDK7-PCB                                    
411700     IF AREG-KDORDBEK = 58                                                
411800        MOVE 'W411AREG GAV KOD 58 = ARTIKEL FEL ' TO FELTEXT              
411900        CALL FELLOG                                                       
412000     ELSE                                                                 
412100        IF WDQ4-RAD                                                       
412200           PERFORM S01A-HAMTA-BENAMNING                                   
412300        END-IF                                                            
412400     END-IF                                                               
412500     .                                                                    
412600     EJECT                                                                
412700 S20-LAES-ART-REG-WDD3            SECTION.                                
412800     MOVE 'S20-LAES-ART-REG-WDD3'       TO PGMPOS                         
412900*****************************************                                 
413000*  OM KODEN I AREG-KDORDBEK = 58 EFTER  *                                 
413100*  SUBPROGRAMMET, INNEBÄR DET ETT       *                                 
413200*  ALLVARLIGT FEL OCH PROGRAMMET SKALL  *                                 
413300*  ABENDA.                              *                                 
413400*****************************************                                 
413500                                                                          
413600     IF BIPACKNING                                                        
413700        MOVE BIPA-IDARTNR     (BIPA-IX) TO AREG-IDARTNR                   
413800        MOVE BIPA-IDDC                  TO AREG-IDDC                      
413900     ELSE                                                                 
414000        MOVE ORAD-IDARTNR               TO WS-IDARTNR                     
414100        MOVE ORAD-IDARTNR               TO AREG-IDARTNR                   
414200        MOVE ORAD-IDDC                  TO AREG-IDDC                      
414300     END-IF                                                               
414400                                                                          
414500     CALL W411AREG USING AREG-W411AREG                                    
414600                         AREG-WDK6-PCB                                    
414700                         AREG-WDK7-PCB                                    
414800     IF AREG-KDORDBEK = 58                                                
414900        MOVE 'W411AREG GAV KOD 58 = ARTIKEL FEL2' TO FELTEXT              
415000        CALL FELLOG                                                       
415100     ELSE                                                                 
415200        IF WDQ4-RAD                                                       
415300           PERFORM S01A-HAMTA-BENAMNING                                   
415400        END-IF                                                            
415500     END-IF                                                               
415600     .                                                                    
415700     EJECT                                                                
415800 S01A-HAMTA-BENAMNING SECTION.                                            
415900                                                                          
416000     MOVE 'S01A-HAMTA-BENAMNING'       TO PGMPOS                          
416100                                                                          
416200     MOVE WS-IDDC TO W-IDDC-B6                                            
416300     PERFORM IMS-GU-WDB601                                                
416400     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT-X                            
416500                                                                          
416600     MOVE AREG-IDARTNR TO W-D3BSEQ-IDARTNR                                
416700                                                                          
416800     PERFORM IMS-GU-BENA-WLBENA11                                         
416900     IF SEGMENT-FINNS                                                     
417000        MOVE TEXT-BEART       TO WS-BEART                                 
417100     ELSE                                                                 
417200        MOVE 'NAME MISSING'   TO WS-BEART                                 
417300     END-IF                                                               
417400     .                                                                    
417500     EJECT                                                                
417600 S02-ORDERBEKRAFTELSE SECTION.                                            
417700                                                                          
417800     MOVE 'S02-ORDERBEKRAFTELS'       TO PGMPOS                           
417900     IF WS-ANTOBKR = 0                                                    
418000        PERFORM S02A-KOLLA-LOPNR                                          
418100     END-IF                                                               
418200                                                                          
418300     MOVE ORAD-IDORDER            TO OBKR-IDORDER                         
418400     MOVE ORAD-IDARTNR            TO OBKR-IDARTNR                         
418500     MOVE W-Q101KY-MIN-IDLOPNR    TO OBKR-IDLOPNR                         
418600     MOVE 1                       TO OBKR-IDSEKVNR                        
418700     MOVE ORAD-IDDC               TO OBKR-IDDC                            
418800     MOVE ORAD-IDDC-RO            TO OBKR-IDDC-RO                         
418900     MOVE ORAD-KDOI               TO OBKR-KDOI                            
419000     MOVE ORAD-CLEARGROUP         TO OBKR-CLEARGROUP                      
419100     IF WS-KDORDBEK    = 91                                               
419200        AND                                                               
419300        ORAD-TIRODAT = 0                                                  
419400        MOVE 90                   TO OBKR-KDORDBEK                        
419500        MOVE IDPGM                TO OBKR-IDPGM                           
419600     ELSE                                                                 
419700        MOVE WS-KDORDBEK          TO OBKR-KDORDBEK                        
419800        MOVE WS-IDPGM             TO OBKR-IDPGM                           
419900     END-IF                                                               
420000     MOVE SPACE                   TO OBKR-BEERS                           
420100     MOVE OHUV-BEKUNDRF           TO OBKR-BEKUNDRF                        
420200     MOVE ORAD-BERADREF           TO OBKR-BERADREF                        
420300     MOVE ORAD-BEVOLREF           TO OBKR-BEVOLREF                        
420400     MOVE ORAD-IDKAMPRF           TO OBKR-IDKAMPRF                        
420500     MOVE 0                       TO OBKR-DIERS-KVOT                      
420600     MOVE ORAD-FLAKPLOC           TO OBKR-FLAKPLOC                        
420700     MOVE ORAD-FLINVEST           TO OBKR-FLINVEST                        
420800     MOVE JA                      TO OBKR-FLOBOK                          
420900     MOVE NEJ                     TO OBKR-FLOBTRAN                        
421000     MOVE NEJ                     TO OBKR-FLOBPRT                         
421100     MOVE ORAD-FLPRTILL           TO OBKR-FLPRTILL                        
421200     MOVE ORAD-FLRESTN            TO OBKR-FLRESTN                         
421300     MOVE JA                      TO OBKR-FLSLATT                         
421400     MOVE ORAD-FLTILLK            TO OBKR-FLTILLK                         
421500     MOVE 0                       TO OBKR-IDARTNR-TILLK                   
421600     MOVE ORAD-IDDISTR            TO OBKR-IDDISTR                         
421700     MOVE ORAD-IDKUNDNR           TO OBKR-IDKUNDNR                        
421800     MOVE W-ODEL-IDKUNDRF         TO OBKR-IDKUNDRF                        
421900                                                                          
422000     MOVE OHUV-IDKONTO            TO WS-IDKONTO                           
422100     MOVE ORAD-IDKUNDRF-RO        TO OBKR-IDKUNDRF-RO                     
422200     MOVE ORAD-IDLEVNR            TO OBKR-IDLEVNR                         
422300     MOVE ORAD-IDLOPNR-RO         TO OBKR-IDLOPNR-RO                      
422400     MOVE ORAD-IDSYSTEM           TO OBKR-IDSYSTEM                        
422500     MOVE ORAD-KDDSP              TO OBKR-KDDSP                           
422600     MOVE 0                       TO OBKR-KDERS                           
422700     MOVE ORAD-KDKVBRYT           TO OBKR-KDKVBRYT                        
422800     MOVE ORAD-KDPRTYP            TO OBKR-KDPRTYP                         
422900     MOVE ORAD-KDTPOTYP           TO OBKR-KDTPOTYP                        
423000     MOVE ORAD-KDVRINFO           TO OBKR-KDVRINFO                        
423100                                                                          
423200     IF WS-KDORDBEK = 90 OR 91                                            
423300        MOVE WS-KVROS             TO OBKR-KVRO                            
423400        MOVE 0                    TO OBKR-KVBEART                         
423500     ELSE                                                                 
423600        MOVE 0                    TO OBKR-KVRO                            
423700        MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART                         
423800     END-IF                                                               
423900                                                                          
424000     IF WS-KDORDBEK = 92                                                  
424100        COMPUTE OBKR-KVPRERO = ORAD-KVBEART-Q - WS-KVAVBART               
424200     ELSE                                                                 
424300        MOVE 0                    TO OBKR-KVPRERO                         
424400     END-IF                                                               
424500                                                                          
424600     IF WS-KDORDBEK = 80                                                  
424700        COMPUTE OBKR-KVANNANT = ORAD-KVBEART-Q - WS-KVAVBART              
424800     ELSE                                                                 
424900        MOVE 0                    TO OBKR-KVANNANT                        
425000     END-IF                                                               
425100                                                                          
425200     MOVE WS-KVAVBART             TO OBKR-KVAVBART                        
425300     MOVE ORAD-KVBEART-Q          TO OBKR-KVBEART-Q                       
425400     MOVE ORAD-KVPREAVB           TO OBKR-KVPREAVB                        
425500     MOVE AREG-KVQPACK-1          TO OBKR-KVQPACK                         
425600     MOVE 0                       TO OBKR-KVBEART-TILLK                   
425700     MOVE ORAD-PRARTNTO           TO OBKR-PRARTNTO                        
425800     MOVE ORAD-DEAL-PR-LINE       TO OBKR-DEAL-PR-LINE                    
425900     MOVE ORAD-PRBPRIS            TO OBKR-PRBPRIS                         
426000     MOVE ORAD-REKSIFFR           TO OBKR-REKSIFFR                        
426100     MOVE 0                       TO OBKR-REKSIFFR-TILLK                  
426200     MOVE ORAD-RERF-RAD           TO OBKR-RERF-RAD                        
426300     MOVE ORAD-KVSLATT            TO OBKR-KVSLATT                         
426400     MOVE AREG-TIDISPIN           TO OBKR-TIDISPIN                        
426500     MOVE ORAD-TIPRIS             TO OBKR-TIPRIS                          
426600     MOVE WS-DATUM-LOK            TO OBKR-TIREGDAT                        
426700     MOVE WS-TIHHMMSS-LOK         TO OBKR-TIREGTID                        
426800                                                                          
426900     IF WS-KDORDBEK = 90 OR 91                                            
427000        MOVE WS-DATUM-LOK         TO OBKR-TIRODAT                         
427100     ELSE                                                                 
427200        MOVE ZERO                 TO OBKR-TIRODAT                         
427300     END-IF                                                               
427400                                                                          
427500     MOVE ARB-KDFRAKT             TO OBKR-KDFRAKT                         
427600     MOVE OHUV-KDORDKL            TO OBKR-KDORDKL                         
427700     MOVE SPACE                   TO OBKR-IDBIL                           
427800     MOVE ORAD-TITPO              TO OBKR-TITPO                           
427900     MOVE OHUV-KDORDTYP-LDC       TO OBKR-KDORDTYP-LDC                    
428000     MOVE OHUV-TIREPDAT           TO OBKR-TIREPDAT                        
428100     MOVE ORAD-IDKUNDRF-WIP       TO OBKR-IDKUNDRF-WIP                    
428200     MOVE ZERO                    TO OBKR-TIDLEVDAT                       
428300     MOVE ORAD-PRAVCOST           TO OBKR-PRAVCOST                        
428400     PERFORM S02B-DATUM-9KOMPL                                            
428500                                                                          
428600     IF OBKR-KDORDBEK = 90 OR 91 OR 92 OR 93                              
428700        MOVE OBKR-IDARTNR         TO W-IDARTNR                            
428800        IF OBKR-IDDC NOT = DCS-IDDC                                       
428900           MOVE OBKR-IDDC             TO W-IDDC-B6                        
429000           PERFORM IMS-GU-WDB601                                          
429100        END-IF                                                            
429200        MOVE DCS-IDLANDX2         TO W-IDLAND                             
429300        PERFORM IMS-GU-WDK712                                             
429400        IF SEGMENT-FINNS AND LART-FLREFERAL = JA                          
429500           MOVE 98                TO OBKR-KDORDBEK                        
429600        END-IF                                                            
429700     END-IF                                                               
429800                                                                          
429900     PERFORM IMS-ISRT-ORQM-WLORQM01                                       
430000                                                                          
430100     PERFORM UNTIL SEGMENT-FINNS                                          
430200        ADD 1 TO OBKR-IDSEKVNR                                            
430300        PERFORM IMS-ISRT-ORQM-WLORQM01                                    
430400     END-PERFORM                                                          
430500     ADD 1                        TO WS-ANTOBKR                           
430600     IF (OBKR-IDSYSTEM = 'LDC' OR 'TACD') AND                             
430700        (GMT-FLOBKR-TACD = JA           )                                 
430800*       CONTINUE                                                          
430900        PERFORM S02C-CREATE-TACD-402                                      
431000     END-IF                                                               
431100     .                                                                    
431200     EJECT                                                                
431300 S02A-KOLLA-LOPNR SECTION.                                                
431400                                                                          
431500     MOVE 'S02A-KOLLA-LOPNR'       TO PGMPOS                              
431600     MOVE ORAD-IDORDER    TO W-Q101KY-MIN-IDORDER                         
431700                             W-Q101KY-MAX-IDORDER                         
431800     MOVE ORAD-IDARTNR    TO W-Q101KY-MIN-IDARTNR                         
431900                             W-Q101KY-MAX-IDARTNR                         
432000     MOVE 1               TO W-Q101KY-MIN-IDLOPNR                         
432100                             W-Q101KY-MAX-IDLOPNR                         
432200                             W-Q101KY-MIN-IDSEKVNR                        
432300                             W-Q101KY-MAX-IDSEKVNR                        
432400                                                                          
432500     PERFORM IMS-GU-ORQM-WLORQM01                                         
432600     PERFORM UNTIL SEGMENT-SAKNAS                                         
432700        ADD  1            TO W-Q101KY-MIN-IDLOPNR                         
432800                             W-Q101KY-MAX-IDLOPNR                         
432900        PERFORM IMS-GU-ORQM-WLORQM01                                      
433000     END-PERFORM                                                          
433100     .                                                                    
433200     EJECT                                                                
433300 S02B-DATUM-9KOMPL SECTION.                                               
433400                                                                          
433500     MOVE ORAD-TIREGDAT               TO WS-DATUM-9KOMPL                  
433600     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-DATUM-9KOMPL (1:2)            
433700                                                                          
433800     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
433900                                                                          
434000* OBKR-TIORDREG HAR FORMEN S9(7) COMP-3 (0YYMMDD) OCH MÅSTE DÄRFÖR        
434100* FLYTTAS FÖRE WS-SEKELTAL.                                               
434200     MOVE OHUV-TIREGDAT    TO OBKR-TIORDREG                               
434300     MOVE OBKR-TIORDREG    TO WS-DATUM-9KOMPL                             
434400                                                                          
434500     IF OBKR-TIORDREG  > 500000                                           
434600       MOVE WS-SEKELTAL-19 TO WS-DATUM-9KOMPL (1:2)                       
434700     ELSE                                                                 
434800       MOVE WS-SEKELTAL-20 TO WS-DATUM-9KOMPL (1:2)                       
434900     END-IF                                                               
435000     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
435100     .                                                                    
435200     EJECT                                                                
435300 S02C-CREATE-TACD-402 SECTION.                                            
435400     MOVE 'STA S02C-CREATE-TACD-402'  TO PGMPOS                           
435500     MOVE 'PU1'                   TO 402-IDPTYP                           
435600     MOVE 01                      TO 402-IDVTYP-TACDIS                    
435700     MOVE WS-DATUM-Y2K            TO 402-DAREGDAT                         
435800     MOVE OBKR-IDDISTR            TO 402-IDDISTR                          
435900     MOVE OBKR-IDKUNDNR           TO 402-IDKUNDNR                         
436000     MOVE OBKR-IDORDNR7           TO 402-IDORDNR7                         
436100                                                                          
436200     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
436300     MOVE OBKR-IDARTNR            TO CIA-IDARTBET-IN                      
436400     CALL W009CIA              USING CIA-W009CIA                          
436500     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET                         
436600                                                                          
436700     MOVE OBKR-KDORDBEK           TO 402-KDORDBEK                         
436800     MOVE OBKR-KVBEART            TO 402-KVBEART                          
436900     MOVE OBKR-IDSEKVNR           TO 402-IDSEKVNR                         
437000                                                                          
437100     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
437200     MOVE OBKR-IDARTNR-TILLK      TO CIA-IDARTBET-IN                      
437300     CALL W009CIA              USING CIA-W009CIA                          
437400     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET-TILLK                   
437500                                                                          
437600     MOVE OBKR-KVBEART-TILLK      TO 402-KVLEVART                         
437700     MOVE OBKR-IDDC               TO 402-IDDC                             
437800     MOVE ZERO                    TO 402-DADLEVDAT                        
437900                                                                          
438000*    IF WS-KV402 = 0                                                      
438100       PERFORM S21-SEND-OPEN-TACD                                         
438200       PERFORM S21-PUT-HEADER-TACD                                        
438300*    END-IF                                                               
438400*    ADD +1              TO WS-KV402                                      
438500     PERFORM S21-PUT-LINE-TACD                                            
438600     PERFORM S21-SKICKA-CLOSE-TACD                                        
438700*    skickningen görs tills vidare styckevis                              
438800*    CALL FELLOG                                                          
438900     .                                                                    
439000     EJECT                                                                
439100 S03-NYUPPLAGG-RESTORDER SECTION.                                         
439200                                                                          
439300     MOVE 'STA S03-NYUPPLAGG-REST'   TO PGMPOS                            
439400     PERFORM S03A-HAMTA-PRIORITETSKOD                                     
439500                                                                          
439600     MOVE ORAD-IDDISTR            TO RAD-IDDISTR                          
439700     MOVE ORAD-IDKUNDNR           TO RAD-IDKUNDNR                         
439800     MOVE SPACE                   TO RAD-IDKUNDRF                         
439900     MOVE ORAD-IDKUNDRF (3:5)     TO RAD-IDKUNDRF                         
440000     MOVE ORAD-IDARTNR            TO RAD-IDARTNR                          
440100     MOVE 1                       TO RAD-IDLOPNR                          
440200     MOVE OHUV-BEKUNDRF           TO RAD-BEKUNDRF                         
440300     MOVE ORAD-BERADREF           TO RAD-BERADREF                         
440400     MOVE NEJ                     TO RAD-FLERS                            
440500     MOVE AREG-IDANSK             TO RAD-IDANSK                           
440600                                                                          
440700     MOVE OHUV-IDANALYS           TO RAD-IDANALYS                         
440800     MOVE OHUV-IDKST              TO RAD-IDKST                            
440900     MOVE OHUV-IDKONTO            TO WS-IDKONTO                           
441000     MOVE WS-IDKONTO              TO RAD-IDKONTO                          
441100     MOVE '00000     '            TO RAD-IDKUNDRF-LEV                     
441200     MOVE ORAD-IDDC-RO            TO RAD-IDDC                             
441300                                     RAD-IDDC-RO                          
441400     MOVE ORAD-KDDSP              TO RAD-KDDSP                            
441500     MOVE OHUV-KDFAKTYP           TO RAD-KDFAKTYP                         
441600     MOVE ARB-KDFRAKT             TO RAD-KDFRAKT                          
441700     MOVE ARB-KDROPACK            TO RAD-KDROPACK                         
441800     MOVE ORAD-IDARBREF           TO RAD-IDARBREF                         
441900     MOVE ORAD-KDKVBRYT           TO RAD-KDKVBRYT                         
442000     MOVE ORAD-KDORDING           TO RAD-KDORDING                         
442100     MOVE ORAD-KDOI               TO RAD-KDOI                             
442200     MOVE ORAD-CLEARGROUP         TO RAD-CLEARGROUP                       
442300     MOVE ORAD-KDORDKL            TO RAD-KDORDKL                          
442400     MOVE ORAD-KDPRODSL           TO RAD-KDPRODSL                         
442500     MOVE WS-KDRAPRIO             TO RAD-KDRAPRIO                         
442600     MOVE WS-KDROO                TO RAD-KDROO                            
442700     MOVE WS-KVROS                TO RAD-KVRO                             
442800                                     RAD-KVART                            
442900     MOVE '2'                     TO RAD-KDSTARAD                         
443000     MOVE ORAD-KDTPOTYP           TO RAD-KDTPOTYP                         
443100     MOVE ORAD-KDVRINFO           TO RAD-KDVRINFO                         
443200     MOVE ORAD-PRARTNTO           TO RAD-PRARTNTO                         
443300     MOVE ORAD-DEAL-PR-LINE       TO RAD-DEAL-PR-LINE                     
443400     MOVE ORAD-REKSIFFR           TO RAD-REKSIFFR                         
443500     MOVE 0                       TO RAD-TIAVBOKN                         
443600     MOVE ORAD-TIREGDAT           TO RAD-TIREGDAT                         
443700     MOVE 0                       TO RAD-TIRES                            
443800     MOVE WS-DATUM-LOK            TO RAD-DARODAT                          
443900     IF WS-DATUM-LOK NOT = ZERO                                           
444000       IF WS-DATUM-LOK < 500000                                           
444100         MOVE 20                  TO RAD-DARODAT (1:2)                    
444200       ELSE                                                               
444300         IF WS-DATUM-LOK < 999999                                         
444400           MOVE 19                TO RAD-DARODAT (1:2)                    
444500         ELSE                                                             
444600           MOVE 99999999          TO RAD-DARODAT                          
444700         END-IF                                                           
444800       END-IF                                                             
444900     END-IF                                                               
445000     MOVE ORAD-TITPO              TO RAD-TITPO                            
445100     MOVE ORAD-KDPRTYP            TO RAD-KDPRTYP                          
445200     MOVE ORAD-BEVOLREF           TO RAD-BEVOLREF                         
445300     MOVE ORAD-FLINVEST           TO RAD-FLINVEST                         
445400     MOVE ORAD-FLPRTILL           TO RAD-FLPRTILL                         
445500     MOVE JA                      TO RAD-FLTPOBEK                         
445600     MOVE ORAD-IDKAMPRF           TO RAD-IDKAMPRF                         
445700     MOVE ORAD-IDLEVNR            TO RAD-IDLEVNR                          
445800     MOVE ORAD-IDSYSTEM           TO RAD-IDSYSTEM                         
445900     MOVE ORAD-KVBEART-Q          TO RAD-KVBEART-Q                        
446000     MOVE ORAD-TIREGTID           TO RAD-TIREGTID                         
446100     MOVE 0                       TO RAD-DASENDAT                         
446200                                     RAD-TISENBEK-KL                      
446300     MOVE OHUV-KDORDTYP-LDC       TO RAD-KDORDTYP-LDC                     
446400     MOVE OHUV-TIREPDAT           TO RAD-TIREPDAT                         
446500     MOVE ORAD-IDKUNDRF-WIP       TO RAD-IDKUNDRF-WIP                     
446600     MOVE ORAD-PRAVCOST           TO RAD-PRAVCOST                         
446700*                                                                         
446800*DET FINNS NÅGRA WDQ401 MED SKRÄP I DESSA FÄLT.                           
446900*DETTA KAN EV. TA HAND OM 0C7-FEL...                                      
447000                                                                          
447100     IF ORAD-PRARTNTO-LOC NOT NUMERIC                                     
447200        MOVE ZERO                 TO RAD-PRARTNTO-LOC                     
447300     END-IF                                                               
447400     IF ORAD-IDPRQUES NOT NUMERIC                                         
447500        MOVE ZERO                 TO RAD-IDPRQUES                         
447600     END-IF                                                               
447700     IF ORAD-PRARTBTO-LOC NOT NUMERIC                                     
447800        MOVE ZERO                 TO RAD-PRARTBTO-LOC                     
447900     END-IF                                                               
448000                                                                          
448100     PERFORM S36-ANDRA-WDC711                                             
448200                                                                          
448300     PERFORM IMS-ISRT-ORDP-WLORDP01                                       
448400                                                                          
448500     PERFORM UNTIL SEGMENT-FINNS                                          
448600        ADD 1 TO RAD-IDLOPNR                                              
448700        PERFORM IMS-ISRT-ORDP-WLORDP01                                    
448800     END-PERFORM                                                          
448900     PERFORM S03B-KOLLA-CROSS-DOCKING                                     
449000                                                                          
449010     IF LYNK-NON-API                                                      
449020       PERFORM S03D-CR-NON-API-EVENT                                      
449030     ELSE                                                                 
449100*NEW                                                                      
449200*EVENT HANDLING                                                           
449300*LYND = DÖSKALLE  WDQ2C                                                   
449400       IF OHUV-IDSYSTEM = 'LYND' OR 'TADD'                                
449500         MOVE OHUV-IDDISTR             TO W-IDDISTR-CSEQ                  
449600         MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-CSEQ                 
449700         MOVE ORAD-IDKUNDRF-RO(3:5)    TO W-IDORDNR5-CSEQ                 
449800         PERFORM IMS-GU-WDQ201-CSEQ-GE                                    
451900         IF SEGMENT-FINNS                                                 
451901           MOVE CSQ-OHUV-IDDISTR           TO WS-IDDISTR-EVENT            
451902           MOVE CSQ-OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT           
451903           MOVE CSQ-OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT           
451904           MOVE CSQ-OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT           
451905           MOVE JA                         TO CREATE-EVENT-SW             
452401         END-IF                                                           
452402       ELSE                                                               
452403*LYNV = VOR KUNDRF-LEV -A6                                                
452404         IF OHUV-IDSYSTEM = 'LYNV' OR 'TADV'                              
452405           MOVE LOW-VALUE              TO W-WDA6BSEQ-MIN-X                
452406           MOVE HIGH-VALUE             TO W-WDA6BSEQ-MAX-X                
452900                                                                          
452901           MOVE OHUV-IDDISTR           TO W-A6BSEQ-MIN-IDDISTR            
452902                                          W-A6BSEQ-MAX-IDDISTR            
452903           MOVE OHUV-IDKUNDNR          TO W-A6BSEQ-MIN-IDKUNDNR           
452904                                          W-A6BSEQ-MAX-IDKUNDNR           
452905           MOVE OHUV-IDKUNDRF         TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
452906                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
452907           PERFORM IMS-GU-SEQB-WDA601                                     
453800           IF SEGMENT-FINNS                                               
453801             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
453802             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
453803             MOVE VOR-IDKUNDRF(1:7)    TO WS-IDORDNR7-EVENT               
453804             MOVE VOR-TIREGDAT-URSP    TO WS-TIREGDAT-EVENT               
453805             MOVE JA                   TO CREATE-EVENT-SW                 
454301           END-IF                                                         
454302         ELSE                                                             
454303*LYNB = VERKSTADS/REPARATIONS-ORDER -A5                                   
454304           IF OHUV-IDSYSTEM = 'LYNB' OR 'TADB'                            
454305             MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                
454306                                          W-IDDISTR-A5-MAX                
454307             MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN               
454308                                          W-IDKUNDNR-A5-MAX               
454309             MOVE OHUV-KDORDKL         TO W-KDORDKL                       
454310             MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                  
454311*                                                                         
454312             PERFORM IMS-GU-WDA501                                        
454313             IF SEGMENT-FINNS                                             
454314               MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                
454315               MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT               
454316               MOVE  RAD-IDORDNR5      TO WS-IDORDNR7-EVENT               
454317               MOVE  RAD-TIREGDAT      TO WS-TIREGDAT-EVENT               
454318               MOVE JA                 TO CREATE-EVENT-SW                 
454400             END-IF                                                       
454500           ELSE                                                           
454600             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
454700             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
454800             MOVE OHUV-IDORDNR7        TO WS-IDORDNR7-EVENT               
454900             MOVE OHUV-TIREGDAT        TO WS-TIREGDAT-EVENT               
455000             MOVE JA                   TO CREATE-EVENT-SW                 
455100           END-IF                                                         
455200         END-IF                                                           
455300       END-IF                                                             
455310     END-IF                                                               
455400                                                                          
455500     IF CREATE-EVENT                                                      
455600                                                                          
455700       MOVE OHUV-IDSYSTEM(1:4) TO EVENT-SW                                
455800       IF EVENT-OK OR LYNK-NON-API                                        
455900                                                                          
456000         IF (OHUV-IDSYSTEM(1:3) = 'LYN') OR LYNK-NON-API                  
456100           MOVE 'L'            TO WS-PARTNER                              
456200         END-IF                                                           
456300         IF OHUV-IDSYSTEM(1:3) = 'POL'                                    
456400           MOVE 'P'            TO WS-PARTNER                              
456500         END-IF                                                           
456600         IF OHUV-IDSYSTEM(1:3) = 'ECO'                                    
456700           MOVE 'E'            TO WS-PARTNER                              
456800         END-IF                                                           
456900         IF OHUV-IDSYSTEM(1:3) = 'TAD'                                    
457000           MOVE 'T'            TO WS-PARTNER                              
457100         END-IF                                                           
457200                                                                          
457300         IF OHUV-IDSYSTEM(1:3) = 'ACC'                                    
457400           MOVE 'A'            TO WS-PARTNER                              
457500         END-IF                                                           
457600         IF OHUV-IDSYSTEM(1:3) = 'APA'                                    
457700           MOVE 'K'            TO WS-PARTNER                              
457800         END-IF                                                           
457900         IF OHUV-IDSYSTEM(1:3) = 'APB'                                    
458000           MOVE 'B'            TO WS-PARTNER                              
458100         END-IF                                                           
458200         IF OHUV-IDSYSTEM(1:3) = 'APC'                                    
458300           MOVE 'C'            TO WS-PARTNER                              
458400         END-IF                                                           
458500         IF OHUV-IDSYSTEM(1:3) = 'APD'                                    
458600           MOVE 'D'            TO WS-PARTNER                              
458700         END-IF                                                           
458800         IF OHUV-IDSYSTEM(1:3) = 'APE'                                    
458900           MOVE 'M'            TO WS-PARTNER                              
459000         END-IF                                                           
459100         IF OHUV-IDSYSTEM(1:3) = 'APF'                                    
459200           MOVE 'F'            TO WS-PARTNER                              
459300         END-IF                                                           
459400         IF OHUV-IDSYSTEM(1:3) = 'APG'                                    
459500           MOVE 'G'            TO WS-PARTNER                              
459600         END-IF                                                           
459700         IF OHUV-IDSYSTEM(1:3) = 'APH'                                    
459800           MOVE 'H'            TO WS-PARTNER                              
459900         END-IF                                                           
460000         IF OHUV-IDSYSTEM(1:3) = 'API'                                    
460100           MOVE 'I'            TO WS-PARTNER                              
460200         END-IF                                                           
460300         IF OHUV-IDSYSTEM(1:3) = 'APJ'                                    
460400           MOVE 'J'            TO WS-PARTNER                              
460500         END-IF                                                           
460600                                                                          
460700         MOVE SPACE             TO Z430-REQU-TIMESTAMP                    
460800         PERFORM S03C-CREATE-EVENT-152                                    
460900       END-IF                                                             
461000     END-IF                                                               
461100                                                                          
461200     .                                                                    
461300     EJECT                                                                
461400 S03A-HAMTA-PRIORITETSKOD SECTION.                                        
461500                                                                          
461600     MOVE 'STA S03A-HAMTA-PRIORI'   TO PGMPOS                             
461700     IF OHUV-IDKAMPRF > 0                                                 
461800        MOVE 4             TO W-4512-KDTPOTYP                             
461900     ELSE                                                                 
462000        MOVE OHUV-KDTPOTYP TO W-4512-KDTPOTYP                             
462100     END-IF                                                               
462200                                                                          
462300     MOVE OHUV-KDORDKL TO W-4512-KDORDKL                                  
462400     MOVE OHUV-IDDISTR TO W-4512-IDDISTR-FOM                              
462500                          W-4512-IDDISTR-TOM                              
462600                                                                          
462700     PERFORM IMS-GU-XXJN-WLXXJN11                                         
462800     IF SEGMENT-FINNS                                                     
462900        MOVE 4512-KDRAPRIO  TO WS-KDRAPRIO                                
463000     ELSE                                                                 
463100        MOVE 99             TO WS-KDRAPRIO                                
463200     END-IF                                                               
463300     .                                                                    
463400     EJECT                                                                
463500 S03B-KOLLA-CROSS-DOCKING   SECTION.                                      
463600                                                                          
463700     MOVE 'STA S03B-KOLLA-CROSS'   TO PGMPOS                              
463800     MOVE ORAD-IDARTNR       TO W-IDARTNR                                 
463900     PERFORM IMS-GHU-WDK611                                               
464000     MOVE 1                  TO IX-CD-OMR                                 
464100     PERFORM UNTIL IX-CD-OMR > 4                                          
464200     OR ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                      
464300       ADD 1                 TO IX-CD-OMR                                 
464400     END-PERFORM                                                          
464500     IF IX-CD-OMR <= 4                                                    
464600*                                                                         
464700*BOKA UPP CROSS DOCKING SALDO                                             
464800*                                                                         
464900        ADD ORAD-KVBEART-Q                                                
465000                         TO CLAG-KVLS-CD (IX-CD-OMR)                      
465100     END-IF                                                               
465200     PERFORM IMS-REPL-WDK6                                                
465300     SKIP2                                                                
465400     .                                                                    
465500     EJECT                                                                
465600 S03C-CREATE-EVENT-152 SECTION.                                           
465700     MOVE 'STA S03C-CREATE-EVENT'   TO PGMPOS                             
465800                                                                          
465900     IF LYNK-NON-API                                                      
465910        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
465920     ELSE                                                                 
465930        MOVE OHUV-IDSYSTEM      TO Z430-REQU-IDEVENTREC                   
465940     END-IF                                                               
465950                                                                          
466000                                                                          
466100     MOVE IDMSGVER              TO Z430-REQU-IDMSGVER                     
466200     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
466300     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
466400***  MOVE WS-TIMESTAMP          TO Z430-REQU-TIMESTAMP                    
466500     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
466600     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
466700     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
466800     MOVE '152'                 TO Z430-IDMSG                             
466900     MOVE 'ONE ORDER LINE WAS BACKORDERED'                                
467000                                TO Z430-TEMFSINF                          
467100                                                                          
467200*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
467300*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
467400     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
467500     MOVE 'Z430'               TO MSG-IDTRANS-1                           
467600     MOVE '1'                  TO MSG-KDMFSFOR-1                          
467700     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
467800     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
467900          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
468000                                                                          
468100     ADD  1                    TO MSG-KOM-TIKLOCK                         
468200     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
468300     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
468400                                                                          
468500     CALL W006KOM USING MSG-PCB                                           
468600                        0693-PCB                                          
468700                        KOM-WDP8-PCB                                      
468800                        MSG-KOM-WMSGKOM                                   
468900                        MSG-IO-AREA                                       
469000                                                                          
469100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
469200        MOVE                                                              
469300        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
469400                                     TO FELTEXT                           
469500        CALL ABEND USING RKOD-ABEND-WITHOUT-DUMP                          
469600     END-IF                                                               
469700     .                                                                    
469800     EJECT                                                                
469900 S03D-CR-NON-API-EVENT   SECTION.                                         
470000                                                                          
470100     MOVE 'STA S03D-CR-NON-AP'   TO PGMPOS                                
470101                                                                          
470102     MOVE NEJ                  TO CREATE-EVENT-SW                         
470103     IF VOR                                                               
470104        MOVE LOW-VALUE              TO W-WDA6BSEQ-MIN-X                   
470105        MOVE HIGH-VALUE             TO W-WDA6BSEQ-MAX-X                   
470106                                                                          
470107        MOVE OHUV-IDDISTR           TO W-A6BSEQ-MIN-IDDISTR               
470108                                       W-A6BSEQ-MAX-IDDISTR               
470109        MOVE OHUV-IDKUNDNR          TO W-A6BSEQ-MIN-IDKUNDNR              
470110                                       W-A6BSEQ-MAX-IDKUNDNR              
470111        MOVE OHUV-IDKUNDRF          TO W-A6BSEQ-MIN-IDKUNDRF-LEV          
470112                                       W-A6BSEQ-MAX-IDKUNDRF-LEV          
470113        PERFORM IMS-GU-SEQB-WDA601                                        
470114        IF SEGMENT-FINNS                                                  
470115          MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                   
470116          MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT                  
470117          MOVE VOR-IDKUNDRF(1:7)    TO WS-IDORDNR7-EVENT                  
470118          MOVE VOR-TIREGDAT-URSP    TO WS-TIREGDAT-EVENT                  
470119          MOVE JA                   TO CREATE-EVENT-SW                    
470120        END-IF                                                            
470121     ELSE                                                                 
470122        IF (ORAD-IDKUNDRF-RO NOT = '0000000   ') AND                      
470123           (ORAD-IDKUNDRF-RO NOT = '00000     ')                          
470124          MOVE OHUV-IDDISTR             TO W-IDDISTR-CSEQ                 
470125          MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-CSEQ                
470126          MOVE ORAD-IDKUNDRF-RO(3:5)    TO W-IDORDNR5-CSEQ                
470127          PERFORM IMS-GU-WDQ201-CSEQ-GE                                   
470128          IF SEGMENT-FINNS                                                
470129            MOVE CSQ-OHUV-IDDISTR           TO WS-IDDISTR-EVENT           
470130            MOVE CSQ-OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT          
470131            MOVE CSQ-OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT          
470132            MOVE CSQ-OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT          
470133            MOVE JA                         TO CREATE-EVENT-SW            
470134          END-IF                                                          
470135        ELSE                                                              
470136          MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                   
470137                                       W-IDDISTR-A5-MAX                   
470138          MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN                  
470139                                       W-IDKUNDNR-A5-MAX                  
470140          MOVE OHUV-KDORDKL         TO W-KDORDKL                          
470141          MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                     
470142                                                                          
470143          PERFORM IMS-GU-WDA501                                           
470144          IF SEGMENT-FINNS                                                
470145            MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                   
470146            MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT                  
470147            MOVE  RAD-IDORDNR5      TO WS-IDORDNR7-EVENT                  
470148            MOVE  RAD-TIREGDAT      TO WS-TIREGDAT-EVENT                  
470149            MOVE JA                 TO CREATE-EVENT-SW                    
470150          END-IF                                                          
470151        END-IF                                                            
470158     END-IF                                                               
470159     IF CREATE-EVENT-SW = NEJ                                             
470160        MOVE OHUV-IDDISTR           TO WS-IDDISTR-EVENT                   
470161        MOVE OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT                  
470162        MOVE OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT                  
470163        MOVE OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT                  
470164        MOVE JA                     TO CREATE-EVENT-SW                    
470165     END-IF                                                               
470166     .                                                                    
470167     EJECT                                                                
470168 S04-UPPDAT-ART-REG-WDK6 SECTION.                                         
470169                                                                          
470170     MOVE 'STA S04-UPPDAT-ART'   TO PGMPOS                                
470200     IF ARTIKEL-OK                                                        
470300        AND                                                               
470400        OHUV-FLLSBOK = JA                                                 
470500        AND                                                               
470600        ORAD-IDLEVNR = SPACE                                              
470700                                                                          
470800        PERFORM S04A-UPPDAT-KAMPANJ-WDM2                                  
470900        IF WS-KVROS  NOT = 0                                              
471000           OR                                                             
471100           WS-KVLS   NOT = 0                                              
471200           OR                                                             
471300           WS-KVEFRS NOT = 0                                              
471400           OR                                                             
471500           WS-KVRESS NOT = 0                                              
471600           MOVE ORAD-IDARTNR    TO W-IDARTNR                              
471700                                                                          
471800           PERFORM IMS-GHU-WDK611                                         
471900           IF SEGMENT-FINNS                                               
472000              IF CDC                                                      
472100                ADD WS-KVEFRS         TO CLAG-KVEFRS                      
472200****** FÖLJANDE SKALL LOGGA SALDOT PÅ DATABAS WDL9. *******               
472300                IF WS-KVEFRS = 0                                          
472400                  MOVE '+'            TO LOGG-IDTECKEN-KVEFRS             
472500                  MOVE 0              TO LOGG-KVART-SALDO                 
472600                ELSE                                                      
472700                  MOVE '+'            TO LOGG-IDTECKEN-KVEFRS             
472800                  MOVE WS-KVEFRS      TO LOGG-KVART-SALDO                 
472900                END-IF                                                    
473000*SVS FROG                                                                 
473100                MOVE ORAD-IDDISTR   TO TEST-IDDISTR                       
473200                IF DIST20-EMBALLAGE-SVS                                   
473300                OR LOR-IDPRC = 2600                                       
473400                                                                          
473500                  IF WS-KVLS < 0                                          
473600                    MOVE ZERO         TO   CLAG-KVLS-SVS                  
473700                  ELSE                                                    
473800                    IF CLAG-KVLS-SVS <= ZERO                              
473900                      MOVE ZERO       TO   CLAG-KVLS-SVS                  
474000                    ELSE                                                  
474100                      SUBTRACT WS-KVLS FROM CLAG-KVLS-SVS                 
474200                    END-IF                                                
474300                  END-IF                                                  
474400                END-IF                                                    
474500                                                                          
474600                SUBTRACT WS-KVLS      FROM CLAG-KVLS                      
474700****** FÖLJANDE SKALL LOGGA SALDOT PÅ DATABAS WDL9. *******               
474800                IF WS-KVLS = 0                                            
474900                  IF WS-KVEFRS > 0                                        
475000                    CONTINUE                                              
475100                   ELSE                                                   
475200                    MOVE '-'          TO LOGG-IDTECKEN-KVLS               
475300                    MOVE 0            TO LOGG-KVART-SALDO                 
475400                  END-IF                                                  
475500                ELSE                                                      
475600                  MOVE '-'            TO LOGG-IDTECKEN-KVLS               
475700                  MOVE WS-KVLS        TO LOGG-KVART-SALDO                 
475800                END-IF                                                    
475900                PERFORM S04D-CREATE-SALDOLOGG                             
476000                                                                          
476100                                                                          
476200                IF ORAD-IDKAMPRF      > 0                                 
476300                   AND                                                    
476400                   WS-KART-KVRESS-ART <= 0                                
476500                   CONTINUE                                               
476600                ELSE                                                      
476700                   SUBTRACT WS-KVRESS FROM CLAG-KVRESS                    
476800                END-IF                                                    
476900              END-IF                                                      
477000                                                                          
477100              IF WS-KVROS > 0                                             
477200                 PERFORM S04C-EV-LARM-2191-MID                            
477300                 ADD WS-KVROS       TO CLAG-KVROS                         
477400              END-IF                                                      
477500              PERFORM IMS-REPL-WDK6                                       
477600           END-IF                                                         
477700        END-IF                                                            
477800                                                                          
477900        PERFORM S04B-UPPDAT-SKROT-WDK6                                    
478000     END-IF                                                               
478100     .                                                                    
478200     EJECT                                                                
478300 S04A-UPPDAT-KAMPANJ-WDM2 SECTION.                                        
478400                                                                          
478500     MOVE 'STA S04A-UPPDAT-KAMP'   TO PGMPOS                              
478600     IF WS-KVRESS > 0                                                     
478700        AND                                                               
478800        ORAD-IDKAMPRF > 0                                                 
478900                                                                          
479000        MOVE ORAD-IDKAMPRF     TO W-KAMP-IDKAMPRF                         
479100        MOVE ORAD-IDDC-RO      TO W-KAMP-IDDC                             
479200        MOVE ORAD-IDARTNR      TO W-KART-IDARTNR                          
479300                                                                          
479400        PERFORM IMS-GHU-WDM211                                            
479500        IF SEGMENT-FINNS                                                  
479600           IF WS-KVAVBART = 0 AND ORAD-FLRESTN = NEJ                      
479700              SUBTRACT ORAD-KVBEART-Q FROM KART-KVBEART-KUND              
479800           ELSE                                                           
479900              MOVE KART-KVRESS-ART      TO WS-KART-KVRESS-ART             
480000              SUBTRACT WS-KVRESS        FROM KART-KVRESS-ART              
480100           END-IF                                                         
480200           PERFORM IMS-REPL-WDM211                                        
480300        END-IF                                                            
480400                                                                          
480500        MOVE ORAD-IDDISTR      TO W-KMRK-IDDISTR-FOM                      
480600        MOVE ORAD-IDDISTR      TO W-KMRK-IDDISTR-TOM                      
480700        MOVE ORAD-IDKUNDNR     TO W-KMRK-IDKUNDNR-FOM                     
480800        MOVE ORAD-IDKUNDNR     TO W-KMRK-IDKUNDNR-TOM                     
480900                                                                          
481000        PERFORM S20-FINN-INTERVALL                                        
481100                                                                          
481200        PERFORM IMS-GHU-WDM221                                            
481300        IF SEGMENT-FINNS                                                  
481400           IF WS-KVAVBART = 0 AND ORAD-FLRESTN = NEJ                      
481500              SUBTRACT ORAD-KVBEART-Q FROM KMRK-KVBEART-KUND              
481600              PERFORM IMS-REPL-WDM221                                     
481700           END-IF                                                         
481800        END-IF                                                            
481900     END-IF                                                               
482000     .                                                                    
482100     EJECT                                                                
482200 S04B-UPPDAT-SKROT-WDK6     SECTION.                                      
482300                                                                          
482400     MOVE 'STA S04B-UPPDAT-SKROT'   TO PGMPOS                             
482500     MOVE W-ODEL-IDDISTR TO DIST18-IDDISTR                                
482600                                                                          
482700     IF DIST18-SKROT AND CDC                                              
482800        IF WS-KVAVBART > 0                                                
482900           MOVE ORAD-IDARTNR   TO W-IDARTNR                               
483000           PERFORM IMS-GHU-WDK611                                         
483100           MOVE NEJ            TO CLAG-FLSKROT-BEORD                      
483200           PERFORM IMS-REPL-WDK6                                          
483300                                                                          
483400           PERFORM IMS-GHNP-WDK627                                        
483500           IF SEGMENT-FINNS                                               
483600              MOVE WS-KVAVBART TO SKROT-KVSKROT                           
483700              MOVE WS-DATUM-Y2K TO SKROT-DASKROT                          
483800              PERFORM IMS-REPL-WDK6                                       
483900           ELSE                                                           
484000              MOVE WS-KVAVBART TO SKROT-KVSKROT                           
484100              MOVE WS-DATUM-Y2K TO SKROT-DASKROT                          
484200              MOVE ZERO        TO SKROT-TISKROT-BEORD                     
484300              PERFORM IMS-ISRT-WDK627                                     
484400           END-IF                                                         
484500        END-IF                                                            
484600     END-IF                                                               
484700     .                                                                    
484800     EJECT                                                                
484900 S04C-EV-LARM-2191-MID SECTION.                                           
485000                                                                          
485100     MOVE 'STA S04C-EV-LARM-2191'   TO PGMPOS                             
485200** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
485300                                                                          
485400     IF OUTPUT-MSG-IX < MAX-ANT-OUTPUT-MSG                                
485500     IF CLAG-KVROS = 0                                                    
485600       IF CLAG-KVAKS-CDC = 0                                              
485700                                                                          
485800         COMPUTE ALT5-LL = LENGTH OF ALT5-MID-W2I19101 + 17               
485900         MOVE +1               TO ALT5-MID-KDCLAGER                       
486000         MOVE ORAD-IDARTNR     TO ALT5-MID-IDARTNR                        
486100         MOVE ZERO             TO ALT5-MID-TISENBEK-DAG                   
486200                                  ALT5-MID-TISENBEK-KL                    
486300         MOVE SPACE            TO ALT5-MID-IDKR                           
486400         MOVE AREG-IDANSK      TO ALT5-MID-IDANSK                         
486500         MOVE 210              TO ALT5-MID-KDLARM                         
486600         MOVE W-ODEL-IDDISTR   TO WS-IDDISTR-NUM4                         
486700         MOVE WS-IDDISTR-NUM4  TO ALT5-MID-IDDISTR                        
486800         MOVE W-ODEL-IDKUNDNR  TO WS-IDKUNDNR-NUM6                        
486900         MOVE WS-IDKUNDNR-NUM6 TO ALT5-MID-IDKUNDNR                       
487000         MOVE W-ODEL-IDKUNDRF  TO ALT5-MID-IDKUNDRF                       
487100         MOVE 'J'              TO ALT5-MID-FLNYLARM                       
487200         MOVE WC-CDC-SE        TO ALT5-MID-IDDC                           
487300         MOVE SPACE            TO ALT5-MID-IDLEVNR                        
487400                                                                          
487500**** FIX-START  OM DET BLIR FÖR MÅNGA INSERTER MOT ALT5-PCB ****          
487600*        IF MID-IDPRODNR =  '0999955'                                     
487700*        AND OUTPUT-MSG-IX > 22                                           
487800*           CONTINUE                                                      
487900*        ELSE                                                             
488000            PERFORM IMS-PURG-ALT5-MSG                                     
488100*        END-IF                                                           
488200**** FIX-SLUT                                               ****          
488300         ADD 1                 TO OUTPUT-MSG-IX                           
488400       END-IF                                                             
488500     END-IF                                                               
488600     END-IF                                                               
488700     .                                                                    
488800     EJECT                                                                
488900***************************************************************           
489000* FÖLJANDE SECTION SKALL UPPDATERA  DATABAS WDL9/WLLOGA       *           
489100***************************************************************           
489200 S04D-CREATE-SALDOLOGG SECTION.                                           
489300                                                                          
489400     MOVE 'STA S04D-CREATE-SALDOL'  TO PGMPOS                             
489500     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
489600                                                                          
489700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
489800     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
489900                                   - WS-AAAAMMDD                          
490000     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
490100     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
490200                                   - WS-TTMMSSTH                          
490300     MOVE 9                        TO LOGG-IDSEKVNR                       
490400     MOVE 11                       TO LOGG-IDDC                           
490500     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
490600     MOVE 'PRT'                    TO LOGG-IDSUBTYP                       
490700     MOVE 'W4037500'               TO LOGG-IDPGM                          
490800     MOVE W-IDTRANS                TO LOGG-IDTRANS                        
490900     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
491000     MOVE SPACE                    TO LOGG-REF                            
491100     MOVE ORAD-IDDISTR             TO LOGG-IDDISTR                        
491200     MOVE ORAD-IDKUNDNR            TO LOGG-IDKUNDNR                       
491300     MOVE ORAD-IDORDNR7            TO LOGG-IDORDNR5                       
491400     MOVE MID-IDPRODNR             TO LOGG-IDPRODNR                       
491500     MOVE MID-IDPLKLST             TO LOGG-IDPLKLST                       
491600     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
491700                                                                          
491800     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                  
491900                        + CLAG-KVAKS-T                                    
492000                                                                          
492100     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
492200     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
492300     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
492400     MOVE CLAG-KVLS                TO LOGG-KVLS                           
492500     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
492600                                                                          
492700     PERFORM IMS-ISRT-WDL901                                              
492800     IF SEGMENT-FINNS-REDAN                                               
492900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
493000         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
493100         PERFORM IMS-ISRT-WDL901                                          
493200       END-PERFORM                                                        
493300     END-IF                                                               
493400     .                                                                    
493500     EJECT                                                                
493600 S04E-LARM-2191-MID-CN-US SECTION.                                        
493700                                                                          
493800     MOVE 'STA S04E-LARM-2191-CN-US' TO PGMPOS                            
493900** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
494000                                                                          
494100     COMPUTE ALT5-LL = LENGTH OF ALT5-MID-W2I19101 + 17                   
494200     MOVE +1                   TO ALT5-MID-KDCLAGER                       
494300     MOVE ORAD-IDARTNR         TO ALT5-MID-IDARTNR                        
494400     MOVE ZERO                 TO ALT5-MID-TISENBEK-DAG                   
494500                                  ALT5-MID-TISENBEK-KL                    
494600     MOVE SPACE                TO ALT5-MID-IDKR                           
494700*                                                                         
494800     PERFORM IMS-GHU-WDK611                                               
494900     MOVE CLAG-IDANSK          TO ALT5-MID-IDANSK                         
495000     PERFORM IMS-GU-WDK722                                                
495100     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
495200       MOVE XLAG-IDANSK        TO ALT5-MID-IDANSK                         
495300     END-IF                                                               
495400*                                                                         
495500     MOVE 210                  TO ALT5-MID-KDLARM                         
495600     MOVE W-ODEL-IDDISTR       TO WS-IDDISTR-NUM4                         
495700     MOVE WS-IDDISTR-NUM4      TO ALT5-MID-IDDISTR                        
495800     MOVE W-ODEL-IDKUNDNR      TO WS-IDKUNDNR-NUM6                        
495900     MOVE WS-IDKUNDNR-NUM6     TO ALT5-MID-IDKUNDNR                       
496000     MOVE W-ODEL-IDKUNDRF      TO ALT5-MID-IDKUNDRF                       
496100     MOVE 'J'                  TO ALT5-MID-FLNYLARM                       
496200     MOVE SLAG-IDDC            TO ALT5-MID-IDDC                           
496300     MOVE SLAG-IDLEVNR         TO ALT5-MID-IDLEVNR                        
496400                                                                          
496500     PERFORM IMS-PURG-ALT5-MSG                                            
496600     ADD 1                     TO OUTPUT-MSG-IX                           
496700     .                                                                    
496800     EJECT                                                                
496900                                                                          
497000 S05-CREATE-PLOCKSATS-PU SECTION.                                         
497100                                                                          
497200     MOVE 'STA S05-CREATE-PLOCKS'  TO PGMPOS                              
497300     MOVE MID-IDPRODNR       TO W-4007-PU-IDPRODNR                        
497400     MOVE MID-IDPLKLST       TO W-4007-PU-IDPLKLST                        
497500     MOVE W-4007-PU-IDHTYP-X TO WL400701                                  
497600     PERFORM IMS-ISRT-4007-WL400701                                       
497700     PERFORM IMS-ISRT-4007-WL400711                                       
497800                                                                          
497900     MOVE JA TO PLOCKSATS-PU-SW                                           
498000     .                                                                    
498100     EJECT                                                                
498200 S06-UPPLAGG-PU-PLE SECTION.                                              
498300                                                                          
498400     MOVE 'STA S06-UPPLAGG-PU'   TO PGMPOS                                
498500*OM LAGEROMRÅDE FÅR 01 PÅ PU OCH ETIKETT SÅ KOMMER DETTA FRÅN             
498600*W411ADRS ELLER W411BIPA. PGA ATT LAGERADRESSEN ÄR 0 PÅ DB.               
498700     IF ARTIKEL-OK                                                        
498800        MOVE ORAD-IDARTNR          TO W-IDARTNR                           
498900        PERFORM IMS-GHU-WDK611                                            
499000        IF SEGMENT-FINNS                                                  
499100           MOVE CLAG-KVQPACK-3     TO 4010-KVQPACK-3                      
499200*CLAG-ADLAGOMR ETC GER K6-ART.ADRESS PÅ PU/PE IST FÖR Q4-ADRESS.          
499300*MEN KAN BLI OSORTERAT VID BIPACKN.                                       
499400           IF CDC                                                         
499500              PERFORM S06Z-KOLLA-AENDRA-LAGOMR                            
499600              IF AENDRA-LAGOMR                                            
499700                MOVE CLAG-ADLAGOMR TO WS-ADLAGOMR                         
499800                MOVE CLAG-ADGANG   TO WS-ADGANG                           
499900                MOVE CLAG-ADPLATS  TO WS-ADPLATS                          
500000*SVS FROG                                                                 
500100                MOVE ORAD-IDDISTR   TO TEST-IDDISTR                       
500200                IF DIST20-EMBALLAGE-SVS                                   
500300                OR LOR-IDPRC = 2600                                       
500400                                                                          
500500                  IF CLAG-ADLAGOMR-SVS > ZERO                             
500600                    MOVE CLAG-ADLAGOMR-SVS TO WS-ADLAGOMR                 
500700                    MOVE CLAG-ADGANG-SVS TO WS-ADGANG                     
500800                    MOVE CLAG-ADPLATS-SVS TO WS-ADPLATS                   
500900                  END-IF                                                  
501000                END-IF                                                    
501100              ELSE                                                        
501200                MOVE ORAD-ADLAGOMR TO WS-ADLAGOMR                         
501300                MOVE ORAD-ADGANG   TO WS-ADGANG                           
501400                MOVE ORAD-ADPLATS  TO WS-ADPLATS                          
501500                MOVE ORAD-IDDISTR   TO TEST-IDDISTR                       
501600                IF DIST20-EMBALLAGE-SVS                                   
501700                OR LOR-IDPRC = 2600                                       
501800                  IF CLAG-ADLAGOMR-SVS > ZERO                             
501900                    MOVE CLAG-ADLAGOMR-SVS TO WS-ADLAGOMR                 
502000                    MOVE CLAG-ADGANG-SVS   TO WS-ADGANG                   
502100                    MOVE CLAG-ADPLATS-SVS  TO WS-ADPLATS                  
502200                  END-IF                                                  
502300                END-IF                                                    
502400              END-IF                                                      
502500           ELSE                                                           
502600              IF NDC                                                      
502700                 MOVE ORAD-IDARTNR      TO W-IDARTNR                      
502800                 MOVE ORAD-IDDC         TO W-IDDC                         
502900                 PERFORM IMS-GU-WDK711                                    
503000                 MOVE SLAG-ADLAGOMR     TO WS-ADLAGOMR                    
503100                 MOVE SLAG-ADGANG       TO WS-ADGANG                      
503200                 MOVE SLAG-ADPLATS      TO WS-ADPLATS                     
503300              ELSE                                                        
503400                 MOVE ORAD-ADLAGOMR TO WS-ADLAGOMR                        
503500                 MOVE ORAD-ADGANG  TO WS-ADGANG                           
503600                 MOVE ORAD-ADPLATS TO WS-ADPLATS                          
503700              END-IF                                                      
503800           END-IF                                                         
503900           MOVE CLAG-KDARTHNT      TO WS-KDARTHNT                         
504000        ELSE                                                              
504100           MOVE +0                 TO WS-KDARTHNT                         
504200           MOVE +0                 TO 4010-KVQPACK-3                      
504300           MOVE ORAD-ADLAGOMR      TO WS-ADLAGOMR                         
504400           MOVE ORAD-ADGANG        TO WS-ADGANG                           
504500           MOVE ORAD-ADPLATS       TO WS-ADPLATS                          
504600        END-IF                                                            
504700        IF PLOCKSATS-ETIK-SAKNAS                                          
504800           PERFORM S09-CREATE-PLOCKSATS-ETIK                              
504900        END-IF                                                            
505000                                                                          
505100        IF PLOCKSATS-PU-SAKNAS                                            
505200           PERFORM S05-CREATE-PLOCKSATS-PU                                
505300        END-IF                                                            
505400                                                                          
505500        IF OHUV-BEVARREF = SPACE                                          
505600          MOVE ORAD-BERADREF TO WS-HFAK-REF-X10                           
505700        ELSE                                                              
505800          MOVE OHUV-BEVARREF TO WS-HFAK-REF-X10                           
505900          PERFORM S15-KOLLA-I-HFAK-TAB                                    
506000          IF BEVARREF-I-HFAK-TAB                                          
506100            MOVE OHUV-BEVARREF TO WS-HFAK-REF-X10                         
506200          ELSE                                                            
506300            MOVE ORAD-BERADREF TO WS-HFAK-REF-X10                         
506400          END-IF                                                          
506500        END-IF                                                            
506600                                                                          
506700        PERFORM S16-KOLLA-OM-RENOVA                                       
506800        IF RENOVA                                                         
506900          MOVE SPACE      TO WS-HFAK-REF-X10                              
507000        END-IF                                                            
507100                                                                          
507200        ADD 1 TO WS-IDRADNR-SISTA                                         
507300        PERFORM S06AA-HAMTA-KDARTURS                                      
507400        PERFORM S06A-HAMTA-PRINTER-PLE                                    
507500        PERFORM S06C-CREATE-PLE                                           
507600        PERFORM S06B-HAMTA-PRINTER-PU                                     
507700        PERFORM S06D-CREATE-PU                                            
507800        ADD 1 TO 4004-KVRADER (100)                                       
507900     END-IF                                                               
508000     .                                                                    
508100     EJECT                                                                
508200 S06A-HAMTA-PRINTER-PLE SECTION.                                          
508300                                                                          
508400     MOVE 'STA S06A-HAMTA-PRINT'  TO PGMPOS                               
508500     MOVE 4454-KDSS-PLE (LOR-ADLAGOMR) TO 4006-KDSS-PLE                   
508600                                                                          
508700     IF 4002-KDPRT-PLE NOT = SPACE AND                                    
508800        NOT BILD-4351-4352                                                
508900        MOVE 4002-KDPRT-PLE TO 4006-KDPRT                                 
509000     ELSE                                                                 
509100                                                                          
509200        MOVE NEJ TO HF-AK-SW                                              
509300        IF CDC                                                            
509400           AND                                                            
509500          (OHUV-FLFORBI  =  JA OR                                         
509600           OHUV-FLFORBI  =  SPEC-FORBI)                                   
509700           PERFORM S06AA-KOLLA-HF-AK-PLE                                  
509800        END-IF                                                            
509900                                                                          
510000        IF HF-AK-PLOCK                                                    
510100           CONTINUE                                                       
510200        ELSE                                                              
510300          IF 4454-KDPRT-PLE (LOR-ADLAGOMR)  NOT = SPACE                   
510400            MOVE 4454-KDPRT-PLE (LOR-ADLAGOMR) TO 4006-KDPRT              
510500            ADD 1 TO 4004-KVRADER (LOR-ADLAGOMR)                          
510600          ELSE                                                            
510700            IF BILD-4351-4352 AND                                         
510800               4002-KDPRT-PLE NOT = SPACE                                 
510900               MOVE 4002-KDPRT-PLE          TO 4006-KDPRT                 
511000            ELSE                                                          
511100               MOVE 4454-KDPRTGEN-PLE          TO 4006-KDPRT              
511200            END-IF                                                        
511300          END-IF                                                          
511400        END-IF                                                            
511500     END-IF                                                               
511600     .                                                                    
511700     EJECT                                                                
511800 S06AA-KOLLA-HF-AK-PLE SECTION.                                           
511900                                                                          
512000*STYRNING PÅ HUVUDNIVÅ TILL PRODUKTIONSKANAL.                             
512100                                                                          
512200     MOVE 'STA S06AA-KOLLA'  TO PGMPOS                                    
512300     IF WS-HFAK-REF-X10 NOT = SPACE                                       
512400        MOVE 1 TO IX1                                                     
512500        PERFORM UNTIL IX1 > MAX-HFAK-IX                                   
512600           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (IX1)                 
512700           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
512800           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
512900           AND HFAK-BERADREF-1 (IX1) = '#')                               
513000              IF AREG-ADLAGOMR > 0  AND  < 100                            
513100                 MOVE AREG-ADLAGOMR  TO IX2                               
513200                 IF 4454-KDPRT-PLE      (IX2) NOT = SPACE                 
513300                    MOVE 4454-KDPRT-PLE (IX2) TO 4006-KDPRT               
513400                    MOVE 4454-KDSS-PLE  (IX2) TO 4006-KDSS-PLE            
513500                    ADD 1   TO 4004-KVRADER (IX2)                         
513600                    MOVE JA TO HF-AK-SW                                   
513700                 END-IF                                                   
513800              END-IF                                                      
513900              MOVE 99 TO IX1                                              
514000           END-IF                                                         
514100           ADD 1 TO IX1                                                   
514200        END-PERFORM                                                       
514300     END-IF                                                               
514400     .                                                                    
514500     EJECT                                                                
514600 S06B-HAMTA-PRINTER-PU SECTION.                                           
514700                                                                          
514800     MOVE 4454-KDSS-PU (LOR-ADLAGOMR)   TO 4010-KDSS-PU                   
514900                                                                          
515000     IF 4002-KDPRT-PU NOT = SPACE AND                                     
515100        NOT BILD-4351-4352                                                
515200        MOVE 4002-KDPRT-PU TO 4010-KDPRT                                  
515300     ELSE                                                                 
515400                                                                          
515500        MOVE NEJ TO HF-AK-SW                                              
515600        IF CDC                                                            
515700           AND                                                            
515800          (OHUV-FLFORBI   = JA OR                                         
515900           OHUV-FLFORBI   = SPEC-FORBI)                                   
516000          PERFORM S06BA-KOLLA-HF-AK-PU                                    
516100        END-IF                                                            
516200                                                                          
516300        IF HF-AK-PLOCK                                                    
516400           CONTINUE                                                       
516500        ELSE                                                              
516600           IF 4454-KDPRT-PU (LOR-ADLAGOMR)  NOT = SPACE                   
516700              MOVE 4454-KDPRT-PU (LOR-ADLAGOMR)  TO 4010-KDPRT            
516800           ELSE                                                           
516900              IF BILD-4351-4352 AND                                       
517000                 4002-KDPRT-PU NOT = SPACE                                
517100                 MOVE 4002-KDPRT-PU          TO 4010-KDPRT                
517200              ELSE                                                        
517300                 MOVE 4454-KDPRTGEN-PU       TO 4010-KDPRT                
517400              END-IF                                                      
517500           END-IF                                                         
517600        END-IF                                                            
517700     END-IF                                                               
517800     .                                                                    
517900     EJECT                                                                
518000 S06BA-KOLLA-HF-AK-PU SECTION.                                            
518100                                                                          
518200     IF WS-HFAK-REF-X10 NOT = SPACE                                       
518300        MOVE 1 TO IX1                                                     
518400        PERFORM UNTIL IX1 > MAX-HFAK-IX                                   
518500           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (IX1)                 
518600           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
518700           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
518800           AND HFAK-BERADREF-1 (IX1) = '#')                               
518900              IF AREG-ADLAGOMR > 0  AND < 100                             
519000                 MOVE AREG-ADLAGOMR  TO IX2                               
519100                 IF 4454-KDPRT-PU       (IX2) NOT = SPACE                 
519200                    MOVE 4454-KDPRT-PU  (IX2) TO 4010-KDPRT               
519300                    MOVE 4454-KDSS-PU   (IX2) TO 4010-KDSS-PU             
519400                    MOVE JA TO HF-AK-SW                                   
519500                 END-IF                                                   
519600              END-IF                                                      
519700              MOVE 99 TO IX1                                              
519800           END-IF                                                         
519900           ADD 1 TO IX1                                                   
520000        END-PERFORM                                                       
520100     END-IF                                                               
520200     .                                                                    
520300     EJECT                                                                
520400 S06C-CREATE-PLE SECTION.                                                 
520500                                                                          
520600     MOVE 'STA S06C-CREATE' TO PGMPOS                                     
520700                                                                          
520800     IF WS-IDDC NOT = W-IDDC-B6                                           
520900        MOVE WS-IDDC TO W-IDDC-B6                                         
521000        PERFORM IMS-GU-WDB601                                             
521100     END-IF                                                               
521200                                                                          
521300     IF OHUV-FLFORBI = SPEC-FORBI  OR                                     
521400        ORAD-IDKAMPRF > 0                                                 
521500        MOVE AREG-ADLAGOMR   TO 4006-ADLAGOMR                             
521600     ELSE                                                                 
521700        MOVE WS-ADLAGOMR     TO 4006-ADLAGOMR                             
521800     END-IF                                                               
521900                                                                          
522000     MOVE WS-ADPLATS         TO 4006-ADPLATS                              
522100                                4006-ADPLATS-ORD                          
522200                                                                          
522300     MOVE ORAD-IDARTNR       TO 4006-IDARTNR                              
522400     MOVE ORAD-IDLOPNR       TO 4006-IDLOPNR                              
522500     MOVE ORAD-IDSYSTEM      TO 4006-IDSYSTEM                             
522600                                                                          
522700     IF OHUV-FLFORBI = SPEC-FORBI    OR                                   
522800        ORAD-IDKAMPRF > 0                                                 
522900        MOVE AREG-ADLAGOMR   TO 4006-ADLAGOMR-ORD                         
523000     ELSE                                                                 
523100        MOVE WS-ADLAGOMR     TO 4006-ADLAGOMR-ORD                         
523200     END-IF                                                               
523300                                                                          
523400     MOVE WS-ADGANG          TO 4006-ADGANG                               
523500                                                                          
523600     MOVE WS-BEART           TO 4006-BEART                                
523700     MOVE OHUV-KDORDTYP-LDC  TO 4006-KDORDTYP-LDC                         
523800     MOVE OHUV-TIREPDAT      TO 4006-TIREPDAT                             
523900     MOVE ORAD-IDKUNDRF-WIP  TO 4006-IDKUNDRF-WIP                         
524000     IF GMT-FLLDCKND = JA                                                 
524100       MOVE OHUV-IDDEPT      tO 4006-IDDEPT                               
524200     ELSE                                                                 
524300       MOVE ZERO             TO 4006-IDDEPT                               
524400     END-IF                                                               
524500*LDC-GB                                                                   
524600     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
524700     IF LDC-GB-3A                                                         
524800       IF DIST34-ENGLAND-SDC AND                                          
524900          GMT-FLLDCKND = JA                                               
525000         IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                         
525100         OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
525200           IF ORAD-BERADREF(1:4) = 'AUTO'                                 
525300             MOVE ORAD-BERADREF TO 4006-BERADREF                          
525400           ELSE                                                           
525500*lk          MOVE ORAD-BERADREF TO 4006-BERADREF                          
525600             MOVE WS-LOPNR-WIPID TO 4006-BERADREF                         
525700           END-IF                                                         
525800         ELSE                                                             
525900           MOVE ORAD-BERADREF TO 4006-BERADREF                            
526000         END-IF                                                           
526100       ELSE                                                               
526200         MOVE ORAD-BERADREF TO 4006-BERADREF                              
526300       END-IF                                                             
526400     ELSE                                                                 
526500       MOVE ORAD-BERADREF   TO 4006-BERADREF                              
526600     END-IF                                                               
526700* ¤-TECKEN MFL STÖR UTSKRIFT PÅ PRINTRAR                                  
526800     INSPECT 4006-BERADREF REPLACING ALL '¤' BY 'O'                       
526900     INSPECT 4006-BERADREF REPLACING ALL 'É' BY 'E'                       
527000     INSPECT 4006-BERADREF REPLACING ALL 'ü' BY 'U'                       
527100     INSPECT 4006-BERADREF REPLACING ALL 'Ü' BY 'U'                       
527200     IF ORAD-IDDC = '41' OR '42' OR '43'                                  
527300       INSPECT 4006-BERADREF REPLACING ALL '#' BY 'N'                     
527400       INSPECT 4006-BERADREF REPLACING ALL 'å' BY 'a'                     
527500       INSPECT 4006-BERADREF REPLACING ALL 'Å' BY 'A'                     
527600       INSPECT 4006-BERADREF REPLACING ALL 'ä' BY 'a'                     
527700       INSPECT 4006-BERADREF REPLACING ALL 'Ä' BY 'A'                     
527800       INSPECT 4006-BERADREF REPLACING ALL 'ö' BY 'o'                     
527900       INSPECT 4006-BERADREF REPLACING ALL 'Ö' BY 'O'                     
528000     END-IF                                                               
528100                                                                          
528200*EX PÅ "REFERENCE MODIFYING".                                             
528300     MOVE 1 TO TECKEN-IX                                                  
528400     PERFORM UNTIL TECKEN-IX > 10                                         
528500       IF 4006-BERADREF (TECKEN-IX:1) < SPACE                             
528600         MOVE SPACE TO 4006-BERADREF (TECKEN-IX:1)                        
528700       END-IF                                                             
528800       ADD   1 TO TECKEN-IX                                               
528900     END-PERFORM                                                          
529000                                                                          
529100     MOVE W-ODEL-DARFS (3:6) TO 4006-TIRFSDAT                             
529200     MOVE GMT-FLLDCKND       TO 4006-FLLDCKND                             
529300     MOVE ORAD-FLAKPLOC      TO 4006-FLAKPLOC                             
529400     MOVE 4002-IDBORD        TO 4006-IDBORD                               
529500     MOVE ORAD-IDGMTREF      TO 4006-IDGMTREF                             
529600     MOVE 4002-IXHEL         TO 4006-IDLOPNR-ORD                          
529700     MOVE 4002-IDLOPNR-PL    TO 4006-IDLOPNR-PL                           
529800     MOVE W-ODEL-IDPLKLST    TO 4006-IDPLKLST                             
529900     MOVE W-ODEL-IDPRC       TO 4006-IDPRC                                
530000     MOVE W-ODEL-IDPRODNR    TO 4006-IDPRODNR                             
530100     MOVE WS-IDRADNR-SISTA   TO 4006-IDRADNR                              
530200     MOVE ORAD-IDSPECEMB     TO 4006-IDSPECEMB                            
530300     MOVE WS-KDARTHNT        TO 4006-KDARTHNT                             
530400     MOVE WS-KDARTURS        TO 4006-KDARTURS                             
530500     MOVE WS-KDEMBAL         TO 4006-KDEMBAL                              
530600     MOVE ORAD-KDFARLIG      TO 4006-KDFARLIG                             
530700     MOVE OHUV-KDORDKL       TO 4006-KDORDKL                              
530800     MOVE AREG-KDSORT        TO 4006-KDSORT                               
530900     MOVE WS-KVAVBART        TO 4006-KVAVBART                             
531000     MOVE WS-GMT-IDZON       TO 4006-IDZON                                
531100     MOVE WS-IDPSN           TO 4006-IDPSN                                
531200     MOVE ARB-KDFRAKT        TO 4006-KDFRAKT                              
531300                                                                          
531400     IF CDC                                                               
531500        AND                                                               
531600       (OHUV-FLFORBI   = JA OR                                            
531700        OHUV-FLFORBI   = SPEC-FORBI)                                      
531800        AND                                                               
531900        WS-HFAK-REF-X10 NOT = SPACE                                       
532000                                                                          
532100        MOVE 1 TO IX1                                                     
532200        PERFORM UNTIL IX1 > MAX-HFAK-IX                                   
532300           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (IX1)                 
532400           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
532500           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
532600           AND HFAK-BERADREF-1 (IX1) = '#')                               
532700              IF AREG-ADLAGOMR > 0  AND < 100                             
532800                MOVE AREG-ADLAGOMR  TO 4006-ADLAGOMR                      
532900                                       4006-ADLAGOMR-ORD                  
533000                MOVE AREG-ADGANG    TO 4006-ADGANG                        
533100                MOVE AREG-ADPLATS   TO 4006-ADPLATS                       
533200                                       4006-ADPLATS-ORD                   
533300              END-IF                                                      
533400              MOVE 99 TO IX1                                              
533500           END-IF                                                         
533600           ADD 1 TO IX1                                                   
533700        END-PERFORM                                                       
533800     END-IF                                                               
533900                                                                          
534000     PERFORM IMS-ISRT-4003-WL400321                                       
534100                                                                          
534200     PERFORM UNTIL SEGMENT-FINNS                                          
534300        ADD 1 TO 4006-IDLOPNR                                             
534400        PERFORM IMS-ISRT-4003-WL400321                                    
534500     END-PERFORM                                                          
534600     .                                                                    
534700     EJECT                                                                
534800 S06D-CREATE-PU SECTION.                                                  
534900                                                                          
535000     MOVE 'STA S06D-CREATE' TO PGMPOS                                     
535100     MOVE OHUV-IDORDER           TO 4010-IDORDER                          
535200                                                                          
535300     IF WS-IDDC NOT = W-IDDC-B6                                           
535400        MOVE WS-IDDC TO W-IDDC-B6                                         
535500        PERFORM IMS-GU-WDB601                                             
535600     END-IF                                                               
535700                                                                          
535800     IF OHUV-FLFORBI = SPEC-FORBI   OR                                    
535900        ORAD-IDKAMPRF > 0                                                 
536000        MOVE AREG-ADLAGOMR       TO 4010-ADLAGOMR                         
536100     ELSE                                                                 
536200        MOVE WS-ADLAGOMR         TO 4010-ADLAGOMR                         
536300     END-IF                                                               
536400                                                                          
536500     MOVE WS-ADPLATS             TO 4010-ADPLATS                          
536600                                    4010-ADPLATS-ORD                      
536700                                                                          
536800     MOVE ORAD-IDARTNR           TO 4010-IDARTNR                          
536900     MOVE ORAD-IDLOPNR           TO 4010-IDLOPNR                          
537000                                                                          
537100     IF OHUV-FLFORBI = SPEC-FORBI    OR                                   
537200        ORAD-IDKAMPRF > 0                                                 
537300        MOVE AREG-ADLAGOMR       TO 4010-ADLAGOMR-ORD                     
537400     ELSE                                                                 
537500        MOVE WS-ADLAGOMR         TO 4010-ADLAGOMR-ORD                     
537600     END-IF                                                               
537700                                                                          
537800     MOVE WS-ADGANG              TO 4010-ADGANG                           
537900                                                                          
538000     MOVE WS-BEART               TO 4010-BEART                            
538100     MOVE ORAD-FLAKPLOC          TO 4010-FLAKPLOC                         
538200     MOVE ORAD-FLSDCLEV          TO 4010-FLSDCLEV                         
538300     MOVE 4002-IDBORD            TO 4010-IDBORD                           
538400     MOVE ORAD-IDKUNDRF          TO 4010-IDKUNDRF                         
538500     MOVE ORAD-IDKUNDRF-RO       TO 4010-IDKUNDRF-RO                      
538600     MOVE 4002-IXHEL             TO 4010-IDLOPNR-ORD                      
538700     MOVE 4002-IDLOPNR-PL        TO 4010-IDLOPNR-PL                       
538800     MOVE W-ODEL-IDPRC           TO 4010-IDPRC                            
538900     MOVE W-ODEL-IDPRODNR        TO 4010-IDPRODNR                         
539000     MOVE WS-IDRADNR-SISTA       TO 4010-IDPURAD                          
539100     MOVE ORAD-IDSPECEMB         TO 4010-IDSPECEMB                        
539200     MOVE 4002-IDUSER            TO 4010-IDUSER                           
539300     MOVE WS-KDARTURS            TO 4010-KDARTURS                         
539400     MOVE ORAD-IDDC              TO 4010-IDDC                             
539500     MOVE ORAD-IDDC-RO           TO 4010-IDDC-RO                          
539600     MOVE W-ODEL-KDFDKRAV        TO 4010-KDFDKRAV                         
539700     MOVE WS-KVAVBART            TO 4010-KVAVBART                         
539800     MOVE ORAD-KVBEART-Q         TO 4010-KVBEART-Q                        
539900     MOVE 0                      TO 4010-KVHANTTI                         
540000     MOVE ORAD-REKSIFFR          TO 4010-REKSIFFR                         
540100     MOVE W-ODEL-DALSTORD (3:10) TO 4010-TILST                            
540200     MOVE W-ODEL-TIREGDAT        TO 4010-TIREGDAT                         
540300     MOVE W-ODEL-TIREGTID        TO 4010-TIREGTID                         
540400     MOVE W-ODEL-DARFS (3:10)    TO 4010-TIRFS                            
540500     MOVE W-ODEL-DAUTSKR (3:6)   TO 4010-TIUTSKR                          
540600     MOVE W-ODEL-TIUTSTID        TO 4010-TIUTSTID                         
540700     MOVE OHUV-KDORDTYP-LDC      TO 4010-KDORDTYP-LDC                     
540800     MOVE OHUV-TIREPDAT          TO 4010-TIREPDAT                         
540900     MOVE ORAD-IDKUNDRF-WIP      TO 4010-IDKUNDRF-WIP                     
541000     MOVE ORAD-PRAVCOST          TO 4010-PRAVCOST                         
541100                                                                          
541200     IF WS-KVAVBART NOT = ORAD-KVBEART-Q                                  
541300        COMPUTE WS-ORDERVIKT-PER-RAD =                                    
541400               ((WS-KVAVBART * ORAD-VKART) / 1000)                        
541500        COMPUTE WS-ORDERVOLYM-PER-RAD =                                   
541600               ((WS-KVAVBART * ORAD-VLARTNTO) / 1000000)                  
541700     END-IF                                                               
541800                                                                          
541900     MOVE WS-ORDERVIKT-PER-RAD  TO 4010-VKORDNTO                          
542000     MOVE WS-ORDERVOLYM-PER-RAD TO 4010-VLORDNTO                          
542100*LDC-GB                                                                   
542200     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
542300     IF NOT DCS-CDC                                                       
542400       IF DIST34-ENGLAND-SDC AND                                          
542500          GMT-FLLDCKND = JA                                               
542600         IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                         
542700         OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
542800           IF ORAD-BERADREF(1:4) = 'AUTO'                                 
542900             MOVE ORAD-BERADREF TO 4010-BERADREF                          
543000           ELSE                                                           
543100*lk          MOVE ORAD-BERADREF TO 4010-BERADREF                          
543200             MOVE WS-LOPNR-WIPID TO 4010-BERADREF                         
543300           END-IF                                                         
543400         ELSE                                                             
543500           MOVE ORAD-BERADREF   TO 4010-BERADREF                          
543600         END-IF                                                           
543700       ELSE                                                               
543800         MOVE ORAD-BERADREF     TO 4010-BERADREF                          
543900       END-IF                                                             
544000     ELSE                                                                 
544100       MOVE ORAD-BERADREF       TO 4010-BERADREF                          
544200     END-IF                                                               
544300* ¤-TECKEN MFL STÖR UTSKRIFT PÅ PRINTRAR                                  
544400     INSPECT 4010-BERADREF REPLACING ALL '¤' BY 'O'                       
544500     INSPECT 4010-BERADREF REPLACING ALL 'É' BY 'E'                       
544600     INSPECT 4010-BERADREF REPLACING ALL 'ü' BY 'U'                       
544700     INSPECT 4010-BERADREF REPLACING ALL 'Ü' BY 'U'                       
544800     IF ORAD-IDDC = '41' OR '42' OR '43'                                  
544900       INSPECT 4010-BERADREF REPLACING ALL '#' BY 'N'                     
545000       INSPECT 4010-BERADREF REPLACING ALL 'å' BY 'a'                     
545100       INSPECT 4010-BERADREF REPLACING ALL 'Å' BY 'A'                     
545200       INSPECT 4010-BERADREF REPLACING ALL 'ä' BY 'a'                     
545300       INSPECT 4010-BERADREF REPLACING ALL 'Ä' BY 'A'                     
545400       INSPECT 4010-BERADREF REPLACING ALL 'ö' BY 'o'                     
545500       INSPECT 4010-BERADREF REPLACING ALL 'Ö' BY 'O'                     
545600     END-IF                                                               
545700                                                                          
545800     INSPECT 4010-BEVOLREF REPLACING ALL 'É' BY 'E'                       
545900     INSPECT 4010-BEVOLREF REPLACING ALL 'ü' BY 'U'                       
546000     INSPECT 4010-BEVOLREF REPLACING ALL 'Ü' BY 'U'                       
546100     INSPECT 4010-BEVOLREF REPLACING ALL '¤' BY 'O'                       
546200     IF ORAD-IDDC = '41' OR '42' OR '43'                                  
546300       INSPECT 4010-BEVOLREF REPLACING ALL '#' BY 'N'                     
546400       INSPECT 4010-BEVOLREF REPLACING ALL 'å' BY 'a'                     
546500       INSPECT 4010-BEVOLREF REPLACING ALL 'Å' BY 'A'                     
546600       INSPECT 4010-BEVOLREF REPLACING ALL 'ä' BY 'a'                     
546700       INSPECT 4010-BEVOLREF REPLACING ALL 'Ä' BY 'A'                     
546800       INSPECT 4010-BEVOLREF REPLACING ALL 'ö' BY 'o'                     
546900       INSPECT 4010-BERADREF REPLACING ALL 'Ö' BY 'O'                     
547000     END-IF                                                               
547100                                                                          
547200*EX PÅ "REFERENCE MODIFYING".                                             
547300     MOVE 1 TO TECKEN-IX                                                  
547400     PERFORM UNTIL TECKEN-IX > 10                                         
547500       IF 4010-BERADREF (TECKEN-IX:1) < SPACE                             
547600         MOVE SPACE TO 4010-BERADREF (TECKEN-IX:1)                        
547700       END-IF                                                             
547800       ADD   1 TO TECKEN-IX                                               
547900     END-PERFORM                                                          
548000                                                                          
548100     MOVE ORAD-BEVOLREF         TO 4010-BEVOLREF                          
548200     MOVE ORAD-FLINVEST         TO 4010-FLINVEST                          
548300     MOVE ORAD-FLPRTILL         TO 4010-FLPRTILL                          
548400     MOVE ORAD-FLTILLK          TO 4010-FLTILLK                           
548500     MOVE ORAD-IDLEVNR          TO 4010-IDLEVNR                           
548600     MOVE W-ODEL-IDPLKLST       TO 4010-IDPLKLST                          
548700     MOVE ORAD-IDLOPNR-RO       TO 4010-IDLOPNR-RO                        
548800     MOVE ORAD-KDDSP            TO 4010-KDDSP                             
548900     MOVE ORAD-KDFARLIG         TO 4010-KDFARLIG                          
549000     MOVE ORAD-KDKVBRYT         TO 4010-KDKVBRYT                          
549100     MOVE ORAD-KDPRODSL         TO 4010-KDPRODSL                          
549200     MOVE ORAD-KDPRTYP          TO 4010-KDPRTYP                           
549300     MOVE ORAD-KDORDING         TO 4010-KDORDING                          
549400     MOVE ORAD-KDOI             TO 4010-KDOI                              
549500     MOVE ORAD-CLEARGROUP       TO 4010-CLEARGROUP                        
549600     MOVE ORAD-KDORDKL          TO 4010-KDORDKL                           
549700     MOVE ORAD-KDVRINFO         TO 4010-KDVRINFO                          
549800     MOVE ORAD-KDVALISO         TO 4010-KDVALISO                          
549900*KDVALISO GLOBAL EXPORT TESTING                                           
550000     MOVE ORAD-KVSLATT          TO 4010-KVSLATT                           
550100     MOVE ORAD-PRARTNTO         TO 4010-PRARTNTO                          
550200     MOVE ORAD-DEAL-PR-LINE     TO 4010-DEAL-PR-LINE                      
550300     MOVE ORAD-TIPRIS           TO 4010-TIPRIS                            
550400     MOVE ORAD-TIRODAT          TO 4010-TIRODAT                           
550500     MOVE ORAD-VKART            TO 4010-VKART                             
550600     MOVE ORAD-VKART-NTO        TO 4010-VKART-NTO                         
550700     MOVE ORAD-VLARTNTO         TO 4010-VLARTNTO                          
550800     MOVE ORAD-FLRESTN          TO 4010-FLRESTN                           
550900     MOVE ORAD-IDKAMPRF         TO 4010-IDKAMPRF                          
551000     MOVE ORAD-IDSYSTEM         TO 4010-IDSYSTEM                          
551100     MOVE ORAD-IDBIL            TO 4010-IDBIL                             
551200     MOVE ORAD-IDKLIENT         TO 4010-IDKLIENT                          
551300     MOVE ORAD-IDARBREF         TO 4010-IDARBREF                          
551400     MOVE ORAD-IDVIN            TO 4010-IDVIN                             
551500     MOVE OHUV-IDANALYS         TO 4010-IDANALYS                          
551600                                                                          
551700     MOVE WS-IDPSN              TO 4010-IDPSN                             
551800     MOVE WS-GMT-IDZON          TO 4010-IDZON                             
551900     MOVE WS-GMT-FLCOD          TO 4010-FLCOD                             
552000                                                                          
552100     PERFORM S06DA-HAEMTA-DATA-WDD5                                       
552200     IF SEGMENT-FINNS                                                     
552300       MOVE ART-VKART-FG        TO 4010-VKART-FG                          
552400       MOVE ART-SUEQFG          TO 4010-SUEQFG                            
552500       MOVE ART-VLFG            TO 4010-VLFG                              
552600     ELSE                                                                 
552700       MOVE ZERO                TO 4010-VKART-FG                          
552800                                   4010-SUEQFG                            
552900                                   4010-VLFG                              
553000     END-IF                                                               
553100                                                                          
553200     IF CDC                                                               
553300        AND                                                               
553400       (OHUV-FLFORBI   = JA OR                                            
553500        OHUV-FLFORBI   = SPEC-FORBI)                                      
553600        AND                                                               
553700        WS-HFAK-REF-X10 NOT = SPACE                                       
553800                                                                          
553900        MOVE 1 TO IX1                                                     
554000        PERFORM UNTIL IX1 > MAX-HFAK-IX                                   
554100           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (IX1)                 
554200           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
554300           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
554400           AND HFAK-BERADREF-1 (IX1) = '#')                               
554500              IF AREG-ADLAGOMR > 0  AND < 100                             
554600                MOVE AREG-ADLAGOMR  TO 4010-ADLAGOMR                      
554700                                       4010-ADLAGOMR-ORD                  
554800                MOVE AREG-ADGANG    TO 4010-ADGANG                        
554900                MOVE AREG-ADPLATS   TO 4010-ADPLATS                       
555000                                       4010-ADPLATS-ORD                   
555100              END-IF                                                      
555200              MOVE 99 TO IX1                                              
555300           END-IF                                                         
555400           ADD 1 TO IX1                                                   
555500        END-PERFORM                                                       
555600     END-IF                                                               
555700                                                                          
555800     PERFORM S06DB-JUSTERA-KONTO                                          
555900                                                                          
556000     PERFORM IMS-ISRT-4007-WL400721                                       
556100                                                                          
556200     PERFORM UNTIL SEGMENT-FINNS                                          
556300        ADD 1 TO 4010-IDLOPNR                                             
556400        PERFORM IMS-ISRT-4007-WL400721                                    
556500     END-PERFORM                                                          
556600     .                                                                    
556700     EJECT                                                                
556800 S06DA-HAEMTA-DATA-WDD5 SECTION.                                          
556900                                                                          
557000     MOVE ORAD-IDARTNR TO W-ART-IDARTNR                                   
557100     PERFORM IMS-GU-ARTN-WLARTN01                                         
557200     .                                                                    
557300     EJECT                                                                
557400 S06DB-JUSTERA-KONTO SECTION.                                             
557500                                                                          
557600     MOVE OHUV-IDKONTO      TO WS-IDKONTO                                 
557700     MOVE OHUV-IDANALYS     TO 4010-IDANALYS                              
557800     MOVE OHUV-IDKST        TO 4010-IDKST                                 
557900     MOVE WS-IDKONTO        TO 4010-IDKONTO                               
558000     .                                                                    
558100     EJECT                                                                
558200 S06AA-HAMTA-KDARTURS  SECTION.                                           
558300                                                                          
558400     IF CDC                                                               
558500       MOVE AREG-KDARTURS            TO WS-KDARTURS                       
558600       MOVE AREG-IDPSN               TO WS-IDPSN                          
558700     ELSE                                                                 
558800       MOVE ORAD-IDARTNR             TO W-IDARTNR                         
558900       MOVE ORAD-IDDC                TO W-IDDC                            
559000* 060914 FIX FÖR ATT KLARA TF-ORDRAR NÄR ART. SAKNAS PÅ WDK711.           
559100       IF OHUV-FLOVRLEV = JA                                              
559200         PERFORM IMS-GU-WDK711-GE                                         
559300         IF SEGMENT-SAKNAS                                                
559400           MOVE AREG-KDARTURS        TO WS-KDARTURS                       
559500           IF NDC-NA                                                      
559600             MOVE ZERO               TO WS-IDPSN                          
559700           ELSE                                                           
559800             MOVE AREG-IDPSN         TO WS-IDPSN                          
559900           END-IF                                                         
560000         ELSE                                                             
560100           IF (SLAG-IDLEVNR = '1441 ' OR                                  
560200               SLAG-IDLEVNR = 'BP2TW')                                    
560300             MOVE AREG-KDARTURS      TO WS-KDARTURS                       
560400           END-IF                                                         
560500                                                                          
560600           IF ORAD-IDDC NOT = DCS-IDDC                                    
560700             MOVE ORAD-IDDC          TO W-IDDC-B6                         
560800             PERFORM IMS-GU-WDB601                                        
560900           END-IF                                                         
561000           MOVE DCS-IDLANDX2         TO W-IDLAND                          
561100           IF DCS-NDC                                                     
561200             PERFORM IMS-GU-WDK712                                        
561300             IF SEGMENT-FINNS                                             
561400               IF (SLAG-IDLEVNR = '1441 ' OR                              
561500                   SLAG-IDLEVNR = 'BP2TW')                                
561600                 MOVE AREG-KDARTURS    TO WS-KDARTURS                     
561700               ELSE                                                       
561800                 IF SLAG-IDLEVNR NOT = SPACE AND                          
561900                    LART-KDARTURS > SPACE                                 
562000                   MOVE LART-KDARTURS  TO WS-KDARTURS                     
562100                 ELSE                                                     
562200                    MOVE AREG-KDARTURS TO WS-KDARTURS                     
562300                 END-IF                                                   
562400               END-IF                                                     
562500               IF LART-IDPSN-DC > ZERO                                    
562600                 MOVE LART-IDPSN-DC  TO WS-IDPSN                          
562700               ELSE                                                       
562800                 IF NDC-NA                                                
562900                   MOVE ZERO         TO WS-IDPSN                          
563000                 ELSE                                                     
563100                   MOVE AREG-IDPSN   TO WS-IDPSN                          
563200                 END-IF                                                   
563300               END-IF                                                     
563400             ELSE                                                         
563500               MOVE AREG-KDARTURS    TO WS-KDARTURS                       
563600               IF NDC-NA                                                  
563700                 MOVE ZERO           TO WS-IDPSN                          
563800               ELSE                                                       
563900                 MOVE AREG-IDPSN     TO WS-IDPSN                          
564000               END-IF                                                     
564100             END-IF                                                       
564200           ELSE                                                           
564300             MOVE AREG-KDARTURS      TO WS-KDARTURS                       
564400             MOVE AREG-IDPSN         TO WS-IDPSN                          
564500           END-IF                                                         
564600         END-IF                                                           
564700*FIX-SLUT                                                                 
564800       ELSE                                                               
564900         PERFORM IMS-GU-WDK711                                            
565000         IF (SLAG-IDLEVNR = '1441 ' OR                                    
565100             SLAG-IDLEVNR = 'BP2TW')                                      
565200           MOVE AREG-KDARTURS        TO WS-KDARTURS                       
565300         END-IF                                                           
565400                                                                          
565500         IF ORAD-IDDC NOT = DCS-IDDC                                      
565600            MOVE ORAD-IDDC           TO W-IDDC-B6                         
565700            PERFORM IMS-GU-WDB601                                         
565800         END-IF                                                           
565900         MOVE DCS-IDLANDX2           TO W-IDLAND                          
566000         IF DCS-NDC                                                       
566100           PERFORM IMS-GU-WDK712                                          
566200           IF SEGMENT-FINNS                                               
566300             IF (SLAG-IDLEVNR = '1441 ' OR                                
566400                 SLAG-IDLEVNR = 'BP2TW')                                  
566500               MOVE AREG-KDARTURS    TO WS-KDARTURS                       
566600             ELSE                                                         
566700               IF SLAG-IDLEVNR NOT = SPACE AND                            
566800                  LART-KDARTURS > SPACE                                   
566900                 MOVE LART-KDARTURS  TO WS-KDARTURS                       
567000               ELSE                                                       
567100                 MOVE AREG-KDARTURS  TO WS-KDARTURS                       
567200               END-IF                                                     
567300             END-IF                                                       
567400             IF LART-IDPSN-DC > 0                                         
567500               MOVE LART-IDPSN-DC    TO WS-IDPSN                          
567600             ELSE                                                         
567700               IF NDC-NA                                                  
567800                 MOVE ZERO           TO WS-IDPSN                          
567900               ELSE                                                       
568000                 MOVE AREG-IDPSN     TO WS-IDPSN                          
568100               END-IF                                                     
568200             END-IF                                                       
568300           ELSE                                                           
568400             MOVE AREG-KDARTURS      TO WS-KDARTURS                       
568500             IF NDC-NA                                                    
568600               MOVE ZERO             TO WS-IDPSN                          
568700             ELSE                                                         
568800               MOVE AREG-IDPSN       TO WS-IDPSN                          
568900             END-IF                                                       
569000           END-IF                                                         
569100         ELSE                                                             
569200           MOVE AREG-KDARTURS        TO WS-KDARTURS                       
569300           MOVE AREG-IDPSN           TO WS-IDPSN                          
569400         END-IF                                                           
569500       END-IF                                                             
569600     END-IF                                                               
569700     .                                                                    
569800     SKIP2                                                                
569900 S06Z-KOLLA-AENDRA-LAGOMR SECTION.                                        
570000                                                                          
570100     MOVE 'STA S06Z-KOLLA'  TO PGMPOS                                     
570200     MOVE NEJ                      TO SW-AENDRA-LAGOMR                    
570300     IF  CLAG-ADLAGOMR = ORAD-ADLAGOMR                                    
570400     AND CLAG-ADGANG   = ORAD-ADGANG                                      
570500     AND CLAG-ADPLATS  = ORAD-ADPLATS                                     
570600     OR BIPACKNING                                                        
570700*......INGEN FÖRÄNDRING AV LAGOMR                                         
570800       CONTINUE                                                           
570900     ELSE                                                                 
571000                                                                          
571100       PERFORM S06ZA-ANROP-W411LAST                                       
571200                                                                          
571300       IF  LAST-ADLAGOMR-UT = +000                                        
571400       AND LAST-KVANTAL-UT  = +0000000                                    
571500       AND LAST-KVBEART-UT  = +0000000                                    
571600                                                                          
571700         PERFORM S06ZB-ANROP-W413ADRS                                     
571800                                                                          
571900         IF (ADRS-ADLAGOMR-UT = +000                                      
572000         AND ADRS-ADPLATS-UT  = +00000)                                   
572100           CONTINUE                                                       
572200         ELSE                                                             
572300           IF (ADRS-ADLAGOMR-UT = ORAD-ADLAGOMR                           
572400           AND ADRS-ADPLATS-UT  = ORAD-ADPLATS)                           
572500*............orderrad gäller.                                             
572600             CONTINUE                                                     
572700           ELSE                                                           
572800*............LAGOMR I CLAG- GÄLLER!                                       
572900               MOVE JA             TO SW-AENDRA-LAGOMR                    
573000           END-IF                                                         
573100*..........LAGOMR JUSTERAD AV W413ADRS. ÄNDRA INTE.                       
573200         END-IF                                                           
573300       ELSE                                                               
573400         CONTINUE                                                         
573500*........LAGOMR JUSTERAD AV W411LAST. ÄNDRA INTE.                         
573600       END-IF                                                             
573700     END-IF                                                               
573800     .                                                                    
573900     SKIP2                                                                
574000 S06ZA-ANROP-W411LAST SECTION.                                            
574100                                                                          
574200     MOVE 'STA S06ZA-ANROP'  TO PGMPOS                                    
574300     MOVE ORAD-ADLAGOMR            TO LAST-ADLAGOMR                       
574400     MOVE OHUV-FLFORBI             TO LAST-FLFORBI                        
574500     MOVE OHUV-FLOVRLEV            TO LAST-FLOVRLEV                       
574600     MOVE OHUV-FLORDSPE            TO LAST-FLORDSPE                       
574700     MOVE ORAD-IDLEVNR             TO LAST-IDLEVNR                        
574800     MOVE ORAD-IDDC                TO LAST-IDDC                           
574900     MOVE ARB-KDFDKRAV             TO LAST-KDFDKRAV                       
575000     MOVE ORAD-KVPREAVB            TO LAST-KVPREAVB                       
575100     MOVE CLAG-KVQPACK-3           TO LAST-KVQPACK-3                      
575200     MOVE CLAG-KVQPACK-4           TO LAST-KVQPACK-4                      
575300                                                                          
575400     CALL W411LAST              USING LAST-W411LAST                       
575500     .                                                                    
575600     SKIP2                                                                
575700 S06ZB-ANROP-W413ADRS SECTION.                                            
575800                                                                          
575900     MOVE 'STA S06ZB-ANROP'  TO PGMPOS                                    
576000     MOVE ORAD-ADLAGOMR            TO ADRS-ADLAGOMR-IN                    
576100     MOVE ORAD-ADPLATS             TO ADRS-ADPLATS-IN                     
576200     MOVE OHUV-BEVARREF            TO ADRS-BEVARREF-IN                    
576300     MOVE OHUV-FLFORBI             TO ADRS-FLFORBI-IN                     
576400     MOVE OHUV-IDDISTR             TO ADRS-IDDISTR-IN                     
576500     MOVE 1                        TO ADRS-KDCALL-IN                      
576600     MOVE ORAD-IDDC                TO ADRS-IDDC-IN                        
576700     MOVE OHUV-KDORDKL             TO ADRS-KDORDKL-IN                     
576800     MOVE ORAD-KVBEART-Q           TO ADRS-KVBEART-Q-IN                   
576900     MOVE ORAD-VLARTNTO            TO ADRS-VLARTNTO-IN                    
577000                                                                          
577100     CALL W413ADRS              USING ADRS-W413ADRS                       
577200     .                                                                    
577300     EJECT                                                                
577400 S07-BORTTAG-ORDERRAD SECTION.                                            
577500                                                                          
577600     IF ARTIKEL-OK                                                        
577700        PERFORM IMS-DLET-ORQF-WLORQF01                                    
577800     END-IF                                                               
577900     .                                                                    
578000     EJECT                                                                
578100 S08-SKAPA-RYETRANS SECTION.                                              
578200                                                                          
578300     MOVE 'STA S08-SKAPA'  TO PGMPOS                                      
578400     PERFORM S08A-SKAPA-RYEPOST                                           
578500                                                                          
578600     PERFORM S08B-SKAPA-SORTPOST                                          
578700                                                                          
578800     MOVE W-RYEPOST  TO LOGGPOST                                          
578900     MOVE W-SORTPOST TO SORTPOST                                          
579000     MOVE WS-DATUM   TO TIAAMMDD                                          
579100     ACCEPT TIKLOCK FROM TIME                                             
579200     MOVE 1          TO IDLOGLOP                                          
579300                                                                          
579400     PERFORM IMS-ISRT-ZZAC-WLZZAC01                                       
579500                                                                          
579600     PERFORM UNTIL SEGMENT-FINNS                                          
579700        IF IDLOGLOP = 9                                                   
579800           ACCEPT TIKLOCK FROM TIME                                       
579900           MOVE 0 TO IDLOGLOP                                             
580000        END-IF                                                            
580100        ADD 1 TO IDLOGLOP                                                 
580200        PERFORM IMS-ISRT-ZZAC-WLZZAC01                                    
580300     END-PERFORM                                                          
580400     .                                                                    
580500     EJECT                                                                
580600 S08A-SKAPA-RYEPOST SECTION.                                              
580700                                                                          
580800     MOVE 'RYE'              TO W-RYE-IDPTYP                              
580900     MOVE ORAD-BERADREF      TO W-RYE-BERADREF                            
581000     MOVE ORAD-BEVOLREF      TO W-RYE-BEVOLREF                            
581100                                                                          
581200     IF ORAD-IDLEVNR NOT = SPACE                                          
581300        MOVE JA              TO W-RYE-FLDIRLEV                            
581400     ELSE                                                                 
581500        MOVE NEJ             TO W-RYE-FLDIRLEV                            
581600     END-IF                                                               
581700                                                                          
581800     MOVE OHUV-FLLSBOK       TO W-RYE-FLLSBOK                             
581900     MOVE OHUV-FLORDSPE      TO W-RYE-FLORDSPE                            
582000     MOVE ORAD-FLTILLK       TO W-RYE-FLTILLK                             
582100     MOVE ORAD-IDARTNR       TO W-RYE-IDARTNR                             
582200     MOVE ORAD-IDKUNDRF      TO W-RYE-IDKUNDRF                            
582300     MOVE ORAD-IDKUNDRF-RO TO W-RYE-IDKUNDRF-RO                           
582400     MOVE ORAD-IDDC          TO W-RYE-IDDC                                
582500     MOVE ORAD-KDDSP         TO W-RYE-KDDSP                               
582600     MOVE OHUV-KDFAKTYP      TO W-RYE-KDFAKTYP                            
582700     MOVE ORAD-KDKVBRYT      TO W-RYE-KDKVBRYT                            
582800     MOVE WS-KDORDBEK        TO W-RYE-KDORDBEK                            
582900     MOVE ORAD-KDORDING      TO W-RYE-KDORDING                            
583000     MOVE AREG-KDPRODSL      TO W-RYE-KDPRODSL                            
583100     MOVE ORAD-KDVRINFO      TO W-RYE-KDVRINFO                            
583200                                                                          
583300     IF ORAD-KDTPOTYP NOT = 1                                             
583400        MOVE 0               TO W-RYE-KDVRTPO                             
583500     ELSE                                                                 
583600        IF ORAD-KDTPOTYP = 1                                              
583700           IF ORAD-IDSYSTEM = 'VR'                                        
583800              MOVE 1         TO W-RYE-KDVRTPO                             
583900           ELSE                                                           
584000              MOVE 2         TO W-RYE-KDVRTPO                             
584100           END-IF                                                         
584200        END-IF                                                            
584300     END-IF                                                               
584400                                                                          
584500     MOVE WS-KVLS            TO W-RYE-KVAVBART                            
584600     MOVE ORAD-KVBEART-Q     TO W-RYE-KVBEART-Q                           
584700                                                                          
584800     IF WS-KDORDBEK = 80                                                  
584900        MOVE ORAD-KVBEART-Q  TO W-RYE-KVRO                                
585000     ELSE                                                                 
585100       IF WS-KDORDBEK = 92                                                
585200          COMPUTE W-RYE-KVRO = ORAD-KVBEART-Q - WS-KVAVBART               
585300       ELSE                                                               
585400          MOVE WS-KVROS        TO W-RYE-KVRO                              
585500       END-IF                                                             
585600     END-IF                                                               
585700                                                                          
585800     MOVE ORAD-REKSIFFR      TO W-RYE-REKSIFFR                            
585900     MOVE AREG-TIDISPIN      TO W-RYE-TIDISPIN                            
586000     MOVE ORAD-TIREGDAT      TO W-RYE-TIORDREG                            
586100     MOVE ORAD-TIRODAT       TO W-RYE-TIRODAT                             
586200     .                                                                    
586300     EJECT                                                                
586400 S08B-SKAPA-SORTPOST SECTION.                                             
586500                                                                          
586600     MOVE ORAD-IDDISTR        TO W-RYES-IDDISTR                           
586700     MOVE ORAD-IDKUNDNR       TO W-RYES-IDKUNDNR                          
586800     IF  OHUV-FLVORKO = JA                                                
586900     OR  OHUV-FLVORKO = YES                                               
587000         MOVE JA              TO W-RYES-FLVORKO                           
587100     ELSE                                                                 
587200         MOVE OHUV-FLVORKO    TO W-RYES-FLVORKO                           
587300     END-IF                                                               
587400     MOVE OHUV-FLFORBI        TO W-RYES-FLFORBI                           
587500     MOVE OHUV-FLOVRLEV       TO W-RYES-FLOVRLEV                          
587600     MOVE AREG-KDERS          TO W-RYES-KDERS                             
587700     MOVE ARB-KDFRAKT         TO W-RYES-KDFRAKT                           
587800     MOVE ORAD-KDORDKL        TO W-RYES-KDORDKL                           
587900     MOVE ORAD-KDTPOTYP       TO W-RYES-KDTPOTYP                          
588000     MOVE ORAD-KVBEART        TO W-RYES-KVBEART                           
588100                                                                          
588200     MOVE ORAD-KVSLATT        TO W-RYES-KVSLATT                           
588300     MOVE AREG-KVQPACK-1      TO W-RYES-KVQPACK-1                         
588400     .                                                                    
588500     EJECT                                                                
588600 S20-FINN-INTERVALL  SECTION.                                             
588700                                                                          
588800     MOVE 'STA S20-FINN-'  TO PGMPOS                                      
588900     PERFORM IMS-GU-WDM211                                                
589000     IF SEGMENT-FINNS                                                     
589100       PERFORM IMS-GNP-WDM221                                             
589200       PERFORM UNTIL SEGMENT-SAKNAS                                       
589300         IF  ORAD-IDDISTR > KMRK-IDDISTR-TOM                              
589400         OR  ORAD-IDDISTR < KMRK-IDDISTR-FOM                              
589500           CONTINUE                                                       
589600         ELSE                                                             
589700           IF  ORAD-IDDISTR  = KMRK-IDDISTR-TOM                           
589800           AND ORAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
589900             CONTINUE                                                     
590000           ELSE                                                           
590100             IF  ORAD-IDDISTR  = KMRK-IDDISTR-FOM                         
590200             AND ORAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
590300               CONTINUE                                                   
590400             ELSE                                                         
590500               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
590600               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
590700               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
590800               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
590900             END-IF                                                       
591000           END-IF                                                         
591100         END-IF                                                           
591200         PERFORM IMS-GNP-WDM221                                           
591300       END-PERFORM                                                        
591400     END-IF                                                               
591500     .                                                                    
591600     EJECT                                                                
591700                                                                          
591800 S98-SKAPA-RYXTRANS SECTION.                                              
591900                                                                          
592000     PERFORM S98A-SKAPA-RYXPOST                                           
592100                                                                          
592200     PERFORM S98B-SKAPA-SORTPOST                                          
592300                                                                          
592400     MOVE W-RYXPOST      TO LOGGPOST                                      
592500     MOVE W-RYX-SORTPOST TO SORTPOST                                      
592600                                                                          
592700     PERFORM IMS-ISRT-ZZAC-WLZZAC01                                       
592800     .                                                                    
592900     EJECT                                                                
593000 S99-SKAPA-RYXTRANS SECTION.                                              
593100                                                                          
593200     PERFORM S98A-SKAPA-RYXPOST                                           
593300                                                                          
593400     PERFORM S98B-SKAPA-SORTPOST                                          
593500                                                                          
593600     MOVE W-RYXPOST      TO LOGGPOST                                      
593700     MOVE W-RYX-SORTPOST TO SORTPOST                                      
593800     MOVE WS-DATUM   TO TIAAMMDD                                          
593900     ACCEPT TIKLOCK FROM TIME                                             
594000     MOVE 1          TO IDLOGLOP                                          
594100                                                                          
594200     PERFORM IMS-ISRT-ZZAC-WLZZAC01                                       
594300                                                                          
594400     PERFORM UNTIL SEGMENT-FINNS                                          
594500        IF IDLOGLOP = 9                                                   
594600           ACCEPT TIKLOCK FROM TIME                                       
594700           MOVE 0 TO IDLOGLOP                                             
594800        END-IF                                                            
594900        ADD 1 TO IDLOGLOP                                                 
595000        PERFORM IMS-ISRT-ZZAC-WLZZAC01                                    
595100     END-PERFORM                                                          
595200     .                                                                    
595300     EJECT                                                                
595400 S98A-SKAPA-RYXPOST SECTION.                                              
595500                                                                          
595600     MOVE 'RYX'              TO W-RYX-IDPTYP                              
595700     MOVE DEAV-IDKAMPRF-IN   TO W-RYX-IDKAMPRF-IN                         
595800     MOVE DEAV-IDARTNR-IN    TO W-RYX-IDARTNR-IN                          
595900     MOVE DEAV-TIRODAT-IN    TO W-RYX-TIRODAT-IN                          
596000     MOVE DEAV-ADLAGOMR-IN   TO W-RYX-ADLAGOMR-IN                         
596100     MOVE DEAV-KDORDKL-IN    TO W-RYX-KDORDKL-IN                          
596200     MOVE DEAV-IDDC-RO-IN    TO W-RYX-IDDC-RO-IN                          
596300     MOVE DEAV-IDDISTR-IN    TO W-RYX-IDDISTR-IN                          
596400     MOVE DEAV-KVAKS-CDC-IN  TO W-RYX-KVAKS-CDC-IN                        
596500     MOVE DEAV-KVAKS-PAV-IN  TO W-RYX-KVAKS-PAV-IN                        
596600     MOVE DEAV-KVLS-IN       TO W-RYX-KVLS-IN                             
596700     MOVE DEAV-KVRESS-IN     TO W-RYX-KVRESS-IN                           
596800     MOVE DEAV-KVSPANT-IN    TO W-RYX-KVSPANT-IN                          
596900     MOVE DEAV-KVUTRS-IN     TO W-RYX-KVUTRS-IN                           
597000     MOVE DEAV-RERF-ART-IN   TO W-RYX-RERF-ART-IN                         
597100     MOVE DEAV-RERF-RAD-NY-IN TO W-RYX-RERF-RAD-NY-IN                     
597200     MOVE DEAV-KDSORT-IN     TO W-RYX-KDSORT-IN                           
597300     MOVE DEAV-KDPRODSL-IN   TO W-RYX-KDPRODSL-IN                         
597400     MOVE DEAV-KDLEVSP-IN    TO W-RYX-KDLEVSP-IN                          
597500     MOVE DEAV-FLAKPLOC-IN   TO W-RYX-FLAKPLOC-IN                         
597600     MOVE DEAV-KVBEART-Q-IN  TO W-RYX-KVBEART-Q-IN                        
597700     MOVE DEAV-KVPREAVB-IN   TO W-RYX-KVPREAVB-IN                         
597800     MOVE DEAV-KVPRERO-IN    TO W-RYX-KVPRERO-IN                          
597900     MOVE DEAV-RERF-RAD-IN   TO W-RYX-RERF-RAD-IN                         
598000     MOVE DEAV-FLRESTN-IN    TO W-RYX-FLRESTN-IN                          
598100     MOVE DEAV-KVSPARR-KVAL-IN TO W-RYX-KVSPARR-KVAL-IN                   
598200     MOVE DEAV-IDKUNDNR-IN   TO W-RYX-IDKUNDNR-IN                         
598300     MOVE DEAV-IDORDNR5-IN   TO W-RYX-IDORDNR5-IN                         
598400     .                                                                    
598500     EJECT                                                                
598600 S98B-SKAPA-SORTPOST SECTION.                                             
598700                                                                          
598800     MOVE DEAV-FLAKPLOC-UT    TO W-RYXS-FLAKPLOC-UT                       
598900     MOVE DEAV-KVAVBART-UT    TO W-RYXS-KVAVBART-UT                       
599000     MOVE DEAV-KDORDBEK-UT    TO W-RYXS-KDORDBEK-UT                       
599100     MOVE DEAV-RERF-RAD-UT    TO W-RYXS-RERF-RAD-UT                       
599200     MOVE DEAV-KVEFRS-UT      TO W-RYXS-KVEFRS-UT                         
599300     MOVE DEAV-KVLS-UT        TO W-RYXS-KVLS-UT                           
599400     MOVE DEAV-KVRESS-UT      TO W-RYXS-KVRESS-UT                         
599500     MOVE DEAV-KVROS-UT       TO W-RYXS-KVROS-UT                          
599600     MOVE DEAV-KDROO-UT       TO W-RYXS-KDROO-UT                          
599700     .                                                                    
599800     EJECT                                                                
599900 S09-CREATE-PLOCKSATS-ETIK SECTION.                                       
600000                                                                          
600100     MOVE MID-IDPRODNR         TO W-4003-ETIK-IDPRODNR                    
600200     MOVE MID-IDPLKLST         TO W-4003-ETIK-IDPLKLST                    
600300     MOVE W-4003-ETIK-IDHTYP-X TO WL400301                                
600400     PERFORM IMS-ISRT-4003-WL400301                                       
600500     PERFORM IMS-ISRT-4003-WL400311                                       
600600                                                                          
600700     MOVE JA TO PLOCKSATS-ETIK-SW                                         
600800     .                                                                    
600900     EJECT                                                                
601000 S10-UPPDAT-BEFINTLIG-RESTORDER SECTION.                                  
601100     MOVE 'STA S10-UPP'          TO PGMPOS                                
601200                                                                          
601300     MOVE ORAD-IDDISTR               TO W-A501KY-IDDISTR                  
601400     MOVE ORAD-IDKUNDNR              TO W-A501KY-IDKUNDNR                 
601500     MOVE ORAD-IDKUNDRF-RO (3:5)     TO W-A501KY-IDKUNDRF                 
601600     MOVE ORAD-IDARTNR               TO W-A501KY-IDARTNR                  
601700     MOVE ORAD-IDLOPNR-RO            TO W-A501KY-IDLOPNR                  
601800                                                                          
601900     PERFORM IMS-GHU-ORDP-WLORDP01                                        
602000     IF SEGMENT-SAKNAS                                                    
602100       PERFORM S03-NYUPPLAGG-RESTORDER                                    
602200     ELSE                                                                 
602300                                                                          
602400*    IF RAD-KDSTARAD = '4'                                                
602500*    OR RAD-KDSTARAD = 4                                                  
602600        MOVE WS-KVROS                TO RAD-KVART                         
602700*    ELSE                                                                 
602800*FIX    IF ORAD-IDKUNDRF-RO (3:5) = '47519'                               
602900*       AND ORAD-IDARTNR = +000272196                                     
603000*         MOVE WS-KVROS              TO RAD-KVART                         
603100*       ELSE                                                              
603200*       MOVE 'EJ STATUS 4 VID ÅTER TILL RO, S10-XXX..' TO FELTEXT         
603300*       CALL FELLOG USING RKOD-RET-RO                                     
603400*      END-IF                                                             
603500*    END-IF                                                               
603600                                                                          
603700     MOVE '2'                        TO RAD-KDSTARAD                      
603800     MOVE 0                          TO RAD-TIRES                         
603900     MOVE ORAD-IDDC-RO               TO RAD-IDDC                          
604000                                        RAD-IDDC-RO                       
604100                                                                          
604200     IF RAD-DARODAT = 0                                                   
604300        MOVE WS-DATUM-LOK         TO RAD-DARODAT                          
604400        IF WS-DATUM-LOK NOT = ZERO                                        
604500          IF WS-DATUM-LOK < 500000                                        
604600            MOVE 20               TO RAD-DARODAT (1:2)                    
604700          ELSE                                                            
604800            IF WS-DATUM-LOK < 999999                                      
604900              MOVE 19             TO RAD-DARODAT (1:2)                    
605000            ELSE                                                          
605100              MOVE 99999999       TO RAD-DARODAT                          
605200            END-IF                                                        
605300          END-IF                                                          
605400        END-IF                                                            
605500     END-IF                                                               
605600     PERFORM S10A-ANDRA-WDC711                                            
605700                                                                          
605800     PERFORM IMS-REPL-ORDP-WLORDP01                                       
605900     END-IF                                                               
606000     MOVE 'END S10-UPP'          TO PGMPOS                                
606100     .                                                                    
606200     EJECT                                                                
606300 S10A-ANDRA-WDC711  SECTION.                                              
606400                                                                          
606500     IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                          
606600       IF ORAD-IDPRQUES = 0                                               
606700         PERFORM S36-ANDRA-WDC711                                         
606800       ELSE                                                               
606900         INITIALIZE  PRQU-W335PRQU                                        
607000         MOVE ORAD-IDDISTR        TO PRQU-IDDISTR                         
607100         MOVE ORAD-IDKUNDNR       TO PRQU-IDKUNDNR                        
607200         MOVE '0000000   '        TO PRQU-IDKUNDRF                        
607300         MOVE ORAD-IDORDNR5       TO PRQU-IDKUNDRF(3:5)                   
607400         MOVE ORAD-IDPRQUES       TO PRQU-IDPRQUES                        
607500         MOVE 6                   TO PRQU-KDCALL                          
607600         CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                  
607700                                           PRQU-WDC7-PCB                  
607800                                           PRQU-SJKO-WDK6-PCB             
607900       END-IF                                                             
608000       MOVE 'N'                   TO RAD-FLPRTILL                         
608100     END-IF                                                               
608200     .                                                                    
608300     EJECT                                                                
608400 S11-UPPDAT-VOR SECTION.                                                  
608500                                                                          
608600     MOVE 'STA S11-UPP'          TO PGMPOS                                
608700     MOVE ORAD-IDDISTR         TO 4542-IDDISTR                            
608800     MOVE ORAD-IDDC            TO 4542-IDDC                               
608900     MOVE AREG-IDANSK          TO 4542-IDANSK                             
609000     MOVE ORAD-IDARTNR         TO 4542-IDARTNR                            
609100     MOVE 1                    TO 4542-IDLOPNR                            
609200     MOVE ORAD-IDORDER         TO 4542-IDORDER                            
609300     MOVE ORAD-BERADREF        TO 4542-BERADREF                           
609400     MOVE ORAD-IDKUNDNR        TO 4542-IDKUNDNR                           
609500     MOVE ORAD-IDKUNDRF        TO 4542-IDKUNDRF                           
609600     MOVE SPACE                TO 4542-IDUSER                             
609700     MOVE WS-KDORDBEK          TO 4542-KDORDBEK                           
609800     MOVE ORAD-KDPRTYP         TO 4542-KDPRTYP                            
609900     MOVE ZERO                 TO 4542-KDVORATG                           
610000     MOVE ORAD-KVBEART         TO 4542-KVBEART                            
610100     MOVE ORAD-KVBEART-Q       TO 4542-KVBEART-Q                          
610200     MOVE WS-KVAVBART          TO 4542-KVPREAVB                           
610300     MOVE ORAD-PRARTNTO        TO 4542-PRARTNTO                           
610400     MOVE ORAD-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
610500     MOVE SPACE                TO 4542-TEVORMRK                           
610600     MOVE WS-DATUM-LOK         TO 4542-TIREGDAT                           
610700     MOVE WS-TIHHMMSS-LOK      TO 4542-TIREGTID                           
610800     MOVE ZERO                 TO 4542-TIUPPDAT                           
610900                                  4542-TIUPPTID                           
611000     MOVE ORAD-IDLEVNR         TO 4542-IDLEVNR                            
611100                                                                          
611200     PERFORM IMS-ISRT-4541-WL454111                                       
611300                                                                          
611400     PERFORM UNTIL SEGMENT-FINNS                                          
611500        ADD 1 TO 4542-IDLOPNR                                             
611600        PERFORM IMS-ISRT-4541-WL454111                                    
611700     END-PERFORM                                                          
611800                                                                          
611900     IF  DCS-NDC-CN                                                       
612000     OR (DCS-NDC-NA AND DCS-USA)                                          
612100       IF SLAG-IDDC-REF = SPACE                                           
612200                                                                          
612300          MOVE ORAD-IDDISTR       TO S27-IDDISTR                          
612400          MOVE ORAD-IDKUNDNR      TO S27-IDKUNDNR                         
612500          MOVE ORAD-IDARTNR       TO S27-IDARTNR                          
612600          MOVE AREG-IDANSK        TO S27-IDANSK                           
612700          MOVE SLAG-IDDC          TO S27-IDDC                             
612800          MOVE SLAG-IDLEVNR       TO S27-IDLEVNR                          
612900          PERFORM S27-STARTA-W2T191X                                      
613000       END-IF                                                             
613100     END-IF                                                               
613200     .                                                                    
613300     EJECT                                                                
613400 S11D-UPDATE-VORKONY          SECTION.                                    
613500                                                                          
613600     MOVE 'STA S11D-UPP'          TO PGMPOS                               
613700     MOVE ORAD-IDARTNR        TO S28-IDARTNR                              
613800     MOVE DCS-IDDC            TO S28-IDDC                                 
613900     PERFORM S28-BESTAM-LENVR-ANSK                                        
614000                                                                          
614100*----------------------------------RADEN SKALL FINNAS PÅ VORKÖ            
614200                                                                          
614300     PERFORM S26-SOK-RAD-VORKO                                            
614400                                                                          
614500     IF  TRAFF-VORKO                                                      
614600         COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                             
614700                               - ORAD-KVBEART-Q                           
614800                               + WS-KVAVBART                              
614900         END-COMPUTE                                                      
615000         COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                            
615100                               - ORAD-KVBEART-Q                           
615200                               + WS-KVAVBART                              
615300         END-COMPUTE                                                      
615400         IF  VOR-KVPREAVB = 0                                             
615500             MOVE '7'              TO VOR-KDVORATG                        
615600             MOVE WS-KDORDBEK      TO VOR-KDORDBEK                        
615700             IF VOR-TIKLAR = ZERO                                         
615800                MOVE WS-DATUM      TO VOR-TIKLAR                          
615900                COMPUTE VOR-TIKLATID  = WS-VOR-TID-BRIST                  
616000                                      / 100                               
616100                END-COMPUTE                                               
616200             END-IF                                                       
616300         END-IF                                                           
616400         PERFORM IMS-REPL-SEQB-WDA601                                     
616500                                                                          
616600         MOVE WS-DATUM          TO VOR-TIREGDAT-AVV                       
616700         ADD +1                 TO WS-VOR-TID-BRIST                       
616800         MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                       
616900         SUBTRACT WS-DATUM        FROM 9999999                            
617000                                  GIVING VOR-TIREGDAT-AVV9                
617100         SUBTRACT WS-VOR-TID-BRIST FROM 999999999                         
617200                                   GIVING VOR-TIREGTID-AVV9               
617300         MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                       
617400         MOVE 0                 TO VOR-TIREGDAT-LEV                       
617500         MOVE 0                 TO VOR-TIREGTID-LEV                       
617600         SUBTRACT WS-KVAVBART                                             
617700                              FROM ORAD-KVBEART-Q                         
617800                              GIVING VOR-KVBEART                          
617900                                     VOR-KVBEART-Q                        
618000         MOVE 0                 TO VOR-KVPREAVB                           
618100         MOVE WS-IDDC           TO VOR-IDDC                               
618200         MOVE SPACE             TO VOR-IDUSER                             
618300         MOVE WS-KDORDBEK       TO VOR-KDORDBEK                           
618400         MOVE '0'               TO VOR-KDVORATG                           
618500         MOVE 0                 TO VOR-TIKLAR                             
618600         MOVE 0                 TO VOR-TIKLATID                           
618700                                                                          
618800         PERFORM IMS-ISRT-WDA601                                          
618900         PERFORM UNTIL ISRT-OK                                            
619000            ADD +1                TO WS-VOR-TID-BRIST                     
619100                                   VOR-TIREGTID-URSP                      
619200            MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                     
619300            SUBTRACT WS-VOR-TID-BRIST FROM 999999999                      
619400                                  GIVING VOR-TIREGTID-AVV9                
619500            PERFORM IMS-ISRT-WDA601                                       
619600         END-PERFORM                                                      
619700     ELSE                                                                 
619800*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
619900*                                        BORDE NOG INTE FÖREKOMMA         
620000       MOVE ORAD-IDDISTR        TO VOR-IDDISTR                            
620100       MOVE ORAD-IDKUNDNR       TO VOR-IDKUNDNR                           
620200       MOVE ORAD-IDKUNDRF       TO VOR-IDKUNDRF                           
620300       MOVE ORAD-TIREGDAT       TO VOR-TIREGDAT-URSP                      
620400       MOVE ORAD-IDARTNR        TO VOR-IDARTNR                            
620500       ADD +1                   TO WS-VOR-TID-BRIST                       
620600       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-URSP                      
620700       MOVE 0                   TO VOR-TIREGDAT-AVV                       
620800       MOVE 0                   TO VOR-TIREGTID-AVV                       
620900       SUBTRACT 0            FROM 9999999                                 
621000                              GIVING VOR-TIREGDAT-AVV9                    
621100       SUBTRACT 0            FROM 999999999                               
621200                              GIVING VOR-TIREGTID-AVV9                    
621300       MOVE ORAD-IDKUNDRF       TO VOR-IDKUNDRF-LEV                       
621400       MOVE ORAD-TIREGDAT       TO VOR-TIREGDAT-LEV                       
621500       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-LEV                       
621600       MOVE S28-IDANSK          TO VOR-IDANSK                             
621700                                                                          
621800       MOVE VOR-IDDISTR         TO W-IDDISTR-P4                           
621900       PERFORM IMS-GU-WDP4A1                                              
622000       IF SEGMENT-SAKNAS                                                  
622100         MOVE DEF-IDROLL        TO SEQA-IDROLL                            
622200       END-IF                                                             
622300       MOVE SEQA-IDROLL         TO VOR-IDROLL                             
622400                                                                          
622500       MOVE S28-IDLEVNR         TO VOR-IDLEVNR                            
622600       MOVE ORAD-BERADREF       TO VOR-BERADREF                           
622700       SUBTRACT WS-KVAVBART                                               
622800                            FROM   ORAD-KVBEART-Q                         
622900                            GIVING VOR-KVBEART-URSP                       
623000                                   VOR-KVBEART                            
623100       MOVE 0                   TO VOR-KVPREAVB                           
623200                                   VOR-KVBEART-Q                          
623300       MOVE WS-IDDC             TO VOR-IDDC                               
623400       MOVE SPACE               TO VOR-IDUSER                             
623500       MOVE WS-KDORDBEK         TO VOR-KDORDBEK                           
623600       MOVE ORAD-KDPRTYP        TO VOR-KDPRTYP                            
623700       MOVE '7'                 TO VOR-KDVORATG                           
623800       MOVE ORAD-PRARTNTO       TO VOR-PRARTNTO                           
623900       MOVE '  '                TO VOR-TEVORMRK                           
624000*      MOVE '  '                TO VOR-TEVORMRK-SC                        
624100       MOVE 0                   TO VOR-TIUPPDAT                           
624200       MOVE 0                   TO VOR-TIUPPTID                           
624300       MOVE WS-DATUM            TO VOR-TIKLAR                             
624400       COMPUTE VOR-TIKLATID     = WS-VOR-TID-BRIST                        
624500                                / 100                                     
624600       END-COMPUTE                                                        
624700       MOVE ORAD-DEAL-PR-LINE   TO VOR-DEAL-PR-LINE                       
624800       MOVE NEJ                 TO VOR-FLVORFK                            
624900       PERFORM IMS-ISRT-WDA601                                            
625000       PERFORM UNTIL ISRT-OK                                              
625100          ADD +1              TO VOR-TIREGTID-URSP                        
625200          ADD +1              TO VOR-TIREGTID-LEV                         
625300          PERFORM IMS-ISRT-WDA601                                         
625400       END-PERFORM                                                        
625500                                                                          
625600*--------------------------------------- EJ TRÄFF, AVVIK RAD              
625700       MOVE WS-DATUM          TO VOR-TIREGDAT-AVV                         
625800       ADD +1                 TO WS-VOR-TID-BRIST                         
625900       MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                         
626000       SUBTRACT WS-DATUM         FROM 9999999                             
626100                                 GIVING VOR-TIREGDAT-AVV9                 
626200       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
626300                                 GIVING VOR-TIREGTID-AVV9                 
626400       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
626500       MOVE 0                 TO VOR-TIREGDAT-LEV                         
626600       MOVE 0                 TO VOR-TIREGTID-LEV                         
626700       SUBTRACT WS-KVAVBART                                               
626800                            FROM ORAD-KVBEART-Q                           
626900                            GIVING VOR-KVBEART-Q                          
627000       MOVE 0                 TO VOR-KVPREAVB                             
627100       MOVE WS-KDORDBEK       TO VOR-KDORDBEK                             
627200       MOVE '0'               TO VOR-KDVORATG                             
627300       MOVE 0                 TO VOR-TIKLAR                               
627400       MOVE 0                 TO VOR-TIKLATID                             
627500                                                                          
627600       PERFORM IMS-ISRT-WDA601                                            
627700       PERFORM UNTIL ISRT-OK                                              
627800          ADD +1                TO WS-VOR-TID-BRIST                       
627900                                   VOR-TIREGTID-URSP                      
628000          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
628100          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
628200                                GIVING VOR-TIREGTID-AVV9                  
628300          PERFORM IMS-ISRT-WDA601                                         
628400       END-PERFORM                                                        
628500     END-IF                                                               
628600                                                                          
628700     COMPUTE CLAG-KVVORKO       = CLAG-KVVORKO                            
628800                                + VOR-KVBEART-Q                           
628900                                - VOR-KVPREAVB                            
629000     END-COMPUTE                                                          
629100                                                                          
629200     PERFORM IMS-REPL-WDK611                                              
629300                                                                          
629400     MOVE ORAD-IDDISTR          TO S27-IDDISTR                            
629500     MOVE ORAD-IDKUNDNR         TO S27-IDKUNDNR                           
629600     MOVE ORAD-IDARTNR          TO S27-IDARTNR                            
629700     MOVE S28-IDANSK            TO S27-IDANSK                             
629800     MOVE WC-CDC-SE             TO S27-IDDC                               
629900     MOVE SPACE                 TO S27-IDLEVNR                            
630000     PERFORM S27-STARTA-W2T191X                                           
630100                                                                          
630200     .                                                                    
630300     EJECT                                                                
630400 S26-SOK-RAD-VORKO SECTION.                                               
630500                                                                          
630600     MOVE 'STA S26-SOK'          TO PGMPOS                                
630700     MOVE LOW-VALUE      TO W-WDA601KY-MIN-X                              
630800     MOVE HIGH-VALUE     TO W-WDA601KY-MAX-X                              
630900                                                                          
631000     MOVE ORAD-IDDISTR      TO W-A601KY-MIN-IDDISTR                       
631100                               W-A601KY-MAX-IDDISTR                       
631200     MOVE ORAD-IDKUNDNR     TO W-A601KY-MIN-IDKUNDNR                      
631300                               W-A601KY-MAX-IDKUNDNR                      
631400     MOVE ORAD-IDKUNDRF     TO W-A601KY-MIN-IDKUNDRF                      
631500                               W-A601KY-MAX-IDKUNDRF                      
631600     MOVE ORAD-TIREGDAT     TO W-A601KY-MIN-TIREGDAT                      
631700                               W-A601KY-MAX-TIREGDAT                      
631800     MOVE ORAD-IDARTNR      TO W-A601KY-MIN-IDARTNR                       
631900                               W-A601KY-MAX-IDARTNR                       
632000     MOVE NEJ               TO TRAFF-VORKO-SW                             
632100                                                                          
632200     PERFORM IMS-GHU-SEQB-WDA601                                          
632300     PERFORM UNTIL SEGMENT-SAKNAS                                         
632400                OR SEGMENT-SLUT                                           
632500                OR TRAFF-VORKO                                            
632600       IF  ORAD-KVBEART-Q = VOR-KVPREAVB                                  
632700           MOVE JA       TO TRAFF-VORKO-SW                                
632800       ELSE                                                               
632900           PERFORM IMS-GHN-SEQB-WDA601                                    
633000       END-IF                                                             
633100     END-PERFORM                                                          
633200     .                                                                    
633300     EJECT                                                                
633400 S27-STARTA-W2T191X  SECTION.                                             
633500                                                                          
633600     MOVE 'STA S27-STA'          TO PGMPOS                                
633700     COMPUTE ALT5-LL = LENGTH OF ALT5-MID-W2I19101 + 17                   
633800     MOVE +1                    TO ALT5-MID-KDCLAGER                      
633900     MOVE S27-IDARTNR-X         TO ALT5-MID-IDARTNR                       
634000     MOVE ZERO                  TO ALT5-MID-TISENBEK-DAG                  
634100                                   ALT5-MID-TISENBEK-KL                   
634200     MOVE SPACE                 TO ALT5-MID-IDKR                          
634300     MOVE S27-IDANSK-X          TO ALT5-MID-IDANSK                        
634400     MOVE '500'                 TO ALT5-MID-KDLARM                        
634500     MOVE S27-IDDISTR-X         TO ALT5-MID-IDDISTR                       
634600     MOVE S27-IDKUNDNR-X        TO ALT5-MID-IDKUNDNR                      
634700     MOVE ORAD-IDKUNDRF         TO ALT5-MID-IDKUNDRF                      
634800     MOVE 'J'                   TO ALT5-MID-FLNYLARM                      
634900     MOVE S27-IDDC              TO ALT5-MID-IDDC                          
635000     MOVE S27-IDLEVNR           TO ALT5-MID-IDLEVNR                       
635100                                                                          
635200     PERFORM IMS-PURG-ALT5-MSG                                            
635300                                                                          
635400     MOVE SPACE                 TO ALT5-MID-W2I19101                      
635500     .                                                                    
635600     EJECT                                                                
635700 S28-BESTAM-LENVR-ANSK SECTION.                                           
635800                                                                          
635900     MOVE 'STA S28-BEST'         TO PGMPOS                                
636000     MOVE S28-IDARTNR    TO W-IDARTNR                                     
636100     MOVE S28-IDDC       TO W-IDDC                                        
636200     PERFORM IMS-GU-WDK601                                                
636300     MOVE ART-IDLEVNR    TO S28-IDLEVNR                                   
636400                                                                          
636500     PERFORM IMS-GHNP-WDK611                                              
636600     MOVE CLAG-KVVORKO   TO S28-KVVORKO                                   
636700     MOVE CLAG-IDANSK    TO S28-IDANSK                                    
636800     .                                                                    
636900     EJECT                                                                
637000 S12-SKAPA-RYKTRANS SECTION.                                              
637100                                                                          
637200     MOVE 'STA S12-SKAP'         TO PGMPOS                                
637300     PERFORM S12A-SKAPA-RYKPOST                                           
637400                                                                          
637500     MOVE W-RYKPOST  TO LOGGPOST                                          
637600     MOVE SPACE      TO SORTPOST                                          
637700     MOVE WS-DATUM   TO TIAAMMDD                                          
637800     ACCEPT TIKLOCK FROM TIME                                             
637900     MOVE 1          TO IDLOGLOP                                          
638000                                                                          
638100     PERFORM IMS-ISRT-ZZAC-WLZZAC01                                       
638200     PERFORM UNTIL SEGMENT-FINNS                                          
638300        IF IDLOGLOP = 9                                                   
638400           ACCEPT TIKLOCK FROM TIME                                       
638500           MOVE 0 TO IDLOGLOP                                             
638600        END-IF                                                            
638700        ADD 1 TO IDLOGLOP                                                 
638800        PERFORM IMS-ISRT-ZZAC-WLZZAC01                                    
638900     END-PERFORM                                                          
639000     .                                                                    
639100     EJECT                                                                
639200 S12A-SKAPA-RYKPOST SECTION.                                              
639300                                                                          
639400     MOVE 'RYK'              TO W-RYK-IDPTYP                              
639500     MOVE ORAD-IDDISTR       TO W-RYK-IDDISTR                             
639600     MOVE ORAD-IDKUNDNR      TO W-RYK-IDKUNDNR                            
639700                                                                          
639800     IF ORAD-IDKUNDRF-RO = '0000000   '                                   
639900       MOVE ORAD-IDORDER     TO W-RYK-IDORDER                             
640000     ELSE                                                                 
640100       MOVE ORAD-IDDISTR     TO W-WDQ2CSEQ-IDDISTR                        
640200       MOVE ORAD-IDKUNDNR    TO W-WDQ2CSEQ-IDKUNDNR                       
640300       MOVE ORAD-IDKUNDRF-RO TO W-WDQ2CSEQ-IDKUNDRF                       
640400       PERFORM IMS-GU-ORQI01-CSEQ                                         
640500       MOVE CSQ-OHUV-IDORDER TO W-RYK-IDORDER                             
640600     END-IF                                                               
640700                                                                          
640800     MOVE ORAD-IDARTNR       TO W-RYK-IDARTNR                             
640900     MOVE WS-DATUM           TO W-RYK-TIRODAT                             
641000     MOVE 0                  TO W-RYK-KVLEVART                            
641100     MOVE ORAD-KVBEART-Q     TO W-RYK-KVBEART-Q                           
641200     MOVE ORAD-KDORDKL       TO W-RYK-KDORDKL                             
641300     MOVE AREG-KDPRODSL      TO W-RYK-KDPRODSL                            
641400     MOVE WS-KDORDBEK        TO W-RYK-KDORDBEK                            
641500     .                                                                    
641600     EJECT                                                                
641700 S13-ATERSTALL-ORDERDEL SECTION.                                          
641800                                                                          
641900     MOVE 'R'       TO ODEL-KDODELSTA                                     
642000     MOVE SPACE     TO ODEL-IDUSER                                        
642100                       ODEL-IDBORD                                        
642200     MOVE ZERO      TO ODEL-DAUTSKR                                       
642300                       ODEL-TIUTSTID                                      
642400                                                                          
642500     PERFORM IMS-REPL-ORQA-WLORQA01                                       
642600     .                                                                    
642700     EJECT                                                                
642800 S14-ANROP-WDATKONV SECTION.                                              
642900                                                                          
643000     MOVE 'AAMMDD'  TO DAT-KDDATFORM                                      
643100                                                                          
643200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
643300                         DAT-O-TIDATUM DAT-KDSVAR                         
643400                                                                          
643500     IF DAT-KDSVAR-OK                                                     
643600        CONTINUE                                                          
643700     ELSE                                                                 
643800        MOVE 'DATUMKONVERTERINGEN HAR GÅTT SNETT' TO FELTEXT              
643900        CALL FELLOG                                                       
644000     END-IF                                                               
644100     .                                                                    
644200     EJECT                                                                
644300 S15-KOLLA-I-HFAK-TAB   SECTION.                                          
644400                                                                          
644500     IF WS-HFAK-REF-X10 NOT = SPACE                                       
644600        MOVE 1 TO HFAK-TAB-IX                                             
644700        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
644800           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
644900           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
645000           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
645100           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
645200              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
645300              MOVE 99 TO HFAK-TAB-IX                                      
645400           END-IF                                                         
645500           ADD 1 TO HFAK-TAB-IX                                           
645600        END-PERFORM                                                       
645700     END-IF                                                               
645800     .                                                                    
645900     SKIP2                                                                
646000 S16-KOLLA-OM-RENOVA    SECTION.                                          
646100                                                                          
646200     IF OHUV-BEVARREF   = 'RENOVA    '                                    
646300     OR ORAD-BERADREF   = 'RENOVA    '                                    
646400     OR WS-HFAK-REF-X10 = 'RENOVA    '                                    
646500        MOVE JA TO RENOVA-SW                                              
646600     END-IF                                                               
646700     .                                                                    
646800     EJECT                                                                
646900 S17-DIST-KUND-LDC      SECTION.                                          
647000                                                                          
647100     MOVE OHUV-IDDISTR        TO  W-IDDISTR-WDB2                          
647200     MOVE OHUV-IDKUNDNR       TO  W-IDKUNDNR-WDB2                         
647210     MOVE NEJ                 TO  SW-LYNK-NON-API                         
647220                                  SW-VOR                                  
647300                                                                          
647400     PERFORM IMS-GET-WDB201-UNIK                                          
647500                                                                          
647600     IF SEGMENT-SAKNAS                                                    
647700        MOVE NEJ              TO  GMT-FLLDCKND                            
647800     ELSE                                                                 
647900        MOVE GMT-IDZON        TO WS-GMT-IDZON                             
648000        MOVE GMT-FLCOD        TO WS-GMT-FLCOD                             
648020        IF GMT-KDKUNDKAT = 03                                             
648021           IF  OHUV-IDSYSTEM(1:3) NOT = 'LYN'                             
648030              MOVE JA         TO SW-LYNK-NON-API                          
648031           END-IF                                                         
648032           IF OHUV-KDORDKL = 0                                            
648033              MOVE JA         TO SW-VOR                                   
648034           END-IF                                                         
648040        END-IF                                                            
648100     END-IF                                                               
648200     .                                                                    
648300     EJECT                                                                
648400 S18-ADD-PRICE-Q-LINE SECTION.                                            
648500                                                                          
648600     MOVE 'STA S18-ADD-'         TO PGMPOS                                
648700     IF DIST79-DEALER-PRICE                                               
648800       IF BIPA-IDPRQUES (BIPA-IX) = +0                                    
648900******* HÄMTAR NÄSTA LEDIGA PRISFRÅGENR                                   
649000         MOVE +0                    TO PRNO-IDPRQUES-IN                   
649100         MOVE +1                    TO PRNO-KDCALL                        
649200                                                                          
649300         CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                  
649400                                                                          
649500********UPPDATERAR WDC7 MED EN PRISFRÅGA                                  
649600         MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                      
649700         MOVE +1                    TO PRQU-KDCALL                        
649800                                                                          
649900         MOVE ORAD-IDDISTR          TO PRQU-IDDISTR                       
650000         MOVE ORAD-IDKUNDNR         TO PRQU-IDKUNDNR                      
650100         MOVE ORAD-IDKUNDRF         TO PRQU-IDKUNDRF                      
650200         MOVE ORAD-IDORDER          TO PRQU-IDORDER                       
650300         MOVE ORAD-KDORDKL          TO PRQU-KDORDKL                       
650400         MOVE 'N'                   TO PRQU-KDPRSTA                       
650500         MOVE ORAD-IDARTNR          TO PRQU-IDARTNR                       
650600         MOVE ORAD-KVBEART-Q        TO PRQU-KVBEART-Q                     
650700         MOVE ORAD-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                  
650800         MOVE +0                    TO PRQU-PRARTNTO-LOCPREL              
650900         MOVE ORAD-IDSYSTEM         TO PRQU-IDSYSTEM                      
651000                                                                          
651100         PERFORM S18A-HAEMTA-STA-STO-DATUM                                
651200                                                                          
651300         IF ORAD-KDVALISO  = SPACE                                        
651400           PERFORM S18B-HAEMTA-KDVALISO                                   
651500           MOVE ORAD-KDVALISO          TO PRQU-KDVALISO                   
651600         ELSE                                                             
651700           MOVE ORAD-KDVALISO          TO PRQU-KDVALISO                   
651800         END-IF                                                           
651900                                                                          
652000                                                                          
652100         CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                  
652200                                         PRQU-WDC7-PCB                    
652300                                         PRQU-SJKO-WDK6-PCB               
652400                                                                          
652500         MOVE PRQU-IDPRQUES         TO  ORAD-IDPRQUES                     
652600         MOVE PRQU-FLPRTILL         TO  ORAD-FLPRTILL                     
652700                                                                          
652800         MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL             
652900                                                                          
653000* UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                                      
653100                                                                          
653200         MOVE PRQU-IDPRQUES         TO  PRNO-IDPRQUES-IN                  
653300         MOVE +3                    TO  PRNO-KDCALL                       
653400                                                                          
653500         CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                  
653600*SKICKA PRISFRÅGA                                                         
653700         PERFORM S18C-SKICKA-PRISFRAGA                                    
653800       END-IF                                                             
653900     END-IF                                                               
654000     .                                                                    
654100 S18A-HAEMTA-STA-STO-DATUM SECTION.                                       
654200                                                                          
654300      MOVE ORAD-IDDISTR                TO W-IDDISTR-WDB2                  
654400                                       W-IDDISTR-WDB2-MIN                 
654500                                       W-IDDISTR-WDB2-MAX                 
654600      MOVE ORAD-IDKUNDNR               TO W-IDKUNDNR-WDB2                 
654700      PERFORM IMS-GET-WDB201-UNIK                                         
654800      IF SEGMENT-FINNS                                                    
654900         CONTINUE                                                         
655000      ELSE                                                                
655100         PERFORM IMS-GU-WDB201                                            
655200      END-IF                                                              
655300      .                                                                   
655400      EJECT                                                               
655500 S18B-HAEMTA-KDVALISO      SECTION.                                       
655600                                                                          
655700                                                                          
655800      MOVE GMT-IDPARTNR              TO W-WDB1-IDPARTNR                   
655900      MOVE WS-DCS-IDFTG              TO W-WDB1-IDFTG                      
656000      PERFORM IMS-GU-WDB101                                               
656100      IF SEGMENT-FINNS                                                    
656200        MOVE BET-KDVALISO            TO ORAD-KDVALISO                     
656300      END-IF                                                              
656400      .                                                                   
656500                                                                          
656600 S18C-SKICKA-PRISFRAGA SECTION.                                           
656700                                                                          
656800     MOVE 1                       TO PRQU-REQU-IDMSGVER                   
656900     MOVE SPACE                   TO PRQU-REQU-KDPGMACT                   
657000     MOVE 'W4037500'              TO PRQU-REQU-IDUSER                     
657100                                                                          
657200     MOVE 'STA S18C-SKIC'         TO PGMPOS                               
657300     MOVE SPACE                   TO 3039-MID-IDBUNDLE                    
657400     MOVE ORAD-IDDISTR            TO 3039-MID-IDDISTR                     
657500     MOVE ORAD-IDKUNDNR           TO 3039-MID-IDKUNDNR                    
657600     MOVE ORAD-IDORDNR7           TO 3039-MID-IDBUNDLE                    
657700     MOVE PRQU-IDPRQUES           TO 3039-MID-IDPRQUES                    
657800                                                                          
657900     PERFORM S19-SKICKA-OPEN-PRQU                                         
658000     PERFORM S19-SKICKA-MEDDELANDE-PRQU                                   
658100     PERFORM S19-SKICKA-CLOSE-PRQU                                        
658200     .                                                                    
658300                                                                          
658400 S19-SKICKA-OPEN-PRQU SECTION.                                            
658500                                                                          
658600     MOVE 'STA S19-SKIC'         TO PGMPOS                                
658700     MOVE 'OPEN'                TO PRQU-SEND-KDFUNC                       
658800     MOVE 'CARPARTS.PULS.PRQRY' TO PRQU-SEND-ADDISPABS                    
658900     CALL WZ01SEND USING PRQU-SEND-CONTROL-AREA                           
659000                         PRQU-SEND-OPEN-AREA                              
659100                                                                          
659200     IF PRQU-SEND-KDRC > 0                                                
659300       MOVE PRQU-SEND-KDRC TO KDRC-DISPLAY                                
659400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
659500       DELIMITED BY SIZE INTO FELTEXT                                     
659600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
659700     END-IF                                                               
659800     .                                                                    
659900     SKIP3                                                                
660000 S19-SKICKA-MEDDELANDE-PRQU SECTION.                                      
660100                                                                          
660200     MOVE 'PUT'                      TO PRQU-SEND-KDFUNC                  
660300     MOVE LENGTH OF SEND-AREA-PRQU   TO PRQU-SEND-KVDLEN                  
660400     CALL WZ01SEND USING PRQU-SEND-CONTROL-AREA                           
660500                         PRQU-SEND-OPEN-AREA                              
660600                                                                          
660700     IF PRQU-SEND-KDRC > 0                                                
660800       MOVE PRQU-SEND-KDRC TO KDRC-DISPLAY                                
660900       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
661000       DELIMITED BY SIZE INTO FELTEXT                                     
661100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
661200     END-IF                                                               
661300     .                                                                    
661400     SKIP3                                                                
661500 S19-SKICKA-CLOSE-PRQU SECTION.                                           
661600                                                                          
661700     MOVE 'CLOSE'                    TO PRQU-SEND-KDFUNC                  
661800     CALL WZ01SEND USING PRQU-SEND-CONTROL-AREA                           
661900                                                                          
662000*    IF PRQU-SEND-KDRC > 0                                                
662100       MOVE PRQU-SEND-KDRC TO KDRC-DISPLAY                                
662200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
662300       DELIMITED BY SIZE INTO FELTEXT                                     
662400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
662500*    END-IF                                                               
662600     .                                                                    
662700     EJECT                                                                
662800                                                                          
662900 S21-SEND-OPEN-TACD SECTION.                                              
663000     MOVE 'STA S21-SEND'         TO PGMPOS                                
663100     MOVE 'CARPARTS.DAP.DISTRDOC' TO TACD-SEND-ADDISPABS                  
663200     MOVE 'OPEN'                  TO TACD-SEND-KDFUNC                     
663300     CALL WZ01SEND             USING TACD-SEND-CONTROL-AREA               
663400                                     TACD-SEND-OPEN-AREA                  
663500     IF TACD-SEND-KDRC > ZERO                                             
663600       MOVE TACD-SEND-KDRC        TO KDRC-DISPLAY                         
663700       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
663800       DELIMITED BY SIZE INTO FELTEXT                                     
663900       DISPLAY FELTEXT                                                    
664000       CALL FELLOG                                                        
664100     END-IF                                                               
664200     .                                                                    
664300     EJECT                                                                
664400 S21-PUT-HEADER-TACD SECTION.                                             
664500     MOVE 'STA S21-PUT-H'         TO PGMPOS                               
664600     MOVE 1                       TO TACD-REQU-IDMSGVER                   
664700     MOVE 'R'                     TO TACD-REQU-KDPGMACT                   
664800     MOVE IDPGM                   TO TACD-REQU-IDUSER                     
664900     MOVE SPACES                  TO TACD-HDR-WZ04HDR                     
665000     MOVE 'ORDERCONF'             TO TACD-HDR-IDOUTTYPE                   
665100     MOVE OBKR-IDKUNDNR           TO WS-IDKUNDNR-NUM6                     
665200     MOVE WS-IDKUNDNR-NUM6        TO TACD-HDR-IDOUTREC                    
665300     MOVE OBKR-IDORDNR7           TO TACD-HDR-IDLIST                      
665400     MOVE 'PUT'                   TO TACD-SEND-KDFUNC                     
665500     MOVE LENGTH OF HDR-AREA-TACD       TO                                
665600                                     TACD-SEND-KVDLEN                     
665700                                                                          
665800     CALL WZ01SEND             USING TACD-SEND-CONTROL-AREA               
665900                                     TACD-SEND-KVDLEN                     
666000                                     HDR-AREA-TACD                        
666100                                                                          
666200     IF TACD-SEND-KDRC > ZERO                                             
666300       MOVE TACD-SEND-KDRC            TO KDRC-DISPLAY                     
666400       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
666500       DELIMITED BY SIZE       INTO FELTEXT                               
666600       DISPLAY FELTEXT                                                    
666700       CALL FELLOG                                                        
666800     END-IF                                                               
666900     .                                                                    
667000     EJECT                                                                
667100 S21-PUT-LINE-TACD SECTION.                                               
667200     MOVE 'STA S21-PUT-L'         TO PGMPOS                               
667300     MOVE 'PUT'                   TO TACD-SEND-KDFUNC                     
667400     MOVE LENGTH OF 402-W402TACD  TO TACD-SEND-KVDLEN                     
667500     CALL WZ01SEND             USING TACD-SEND-CONTROL-AREA               
667600                                     TACD-SEND-KVDLEN                     
667700                                     402-W402TACD                         
667800     IF TACD-SEND-KDRC > ZERO                                             
667900       MOVE TACD-SEND-KDRC            TO KDRC-DISPLAY                     
668000       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
668100       DELIMITED BY SIZE       INTO FELTEXT                               
668200       DISPLAY FELTEXT                                                    
668300       CALL FELLOG                                                        
668400     END-IF                                                               
668500     .                                                                    
668600     EJECT                                                                
668700                                                                          
668800 S21-SKICKA-CLOSE-TACD SECTION.                                           
668900     MOVE 'STA S21-SKICKA-CL'         TO PGMPOS                           
669000*    IF WS-KV402 > 0                                                      
669100       MOVE 'CLOSE'               TO TACD-SEND-KDFUNC                     
669200       CALL WZ01SEND           USING TACD-SEND-CONTROL-AREA               
669300       IF TACD-SEND-KDRC > 0                                              
669400         MOVE TACD-SEND-KDRC      TO KDRC-DISPLAY                         
669500         STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                  
669600         DELIMITED BY SIZE     INTO FELTEXT                               
669700         DISPLAY FELTEXT                                                  
669800         CALL FELLOG                                                      
669900       END-IF                                                             
670000*    END-IF                                                               
670100     .                                                                    
670200     EJECT                                                                
670300                                                                          
670400 S23-DELETE-PRICE-Q-LINE SECTION.                                         
670500     MOVE 'STA S23-DELETE-PR'         TO PGMPOS                           
670600     MOVE OHUV-IDDISTR           TO TEST-IDDISTR                          
670700     IF DIST79-DEALER-PRICE                                               
670800       IF ORAD-IDPRQUES > ZERO                                            
670900         INITIALIZE PRQU-W335PRQU                                         
671000         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
671100         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
671200         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
671300         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
671400         MOVE 4                  TO PRQU-KDCALL                           
671500         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
671600                                            PRQU-WDC7-PCB                 
671700                                            PRQU-SJKO-WDK6-PCB            
671800       END-IF                                                             
671900     END-IF                                                               
672000     .                                                                    
672100     EJECT                                                                
672200 S36-ANDRA-WDC711  SECTION.                                               
672300                                                                          
672400     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
672500       IF RAD-IDPRQUES > ZERO                                             
672600         INITIALIZE PRQU-W335PRQU                                         
672700         MOVE RAD-IDDISTR             TO PRQU-IDDISTR                     
672800         MOVE RAD-IDKUNDNR            TO PRQU-IDKUNDNR                    
672900         MOVE RAD-IDKUNDRF(1:5)       TO PRQU-IDKUNDRF(3:5)               
673000         MOVE '00'                    TO PRQU-IDKUNDRF(1:2)               
673100         MOVE RAD-IDPRQUES            TO PRQU-IDPRQUES                    
673200         MOVE 6                       TO PRQU-KDCALL                      
673300         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
673400                                            PRQU-WDC7-PCB                 
673500                                            PRQU-SJKO-WDK6-PCB            
673600         MOVE 'N'                     TO RAD-FLPRTILL                     
673700         IF RAD-PRARTNTO-LOC > +0                                         
673800           MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL             
673900           MOVE ZERO                  TO RAD-PRARTNTO-LOC                 
674000         END-IF                                                           
674100         IF PRQU-KDCALL = -1                                              
674200           PERFORM S36-NY-FRAGA                                           
674300         END-IF                                                           
674400       ELSE                                                               
674500*        SKAPA NY PRISFRÅGA                                               
674600         PERFORM S36-NY-FRAGA                                             
674700       END-IF                                                             
674800     END-IF                                                               
674900     .                                                                    
675000     EJECT                                                                
675100 S36-NY-FRAGA  SECTION.                                                   
675200                                                                          
675300     MOVE ZERO                     TO WS-IDPRQUES                         
675400     IF WS-IDPRQUES                = +0                                   
675500        MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                    
675600        MOVE +1                    TO PRNO-KDCALL                         
675700                                                                          
675800        CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                   
675900                                                                          
676000        MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                       
676100                                      WS-IDPRQUES                         
676200        MOVE +1                    TO PRQU-KDCALL                         
676300     END-IF                                                               
676400     MOVE RAD-IDDISTR              TO PRQU-IDDISTR                        
676500     MOVE RAD-IDKUNDNR             TO PRQU-IDKUNDNR                       
676600     MOVE RAD-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                  
676700     MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                  
676800     MOVE ZERO                     TO PRQU-IDORDER                        
676900     MOVE RAD-KDORDKL              TO PRQU-KDORDKL                        
677000     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
677100       AND RAD-DARODAT > 0                                                
677200       MOVE 4                      TO PRQU-KDORDKL                        
677300     END-IF                                                               
677400     MOVE 'Q'                      TO PRQU-KDPRSTA                        
677500     MOVE RAD-IDARTNR              TO PRQU-IDARTNR                        
677600     MOVE RAD-KVBEART-Q            TO PRQU-KVBEART-Q                      
677700     MOVE RAD-KDVALISO             TO PRQU-KDVALISO                       
677800     MOVE RAD-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                   
677900     MOVE +0                       TO PRQU-PRARTNTO-LOCPREL               
678000     MOVE RAD-IDSYSTEM             TO PRQU-IDSYSTEM                       
678100*        PERFORM IMS-GU-GMTA-WDB201                                       
678200                                                                          
678300     CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                    
678400                                         PRQU-WDC7-PCB                    
678500                                         PRQU-SJKO-WDK6-PCB               
678600     MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                       
678700                                       WS-IDPRQUES                        
678800     MOVE 'N'                      TO  RAD-FLPRTILL                       
678900                                                                          
679000     IF RAD-PRARTNTO-LOC = +0                                             
679100        MOVE PRQU-PRARTNTO-LOCPREL TO                                     
679200                       RAD-PRARTNTO-LOCPREL                               
679300     ELSE                                                                 
679400        MOVE RAD-PRARTNTO-LOC  TO RAD-PRARTNTO-LOCPREL                    
679500        MOVE ZERO              TO RAD-PRARTNTO-LOC                        
679600     END-IF                                                               
679700                                                                          
679800     MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                     
679900     MOVE +3                      TO PRNO-KDCALL                          
680000                                                                          
680100     CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                      
680200     .                                                                    
680300     EJECT                                                                
680400* --- IMS SEKTIONER ---                                                   
680500     SKIP3                                                                
680600 IMS-GU-MSG SECTION.                                                      
680700                                                                          
680800     MOVE '  QC' TO GODK-STATUSKODER                                      
680900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
681000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
681100     PERFORM IMS-STATUSKONTROLL                                           
681200     .                                                                    
681300     SKIP3                                                                
681400 IMS-ISRT-MSG-ALT1 SECTION.                                               
681500                                                                          
681600     MOVE SPACE TO GODK-STATUSKODER                                       
681700     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
681800     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
681900     PERFORM IMS-STATUSKONTROLL                                           
682000     .                                                                    
682100     SKIP3                                                                
682200 IMS-ISRT-MSG-ALT2 SECTION.                                               
682300                                                                          
682400     MOVE SPACE TO GODK-STATUSKODER                                       
682500     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
682600     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
682700     PERFORM IMS-STATUSKONTROLL                                           
682800     .                                                                    
682900     SKIP3                                                                
683000 IMS-ISRT-MSG-ALT3 SECTION.                                               
683100                                                                          
683200     MOVE SPACE TO GODK-STATUSKODER                                       
683300     CALL CBLTDLI USING ISRT ALT3-PCB P-TO-P-SW3                          
683400     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
683500     PERFORM IMS-STATUSKONTROLL                                           
683600     .                                                                    
683700     EJECT                                                                
683800 IMS-PURG-ALT5-MSG SECTION.                                               
683900     MOVE LOW-VALUE TO ALT5-Z1 ALT5-Z2                                    
684000     MOVE '  '  TO GODK-STATUSKODER                                       
684100     CALL CBLTDLI USING PURG ALT5-PCB ALT5-IO-AREA                        
684200     MOVE ALT5-STATUS-CODE TO STATUS-WS                                   
684300     PERFORM IMS-STATUSKONTROLL                                           
684400     .                                                                    
684500     SKIP2                                                                
684600 IMS-ISRT-MSG-ALT6 SECTION.                                               
684700                                                                          
684800     MOVE SPACE TO GODK-STATUSKODER                                       
684900     CALL CBLTDLI USING ISRT ALT6-PCB P-TO-P-SW4                          
685000     MOVE ALT6-STATUS-CODE TO STATUS-WS                                   
685100     PERFORM IMS-STATUSKONTROLL                                           
685200     .                                                                    
685300     EJECT                                                                
685400 IMS-ISRT-MSG-ALT7 SECTION.                                               
685500                                                                          
685600     MOVE SPACE TO GODK-STATUSKODER                                       
685700     CALL CBLTDLI USING ISRT ALT7-PCB P-TO-P-SW4                          
685800     MOVE ALT7-STATUS-CODE TO STATUS-WS                                   
685900     PERFORM IMS-STATUSKONTROLL                                           
686000     .                                                                    
686100     EJECT                                                                
686200 IMS-INSERT-4397-TRANS SECTION.                                           
686300                                                                          
686400     MOVE LOW-VALUE TO 4397-Z1 4397-Z2                                    
686500     MOVE SPACE TO GODK-STATUSKODER                                       
686600     CALL CBLTDLI USING ISRT 4397-PCB 4397-IO-AREA                        
686700     MOVE 4397-STATUS-CODE TO STATUS-WS                                   
686800     PERFORM IMS-STATUSKONTROLL                                           
686900     .                                                                    
687000     EJECT                                                                
687100 IMS-GHU-ORQA-WLORQA01 SECTION.                                           
687200                                                                          
687300     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
687400          DELIMITED BY SIZE INTO SSA1                                     
687500     MOVE '  GE' TO GODK-STATUSKODER                                      
687600     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
687700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
687800     PERFORM IMS-STATUSKONTROLL                                           
687900     .                                                                    
688000                                                                          
688100 IMS-GU-ORQA-STATUS    SECTION.                                           
688200                                                                          
688300     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
688400                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
688500                    '&KDODELST =' W-KDODELST     ')'                      
688600          DELIMITED BY SIZE INTO SSA1                                     
688700     MOVE '  GE' TO GODK-STATUSKODER                                      
688800     CALL CBLTDLI USING GU  ORQA-PCB DLI-IO-AREA SSA1                     
688900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
689000     PERFORM IMS-STATUSKONTROLL                                           
689100     .                                                                    
689200                                                                          
689300 IMS-REPL-ORQA-WLORQA01 SECTION.                                          
689400                                                                          
689500     MOVE '  ' TO GODK-STATUSKODER                                        
689600     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA                         
689700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
689800     PERFORM IMS-STATUSKONTROLL                                           
689900     .                                                                    
690000                                                                          
690100                                                                          
690200 IMS-ISRT-ORQA-WLORQA01 SECTION.                                          
690300                                                                          
690400     MOVE 'WLORQA01 ' TO SSA1                                             
690500     MOVE '  ' TO GODK-STATUSKODER                                        
690600     CALL CBLTDLI USING ISRT ORQA-PCB DLI-IO-AREA SSA1                    
690700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
690800     PERFORM IMS-STATUSKONTROLL                                           
690900     .                                                                    
691000     EJECT                                                                
691100 IMS-GU-WDQ201-CSEQ-GE   SECTION.                                         
691200                                                                          
691300     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                         
691400             DELIMITED BY SIZE INTO SSA1                                  
691500     MOVE    '  GE'              TO GODK-STATUSKODER                      
691600     CALL    CBLTDLI USING       GU   WDQ2-PCB DLI-IO-AREA11              
691700                                      SSA1                                
691800     MOVE    WDQ2-STATUS-CODE TO STATUS-WS                                
691900     PERFORM IMS-STATUSKONTROLL                                           
692000     .                                                                    
692100                                                                          
692200 IMS-GU-ORQI01-CSEQ SECTION.                                              
692300                                                                          
692400     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
692500             DELIMITED BY SIZE INTO SSA1                                  
692600     MOVE    '  '                TO GODK-STATUSKODER                      
692700     CALL    CBLTDLI USING       GU   ORQICSQ-PCB DLI-IO-AREA11           
692800                                      SSA1                                
692900     MOVE    ORQICSQ-STATUS-CODE TO STATUS-WS                             
693000     PERFORM IMS-STATUSKONTROLL                                           
693100     .                                                                    
693200                                                                          
693300 IMS-GHU-ORQI-WLORQI01-WLORQI12 SECTION.                                  
693400                                                                          
693500     STRING 'WLORQI01*D(IDORDER  =' W-IDORDER-X ')'                       
693600          DELIMITED BY SIZE INTO SSA1                                     
693700     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
693800          DELIMITED BY SIZE INTO SSA2                                     
693900     MOVE '  GE' TO GODK-STATUSKODER                                      
694000     CALL CBLTDLI USING GHU ORQI2-PCB DLI-IO-AREA1 SSA1 SSA2              
694100     MOVE ORQI2-STATUS-CODE TO STATUS-WS                                  
694200     PERFORM IMS-STATUSKONTROLL                                           
694300     .                                                                    
694400                                                                          
694500 IMS-REPL-ORQI-WLORQI12 SECTION.                                          
694600                                                                          
694700     MOVE '    ' TO GODK-STATUSKODER                                      
694800     CALL CBLTDLI USING REPL ORQI2-PCB DLI-IO-AREA1                       
694900     MOVE ORQI2-STATUS-CODE TO STATUS-WS                                  
695000     PERFORM IMS-STATUSKONTROLL                                           
695100     .                                                                    
695200     EJECT                                                                
695300 IMS-GU-WDQ212 SECTION.                                                   
695400                                                                          
695500     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
695600          DELIMITED BY SIZE INTO SSA1                                     
695700     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
695800          DELIMITED BY SIZE INTO SSA2                                     
695900     MOVE '  GE' TO GODK-STATUSKODER                                      
696000     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ212 SSA1 SSA2               
696100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
696200     PERFORM IMS-STATUSKONTROLL                                           
696300     .                                                                    
696400                                                                          
696500 IMS-GNP-WDQ221 SECTION.                                                  
696600                                                                          
696700     MOVE 'WLORQI21'    TO SSA1                                           
696800     MOVE '  GE' TO GODK-STATUSKODER                                      
696900     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-WDQ221 SSA1                   
697000     MOVE ORQI-STATUS-CODE TO STATUS-WS-Q221                              
697100                              STATUS-WS                                   
697200     PERFORM IMS-STATUSKONTROLL                                           
697300     .                                                                    
697400                                                                          
697500 IMS-GNP-WDQ221-ADLAG25 SECTION.                                          
697600                                                                          
697700     STRING 'WLORQI21*F(ADLAGOMR =' W-ADLAGOMR-X ')'                      
697800          DELIMITED BY SIZE INTO SSA1                                     
697900     MOVE '  GE' TO GODK-STATUSKODER                                      
698000     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-WDQ221 SSA1                   
698100     MOVE ORQI-STATUS-CODE TO STATUS-WS-Q221                              
698200                              STATUS-WS                                   
698300     PERFORM IMS-STATUSKONTROLL                                           
698400     .                                                                    
698500                                                                          
698600 IMS-GHNP-WDQ221-FIRST SECTION.                                           
698700                                                                          
698800     STRING 'WLORQI21*F(IDPRC    =' W-IDPRC-X ')'                         
698900          DELIMITED BY SIZE INTO SSA1                                     
699000     MOVE '  GE' TO GODK-STATUSKODER                                      
699100     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-WDQ221 SSA1                  
699200     MOVE ORQI-STATUS-CODE TO STATUS-WS-Q221                              
699300                              STATUS-WS                                   
699400     PERFORM IMS-STATUSKONTROLL                                           
699500     .                                                                    
699600                                                                          
699700 IMS-GHNP-WDQ221 SECTION.                                                 
699800                                                                          
699900     STRING 'WLORQI21(IDPRC    =' W-IDPRC-X ')'                           
700000          DELIMITED BY SIZE INTO SSA1                                     
700100     MOVE '  GE' TO GODK-STATUSKODER                                      
700200     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-WDQ221 SSA1                  
700300     MOVE ORQI-STATUS-CODE TO STATUS-WS-Q221                              
700400                              STATUS-WS                                   
700500     PERFORM IMS-STATUSKONTROLL                                           
700600     .                                                                    
700700                                                                          
700800 IMS-REPL-WDQ221 SECTION.                                                 
700900                                                                          
701000     MOVE '    ' TO GODK-STATUSKODER                                      
701100     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-WDQ221                       
701200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
701300     PERFORM IMS-STATUSKONTROLL                                           
701400     .                                                                    
701500                                                                          
701600 IMS-DLET-WDQ221 SECTION.                                                 
701700                                                                          
701800     MOVE '    ' TO GODK-STATUSKODER                                      
701900     CALL CBLTDLI USING DLET ORQI-PCB DLI-IO-WDQ221                       
702000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
702100     PERFORM IMS-STATUSKONTROLL                                           
702200     .                                                                    
702300                                                                          
702400 IMS-GHU-ORQF-WLORQF01 SECTION.                                           
702500                                                                          
702600     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
702700                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
702800          DELIMITED BY SIZE INTO SSA1                                     
702900     MOVE '  GE' TO GODK-STATUSKODER                                      
703000     CALL CBLTDLI USING GHU ORQF-PCB DLI-IO-AREA4 SSA1                    
703100     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
703200     PERFORM IMS-STATUSKONTROLL                                           
703300     .                                                                    
703400                                                                          
703500                                                                          
703600 IMS-GHN-ORQF-WLORQF01 SECTION.                                           
703700                                                                          
703800     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
703900                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
704000          DELIMITED BY SIZE INTO SSA1                                     
704100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
704200     CALL CBLTDLI USING GHN ORQF-PCB DLI-IO-AREA4 SSA1                    
704300     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
704400     PERFORM IMS-STATUSKONTROLL                                           
704500     .                                                                    
704600                                                                          
704700                                                                          
704800 IMS-ISRT-ORQF-WLORQF01 SECTION.                                          
704900                                                                          
705000     MOVE 'WLORQF01 ' TO SSA1                                             
705100     MOVE '  II' TO GODK-STATUSKODER                                      
705200     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA4 SSA1                   
705300     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
705400     PERFORM IMS-STATUSKONTROLL                                           
705500     .                                                                    
705600                                                                          
705700                                                                          
705800 IMS-DLET-ORQF-WLORQF01 SECTION.                                          
705900                                                                          
706000     MOVE '    ' TO GODK-STATUSKODER                                      
706100     CALL CBLTDLI USING DLET ORQF-PCB DLI-IO-AREA4                        
706200     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
706300     PERFORM IMS-STATUSKONTROLL                                           
706400     .                                                                    
706500     EJECT                                                                
706600 IMS-GU-XXKH-WLXXKH11 SECTION.                                            
706700                                                                          
706800     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
706900          DELIMITED BY SIZE INTO SSA1                                     
707000     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
707100          DELIMITED BY SIZE INTO SSA2                                     
707200     MOVE '  GE' TO GODK-STATUSKODER                                      
707300     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
707400     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
707500     PERFORM IMS-STATUSKONTROLL                                           
707600     .                                                                    
707700     EJECT                                                                
707800 IMS-GU-XXKL-WLXXKL11 SECTION.                                            
707900                                                                          
708000     STRING 'WLXXKL01(WDGXKEY  =' W-4453-IDHTYP-X ')'                     
708100          DELIMITED BY SIZE INTO SSA1                                     
708200     STRING 'WLXXKL11(KDSEGKEY =' W-4454-KDSEGKEY-X ')'                   
708300          DELIMITED BY SIZE INTO SSA2                                     
708400     MOVE '  GE' TO GODK-STATUSKODER                                      
708500     CALL CBLTDLI USING GU XXKL-PCB DLI-IO-AREA5 SSA1 SSA2                
708600     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
708700     PERFORM IMS-STATUSKONTROLL                                           
708800     .                                                                    
708900     EJECT                                                                
709000 IMS-GHU-4001-WL400101 SECTION.                                           
709100                                                                          
709200     STRING 'WL400101(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
709300          DELIMITED BY SIZE INTO SSA1                                     
709400     MOVE '    ' TO GODK-STATUSKODER                                      
709500     CALL CBLTDLI USING GHU 4001-PCB DLI-IO-AREA6 SSA1                    
709600     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
709700     PERFORM IMS-STATUSKONTROLL                                           
709800     .                                                                    
709900                                                                          
710000                                                                          
710100 IMS-GHU-4001-WL400111 SECTION.                                           
710200                                                                          
710300     STRING 'WL400101(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
710400          DELIMITED BY SIZE INTO SSA1                                     
710500     MOVE 'WL400111 ' TO SSA2                                             
710600     MOVE '    ' TO GODK-STATUSKODER                                      
710700     CALL CBLTDLI USING GHU 4001-PCB DLI-IO-AREA6 SSA1 SSA2               
710800     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
710900     PERFORM IMS-STATUSKONTROLL                                           
711000     .                                                                    
711100                                                                          
711200 IMS-REPL-4001-WL400111 SECTION.                                          
711300                                                                          
711400     MOVE '    ' TO GODK-STATUSKODER                                      
711500     CALL CBLTDLI USING REPL 4001-PCB DLI-IO-AREA6                        
711600     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
711700     PERFORM IMS-STATUSKONTROLL                                           
711800     .                                                                    
711900                                                                          
712000                                                                          
712100 IMS-DLET-4001-WL400101 SECTION.                                          
712200                                                                          
712300     MOVE '    ' TO GODK-STATUSKODER                                      
712400     CALL CBLTDLI USING DLET 4001-PCB DLI-IO-AREA6                        
712500     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
712600     PERFORM IMS-STATUSKONTROLL                                           
712700     .                                                                    
712800     EJECT                                                                
712900 IMS-GU-4003-WL400311 SECTION.                                            
713000                                                                          
713100     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
713200          DELIMITED BY SIZE INTO SSA1                                     
713300     MOVE 'WL400311 ' TO SSA2                                             
713400     MOVE '  GE' TO GODK-STATUSKODER                                      
713500     CALL CBLTDLI USING GU 4003-PCB DLI-IO-AREA7 SSA1 SSA2                
713600     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
713700     PERFORM IMS-STATUSKONTROLL                                           
713800     .                                                                    
713900                                                                          
714000 IMS-GHU-4003-WL400311 SECTION.                                           
714100                                                                          
714200     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
714300          DELIMITED BY SIZE INTO SSA1                                     
714400     MOVE 'WL400311 ' TO SSA2                                             
714500     MOVE '    ' TO GODK-STATUSKODER                                      
714600     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-AREA7 SSA1 SSA2               
714700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
714800     PERFORM IMS-STATUSKONTROLL                                           
714900     .                                                                    
715000                                                                          
715100 IMS-REPL-4003-WL400311 SECTION.                                          
715200                                                                          
715300     MOVE '    ' TO GODK-STATUSKODER                                      
715400     CALL CBLTDLI USING REPL 4003-PCB DLI-IO-AREA7                        
715500     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
715600     PERFORM IMS-STATUSKONTROLL                                           
715700     .                                                                    
715800                                                                          
715900 IMS-ISRT-4003-WL400301 SECTION.                                          
716000                                                                          
716100     MOVE 'WL400301 ' TO SSA1                                             
716200     MOVE '    ' TO GODK-STATUSKODER                                      
716300     CALL CBLTDLI USING ISRT 4003-PCB DLI-IO-AREA8 SSA1                   
716400     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
716500     PERFORM IMS-STATUSKONTROLL                                           
716600     .                                                                    
716700                                                                          
716800 IMS-ISRT-4003-WL400311 SECTION.                                          
716900                                                                          
717000     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
717100          DELIMITED BY SIZE INTO SSA1                                     
717200     MOVE 'WL400311 ' TO SSA2                                             
717300     MOVE '    ' TO GODK-STATUSKODER                                      
717400     CALL CBLTDLI USING ISRT 4003-PCB DLI-IO-AREA7 SSA1 SSA2              
717500     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
717600     PERFORM IMS-STATUSKONTROLL                                           
717700     .                                                                    
717800                                                                          
717900 IMS-ISRT-4003-WL400321 SECTION.                                          
718000                                                                          
718100     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
718200          DELIMITED BY SIZE INTO SSA1                                     
718300     MOVE 'WL400311 ' TO SSA2                                             
718400     MOVE 'WL400321 ' TO SSA3                                             
718500     MOVE '  II' TO GODK-STATUSKODER                                      
718600     CALL CBLTDLI USING ISRT 4003-PCB DLI-IO-AREA8 SSA1 SSA2 SSA3         
718700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
718800     PERFORM IMS-STATUSKONTROLL                                           
718900     .                                                                    
719000     EJECT                                                                
719100 IMS-GU-4007-WL400711 SECTION.                                            
719200                                                                          
719300     STRING 'WL400701(WDGXKEY  =' W-4007-PU-IDHTYP-X ')'                  
719400          DELIMITED BY SIZE INTO SSA1                                     
719500     MOVE 'WL400711 ' TO SSA2                                             
719600     MOVE '  GE' TO GODK-STATUSKODER                                      
719700     CALL CBLTDLI USING GU 4007-PCB DLI-IO-AREA9 SSA1 SSA2                
719800     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
719900     PERFORM IMS-STATUSKONTROLL                                           
720000     .                                                                    
720100                                                                          
720200 IMS-ISRT-4007-WL400701 SECTION.                                          
720300                                                                          
720400     MOVE 'WL400701 ' TO SSA1                                             
720500     MOVE '    ' TO GODK-STATUSKODER                                      
720600     CALL CBLTDLI USING ISRT 4007-PCB DLI-IO-AREA10 SSA1                  
720700     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
720800     PERFORM IMS-STATUSKONTROLL                                           
720900     .                                                                    
721000                                                                          
721100 IMS-ISRT-4007-WL400711 SECTION.                                          
721200                                                                          
721300     STRING 'WL400701(WDGXKEY  =' W-4007-PU-IDHTYP-X ')'                  
721400          DELIMITED BY SIZE INTO SSA1                                     
721500     MOVE 'WL400711 ' TO SSA2                                             
721600     MOVE '    ' TO GODK-STATUSKODER                                      
721700     CALL CBLTDLI USING ISRT 4007-PCB DLI-IO-AREA9 SSA1 SSA2              
721800     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
721900     PERFORM IMS-STATUSKONTROLL                                           
722000     .                                                                    
722100                                                                          
722200 IMS-ISRT-4007-WL400721 SECTION.                                          
722300                                                                          
722400     STRING 'WL400701(WDGXKEY  =' W-4007-PU-IDHTYP-X ')'                  
722500          DELIMITED BY SIZE INTO SSA1                                     
722600     MOVE 'WL400711 ' TO SSA2                                             
722700     MOVE 'WL400721 ' TO SSA3                                             
722800     MOVE '  II' TO GODK-STATUSKODER                                      
722900     CALL CBLTDLI USING ISRT 4007-PCB DLI-IO-AREA10 SSA1 SSA2 SSA3        
723000     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
723100     PERFORM IMS-STATUSKONTROLL                                           
723200     .                                                                    
723300     EJECT                                                                
723400                                                                          
723500 IMS-GHU-WDGX4017 SECTION.                                                
723600                                                                          
723700     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
723800          DELIMITED BY SIZE INTO SSA1                                     
723900     MOVE '  GE' TO GODK-STATUSKODER                                      
724000     CALL CBLTDLI USING GHU 4017-PCB 4017-WDGX4017 SSA1                   
724100     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
724200     PERFORM IMS-STATUSKONTROLL                                           
724300     .                                                                    
724400                                                                          
724500 IMS-GHU-WDGX4018 SECTION.                                                
724600                                                                          
724700     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
724800          DELIMITED BY SIZE INTO SSA1                                     
724900     STRING 'WDGX4018(KDSEGKEY =' W-4018-KDSEGKEY-X ')'                   
725000          DELIMITED BY SIZE INTO SSA2                                     
725100     MOVE '  GE' TO GODK-STATUSKODER                                      
725200     CALL CBLTDLI USING GHU 4017-PCB 4018-WDGX4018 SSA1 SSA2              
725300     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
725400     PERFORM IMS-STATUSKONTROLL                                           
725500     .                                                                    
725600                                                                          
725700 IMS-REPL-WDGX4018 SECTION.                                               
725800                                                                          
725900     MOVE '    ' TO GODK-STATUSKODER                                      
726000     CALL CBLTDLI USING REPL 4017-PCB 4018-WDGX4018                       
726100     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
726200     PERFORM IMS-STATUSKONTROLL                                           
726300     .                                                                    
726400                                                                          
726500 IMS-ISRT-WDGX4017 SECTION.                                               
726600                                                                          
726700     MOVE 'WDR401   '  TO SSA1                                            
726800     MOVE '    ' TO GODK-STATUSKODER                                      
726900     CALL CBLTDLI USING ISRT 4017-PCB 4017-WDGX4017 SSA1                  
727000     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
727100     PERFORM IMS-STATUSKONTROLL                                           
727200     .                                                                    
727300                                                                          
727400 IMS-ISRT-WDGX4018 SECTION.                                               
727500                                                                          
727600     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
727700          DELIMITED BY SIZE INTO SSA1                                     
727800     MOVE 'WDGX4018 ' TO SSA2                                             
727900     MOVE '    ' TO GODK-STATUSKODER                                      
728000     CALL CBLTDLI USING ISRT 4017-PCB 4018-WDGX4018 SSA1 SSA2             
728100     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
728200     PERFORM IMS-STATUSKONTROLL                                           
728300     .                                                                    
728400                                                                          
728500 IMS-GU-ORQM-WLORQM01 SECTION.                                            
728600                                                                          
728700     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
728800                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
728900             DELIMITED BY SIZE INTO SSA1                                  
729000     MOVE '  GE' TO GODK-STATUSKODER                                      
729100     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
729200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
729300     PERFORM IMS-STATUSKONTROLL                                           
729400     .                                                                    
729500                                                                          
729600                                                                          
729700 IMS-ISRT-ORQM-WLORQM01 SECTION.                                          
729800                                                                          
729900     MOVE 'WLORQM01 ' TO SSA1                                             
730000     MOVE '  II' TO GODK-STATUSKODER                                      
730100     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA SSA1                    
730200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
730300     PERFORM IMS-STATUSKONTROLL                                           
730400     .                                                                    
730500     EJECT                                                                
730600 IMS-GHU-ORDP-WLORDP01 SECTION.                                           
730700                                                                          
730800     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
730900          DELIMITED BY SIZE INTO SSA1                                     
731000     MOVE '  GE' TO GODK-STATUSKODER                                      
731100     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA SSA1                     
731200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
731300     PERFORM IMS-STATUSKONTROLL                                           
731400     .                                                                    
731500                                                                          
731600                                                                          
731700 IMS-REPL-ORDP-WLORDP01 SECTION.                                          
731800                                                                          
731900     MOVE '    ' TO GODK-STATUSKODER                                      
732000     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA                         
732100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
732200     PERFORM IMS-STATUSKONTROLL                                           
732300     .                                                                    
732400                                                                          
732500                                                                          
732600 IMS-ISRT-ORDP-WLORDP01 SECTION.                                          
732700                                                                          
732800     MOVE 'WLORDP01 ' TO SSA1                                             
732900     MOVE '  II' TO GODK-STATUSKODER                                      
733000     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
733100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
733200     PERFORM IMS-STATUSKONTROLL                                           
733300     .                                                                    
733400     EJECT                                                                
733500 IMS-GHU-WDK711 SECTION.                                                  
733600                                                                          
733700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
733800          DELIMITED BY SIZE INTO SSA1                                     
733900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
734000          DELIMITED BY SIZE INTO SSA2                                     
734100     MOVE '  ' TO GODK-STATUSKODER                                        
734200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA13 SSA1 SSA2              
734300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
734400     PERFORM IMS-STATUSKONTROLL                                           
734500     .                                                                    
734600     EJECT                                                                
734700 IMS-GU-WDK711-GE SECTION.                                                
734800                                                                          
734900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
735000          DELIMITED BY SIZE INTO SSA1                                     
735100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
735200          DELIMITED BY SIZE INTO SSA2                                     
735300     MOVE '  GE' TO GODK-STATUSKODER                                      
735400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA13 SSA1 SSA2               
735500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
735600     PERFORM IMS-STATUSKONTROLL                                           
735700     .                                                                    
735800 IMS-GU-WDK711 SECTION.                                                   
735900                                                                          
736000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
736100          DELIMITED BY SIZE INTO SSA1                                     
736200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
736300          DELIMITED BY SIZE INTO SSA2                                     
736400     MOVE '  ' TO GODK-STATUSKODER                                        
736500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA13 SSA1 SSA2               
736600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
736700     PERFORM IMS-STATUSKONTROLL                                           
736800     .                                                                    
736900 IMS-GU-WDK712 SECTION.                                                   
737000                                                                          
737100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
737200          DELIMITED BY SIZE INTO SSA1                                     
737300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
737400          DELIMITED BY SIZE INTO SSA2                                     
737500     MOVE '  GE' TO GODK-STATUSKODER                                      
737600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
737700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
737800     PERFORM IMS-STATUSKONTROLL                                           
737900     .                                                                    
738000 IMS-GU-WDK722 SECTION.                                                   
738100                                                                          
738200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
738300          DELIMITED BY SIZE INTO SSA1                                     
738400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
738500          DELIMITED BY SIZE INTO SSA2                                     
738600     MOVE 'WDK722 '           TO SSA3                                     
738700     MOVE '  GE' TO GODK-STATUSKODER                                      
738800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-wdk722 SSA1 SSA2 ssa3          
738900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
739000     PERFORM IMS-STATUSKONTROLL                                           
739100     .                                                                    
739200 IMS-REPL-WDK7 SECTION.                                                   
739300                                                                          
739400     MOVE '    ' TO GODK-STATUSKODER                                      
739500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA13                       
739600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
739700     PERFORM IMS-STATUSKONTROLL                                           
739800     .                                                                    
739900 IMS-GHU-WDK611 SECTION.                                                  
740000                                                                          
740100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
740200          DELIMITED BY SIZE INTO SSA1                                     
740300     STRING 'WDK611  (KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
740400          DELIMITED BY SIZE INTO SSA2                                     
740500     MOVE '  GE' TO GODK-STATUSKODER                                      
740600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA SSA1 SSA2                
740700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
740800     PERFORM IMS-STATUSKONTROLL                                           
740900     .                                                                    
741000 IMS-GHNP-WDK627 SECTION.                                                 
741100                                                                          
741200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
741300          DELIMITED BY SIZE INTO SSA1                                     
741400     STRING 'WDK611  (KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
741500          DELIMITED BY SIZE INTO SSA2                                     
741600     MOVE 'WDK627   ' TO SSA3                                             
741700     MOVE '  GE' TO GODK-STATUSKODER                                      
741800     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
741900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
742000     PERFORM IMS-STATUSKONTROLL                                           
742100     .                                                                    
742200 IMS-REPL-WDK6 SECTION.                                                   
742300                                                                          
742400     MOVE '    ' TO GODK-STATUSKODER                                      
742500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA                         
742600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
742700     PERFORM IMS-STATUSKONTROLL                                           
742800     .                                                                    
742900 IMS-ISRT-WDK627 SECTION.                                                 
743000                                                                          
743100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
743200          DELIMITED BY SIZE INTO SSA1                                     
743300     STRING 'WDK611  (KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
743400          DELIMITED BY SIZE INTO SSA2                                     
743500     MOVE 'WDK627   ' TO SSA3                                             
743600     MOVE '    ' TO GODK-STATUSKODER                                      
743700     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
743800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
743900     PERFORM IMS-STATUSKONTROLL                                           
744000     .                                                                    
744100     EJECT                                                                
744200 IMS-GU-ARTN-WLARTN01 SECTION.                                            
744300                                                                          
744400     STRING 'WLARTN01(IDARTNR  =' W-ART-IDARTNR-X ')'                     
744500          DELIMITED BY SIZE INTO SSA1                                     
744600     MOVE '  GE' TO GODK-STATUSKODER                                      
744700     CALL CBLTDLI USING GU ARTN-PCB DLI-IO-AREA12 SSA1                    
744800     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
744900     PERFORM IMS-STATUSKONTROLL                                           
745000     .                                                                    
745100 IMS-GU-XXJN-WLXXJN11 SECTION.                                            
745200                                                                          
745300     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
745400          DELIMITED BY SIZE INTO SSA1                                     
745500     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
745600                    '&KDORDKL  =' W-4512-KDORDKL-X                        
745700                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
745800                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
745900          DELIMITED BY SIZE INTO SSA2                                     
746000     MOVE '  GE' TO GODK-STATUSKODER                                      
746100     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA SSA1 SSA2                 
746200     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
746300     PERFORM IMS-STATUSKONTROLL                                           
746400     .                                                                    
746500     EJECT                                                                
746600 IMS-GU-BENA-WLBENA11 SECTION.                                            
746700                                                                          
746800     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
746900          DELIMITED BY SIZE INTO SSA1                                     
747000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
747100          DELIMITED BY SIZE INTO SSA2                                     
747200     MOVE '  GE' TO GODK-STATUSKODER                                      
747300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
747400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
747500     PERFORM IMS-STATUSKONTROLL                                           
747600     .                                                                    
747700     EJECT                                                                
747800 IMS-GU-XXKU-WLXXKU11 SECTION.                                            
747900                                                                          
748000     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
748100          DELIMITED BY SIZE INTO SSA1                                     
748200     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
748300          DELIMITED BY SIZE INTO SSA2                                     
748400     MOVE '  GE' TO GODK-STATUSKODER                                      
748500     CALL CBLTDLI USING GU XXKU-PCB DLI-IO-AREA SSA1 SSA2                 
748600     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
748700     PERFORM IMS-STATUSKONTROLL                                           
748800     .                                                                    
748900     EJECT                                                                
749000 IMS-ISRT-ZZAC-WLZZAC01 SECTION.                                          
749100                                                                          
749200     MOVE 'WLZZAC01 ' TO SSA1                                             
749300     MOVE '  II' TO GODK-STATUSKODER                                      
749400     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
749500     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
749600     PERFORM IMS-STATUSKONTROLL                                           
749700     .                                                                    
749800     EJECT                                                                
749900 IMS-GU-WDM211 SECTION.                                                   
750000                                                                          
750100     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
750200          DELIMITED BY SIZE INTO SSA1                                     
750300     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
750400          DELIMITED BY SIZE INTO SSA2                                     
750500     MOVE '  GE'              TO GODK-STATUSKODER                         
750600     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
750700     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
750800     PERFORM IMS-STATUSKONTROLL                                           
750900     .                                                                    
751000                                                                          
751100 IMS-GNP-WDM221 SECTION.                                                  
751200                                                                          
751300     MOVE 'WDM221 '           TO SSA1                                     
751400     MOVE '    GE'            TO GODK-STATUSKODER                         
751500     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
751600     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
751700     PERFORM IMS-STATUSKONTROLL                                           
751800     .                                                                    
751900                                                                          
752000 IMS-GHU-WDM211 SECTION.                                                  
752100                                                                          
752200     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
752300          DELIMITED BY SIZE INTO SSA1                                     
752400     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
752500          DELIMITED BY SIZE INTO SSA2                                     
752600     MOVE '  GE'              TO GODK-STATUSKODER                         
752700     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
752800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
752900     PERFORM IMS-STATUSKONTROLL                                           
753000     .                                                                    
753100                                                                          
753200 IMS-REPL-WDM211 SECTION.                                                 
753300                                                                          
753400     MOVE '  '             TO GODK-STATUSKODER                            
753500     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
753600     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
753700     PERFORM IMS-STATUSKONTROLL                                           
753800     .                                                                    
753900                                                                          
754000 IMS-GHU-WDM221 SECTION.                                                  
754100                                                                          
754200     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
754300          DELIMITED BY SIZE INTO SSA1                                     
754400     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
754500          DELIMITED BY SIZE INTO SSA2                                     
754600     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
754700          DELIMITED BY SIZE INTO SSA3                                     
754800     MOVE '  GE' TO GODK-STATUSKODER                                      
754900     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
755000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
755100     PERFORM IMS-STATUSKONTROLL                                           
755200     .                                                                    
755300                                                                          
755400 IMS-REPL-WDM221 SECTION.                                                 
755500                                                                          
755600     MOVE '  '             TO GODK-STATUSKODER                            
755700     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
755800     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
755900     PERFORM IMS-STATUSKONTROLL                                           
756000     .                                                                    
756100     EJECT                                                                
756200 IMS-ISRT-4541-WL454111 SECTION.                                          
756300                                                                          
756400     STRING 'WDR401  (WDGXKEY  =' W-4541-IDHTYP-X ')'                     
756500          DELIMITED BY SIZE INTO SSA1                                     
756600     MOVE 'WDGX4542 ' TO SSA2                                             
756700     MOVE '  II' TO GODK-STATUSKODER                                      
756800     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA SSA1 SSA2               
756900     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
757000     PERFORM IMS-STATUSKONTROLL                                           
757100     .                                                                    
757200     EJECT                                                                
757300 IMS-GHU-ARTM-WLARTM01 SECTION.                                           
757400                                                                          
757500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
757600          DELIMITED BY SIZE INTO SSA1                                     
757700     MOVE '  GE' TO GODK-STATUSKODER                                      
757800     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA SSA1                     
757900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
758000     PERFORM IMS-STATUSKONTROLL                                           
758100     .                                                                    
758200     EJECT                                                                
758300 IMS-REPL-ARTM-WLARTM01 SECTION.                                          
758400                                                                          
758500     MOVE '    ' TO GODK-STATUSKODER                                      
758600     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA                         
758700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
758800     PERFORM IMS-STATUSKONTROLL                                           
758900     .                                                                    
759000 IMS-GU-GMTA-WDB201       SECTION.                                        
759100                                                                          
759200     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
759300                      DELIMITED BY SIZE INTO SSA1                         
759400     MOVE '  ' TO GODK-STATUSKODER                                        
759500     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB201 SSA1               
759600     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
759700     PERFORM IMS-STATUSKONTROLL                                           
759800     .                                                                    
759900     SKIP3                                                                
760000 IMS-ISRT-WDL901 SECTION.                                                 
760100                                                                          
760200     MOVE 'WLLOGA01 ' TO SSA1                                             
760300     MOVE '  II' TO GODK-STATUSKODER                                      
760400     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
760500     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
760600     PERFORM IMS-STATUSKONTROLL                                           
760700     .                                                                    
760800     EJECT                                                                
760900 IMS-GU-WDK601                 SECTION.                                   
761000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
761100            DELIMITED BY SIZE INTO SSA1                                   
761200     MOVE '  '                   TO GODK-STATUSKODER                      
761300     CALL  CBLTDLI  USING GU   WDK6-2-PCB DLI-IO-AREA18 SSA1              
761400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
761500     PERFORM IMS-STATUSKONTROLL                                           
761600     .                                                                    
761700     SKIP2                                                                
761800 IMS-GHNP-WDK611               SECTION.                                   
761900     MOVE 'WDK611  '           TO SSA1                                    
762000     MOVE '  '                 TO GODK-STATUSKODER                        
762100     CALL  CBLTDLI  USING GHNP WDK6-2-PCB DLI-IO-AREA  SSA1               
762200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
762300     PERFORM IMS-STATUSKONTROLL                                           
762400     .                                                                    
762500     SKIP2                                                                
762600 IMS-REPL-WDK611               SECTION.                                   
762700     MOVE 'WDK611  '           TO SSA1                                    
762800     MOVE '    '               TO GODK-STATUSKODER                        
762900     CALL  CBLTDLI  USING REPL WDK6-2-PCB DLI-IO-AREA  SSA1               
763000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
763100     PERFORM IMS-STATUSKONTROLL                                           
763200     .                                                                    
763300     SKIP2                                                                
763400 IMS-GU-SEQB-WDA601 SECTION.                                              
763500     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
763600                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
763700            DELIMITED BY SIZE INTO SSA1                                   
763800     MOVE '  GE'                 TO GODK-STATUSKODER                      
763900     CALL  CBLTDLI  USING GU   WDA6B-PCB DLI-IO-AREA17 SSA1               
764000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
764100     PERFORM IMS-STATUSKONTROLL                                           
764200     .                                                                    
764300     SKIP2                                                                
764400 IMS-GHU-SEQB-WDA601            SECTION.                                  
764500     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
764600                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
764700            DELIMITED BY SIZE INTO SSA1                                   
764800     MOVE '  GE'                 TO GODK-STATUSKODER                      
764900     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-AREA17 SSA1              
765000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
765100     PERFORM IMS-STATUSKONTROLL                                           
765200     .                                                                    
765300     SKIP2                                                                
765400 IMS-GHN-SEQB-WDA601            SECTION.                                  
765500     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
765600                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
765700            DELIMITED BY SIZE INTO SSA1                                   
765800     MOVE '  GEGB'               TO GODK-STATUSKODER                      
765900     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA17 SSA1              
766000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
766100     PERFORM IMS-STATUSKONTROLL                                           
766200     .                                                                    
766300     SKIP2                                                                
766400 IMS-REPL-SEQB-WDA601                 SECTION.                            
766500     MOVE 'WDA601  '           TO SSA1                                    
766600     MOVE '    '               TO GODK-STATUSKODER                        
766700     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA17 SSA1               
766800     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
766900     PERFORM IMS-STATUSKONTROLL                                           
767000     .                                                                    
767100     EJECT                                                                
767200 IMS-ISRT-WDA601            SECTION.                                      
767300     MOVE   'WDA601  '         TO SSA1                                    
767400     MOVE '  IINI' TO GODK-STATUSKODER                                    
767500     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-AREA17 SSA1                
767600     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
767700     PERFORM IMS-STATUSKONTROLL                                           
767800     .                                                                    
767900     EJECT                                                                
768000 IMS-GET-WDB201-UNIK SECTION.                                             
768100                                                                          
768200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
768300          DELIMITED BY SIZE INTO SSA1                                     
768400     MOVE '  GE'               TO GODK-STATUSKODER                        
768500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
768600     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
768700     PERFORM IMS-STATUSKONTROLL                                           
768800     .                                                                    
768900     SKIP2                                                                
769000                                                                          
769100 IMS-GU-WDB201 SECTION.                                                   
769200                                                                          
769300     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
769400                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
769500            DELIMITED BY SIZE INTO SSA1                                   
769600     MOVE '  GE'               TO GODK-STATUSKODER                        
769700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
769800     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
769900     PERFORM IMS-STATUSKONTROLL                                           
770000     .                                                                    
770100     SKIP2                                                                
770200 IMS-GU-WDB101 SECTION.                                                   
770300                                                                          
770400     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
770500          DELIMITED BY SIZE INTO SSA1                                     
770600     MOVE '  GE'               TO GODK-STATUSKODER                        
770700     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
770800     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
770900     PERFORM IMS-STATUSKONTROLL                                           
771000     .                                                                    
771100                                                                          
771200 IMS-GU-WDB601    SECTION.                                                
771300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
771400          DELIMITED BY SIZE INTO SSA1                                     
771500     MOVE '  GE' TO GODK-STATUSKODER                                      
771600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
771700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
771800     PERFORM IMS-STATUSKONTROLL                                           
771900     .                                                                    
772000                                                                          
772100 IMS-GU-WDP4A1 SECTION.                                                   
772200                                                                          
772300     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
772400                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
772500          DELIMITED BY SIZE INTO SSA1                                     
772600     MOVE '  GE' TO GODK-STATUSKODER                                      
772700     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
772800                           SSA1                                           
772900     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
773000     PERFORM IMS-STATUSKONTROLL                                           
773100     .                                                                    
773200                                                                          
773300     EJECT                                                                
773400 IMS-GU-WDA501 SECTION.                                                   
773500                                                                          
773600     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
773700                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
773800                    '&KDORDKL  =' W-KDORDKL-X                             
773900                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
774000          DELIMITED BY SIZE INTO SSA1                                     
774100     MOVE '  GE'              TO GODK-STATUSKODER                         
774200     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-AREA  SSA1                     
774300     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
774400     PERFORM IMS-STATUSKONTROLL                                           
774500     .                                                                    
774600 DB2-SELECT-TP4TRAN     SECTION.                                          
774700     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
774800                                                                          
774900     MOVE 000100 TO GODK-SQLCODEKODER                                     
775000                                                                          
775100     EXEC SQL                                                             
775200           SELECT  DISTINCT                                               
775300                   IDDC_REC                                               
775400                                                                          
775500           INTO   :TP4TRAN-IDDC-REC                                       
775600                                                                          
775700           FROM    TP4TRAN                                                
775800                                                                          
775900           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
776000     END-EXEC                                                             
776100                                                                          
776200     MOVE SQLCODE TO SQLCODE-WS                                           
776300     PERFORM DB2-STATUSKONTROLL                                           
776400     .                                                                    
776500     EJECT                                                                
776600                                                                          
776700 IMS-STATUSKONTROLL SECTION.                                              
776800                                                                          
776900     SET STATUS-IX TO 1                                                   
777000     SEARCH GODK-STATUS                                                   
777100       AT END                                                             
777200         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
777300         DELIMITED BY SIZE INTO FELTEXT                                   
777400         CALL FELLOG                                                      
777500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
777600     END-SEARCH                                                           
777700     .                                                                    
777800     EJECT                                                                
777900 DB2-STATUSKONTROLL  SECTION.                                             
778000                                                                          
778100     SET SQLCODE-IX TO 1                                                  
778200     SEARCH GODK-SQLCODE                                                  
778300       AT END                                                             
778400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
778500          DELIMITED BY SIZE INTO FELTEXT                                  
778600          CALL ABEND USING RKOD-ABEND-DB2                                 
778700       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
778800     END-SEARCH                                                           
778900     .                                                                    
779000     EJECT                                                                
779100*    -COPY WY2000P4                                                       
