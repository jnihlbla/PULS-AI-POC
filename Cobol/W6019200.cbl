000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019200.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/03/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR R31-KOPPLINGAR FRÅN INLEVERANSSYSTEMET                
000900*        TILL ANDRA SYSTEM.                                               
001000*                                                                         
001100*                                                                         
001200*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001300*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001400*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001500*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
001600*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
001700*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
001800*        PROGRAMMET UPPDATERAR WLXXCS (WDG3)                              
001900*        PROGRAMMET UPPDATERAR WLXXCT (WDG3)                              
002000*        PROGRAMMET UPPDATERAR WLFILC (WDR3)                              
002100*        PROGRAMMET UPPDATERAR WLFILB (WDR8)                              
002200*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
002300*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ WLLOGA (WDL9)            
002400*        PROGRAMMET UPPDATERAR WDA9                                       
002500*        PROGRAMMET UPPDATERAR WDD4                                       
002600*        PROGRAMMET LÄSER      WDB2   (VIA WDB2BSEQ)                      
002700*        PROGRAMMET LÄSER      WLXXBX (WDR2)                              
002800*        PROGRAMMET LÄSER      WDF1                                       
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSAKTION: W6T192X                                             
003200*        MID:         W6I19201 + WMSGKOM                                  
003300*                                                                         
003400*    UTDATA:                                                              
003500*        TRANSAKTION: W6T192X                                             
003600*        MOD:         W6I19201 + WMSGKOM                                  
003700*        TRANSAKTION: W0T693X                                             
003800*        MOD:         WMSGKOM                                             
003900*                                                                         
004000*    E-TRACKER: 10143271 - CHINA WAREHOUSE PROJECT-1                      
004100*    E-TRACKER: 10143273 - 2012-09  LOCAL SOURCING CHINA                  
004200*    JIRA: 2290 - ABEND W60192                                            
004300*                                                                         
004400                                                                          
004500     SKIP3                                                                
004600 ENVIRONMENT DIVISION.                                                    
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900 WORKING-STORAGE SECTION.                                                 
005000*    -COPY WY2000W3                                                       
005100     SKIP3                                                                
005200 77  IDPGM                       PIC X(08)   VALUE 'W6019200'.            
005300                                                                          
005400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005600                                                                          
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
006000 77  CURRENT-IMS-SECTION         PIC X(24)   VALUE SPACE.                 
006100 77  WS-SPAR-DAREGDAT            PIC 9(8)    VALUE ZERO.                  
006200 77  WS-SPAR-TIREGTID            PIC S9(7)   VALUE ZERO COMP-3.           
006300 77  WS-SPAR-IDDC                PIC X(2)    VALUE SPACE.                 
006400 01  W1-DAREGDAT                 PIC 9(08).                               
006500 01  W1-DAREGDAT-9KOMPL          PIC 9(08).                               
006600 77  W1-TIKLOCK                  PIC 9(09).                               
006700 77  W1-TIKLOCK-9KOMPL           PIC 9(09)   COMP-3.                      
006800 77  WS-SAP-MM-POST              PIC X(1)    VALUE SPACE.                 
006900 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
007000 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007100 77  WS-KDVALISO-HUV-CN          PIC X(3)    VALUE 'CNY'.                 
007200 77  WS-KDVALISO-HUV-US          PIC X(3)    VALUE 'USD'.                 
007300 01  W-PRKURS                    PIC S9(5)V9(5) VALUE +0   COMP-3.        
007400 01  W-REVALUTA                  PIC S9(5)      VALUE +0   COMP-3.        
007500 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
007600 01  WS-SUDIRLON                 PIC S9(7)V9(2) VALUE +0 COMP-3.          
007700 01  WS-SUDIRMTRL                PIC S9(7)V9(2) VALUE +0 COMP-3.          
007800 01  WS-SUHEMT                   PIC S9(7)V9(2) VALUE +0 COMP-3.          
007900 01  WS-SUARTSTD                 PIC S9(9)V9(2) VALUE +0 COMP-3.          
008000 01  SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
008100 01  WS-PRKURS                   PIC S9(5)V9(5) VALUE +0   COMP-3.        
008200 01  WS-REVALUTA                 PIC S9(5)      VALUE +0   COMP-3.        
008300                                                                          
008400 01  WS-IDDC-LOCAL.                                                       
008500     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
008600     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
008700     03  FILLER                  PIC X(1)   VALUE SPACE.                  
008800                                                                          
008900 01  WS-LOKALTID.                                                         
009000     03  WS-LOKAL-SEKEL          PIC X(1).                                
009100     03  FILLER                  PIC X(5).                                
009200                                                                          
009300 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
009400 77  WS-SAP-IDLOPNRM             PIC 9(9)    VALUE ZERO.                  
009500 77  WS-SAP-X-IDLOPNRM           PIC X(9)    VALUE SPACE.                 
009600 77  WS-IDANSK-WDK722            PIC S9(3)   VALUE ZERO COMP-3.           
009700                                                                          
009800*   PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                      
009900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +44   COMP SYNC.        
010000                                                                          
010100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010200     88  EGEN-MID                            VALUE '6192'.                
010300     88  GODK-MID                            VALUE '6192'                 
010400                                                   '6115'.                
010500 01  AVROP-SW                    PIC X(1)    VALUE 'N'.                   
010600     88  AVROP-SAKNAS                        VALUE 'N'.                   
010700                                                                          
010800 01  AVROP-LOKAL-SW              PIC X(1)    VALUE 'N'.                   
010900     88  AVROP-LOKAL-SAKNAS                  VALUE 'N'.                   
011000                                                                          
011100 77  ORDER-FINNS-SW              PIC X(1)    VALUE 'N'.                   
011200     88  ORDER-SAKNAS                        VALUE 'N'.                   
011300     88  ORDER-FINNS                         VALUE 'J'.                   
011400                                                                          
011500 77  BOKAT-KLART-SW              PIC X(1)    VALUE 'N'.                   
011600     88  BOKAT-KLART                         VALUE 'J'.                   
011700     88  INTE-BOKAT-KLART                    VALUE 'N'.                   
011800                                                                          
011900 77  PRIS-FINNS-SW               PIC X(1)    VALUE 'N'.                   
012000     88  PRIS-FINNS                          VALUE 'J'.                   
012100     88  PRIS-SAKNAS                         VALUE 'N'.                   
012200                                                                          
012300 77  SW-WDF1-FINNS               PIC X(1)    VALUE SPACE.                 
012400     88  WDF1-FINNS                          VALUE 'J'.                   
012500                                                                          
012600 01  W-KDINL                     PIC X(3)    VALUE SPACE.                 
012700     EJECT                                                                
012800*      -COPY W200EMAB                                                     
012900     EJECT                                                                
013000 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
013100 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
013200*01  FILLER  -COPY WWBYT01     -RED TEST-IDARTNR.                         
013300     EJECT                                                                
013400                                                                          
013500*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
013600     EJECT                                                                
013700                                                                          
013800*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
013900     EJECT                                                                
014000                                                                          
014100*      --- VALID IDDC CODES                                               
014200*                                                                         
014300*01    -COPY WWDCLAND                                                     
014400*01    -COPY WWDC99                                                       
014500*01    -COPY WWDCKONS                                                     
014600       EJECT                                                              
014700*01    -COPY WWPRODSL                                                     
014800       EJECT                                                              
014900*    --- ALLMÄNNA ARBETSFÄLT                                              
015000 01      FILLER               PIC X(16) VALUE 'WS**************'.         
015100                                                                          
015200 01      W-PRL-DADAT          PIC 9(8)  VALUE ZERO.                       
015300                                                                          
015400 01      WS.                                                              
015500                                                                          
015600*     -- SPARAT FRÅN SEGMENT                                              
015700                                                                          
015800  02     WS-ARTC.                                                         
015900                                                                          
016000   03    WS-ARTC.                                                         
016100    04   WS-ARTC-ADLAGOMR      PIC S9(3)   VALUE ZERO COMP-3.             
016200    04   WS-ARTC-IDFTG         PIC 9(2)    VALUE ZERO.                    
016300    04   WS-ARTC-IDLEVNR       PIC X(5)    VALUE SPACE.                   
016400    04   WS-ARTC-KDPRODSL      PIC S9(3)   VALUE ZERO COMP-3.             
016500    04   WS-ARTC-KDSORT        PIC  X(2)   VALUE SPACE.                   
016600                                                                          
016700    04   WS-ARTC-KDPSLLOC      PIC  9(2)   VALUE ZERO.                    
016800                                                                          
016900    04   WS-ARTC-IDANSK        PIC S9(3)   VALUE ZERO COMP-3.             
017000    04   WS-ARTC-IDINK         PIC S9(3)   VALUE ZERO COMP-3.             
017100    04   WS-ARTC-IDINK-X       PIC  X(4)   VALUE SPACE.                   
017200    04   WS-ARTC-KDHF          PIC S9(1)   VALUE ZERO COMP-3.             
017300                                                                          
017400    04   WS-ARTC-KDTIPPR       PIC S9(1)   VALUE ZERO COMP-3.             
017500    04   WS-ARTC-KDVTH         PIC S9(1)   VALUE ZERO COMP-3.             
017600    04   WS-ARTC-PRARTBES      PIC S9(7)V9(2)                             
017700                                             VALUE ZERO COMP-3.           
017800    04   WS-ARTC-PRDIRLON      PIC S9(4)V9(3)                             
017900                                             VALUE ZERO COMP-3.           
018000    04   WS-ARTC-PRDMTRL       PIC S9(6)V9(3)                             
018100                                             VALUE ZERO COMP-3.           
018200    04   WS-ARTC-PRINK         PIC S9(7)V9(2)                             
018300                                             VALUE ZERO COMP-3.           
018400    04   WS-ARTC-PROVRPAL      PIC S9(4)V9(3)                             
018500                                             VALUE ZERO COMP-3.           
018600    04   WS-ARTC-PRARTSTD      PIC S9(7)V9(2)                             
018700                                             VALUE ZERO COMP-3.           
018800    04   WS-ARTC-PRHEMTAG      PIC S9(7)V9(2)                             
018900                                             VALUE ZERO COMP-3.           
019000                                                                          
019100    04   WS-ARTC-FLTOPP        PIC X(1)    VALUE SPACE.                   
019200    04   WS-ARTC-KVQPACK-0     PIC S9(5)   VALUE ZERO COMP-3.             
019300    04   WS-ARTC-KVQPACK-1     PIC S9(5)   VALUE ZERO COMP-3.             
019400    04   WS-ARTC-KDLTK         PIC S9(1)   VALUE ZERO COMP-3.             
019500    04   WS-ARTC-IDARTNR-EMBQ0 PIC S9(9)   VALUE ZERO COMP-3.             
019600    04   WS-ARTC-IDARTNR-EMBQ3 PIC S9(9)   VALUE ZERO COMP-3.             
019700                                                                          
019800    04   WS-ARTC-KDERS         PIC S9(3)   VALUE ZERO COMP-3.             
019900    04   WS-ARTC-KVROS         PIC S9(7)   VALUE ZERO COMP-3.             
020000                                                                          
020100   03    WS-ARTC21.                                                       
020200    04   WS-ARTC21-PRARTBEL-PR   PIC S9(8)V9(5)                           
020300                                             VALUE ZERO COMP-3.           
020400    04   WS-ARTC21-PRARTBEL-SUM  PIC S9(8)V9(5)                           
020500                                             VALUE ZERO COMP-3.           
020600    04   WS-ARTC21-PRARTBES-PR   PIC S9(7)V9(2)                           
020700                                             VALUE ZERO COMP-3.           
020800    04   WS-ARTC21-KDVALISO      PIC X(3) VALUE SPACE.                    
020900    04   WS-ARTC23-IDAVTAL       PIC 9(13) VALUE ZERO.                    
021000                                                                          
021100   03    WS-INLC.                                                         
021200    04   WS-INLC-IDFAKT        PIC S9(7)    VALUE ZERO COMP-3.            
021300                                                                          
021400  02     WS-TIPRLIST             PIC S9(7)   COMP-3 VALUE ZERO.           
021500  02   WS-ARTS.                                                           
021600   03  WS-ARTS-KVROS             PIC S9(7)   COMP-3 VALUE ZERO.           
021700                                                                          
021800  02     WS-INLB.                                                         
021900                                                                          
022000   03    WS-INLB23.                                                       
022100    04   WS-INLB23-TIAVROP-INL   PIC S9(5)   VALUE ZERO COMP-3.           
022200                                                                          
022300  02     JUST-TIAAMMDD           PIC 9(6)    VALUE ZERO.                  
022400     SKIP2                                                                
022500*     -- DAGENS AAMMDD                                                    
022600  02     WS-TIAAMMDD             PIC 9(6)    VALUE ZERO.                  
022700  02     FILLER                  REDEFINES WS-TIAAMMDD.                   
022800   03    WS-TIAAMMDD-AA          PIC 9(2).                                
022900   03    WS-TIAAMMDD-MM          PIC 9(2).                                
023000   03    WS-TIAAMMDD-DD          PIC 9(2).                                
023100                                                                          
023200*     -- TIAVIDAT MED SEKEL-SIFFRA                                        
023300  02     WS-INLA-TIAVIDAT        PIC 9(8)    VALUE ZERO.                  
023400  02     FILLER REDEFINES WS-INLA-TIAVIDAT.                               
023500   03    WS-TIAVIDAT-SEKEL       PIC 9(2).                                
023600   03    WS-INLA-INL-TIAVIDAT    PIC 9(6).                                
023700*                                                                         
023800*     -- DAAVIDAT TILL SAP                                                
023900  02     WS-DAAVIDAT             PIC 9(8)    VALUE ZERO.                  
024000  02     FILLER REDEFINES WS-DAAVIDAT.                                    
024100   03    WS-DAAVIDAT-SEKEL       PIC 9(2).                                
024200   03    WS-DAAVIDAT-YYMMDD      PIC 9(6).                                
024300*                                                                         
024400*     -- DAGENS AAVVD                                                     
024500  02     WS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
024600  02     FILLER                  REDEFINES WS-TIAAVVD.                    
024700   03    WS-TIAAVVD-AAVV         PIC 9(4).                                
024800   03    FILLER                  REDEFINES WS-TIAAVVD-AAVV.               
024900    04   WS-TIAAVVD-AA           PIC 9(2).                                
025000    04   WS-TIAAVVD-VV           PIC 9(2).                                
025100   03    WS-TIAAVVD-D            PIC 9(1).                                
025200*     -- DATE + TIME                                                      
025300  02     WS-TIAAAAMMDDTTMMSSTH     PIC 9(16)   VALUE ZERO.                
025400  02     FILLER                  REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
025500   03    WS-TISEKEL               PIC 9(2).                               
025600   03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                               
025700   03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                               
025800  02     WLOGG-TID                 PIC S9(9).                             
025900  02     LOGG-DATUM                PIC S9(8)   VALUE ZERO.                
026000                                                                          
026100  02  DAGENS-DATUM-Y2K          PIC 9(8).                                 
026200  02  DAGENS-DATUM              PIC 9(6).                                 
026300  02  DAGENS-TID                PIC 9(8).                                 
026400  02  FILLER REDEFINES DAGENS-TID.                                        
026500     03  DAGENS-TID-6-POS        PIC 9(6).                                
026600     03  FILLER                  PIC 9(2).                                
026700*                                                                         
026800     03  WS-DATE-YYMMDD            PIC 9(06).                             
026900     03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
027000         05  WS-DATE-YYMM          PIC 9(04).                             
027100         05  WS-DATE-DD            PIC 9(02).                             
027200                                                                          
027300*     -- IDLOPNRM I VALFRI FORM                                           
027400  02     WS-IDLOPNRM-AAVVDLLLLK  PIC 9(10)   VALUE ZERO.                  
027500  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
027600   03    WS-IDLOPNRM-AA          PIC 9(2).                                
027700   03    WS-IDLOPNRM-VVDLLLLK    PIC 9(8).                                
027800   03    FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLLK.          
027900    04   WS-IDLOPNRM-VVDLLLL     PIC 9(7).                                
028000    04   FILLER                  PIC X(1).                                
028100  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
028200   03    WS-IDLOPNRM-AAVVDLLLL   PIC 9(9).                                
028300   03    FILLER                  PIC X(1).                                
028400                                                                          
028500*     -- ARTC11-PRARTBES FÖRE UPPDAT                                      
028600 02      WS-PRARTBES-OLD         PIC S9(7)V9(2)                           
028700                                             VALUE ZERO COMP-3.           
028800                                                                          
028900*     -- KVANT FÖR BOKNING LEVPLAN                                        
029000  02     WS-KV-LPLAN             PIC S9(7)   VALUE ZERO COMP-3.           
029100*     -- KVANT KVAR ATT BOKA I LEVPLAN                                    
029200  02     WS-KV-OBOK              PIC S9(7)   VALUE ZERO COMP-3.           
029300*     -- KVANT KVAR AV LEVBESK * 10                                       
029400  02     WS-BSKKVAR-GGR-10       PIC S9(9)   VALUE ZERO COMP-3.           
029500*     -- URSPRUNGIG LEVBESK-KVANT                                         
029600  02     WS-BSKURS               PIC S9(9)   VALUE ZERO COMP-3.           
029700*     -- INLEVERANS-ID (9-KOMPLEMENT TILL DATE+TIME)                      
029800  02     WS-DAINLEV              PIC 9(16)   VALUE ZERO.                  
029900*     -- ANTAL I FÖRPACKNING                                              
030000  02     WS-KVQPACK              PIC S9(5)   VALUE ZERO COMP-3.           
030100*     -- ANTAL AV KVAVIS SOM FÖRPACKAS                                    
030200  02     WS-KVAVIS-FORP          PIC S9(7)   VALUE ZERO COMP-3.           
030300                                                                          
030400*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
030500  02     WS-IDAVINR              PIC 9(7)    VALUE ZERO.                  
030600  02     FILLER                  REDEFINES WS-IDAVINR.                    
030700   03    WS-IDAVINR-TKN          OCCURS 7                                 
030800                                 PIC 9(1).                                
030900*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
031000  02     WS-IDFS                 PIC X(8)    VALUE SPACE.                 
031100  02     FILLER                  REDEFINES WS-IDFS.                       
031200   03    WS-IDFS-TKN             OCCURS 8                                 
031300                                 PIC 9(1).                                
031400*     -- ANTAL BEHANDLADE PARTI-SEG                                       
031500  02     WS-ANT-PARTI-SEG        PIC S9(3)   VALUE ZERO COMP-3.           
031600                                                                          
031700*     -- LÖPNUMMER I LOGGPOST FÖR ATT FÖRSÄKRA SIG OM UNIK NKL.           
031800  02     WS-IDLOGLOP             PIC S9(1)   VALUE ZERO COMP-3.           
031900*     -- LÖPNUMMER I LOGGPOST FÖR WLFILC/WDR3                             
032000  02     WS-IDSEKVNR             PIC S9(3)   VALUE ZERO COMP-3.           
032100                                                                          
032200*     -- LOGG-TRANSAR                                                     
032300  02     WS-ZZAC01.                                                       
032400   03    WS-ZZAC01-LOGGPOST      PIC X(90)   VALUE SPACE.                 
032500   03    FILLER                  REDEFINES WS-ZZAC01-LOGGPOST.            
032600    04   WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                                
032700    04   WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                               
032800   03    WS-ZZAC01-SORTPOST      PIC X(36)   VALUE SPACE.                 
032900                                                                          
033000*     -- LOGG-TRANSAR TILL WLFILC                                         
033100* 02  -COPY WDR301  -PRE WS-                                              
033200                                                                          
033300*                                                                         
033400*     -- ÖVRIGA TILLFÄLLIGA FÄLT                                          
033500  02     WS-ORDER-REST           PIC S9(7)   COMP-3 VALUE ZERO.           
033600  02     WS-KVAVIS-ANT           PIC S9(7)   COMP-3 VALUE ZERO.           
033700  02     WS-TOT-ORD-KVBEART      PIC S9(7)   COMP-3 VALUE ZERO.           
033800  02     WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
033900  02     WS-TIAVRDAT-INL         PIC S9(7)   COMP-3 VALUE ZERO.           
034000  02     WS-IDDISTR-DISP         PIC 9(4)    VALUE ZERO.                  
034100                                                                          
034200     EJECT                                                                
034300 01      IX-INDEXVARIABLER.                                               
034400                                                                          
034500*     -- PRIORITERAT C-LAGER VID BOKN LBESK PÅ BÅDA C-LAGER.              
034600  02     IX-CL-PRIO              PIC S9(9)   VALUE ZERO COMP SYNC.        
034700*     -- C-LAGER VID BOKNING LBESK PÅ ENSKILT C-LAGER.                    
034800  02     IX-CL-BOK               PIC S9(9)   VALUE ZERO COMP SYNC.        
034900*     -- TECKEN I WS-IDFS                                                 
035000  02     IX-IDFS                 PIC S9(9)   VALUE ZERO COMP SYNC.        
035100*     -- TECKEN I WS-IDAVINR                                              
035200  02     IX-IDAVINR              PIC S9(9)   VALUE ZERO COMP SYNC.        
035300     SKIP3                                                                
035400 01      K-KONSTANTER.                                                    
035500                                                                          
035600*     -- UNDRE ANTALSGRÄNS FÖR ATT BRYTA FÖRPACKNING.                     
035700  02     K-KVMIN-FORP            PIC S9(3)   VALUE +50  COMP-3.           
035800*     -- MAX ANTAL BEHANDLADE PARTI-SEG PER TASK.                         
035900  02     K-MAX-ANT-PARTI-SEG     PIC S9(3)   VALUE +5   COMP-3.           
036000*     -- FÄLTLÄNGD IDFS                                                   
036100  02     K-IDFS-LNG              PIC S9(9)   VALUE +8   COMP SYNC.        
036200*     -- FÄLTLÄNGD IDAVINR                                                
036300  02     K-IDAVINR-LNG           PIC S9(9)   VALUE +7   COMP SYNC.        
036400     EJECT                                                                
036500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
036600 01  GENERELLA-SUBPROGRAM.                                                
036700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
036800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
036900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
037000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
037100     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
037200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
037300     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
037400     EJECT                                                                
037500*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
037600*01 -COPY W218LETA              -PRE LETA-                                
037700     SKIP3                                                                
037800 01  MESSAGE-CODES.                                                       
037900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
038000     EJECT                                                                
038100 01  FILLER                    PIC X(16) VALUE 'WORKAREA********'.        
038200                                                                          
038300*01  -COPY WORKAREA                                                       
038400     EJECT                                                                
038500*    --- AREA FÖR WDATKONV                                                
038600 01  FILLER                    PIC X(16) VALUE 'WDATAREA********'.        
038700                                                                          
038800*01  -COPY WDATAREA                                                       
038900     EJECT                                                                
039000*    --- AREA FÖR WMSGINIT                                                
039100 01  FILLER                    PIC X(16) VALUE 'WMSGINIT********'.        
039200                                                                          
039300*01  -COPY WMSGINIT                                                       
039400     EJECT                                                                
039500*    --- AREOR FÖR LOGGTRANSAR                                            
039600 01  FILLER                    PIC X(16) VALUE 'W211310*********'.        
039700                                                                          
039800*01  -COPY W211310  -PRE W211310-                                         
039900     EJECT                                                                
040000 01  FILLER                    PIC X(16) VALUE 'W211FEL*********'.        
040100                                                                          
040200*01  -COPY W211FEL  -PRE W211FEL-                                         
040300     EJECT                                                                
040400 01  FILLER                    PIC X(16) VALUE 'W211M113********'.        
040500                                                                          
040600*01  -COPY W211M113 -PRE M113-                                            
040700     EJECT                                                                
040800 01  FILLER                    PIC X(16) VALUE 'W211M117********'.        
040900                                                                          
041000*01  -COPY W211M117 -PRE M117-                                            
041100     EJECT                                                                
041200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
041300*                                                                         
041400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
041500     SKIP3                                                                
041600*01  MID -COPY W6I19201                                                   
041700     EJECT                                                                
041800 01  FILLER                      PIC X(16)   VALUE 'WMSGKOM'.             
041900     SKIP3                                                                
042000*01  -COPY WMSGKOM                                                        
042100     EJECT                                                                
042200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
042300     SKIP3                                                                
042400*01  -COPY WMSGAREA                                                       
042500     EJECT                                                                
042600     05  -COPY W6I19201 -PRE MOD-  -RED MSG-MID-OUT                       
042700     EJECT                                                                
042800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
042900     SKIP3                                                                
043000*01  -COPY WMFSAREA                                                       
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16)   VALUE 'W510CURR'.            
043300     SKIP3                                                                
043400*01  -COPY W510CURR                                                       
043500     EJECT                                                                
043600 01  FILLER                      PIC X(16)   VALUE 'WLLOGA01'.            
043700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043800*                                                                         
043900     SKIP2                                                                
044000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
044100     SKIP2                                                                
044200 01  NYCKLAR-TILL-DLI.                                                    
044300     03  W-IDARTNR-WDA9-X.                                                
044400         05  W-IDARTNR-WDA9      PIC S9(9)   VALUE ZERO COMP-3.           
044500     03  W-IDDISTR-WDA9-X.                                                
044600         05  W-IDDISTR-WDA9      PIC S9(5)   VALUE ZERO COMP-3.           
044700     03  W-IDARTNR-X.                                                     
044800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
044900     03  W-WDD901KY-X.                                                    
045000         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
045100         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
045200     03  W-IDDC-X.                                                        
045300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
045400     03  W-IDDC-K7-X.                                                     
045500         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
045600     03  W-DAINLEV-X.                                                     
045700         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
045800     03  W-KDCLAGER-X.                                                    
045900         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
046000     03  W-TIPRLIST-X.                                                    
046100*        --  OBS NEGATIVT BINÄRT LAGRAT!                                  
046200         05  W-TIPRLIST          PIC S9(9)   VALUE ZERO COMP.             
046300     03  W-IDLEVNR-21-X.                                                  
046400         05  W-IDLEVNR-21        PIC X(5)    VALUE ZERO.                  
046500     03  W-IDLEVNR-PR-X.                                                  
046600         05  W-IDLEVNR-PR        PIC X(5)    VALUE ZERO.                  
046700     03  W-DAPRLIST-K7-N.                                                 
046800         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
046900     03  W-IDLAND-K7-X.                                                   
047000         05  W-IDLAND-K7         PIC X(2)    VALUE SPACE.                 
047100     03  W-KDAVROP-X.                                                     
047200         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
047300     03  W-KDAVROP-LARM-X.                                                
047400         05  W-KDAVROP-LARM      PIC S9(1)   VALUE ZERO COMP-3.           
047500     03  W-WDD905KY-X.                                                    
047600         05  W-DAAVROP-X.                                                 
047700             07  W-DAAVROP       PIC  9(6)    VALUE ZERO.                 
047800         05  W-TILEVDAG-X.                                                
047900             07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.          
048000     03  W-WDD905KY-LARM-X.                                               
048100         05  W-DAAVROP-LARM-X.                                            
048200             07  W-DAAVROP-LARM  PIC  9(6)    VALUE ZERO.                 
048300         05  W-TILEVDAG-LARM-X.                                           
048400             07  W-TILEVDAG-LARM PIC  S9      VALUE ZERO COMP-3.          
048500     03  W-IDORDNSB-X.                                                    
048600         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
048700     03  W-IDLEVNR-X.                                                     
048800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
048900     03  W-IDLOPNRM-X.                                                    
049000         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
049100     03  W-IDRADNR-INL-X.                                                 
049200         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
049300     03  W-INLB11-IDLEVNR-X.                                              
049400         05  W-INLB11-IDLEVNR    PIC X(5)    VALUE SPACE.                 
049500     03  W-W6D101KY-X.                                                    
049600         05  W-W6D101KY-IDDC     PIC X(2)    VALUE SPACE.                 
049700         05  W-W6D101KY-IDLEVNR  PIC X(5)    VALUE SPACE.                 
049800         05  W-W6D101KY-IDFS     PIC X(8)    VALUE SPACE.                 
049900         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
050000     03  W-IDHTYP-2239-X.                                                 
050100         05  FILLER              PIC X(4)    VALUE '2239'.                
050200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
050300     03  W-IDHTYP-2241-X.                                                 
050400         05  FILLER              PIC X(4)    VALUE '2241'.                
050500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
050600     03  W-WDB2BSEQ-X.                                                    
050700         05  W-IDLEVNR-WDB2B     PIC X(5)   VALUE SPACE.                  
050800     03  W-WDD401-X.                                                      
050900         05  W-WDD401KY          PIC X(13)  VALUE SPACE.                  
051000     03  W-WDGXKEY-2231-X.                                                
051100         05  FILLER              PIC X(4)     VALUE '2231'.               
051200         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
051300     03  W-WDGXKEY-2232-X.                                                
051400         05  W-IDANSK-L          PIC S9(3)    VALUE ZERO COMP-3.          
051500         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
051600     03  W-IDDC-B6-X.                                                     
051700         05 W-IDDC-B6            PIC X(2).                                
051800     03  W-IDLANDX2-X.                                                    
051900         05    W-IDLANDX2        PIC X(2)    VALUE SPACE.                 
052000     03  W-IDLAND-X.                                                      
052100         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
052200     03  W-WDGX9305-X.                                                    
052300         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
052400         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
052500         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
052600         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
052700     03  W-KDVALISO-X.                                                    
052800         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
052900     03  W-TISTADA9-X.                                                    
053000         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
053100     EJECT                                                                
053200*    --- STATUS-KOD FRÅN IMS                                              
053300 01  STATUS-WS                   PIC XX.                                  
053400     88  SEGMENT-FINNS                       VALUE '  '.                  
053500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
053600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
053700     SKIP2                                                                
053800 01  GODK-STATUSKODER.                                                    
053900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
054000     SKIP3                                                                
054100 01  SSA1                        PIC X(64).                               
054200 01  SSA2                        PIC X(64).                               
054300 01  SSA3                        PIC X(64).                               
054400     EJECT                                                                
054500*    --- IMS FUNKTIONSKODER                                               
054600*01  -COPY W0003                                                          
054700     EJECT                                                                
054800*    ---  DLI INPUT-OUTPUT AREA                                           
054900 01  FILLER                      PIC X(16)  VALUE 'WDK623'.               
055000                                                                          
055100*01  WLARTC23 -COPY WDK623                                                
055200     EJECT                                                                
055300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
055400     SKIP3                                                                
055500 01  DLI-IO-AREA.                                                         
055600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
055700     SKIP3                                                                
055800     03  WLARTC01 REDEFINES IO-AREA.                                      
055900*        05  -COPY WDK601  -PRE ARTC-                                     
056000     EJECT                                                                
056100     03  WLARTC11 REDEFINES IO-AREA.                                      
056200*        05  -COPY WDK611  -PRE ARTC-                                     
056300     EJECT                                                                
056400     03  WLARTC21 REDEFINES IO-AREA.                                      
056500*        05  -COPY WDK621  -PRE ARTC-                                     
056600     EJECT                                                                
056700     03  WLINLB11 REDEFINES IO-AREA.                                      
056800*        05  -COPY WDD902  -PRE INLB11-                                   
056900     EJECT                                                                
057000     03  WLINLB23 REDEFINES IO-AREA.                                      
057100*        05  -COPY WDD905  -PRE INLB23-                                   
057200     EJECT                                                                
057300     03  WLINLB24 REDEFINES IO-AREA.                                      
057400*        05  -COPY WDD924   -PRE INLB24-                                  
057500     EJECT                                                                
057600     03  WLINLE01 REDEFINES IO-AREA.                                      
057700*        05  -COPY WDL201  -PRE INLE-                                     
057800     EJECT                                                                
057900     03  WLINLE11 REDEFINES IO-AREA.                                      
058000*        05  -COPY WDL211  -PRE INLE-                                     
058100     EJECT                                                                
058200     03  WLINLE21 REDEFINES IO-AREA.                                      
058300*        05  -COPY WDL221  -PRE INLE-                                     
058400     EJECT                                                                
058500     03  WLZZAC01 REDEFINES IO-AREA.                                      
058600*        05  -COPY WDG601  -PRE ZZAC01-                                   
058700     EJECT                                                                
058800     03  WLINLB32 REDEFINES IO-AREA.                                      
058900*        05  -COPY WDD907  -PRE INLB32-                                   
059000     EJECT                                                                
059100     03  WLFILC01 REDEFINES IO-AREA.                                      
059200*        05  -COPY WDR301  -PRE FILC-                                     
059300         07  FILLER REDEFINES FILC-FIL-WDR301-DATA.                       
059400         09  -COPY W236RY7 -PRE FILC-                                     
059500     EJECT                                                                
059600*    ---  DLI INPUT-OUTPUT AREA 2                                         
059700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
059800     SKIP3                                                                
059900 01  DLI-IO-AREA2.                                                        
060000     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
060100     SKIP3                                                                
060200     03  W6INLA01 REDEFINES IO-AREA2.                                     
060300*        05  -COPY W6D101  -PRE INLA-                                     
060400     EJECT                                                                
060500*    ---  DLI INPUT-OUTPUT AREA 3                                         
060600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
060700     SKIP3                                                                
060800 01  DLI-IO-AREA3.                                                        
060900     03  IO-AREA3                PIC X(200)  VALUE SPACE.                 
061000     SKIP3                                                                
061100     03  W6INLA11 REDEFINES IO-AREA3.                                     
061200*        05  -COPY W6D111  -PRE INLA-                                     
061300     EJECT                                                                
061400*    ---  DLI INPUT-OUTPUT AREA 4                                         
061500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
061600     SKIP3                                                                
061700 01  DLI-IO-AREA4.                                                        
061800     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
061900     SKIP3                                                                
062000     03  WLINLB31 REDEFINES IO-AREA4.                                     
062100*        05  -COPY WDD906  -PRE INLB31-                                   
062200     SKIP3                                                                
062300 01  DLI-IO-AREA5.                                                        
062400     03  IO-AREA5                PIC X(100)  VALUE SPACE.                 
062500     EJECT                                                                
062600*    ---  DLI INPUT-OUTPUT AREA 6                                         
062700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
062800     SKIP3                                                                
062900 01  DLI-IO-AREA6.                                                        
063000     03  IO-AREA6                PIC X(200)  VALUE SPACE.                 
063100     SKIP3                                                                
063200     03  WLXXCS01 REDEFINES IO-AREA6.                                     
063300         05   FILLER             PIC X(30).                               
063400     03  WLXXCS11 REDEFINES IO-AREA6.                                     
063500*        05  -COPY WDGX2240                                               
063600     EJECT                                                                
063700     03  WLXXCT01 REDEFINES IO-AREA6.                                     
063800         05   FILLER             PIC X(30).                               
063900     03  WLXXCT11 REDEFINES IO-AREA6.                                     
064000*        05  -COPY WDGX2242                                               
064100     EJECT                                                                
064200*                                                                         
064300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS01'.         
064400     SKIP3                                                                
064500 01  DLI-IO-AREA-ARTS01.                                                  
064600     03  -COPY WDK701 -PRE ARTS-                                          
064700     EJECT                                                                
064800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS11'.         
064900     SKIP3                                                                
065000 01  DLI-IO-AREA-ARTS11.                                                  
065100     03  -COPY WDK711 -PRE ARTS-                                          
065200     EJECT                                                                
065300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK722'.         
065400 01  DLI-IO-WDK722.                                                       
065500*    03  -COPY WDK722                                                     
065600     EJECT                                                                
065700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
065800     SKIP3                                                                
065900 01  DLI-IO-WDK711.                                                       
066000*        05  -COPY WDK711                                                 
066100     SKIP3                                                                
066200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
066300 01  DLI-IO-WDK712.                                                       
066400*        05  -COPY WDK712                                                 
066500     SKIP3                                                                
066600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK723'.             
066700 01  DLI-IO-WDK723.                                                       
066800*        05  -COPY WDK723                                                 
066900     SKIP3                                                                
067000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK724'.             
067100 01  DLI-IO-WDK724.                                                       
067200*        05  -COPY WDK724                                                 
067300                                                                          
067400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC01'.         
067500 01  DLI-IO-AREA-INLC01.                                                  
067600     03  -COPY WDL601                                                     
067700     EJECT                                                                
067800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC11'.         
067900 01  DLI-IO-AREA-INLC11.                                                  
068000     03  -COPY WDL611                                                     
068100     EJECT                                                                
068200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC12'.         
068300 01  DLI-IO-AREA-INLC12.                                                  
068400     03  -COPY WDL612                                                     
068500     EJECT                                                                
068600*01  WLLOGA01 -COPY WDL901                                                
068700     EJECT                                                                
068800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-FILB01'.         
068900 01  DLI-IO-AREA-FILB01.                                                  
069000*    05  -COPY WDR801       -PRE EKO-                                     
069100       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
069200         09  -COPY W51080   -PRE EKO-                                     
069300       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
069400         09  -COPY W510A11  -PRE LAB-                                     
069500       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
069600         09  -COPY W510EKHA -PRE EKO-                                     
069700     EJECT                                                                
069800 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
069900 01  DLI-IO-WLSAPA01.                                                     
070000*    03  WLSAPA01  -COPY WDR901                                           
070100*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
070200     EJECT                                                                
070300 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDA901  '.               
070400 01  DLI-IO-WDA901.                                                       
070500*    03  WDA901    -COPY WDA901                                           
070600     EJECT                                                                
070700 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDA911  '.               
070800 01  DLI-IO-WDA911.                                                       
070900*    03  WDA911    -COPY WDA911                                           
071000     EJECT                                                                
071100 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDK601  '.               
071200 01  DLI-IO-WDK601.                                                       
071300*    03  WDK601    -COPY WDK601 -PRE REM-                                 
071400     EJECT                                                                
071500 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDB201  '.               
071600 01  DLI-IO-WDB201.                                                       
071700*    03  WDB201    -COPY WDB201                                           
071800     EJECT                                                                
071900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
072000 01  DLI-IO-WDGX01.                                                       
072100*    03  -COPY WDGX01                                                     
072200     EJECT                                                                
072300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401  '.                    
072400 01  DLI-IO-WDD401.                                                       
072500*    03  -COPY WDD401                                                     
072600     EJECT                                                                
072700 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA-BX '.                    
072800 01  DLI-IO-AREA-BX.                                                      
072900     03  IO-AREA-BX              PIC X(50)  VALUE SPACE.                  
073000     SKIP3                                                                
073100     03  WLXXBX01 REDEFINES IO-AREA-BX.                                   
073200*        05  -COPY WDGX01     -PRE XXBX-                                  
073300     SKIP3                                                                
073400     03  WLXXBX20 REDEFINES IO-AREA-BX.                                   
073500*        05  -COPY WDGX2232   -PRE XXBX-                                  
073600     EJECT                                                                
073700 01  DLI-IO-AREA-F1          PIC X(100).                                  
073800     SKIP2                                                                
073900*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
074000     EJECT                                                                
074100                                                                          
074200 01  DLI-IO-AREA-F102        PIC X(100).                                  
074300     SKIP2                                                                
074400*01  WLLEVA11 -COPY WDF102 -PRE F102-     -RED DLI-IO-AREA-F102           
074500     EJECT                                                                
074600                                                                          
074700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
074800 01   DLI-IO-AREA-B601.                                                   
074900*     03  -COPY WDB601                                                    
075000     EJECT                                                                
075100                                                                          
075200 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
075300 01   DLI-IO-AREA-B617.                                                   
075400*     03  -COPY WDB617                                                    
075500     EJECT                                                                
075600                                                                          
075700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
075800 01  DLI-IO-WDGX9306.                                                     
075900*    03  -COPY WDGX9306                                                   
076000     EJECT                                                                
076100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
076200 01  DLI-IO-WDGX9308.                                                     
076300*    03  -COPY WDGX9308                                                   
076400                                                                          
076500 LINKAGE SECTION.                                                         
076600                                                                          
076700*01  -COPY W0009   -PRE MSG-                                              
076800     EJECT                                                                
076900*01  -COPY W0009   -PRE 6192-                                             
077000     EJECT                                                                
077100*01  -COPY W0009   -PRE DISP-                                             
077200     EJECT                                                                
077300*01  -COPY W0008  -PRE USEA-                                              
077400     05  FILLER                  PIC X.                                   
077500     EJECT                                                                
077600*01  -COPY W0008  -PRE INLA-                                              
077700     05  FILLER                  PIC X.                                   
077800     EJECT                                                                
077900*01  -COPY W0008  -PRE ARTC-                                              
078000     05  FILLER                  PIC X.                                   
078100     EJECT                                                                
078200*01  -COPY W0008  -PRE INLB-                                              
078300     05  FILLER                  PIC X(12).                               
078400     05  INLB-KFB-DAAVROP-AVS    PIC 9(6).                                
078500     05  INLB-KFB-TILEVDAG       PIC S9   COMP-3.                         
078600     EJECT                                                                
078700*01  -COPY W0008  -PRE INLE-                                              
078800     05  FILLER                  PIC X.                                   
078900     EJECT                                                                
079000*01  -COPY W0008  -PRE ZZAC-                                              
079100     05  FILLER                  PIC X.                                   
079200                                                                          
079300*01  -COPY W0008  -PRE XXCS-                                              
079400     05  FILLER                  PIC X.                                   
079500                                                                          
079600*01  -COPY W0008  -PRE XXCT-                                              
079700     05  FILLER                  PIC X.                                   
079800     EJECT                                                                
079900*01  -COPY W0008  -PRE ARTS-                                              
080000     05  FILLER                  PIC X.                                   
080100     EJECT                                                                
080200*01  -COPY W0008  -PRE INLC-                                              
080300     05  FILLER                  PIC X.                                   
080400     EJECT                                                                
080500*01  -COPY W0008  -PRE FILC-                                              
080600     05  FILLER                  PIC X.                                   
080700     EJECT                                                                
080800*01  -COPY W0008  -PRE ETA-LEVA-                                          
080900     05  FILLER                  PIC X.                                   
081000     EJECT                                                                
081100*01  -COPY W0008  -PRE ETA-ARTC-                                          
081200     05  FILLER                  PIC X.                                   
081300     EJECT                                                                
081400*01  -COPY W0008  -PRE ETA-ARTS-                                          
081500     05  FILLER                  PIC X.                                   
081600     EJECT                                                                
081700*01  -COPY W0008  -PRE ETA-INLC-                                          
081800     05  FILLER                  PIC X.                                   
081900     EJECT                                                                
082000*01  -COPY W0008  -PRE ETA-WDB6-                                          
082100     05  FILLER                  PIC X.                                   
082200     EJECT                                                                
082300*01  -COPY W0008  -PRE LOGA-                                              
082400     05  FILLER                  PIC X.                                   
082500     EJECT                                                                
082600*01  -COPY W0008  -PRE FILB-                                              
082700     05  FILLER                  PIC X.                                   
082800     EJECT                                                                
082900*01  -COPY W0008  -PRE SAPA-                                              
083000     05  FILLER                  PIC X.                                   
083100     EJECT                                                                
083200*01  -COPY W0008  -PRE WDA9-                                              
083300     05  FILLER                  PIC X.                                   
083400     EJECT                                                                
083500*01  -COPY W0008  -PRE WDB2B-                                             
083600     05  FILLER                  PIC X.                                   
083700     EJECT                                                                
083800*01  -COPY W0008  -PRE WDD4-                                              
083900     05  FILLER                  PIC X.                                   
084000     EJECT                                                                
084100*01  -COPY W0008  -PRE XXBX-                                              
084200     05  FILLER                  PIC X.                                   
084300     EJECT                                                                
084400*01  -COPY W0008  -PRE WDD9-                                              
084500     05  FILLER                  PIC X.                                   
084600     EJECT                                                                
084700*01  -COPY W0008  -PRE WDF1-                                              
084800     05  FILLER                  PIC X.                                   
084900*01  -COPY W0008  -PRE WDK7-                                              
085000     05  FILLER                  PIC X.                                   
085100     EJECT                                                                
085200*01  -COPY W0008     -PRE WDB6-                                           
085300     05 FILLER                   PIC X(18).                               
085400     EJECT                                                                
085500*01  -COPY W0008  -PRE 9305-                                              
085600     05  FILLER                  PIC X.                                   
085700                                                                          
085800 PROCEDURE DIVISION  USING                                                
085900                     MSG-PCB  6192-PCB DISP-PCB                           
086000                     USEA-PCB                                             
086100                     INLA-PCB ARTC-PCB INLB-PCB                           
086200                     INLE-PCB ZZAC-PCB                                    
086300                     XXCS-PCB XXCT-PCB ARTS-PCB                           
086400                     INLC-PCB FILC-PCB                                    
086500                     ETA-LEVA-PCB ETA-ARTC-PCB                            
086600                     ETA-ARTS-PCB ETA-INLC-PCB ETA-WDB6-PCB               
086700                     LOGA-PCB                                             
086800                     FILB-PCB SAPA-PCB                                    
086900                     WDA9-PCB WDB2B-PCB WDD4-PCB XXBX-PCB                 
087000                     WDD9-PCB WDF1-PCB                                    
087100                     WDK7-PCB WDB6-PCB 9305-PCB.                          
087200 MAIN SECTION.                                                            
087300     ENTRY 'DLITCBL' USING                                                
087400                     MSG-PCB  6192-PCB DISP-PCB                           
087500                     USEA-PCB                                             
087600                     INLA-PCB ARTC-PCB INLB-PCB                           
087700                     INLE-PCB ZZAC-PCB                                    
087800                     XXCS-PCB XXCT-PCB ARTS-PCB                           
087900                     INLC-PCB FILC-PCB                                    
088000                     ETA-LEVA-PCB ETA-ARTC-PCB                            
088100                     ETA-ARTS-PCB ETA-INLC-PCB ETA-WDB6-PCB               
088200                     LOGA-PCB                                             
088300                     FILB-PCB SAPA-PCB                                    
088400                     WDA9-PCB WDB2B-PCB WDD4-PCB XXBX-PCB                 
088500                     WDD9-PCB WDF1-PCB                                    
088600                     WDK7-PCB WDB6-PCB 9305-PCB.                          
088700                                                                          
088800     PERFORM IMS-GET-MSG                                                  
088900     IF SEGMENT-FINNS                                                     
089000       PERFORM IMS-GN-MSG                                                 
089100                                                                          
089200       PERFORM A-INIT                                                     
089300       PERFORM B-INIT-SANDNING                                            
089400                                                                          
089500       PERFORM UNTIL (SEGMENT-SAKNAS                                      
089600                  OR  WS-ANT-PARTI-SEG > K-MAX-ANT-PARTI-SEG)             
089700         IF W-KDINL   = '310' AND                                         
089800            INLA-ART-KDRT = 8                                             
089900           PERFORM G-UPPDATERA-KVAKS                                      
090000         ELSE                                                             
090100           PERFORM C-INIT-PARTI                                           
090200           PERFORM D-UPPDATERA                                            
090300         END-IF                                                           
090400                                                                          
090500         PERFORM IMS-GNP-INLA-ART                                         
090600         ADD +1                  TO WS-ANT-PARTI-SEG                      
090700       END-PERFORM                                                        
090800                                                                          
090900       IF  SEGMENT-FINNS                                                  
091000         PERFORM E-STARTA-OM                                              
091100       ELSE                                                               
091200         PERFORM F-FINIT-SANDNING                                         
091300         CONTINUE                                                         
091400       END-IF                                                             
091500     END-IF                                                               
091600                                                                          
091700     MOVE ZERO TO RETURN-CODE                                             
091800     GOBACK                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 A-INIT SECTION.                                                          
092200                                                                          
092300     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I19201                    
092400     MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                               
092500     MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                              
092600                                                                          
092700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
092800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
092900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
093000                                                                          
093100     MOVE LOW-VALUE TO MSG-AREA                                           
093200                                                                          
093300     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
093400     CALL WDATKONV USING         DAT-KDDATFORM                            
093500                                 DAT-I-TIDATUM                            
093600                                 DAT-O-TIDATUM                            
093700                                 DAT-KDSVAR                               
093800                                                                          
093900     IF  DAT-KDSVAR-OK                                                    
094000       MOVE DAT-TIAAMMDD         TO WS-TIAAMMDD                           
094100       MOVE DAT-TIAAVVD          TO WS-TIAAVVD                            
094200     ELSE                                                                 
094300       STRING 'FEL FRÅN WDATKONV:' DAT-KDSVAR                             
094400         DELIMITED BY SIZE INTO FELTEXT                                   
094500       CALL FELLOG                                                        
094600     END-IF                                                               
094700                                                                          
094800     ACCEPT DAGENS-DATUM         FROM DATE                                
094900     ACCEPT DAGENS-TID           FROM TIME                                
095000     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
095100     MOVE 01                          TO WS-DATE-DD                       
095200     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
095300                                                                          
095400     MOVE FUNCTION CURRENT-DATE (1:8) TO  W1-DAREGDAT                     
095500     COMPUTE W1-DAREGDAT-9KOMPL = 99999999 -                              
095600                                          W1-DAREGDAT                     
095700                                                                          
095800     .                                                                    
095900     EJECT                                                                
096000 B-INIT-SANDNING SECTION.                                                 
096100                                                                          
096200     MOVE MID-IDDC               TO W-W6D101KY-IDDC                       
096300                                    W-IDDC                                
096400                                    WS-IDDC                               
096500                                    W-IDDC-K7                             
096600     MOVE MID-IDLEVNR            TO W-W6D101KY-IDLEVNR                    
096700     MOVE MID-IDFS               TO W-W6D101KY-IDFS                       
096800     MOVE MID-TIAVIDAT           TO W-W6D101KY-TIAVIDAT                   
096900     PERFORM IMS-GU-INLA-INL                                              
097000                                                                          
097100     IF CDC-SE                                                            
097200       MOVE 1                    TO W-KDCLAGER                            
097300     ELSE                                                                 
097400       MOVE 2                    TO W-KDCLAGER                            
097500     END-IF                                                               
097600                                                                          
097700     MOVE MID-IDLEVNR            TO W-IDLEVNR                             
097800     MOVE INLA-INL-KDINL         TO W-KDINL                               
097900                                                                          
098000*    -- INIT LÖPNR FÖR LOGGPOSTER                                         
098100     MOVE ZERO                   TO WS-IDLOGLOP                           
098200                                                                          
098300*    -- REDIGERA IDAVINR                                                  
098400     MOVE INLA-INL-IDFS          TO WS-IDFS                               
098500     MOVE ZERO                   TO WS-IDAVINR                            
098600     MOVE K-IDFS-LNG             TO IX-IDFS                               
098700     MOVE K-IDAVINR-LNG          TO IX-IDAVINR                            
098800                                                                          
098900     PERFORM UNTIL (IX-IDFS      = ZERO                                   
099000                OR  IX-IDAVINR   = ZERO)                                  
099100                                                                          
099200       IF  WS-IDFS-TKN (IX-IDFS) NUMERIC                                  
099300         MOVE WS-IDFS-TKN (IX-IDFS)                                       
099400                                 TO WS-IDAVINR-TKN (IX-IDAVINR)           
099500         SUBTRACT 1              FROM IX-IDAVINR                          
099600       END-IF                                                             
099700       SUBTRACT 1                FROM IX-IDFS                             
099800     END-PERFORM                                                          
099900                                                                          
100000     MOVE MID-IDRADNR-INL        TO W-IDRADNR-INL                         
100100     PERFORM IMS-GNP-INLA-ART-GQ                                          
100200     MOVE +1                     TO WS-ANT-PARTI-SEG                      
100300     .                                                                    
100400     EJECT                                                                
100500 C-INIT-PARTI SECTION.                                                    
100600                                                                          
100700     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
100800                                                                          
100900*    -- SKAPA IDINLEV                                                     
101000     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
101100     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
101200     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
101300     COMPUTE WS-DAINLEV          = 9999999999999999                       
101400                                 - WS-TIAAAAMMDDTTMMSSTH                  
101500     END-COMPUTE                                                          
101600                                                                          
101700*    -- REDIGERA IDLOPNRM                                                 
101800     MOVE WS-TIAAVVD-AA          TO WS-IDLOPNRM-AA                        
101900     MOVE INLA-ART-IDLOPNRM      TO WS-IDLOPNRM-VVDLLLLK                  
102000     .                                                                    
102100     EJECT                                                                
102200 D-UPPDATERA SECTION.                                                     
102300                                                                          
102400     PERFORM DB-UPPD-ARTC                                                 
102500**NDC                                                                     
102600     IF CDC                                                               
102700       PERFORM DC-UPPD-INLB-LEVPL                                         
102800                                                                          
102900       MOVE W-IDARTNR          TO TEST-IDARTNR                            
103000       IF BYT02-RENOV                                                     
103100         PERFORM DH-UPPD-REMAN                                            
103200       END-IF                                                             
103300     END-IF                                                               
103400                                                                          
103500**NDC-CN/-US - INLEVERANSER LOKAL ANSKAFFNING                             
103600     IF NDC-CN OR NDC-US                                                  
103700       PERFORM DD-UPPD-INLB-LEVPL-LOKAL                                   
103800     END-IF                                                               
103900                                                                          
104000     PERFORM DE-UPPD-HISTORIK                                             
104100     IF CDC OR NDC-CN OR NDC-US                                           
104200       CONTINUE                                                           
104300     ELSE                                                                 
104400       PERFORM DG-UPPD-ARTS-KVBEART                                       
104500     END-IF                                                               
104600                                                                          
104700     PERFORM DF-UPPD-FILB-LOGG                                            
104800     IF CDC                                                               
104900       PERFORM DJ-SKAPA-LARM                                              
105000     END-IF                                                               
105100     .                                                                    
105200     EJECT                                                                
105300 DB-UPPD-ARTC SECTION.                                                    
105400                                                                          
105500*    -- ARTC01                                                            
105600     PERFORM IMS-GU-ARTC01                                                
105700     MOVE ARTC-ART-IDFTG           TO WS-ARTC-IDFTG                       
105800     MOVE ARTC-ART-IDLEVNR         TO WS-ARTC-IDLEVNR                     
105900     MOVE ARTC-ART-KDPRODSL        TO WS-ARTC-KDPRODSL                    
106000     MOVE ARTC-ART-KDSORT          TO WS-ARTC-KDSORT                      
106100                                                                          
106200*    -- ARTC11                                                            
106300     PERFORM IMS-GHNP-ARTC11                                              
106400     MOVE ARTC-CLAG-KDPSLLOC    TO WS-ARTC-KDPSLLOC                       
106500     MOVE ARTC-CLAG-ADLAGOMR    TO WS-ARTC-ADLAGOMR                       
106600                                                                          
106700**NDC                                                                     
106800     IF CDC                                                               
106900         PERFORM DBB-UPPDATERA-ARTC-CDC                                   
107000     ELSE                                                                 
107100         PERFORM DBC-LAES-ARTC-UPPDAT-ARTS                                
107200     END-IF                                                               
107300                                                                          
107400     MOVE ZERO                   TO WS-ARTC21-PRARTBEL-PR                 
107500     MOVE ZERO                   TO WS-ARTC21-PRARTBEL-SUM                
107600     MOVE ZERO                   TO WS-ARTC21-PRARTBES-PR                 
107700                                                                          
107800     IF  INLA-ART-KDRT = 00 OR 01 OR 02 OR 03 OR 04 OR 05                 
107900       PERFORM DBA-UPPD-ARTC-PRISJUST                                     
108000     END-IF                                                               
108100     PERFORM IMS-GNP-ARTC23                                               
108200     IF SEGMENT-FINNS                                                     
108300       MOVE AVT-IDAVTAL     TO WS-ARTC23-IDAVTAL                          
108400     ELSE                                                                 
108500       MOVE ZERO            TO WS-ARTC23-IDAVTAL                          
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900                                                                          
109000 DBA-UPPD-ARTC-PRISJUST SECTION.                                          
109100     MOVE INLA-INL-TIAVIDAT TO WS-INLA-INL-TIAVIDAT                       
109200                               DAT-I-TIDATUM                              
109300     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
109400     CALL WDATKONV USING       DAT-KDDATFORM                              
109500                               DAT-I-TIDATUM                              
109600                               DAT-O-TIDATUM                              
109700                               DAT-KDSVAR                                 
109800     IF DAT-KDSVAR-FEL                                                    
109900       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
110000       CALL FELLOG                                                        
110100     END-IF                                                               
110200                                                                          
110300     MOVE DAT-TISEKEL TO WS-TIAVIDAT-SEKEL                                
110400                                                                          
110500     MOVE NEJ TO PRIS-FINNS-SW                                            
110600     IF NDC-CN OR NDC-US                                                  
110700       PERFORM DBAA-UPPD-ARTS-PRISJUST                                    
110800     ELSE                                                                 
110900       PERFORM DBAB-UPPD-ARTC-PRISJUST                                    
111000     END-IF                                                               
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400 DBAA-UPPD-ARTS-PRISJUST SECTION.                                         
111500*    -- WDK711                                                            
111600     PERFORM IMS-GHU-WDK711                                               
111700     IF SEGMENT-FINNS                                                     
111800                                                                          
111900*    -- WDK724                                                            
112000       MOVE INLA-INL-IDLEVNR   TO W-IDLEVNR-PR                            
112100       COMPUTE W-DAPRLIST-K7 = 99999999 - DAGENS-DATUM-Y2K                
112200       PERFORM IMS-GHNP-WDK724                                            
112300                                                                          
112400       IF SEGMENT-FINNS                                                   
112500         MOVE JA                TO PRIS-FINNS-SW                          
112600         MOVE SPRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-PR                  
112700         MOVE SPRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-SUM                 
112800         MOVE SPRL-PRARTBES-PR  TO WS-ARTC21-PRARTBES-PR                  
112900         MOVE SPRL-KDVALISO     TO WS-ARTC21-KDVALISO                     
113000                                                                          
113100         IF SPRL-SUINLEV-PR  = ZERO                                       
113200*          MOVE +1                 TO SPRL-SUINLEV-PR                     
113300           PERFORM IMS-REPL-WDK724                                        
113400         ELSE                                                             
113500*          ADD +1                  TO SPRL-SUINLEV-PR                     
113600           PERFORM IMS-REPL-WDK724                                        
113700         END-IF                                                           
113800                                                                          
113900         PERFORM IMS-GHU-ARTC11                                           
114000                                                                          
114100*    -- WDK712                                                            
114200         SEARCH ALL DC-LAND                                               
114300           AT END                                                         
114400             MOVE 'EJ TRÄFF I TAB DCLAND'                                 
114500                                TO FELTEXT                                
114600             CALL FELLOG                                                  
114700           WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC                         
114800             MOVE DCLAND-IDLANDX2 (DCLAND-IX)                             
114900                                TO W-IDLAND-K7                            
115000         END-SEARCH                                                       
115100         PERFORM IMS-GHU-WDK712                                           
115200**** HÄMTA VALUTAKURS FÖR CNY, AVGCO ÄR I CNY SKALL OMVANDLAS TILL        
115300**** SEK                                                                  
115400         MOVE DAGENS-DATUM(1:2) TO W-DATE-AAMM(1:2)                       
115500         MOVE 01                TO W-DATE-AAMM(3:2)                       
115600         MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                      
115700         MOVE WS-IDDC           TO W-IDDC-B6                              
115800         PERFORM IMS-GU-WDB601                                            
115900         MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW                      
116000         MOVE W-DATE-AAMM       TO CURR-TIAAMM                            
116100         MOVE 'A'               TO CURR-KDVALTYP                          
116200         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
116300         IF CURR-KDSVAR = ' '                                             
116400           MOVE CURR-PRKURS-NEW  TO W-PRKURS                              
116500           MOVE CURR-REVALUTA-TO TO W-REVALUTA                            
116600         ELSE                                                             
116700           MOVE 1                TO W-PRKURS                              
116800           MOVE 1                TO W-REVALUTA                            
116900         END-IF                                                           
117000**** PRARTSJK SKALL JUSTERAS VID EN INLEVERANS                            
117100         MOVE SPRL-PRARTBES-PR TO LART-PRARTSJK                           
117200**** JUSTERA OM DET FINNS KALKYLPÅLÄGG PÅ ARTIKELN                        
117300         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
117400         IF SEGMENT-FINNS                                                 
117500           PERFORM IMS-GNP-WDB617                                         
117600           IF SEGMENT-FINNS                                               
117700             COMPUTE LART-PRARTSJK ROUNDED =                              
117800                (SPRL-PRARTBES-PR *  W-PRKURS / W-REVALUTA ) +            
117900                (PROC-REDIRLON * ARTC-CLAG-PRDIRLON) +                    
118000                (PROC-REDMTRL    * ARTC-CLAG-PRDMTRL)                     
118100             END-COMPUTE                                                  
118200           END-IF                                                         
118300         END-IF                                                           
118400         PERFORM IMS-REPL-WDK712                                          
118500       END-IF                                                             
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900                                                                          
119000 DBAB-UPPD-ARTC-PRISJUST SECTION.                                         
119100     MOVE INLA-INL-IDLEVNR   TO W-IDLEVNR-21                              
119200     PERFORM IMS-GHNP-ARTC21                                              
119300                                                                          
119400     PERFORM UNTIL SEGMENT-SAKNAS OR PRIS-FINNS                           
119500      COMPUTE W-PRL-DADAT = 99999999 - ARTC-PRL-DAPRLIST-9KOMPL           
119600      IF ARTC-PRL-KDSTATUS-PR = +1 AND                                    
119700        W-PRL-DADAT <= WS-INLA-TIAVIDAT                                   
119800        MOVE JA               TO PRIS-FINNS-SW                            
119900      ELSE                                                                
120000        PERFORM IMS-GHNP-ARTC21                                           
120100      END-IF                                                              
120200     END-PERFORM                                                          
120300                                                                          
120400     IF  PRIS-FINNS                                                       
120500                                                                          
120600       MOVE ARTC-PRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-PR                
120700       MOVE ARTC-PRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-SUM               
120800       MOVE ARTC-PRL-PRARTBES-PR  TO WS-ARTC21-PRARTBES-PR                
120900       MOVE ARTC-PRL-KDVALISO     TO WS-ARTC21-KDVALISO                   
121000                                                                          
121100       IF  ARTC-PRL-SUINLEV-PR  = ZERO                                    
121200         MOVE +1                 TO ARTC-PRL-SUINLEV-PR                   
121300         PERFORM IMS-REPL-ARTC                                            
121400                                                                          
121500**NDC                                                                     
121600         IF CDC                                                           
121700           PERFORM IMS-GHU-ARTC11                                         
121800           COMPUTE ARTC-CLAG-PRARTSJK = WS-ARTC21-PRARTBES-PR +           
121900                                        ARTC-CLAG-PRDIRLON    +           
122000                                        ARTC-CLAG-PRDMTRL     +           
122100                                        ARTC-CLAG-PROVRPAL                
122200           END-COMPUTE                                                    
122300           MOVE +1                 TO ARTC-CLAG-KDTIPPR                   
122400           PERFORM IMS-REPL-ARTC                                          
122500         END-IF                                                           
122600                                                                          
122700       ELSE                                                               
122800         ADD +1                  TO ARTC-PRL-SUINLEV-PR                   
122900         PERFORM IMS-REPL-ARTC                                            
123000                                                                          
123100       END-IF                                                             
123200     END-IF                                                               
123300     .                                                                    
123400     EJECT                                                                
123500 DBB-UPPDATERA-ARTC-CDC SECTION.                                          
123600                                                                          
123700     MOVE INLA-ART-KVAVIS        TO WS-KV-LPLAN                           
123800                                                                          
123900     IF  INLA-ART-KDRT           = 01 OR 02                               
124000       MOVE INLA-ART-KVAVIS   TO WS-KV-LPLAN                              
124100     END-IF                                                               
124200                                                                          
124300     IF  INLA-ART-KDRT           = 09                                     
124400         ADD INLA-ART-KVAVIS     TO ARTC-CLAG-KVOVERF                     
124500     END-IF                                                               
124600                                                                          
124700     MOVE ARTC-CLAG-IDANSK          TO WS-ARTC-IDANSK                     
124800******                                                                    
124900     MOVE ARTC-CLAG-IDINK           TO WS-ARTC-IDINK-X                    
125000       IF ARTC-CLAG-IDINK (1:3) NUMERIC                                   
125100          MOVE ARTC-CLAG-IDINK (1:3) TO WS-ARTC-IDINK                     
125200       ELSE                                                               
125300          IF ARTC-CLAG-IDINK (2:3) NUMERIC                                
125400             MOVE ARTC-CLAG-IDINK (2:3) TO WS-ARTC-IDINK                  
125500          ELSE                                                            
125600             MOVE ZERO TO WS-ARTC-IDINK                                   
125700          END-IF                                                          
125800       END-IF                                                             
125900******                                                                    
126000     MOVE ARTC-CLAG-KDHF            TO WS-ARTC-KDHF                       
126100                                                                          
126200     IF  INLA-ART-KDRT           <= 05                                    
126300     AND (WS-ARTC-KDHF         > ZERO                                     
126400      OR  INLA-INL-IDLEVNR       = WS-ARTC-IDLEVNR)                       
126500       MOVE INLA-INL-IDLEVNR     TO ARTC-CLAG-IDLEVNR-SEN                 
126600       MOVE WS-IDFS              TO ARTC-CLAG-IDFS-SEN                    
126700       MOVE INLA-INL-TIAVIDAT    TO ARTC-CLAG-TIAVIDAT-SEN                
126800       MOVE INLA-ART-KVAVIS      TO ARTC-CLAG-KVAVIS-SEN                  
126900     END-IF                                                               
127000                                                                          
127100                                                                          
127200     MOVE ARTC-CLAG-KDTIPPR       TO WS-ARTC-KDTIPPR                      
127300     MOVE ARTC-CLAG-KDVTH         TO WS-ARTC-KDVTH                        
127400     MOVE ARTC-CLAG-PRDIRLON      TO WS-ARTC-PRDIRLON                     
127500     MOVE ARTC-CLAG-PRDMTRL       TO WS-ARTC-PRDMTRL                      
127600     MOVE ARTC-CLAG-PRINK         TO WS-ARTC-PRINK                        
127700     MOVE ARTC-CLAG-PROVRPAL      TO WS-ARTC-PROVRPAL                     
127800     MOVE ARTC-CLAG-PRARTSTD      TO WS-ARTC-PRARTSTD                     
127900     MOVE ARTC-CLAG-PRHEMTAG      TO WS-ARTC-PRHEMTAG                     
128000     MOVE ARTC-CLAG-FLTOPP        TO WS-ARTC-FLTOPP                       
128100     MOVE ARTC-CLAG-KDLTK         TO WS-ARTC-KDLTK                        
128200     MOVE ARTC-CLAG-KVQPACK-0     TO WS-ARTC-KVQPACK-0                    
128300     MOVE ARTC-CLAG-KVQPACK-1     TO WS-ARTC-KVQPACK-1                    
128400     MOVE ARTC-CLAG-IDARTNR-EMBQ0 TO WS-ARTC-IDARTNR-EMBQ0                
128500     MOVE ARTC-CLAG-IDARTNR-EMBQ3 TO WS-ARTC-IDARTNR-EMBQ3                
128600                                                                          
128700     MOVE ARTC-CLAG-KDERS          TO WS-ARTC-KDERS                       
128800     MOVE ARTC-CLAG-KVROS          TO WS-ARTC-KVROS                       
128900                                                                          
129000     IF CDC-SE                                                            
129100       ADD INLA-ART-KVAVIS         TO ARTC-CLAG-KVAKS-CDC                 
129200       COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC                           
129300                          + ARTC-CLAG-KVAKS-T                             
129400     ELSE                                                                 
129500       ADD INLA-ART-KVAVIS         TO ARTC-CLAG-KVAKS-T                   
129600       COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC                           
129700                          + ARTC-CLAG-KVAKS-T                             
129800     END-IF                                                               
129900                                                                          
130000     MOVE '+'                 TO LOGG-IDTECKEN-KVAKS                      
130100     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
130200     MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                           
130300     PERFORM IMS-REPL-ARTC                                                
130400     PERFORM S05-SKAPA-SALDOLOGG-WDK6                                     
130500     .                                                                    
130600     EJECT                                                                
130700                                                                          
130800 DBC-LAES-ARTC-UPPDAT-ARTS SECTION.                                       
130900     SKIP2                                                                
131000                                                                          
131100     PERFORM IMS-GHU-ARTC11                                               
131200                                                                          
131300     MOVE ARTC-CLAG-IDANSK          TO WS-ARTC-IDANSK                     
131400******                                                                    
131500     MOVE ARTC-CLAG-IDINK           TO WS-ARTC-IDINK-X                    
131600       IF ARTC-CLAG-IDINK (1:3) NUMERIC                                   
131700          MOVE ARTC-CLAG-IDINK (1:3) TO WS-ARTC-IDINK                     
131800       ELSE                                                               
131900          IF ARTC-CLAG-IDINK (2:3) NUMERIC                                
132000             MOVE ARTC-CLAG-IDINK (2:3) TO WS-ARTC-IDINK                  
132100          ELSE                                                            
132200             MOVE ZERO TO WS-ARTC-IDINK                                   
132300          END-IF                                                          
132400       END-IF                                                             
132500******                                                                    
132600     MOVE ARTC-CLAG-KDHF            TO WS-ARTC-KDHF                       
132700                                                                          
132800     MOVE ARTC-CLAG-KDTIPPR       TO WS-ARTC-KDTIPPR                      
132900     MOVE ARTC-CLAG-KDVTH         TO WS-ARTC-KDVTH                        
133000     MOVE ARTC-CLAG-PRDIRLON      TO WS-ARTC-PRDIRLON                     
133100     MOVE ARTC-CLAG-PRDMTRL       TO WS-ARTC-PRDMTRL                      
133200     MOVE ARTC-CLAG-PRINK         TO WS-ARTC-PRINK                        
133300     MOVE ARTC-CLAG-PRARTSTD      TO WS-ARTC-PRARTSTD                     
133400     MOVE ARTC-CLAG-PRHEMTAG      TO WS-ARTC-PRHEMTAG                     
133500     MOVE ARTC-CLAG-PROVRPAL      TO WS-ARTC-PROVRPAL                     
133600     MOVE ARTC-CLAG-FLTOPP        TO WS-ARTC-FLTOPP                       
133700     MOVE ARTC-CLAG-KDLTK         TO WS-ARTC-KDLTK                        
133800     MOVE ARTC-CLAG-KVQPACK-0     TO WS-ARTC-KVQPACK-0                    
133900     MOVE ARTC-CLAG-KVQPACK-1     TO WS-ARTC-KVQPACK-1                    
134000     MOVE ARTC-CLAG-IDARTNR-EMBQ0 TO WS-ARTC-IDARTNR-EMBQ0                
134100     MOVE ARTC-CLAG-IDARTNR-EMBQ3 TO WS-ARTC-IDARTNR-EMBQ3                
134200                                                                          
134300     MOVE ARTC-CLAG-KDERS          TO WS-ARTC-KDERS                       
134400                                                                          
134500     IF NDC-CN OR NDC-US                                                  
134600       PERFORM DBCA-ANSKAFFARE-LARM-CN-US                                 
134700     END-IF                                                               
134800                                                                          
134900     PERFORM IMS-GHU-ARTS11                                               
135000                                                                          
135100     COMPUTE WS-ARTS-KVROS = ARTS-SLAG-KVROS-BULK +                       
135200                             ARTS-SLAG-KVROS-DAG                          
135300     END-COMPUTE                                                          
135400                                                                          
135500     ADD INLA-ART-KVAVIS          TO ARTS-SLAG-KVAKS-SDC                  
135600                                                                          
135700     PERFORM IMS-REPL-ARTS                                                
135800     PERFORM S06-SKAPA-SALDOLOGG-WDK7                                     
135900                                                                          
136000     MOVE INLA-INL-IDLEVNR      TO W-IDLEVNR                              
136100     PERFORM IMS-GU-WDF101                                                
136200     IF SEGMENT-SAKNAS                                                    
136300       MOVE ZERO TO W-RETULF                                              
136400     ELSE                                                                 
136500       MOVE WS-IDDC          TO W-IDDC-B6                                 
136600       PERFORM IMS-GU-WDB601                                              
136700       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
136800       PERFORM IMS-GNP-WDF102                                             
136900       IF SEGMENT-FINNS                                                   
137000         IF F102-TULL-TITULF < DAGENS-DATUM-Y2K                           
137100           MOVE F102-TULL-RETULF-1  TO W-RETULF                           
137200         ELSE                                                             
137300           MOVE F102-TULL-RETULF-2  TO W-RETULF                           
137400         END-IF                                                           
137500       END-IF                                                             
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900 DBCA-ANSKAFFARE-LARM-CN-US SECTION.                                      
138000     MOVE 'DBCA-ANSKAFF-LARM-CN-US '  TO CURRENT-SECTION                  
138100                                                                          
138200*--- HÄMTA ANSKAFFARE FÖR MID-NDC, (SAKNAS DETTA HÄMTA FRÅN DC71?)        
138300*--- OM DETTA SAKNAS ELLER ÄR = 0, TAG IDANSK FRÅN CDC.                   
138400                                                                          
138500     MOVE WS-ARTC-IDANSK   TO WS-IDANSK-WDK722                            
138600*                                                                         
138700     PERFORM IMS-GU-ARTS11                                                
138800     PERFORM IMS-GNP-WDK722                                               
138900     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
139000       MOVE XLAG-IDANSK      TO WS-IDANSK-WDK722                          
139100     END-IF                                                               
139200     .                                                                    
139300     EJECT                                                                
139400 DC-UPPD-INLB-LEVPL SECTION.                                              
139500                                                                          
139600     MOVE JA             TO AVROP-SW                                      
139700                                                                          
139800     IF  WS-KV-LPLAN             > ZERO                                   
139900                                                                          
140000       IF  INLA-ART-KDRT         = 00                                     
140100       OR  (INLA-ART-KDRT        = 01                                     
140200        AND WS-ARTC-KDHF       > ZERO)                                    
140300       OR  (INLA-ART-KDRT        = 02                                     
140400        AND WS-ARTC-KDHF       > ZERO)                                    
140500       OR  INLA-ART-KDRT         = 03                                     
140600       OR  INLA-ART-KDRT         = 05                                     
140700       OR  (INLA-ART-KDRT        = 06                                     
140800        AND INLA-INL-IDLEVNR     NOT = SPACE                              
140900        AND INLA-INL-IDLEVNR     NOT = '9999 ')                           
141000       OR  INLA-ART-KDRT         = 09                                     
141100       OR  INLA-ART-KDRT         = 10                                     
141200                                                                          
141300         IF  (INLA-ART-KDRT      = 00                                     
141400          OR  INLA-ART-KDRT      = 01                                     
141500          OR  INLA-ART-KDRT      = 02                                     
141600          OR  INLA-ART-KDRT      = 09)                                    
141700         AND WS-ARTC-KDHF      > ZERO                                     
141800           MOVE WS-ARTC-IDLEVNR TO W-INLB11-IDLEVNR                       
141900         ELSE                                                             
142000           MOVE INLA-INL-IDLEVNR TO W-INLB11-IDLEVNR                      
142100         END-IF                                                           
142200                                                                          
142300         MOVE INLA-ART-IDARTNR   TO W-IDARTNR-D9                          
142400         MOVE INLA-ART-IDDC      TO W-IDDC-D9                             
142500         PERFORM IMS-GU-INLB11                                            
142600                                                                          
142700         IF  SEGMENT-FINNS                                                
142800                                                                          
142900           MOVE NEJ TO SW-WDF1-FINNS                                      
143000           IF  INLA-ART-KDRT     NOT = 10                                 
143100             PERFORM DCA-BOKA-LEVPL-AVROP                                 
143200           END-IF                                                         
143300                                                                          
143400           PERFORM DCB-BOKA-LEVPL-LBESK                                   
143500                                                                          
143600           IF  INLA-ART-KDRT     NOT = 03                                 
143700             PERFORM DCC-BOKA-LEVPL-BREST                                 
143800           END-IF                                                         
143900         ELSE                                                             
144000           MOVE NEJ              TO AVROP-SW                              
144100         END-IF                                                           
144200       END-IF                                                             
144300     END-IF                                                               
144400     .                                                                    
144500     EJECT                                                                
144600 DCA-BOKA-LEVPL-AVROP SECTION.                                            
144700                                                                          
144800     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
144900     MOVE ZERO                   TO WS-INLB23-TIAVROP-INL                 
145000                                                                          
145100     IF  INLA-ART-KDRT           NOT = 03                                 
145200                                                                          
145300       MOVE +2                   TO W-KDAVROP                             
145400       PERFORM IMS-GHNP-INLB23-KD                                         
145500       IF SEGMENT-SAKNAS                                                  
145600         MOVE NEJ       TO AVROP-SW                                       
145700       END-IF                                                             
145800       PERFORM UNTIL (SEGMENT-SAKNAS                                      
145900                  OR WS-KV-OBOK  = ZERO)                                  
146000         MOVE INLB23-DAAVROP-AVS TO W-DAAVROP-LARM                        
146100         MOVE INLB23-TILEVDAG    TO W-TILEVDAG-LARM                       
146200         MOVE +2                 TO W-KDAVROP-LARM                        
146300                                                                          
146400         PERFORM DCAA-BOKA-ETT-AVROP                                      
146500                                                                          
146600         IF  WS-KV-OBOK          > ZERO                                   
146700           PERFORM IMS-GHNP-INLB23-KD                                     
146800         END-IF                                                           
146900       END-PERFORM                                                        
147000                                                                          
147100     ELSE                                                                 
147200                                                                          
147300       MOVE +2                   TO W-KDAVROP                             
147400       MOVE WS-IDAVINR (3:4)     TO W-IDORDNSB                            
147500       PERFORM IMS-GNP-INLB32                                             
147600                                                                          
147700       MOVE INLB-KFB-DAAVROP-AVS TO W-DAAVROP                             
147800       MOVE INLB-KFB-TILEVDAG    TO W-TILEVDAG                            
147900       PERFORM IMS-GHNP-INLB23-TI-F                                       
148000                                                                          
148100         MOVE INLB23-DAAVROP-AVS TO W-DAAVROP-LARM                        
148200         MOVE INLB23-TILEVDAG    TO W-TILEVDAG-LARM                       
148300         MOVE +2                 TO W-KDAVROP-LARM                        
148400                                                                          
148500       PERFORM DCAA-BOKA-ETT-AVROP                                        
148600     END-IF                                                               
148700     .                                                                    
148800     EJECT                                                                
148900 DCAA-BOKA-ETT-AVROP SECTION.                                             
149000                                                                          
149100     IF  WS-INLB23-TIAVROP-INL  = ZERO                                    
149200       MOVE INLB23-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
149300       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
149400       CALL WDATKONV USING          DAT-KDDATFORM                         
149500                                    DAT-I-TIDATUM                         
149600                                    DAT-O-TIDATUM                         
149700                                    DAT-KDSVAR                            
149800       IF DAT-KDSVAR-FEL                                                  
149900         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                   
150000         CALL FELLOG                                                      
150100       ELSE                                                               
150200         MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                             
150300         MOVE WS-TIAAVV          TO WS-INLB23-TIAVROP-INL                 
150400       END-IF                                                             
150500     END-IF                                                               
150600                                                                          
150700     IF  INLB23-KVAVROP          <= WS-KV-OBOK                            
150800       SUBTRACT INLB23-KVAVROP   FROM WS-KV-OBOK                          
150900       MOVE INLB23-KVAVROP       TO INLB31-KVAVROP-AVB                    
151000       MOVE ZERO                 TO INLB23-KVAVROP                        
151100       MOVE +9                   TO INLB23-KDAVROP                        
151200     ELSE                                                                 
151300       SUBTRACT WS-KV-OBOK       FROM INLB23-KVAVROP                      
151400       MOVE WS-KV-OBOK           TO INLB31-KVAVROP-AVB                    
151500       MOVE ZERO                 TO WS-KV-OBOK                            
151600     END-IF                                                               
151700                                                                          
151800     MOVE WS-IDLOPNRM-AAVVDLLLL  TO INLB31-IDLOPNRM-PL                    
151900     MOVE INLB23-TIAVRDAT-INL    TO WS-TIAVRDAT-INL                       
152000                                                                          
152100     PERFORM IMS-REPL-INLB                                                
152200                                                                          
152300     PERFORM IMS-ISRT-INLB31                                              
152400                                                                          
152500     IF INLA-ART-KVAVIS > INLB31-KVAVROP-AVB                              
152600        PERFORM DCAAA-KOLL-LARM-230-235                                   
152700     END-IF                                                               
152800     .                                                                    
152900     EJECT                                                                
153000 DCAAA-KOLL-LARM-230-235 SECTION.                                         
153100                                                                          
153200     PERFORM IMS-GU-WDD905-NEXT                                           
153300                                                                          
153400     IF SEGMENT-FINNS                                                     
153500*       SKALL EJ LARMA OM SLÄP                                            
153600        MOVE WS-TIAAMMDD TO JUST-TIAAMMDD                                 
153700        IF SW-WDF1-FINNS = NEJ                                            
153800           PERFORM IMS-GET-WDF101                                         
153900        END-IF                                                            
154000        IF WDF1-FINNS AND F1-LEV-KVDAGAR-INLAVV > ZERO                    
154100*         JUSTERING GÖRS MED ANTALET DAGAR (X) I KVDAGAR-INLAVV           
154200*         DVS ATT VI FÅR JÄMFÖRA MED DAGENS-DATUM + X                     
154300                                                                          
154400          MOVE 2                  TO WORK-KDCALL                          
154500          MOVE '11'               TO WORK-IDDC                            
154600          MOVE WS-TIAAMMDD        TO WORK-TIAAMMDD-FOM                    
154700          MOVE F1-LEV-KVDAGAR-INLAVV  TO WORK-KVWORKD                     
154800          ADD  +1                 TO WORK-KVWORKD                         
154900          CALL WORKDAY USING WORK-KDCALL                                  
155000                    WORK-DATE-AREA WORK-KDSVAR                            
155100          IF WORK-KDSVAR-OK                                               
155200            MOVE WORK-TIAAMMDD-TOM TO JUST-TIAAMMDD                       
155300          ELSE                                                            
155400            STRING 'FEL FRÅN WORKDAY : ' WORK-KDSVAR                      
155500            DELIMITED BY SIZE INTO FELTEXT                                
155600            CALL FELLOG                                                   
155700          END-IF                                                          
155800        END-IF                                                            
155900                                                                          
156000        IF WS-TIAVRDAT-INL >= JUST-TIAAMMDD                               
156100*          LARM 230  TOO EARLY                                            
156200           MOVE 230                TO LAK-KDLARM                          
156300           PERFORM DCAAAA-LARM-230-235                                    
156400        END-IF                                                            
156500     ELSE                                                                 
156600*       LARM 235  NOT SCHEDULED                                           
156700        MOVE 235                TO LAK-KDLARM                             
156800        PERFORM DCAAAA-LARM-230-235                                       
156900     END-IF                                                               
157000     .                                                                    
157100     EJECT                                                                
157200 DCAAAA-LARM-230-235 SECTION.                                             
157300                                                                          
157400       MOVE INLA-INL-IDLEVNR   TO WS-IDLEVNR-EMIL                         
157500       IF (NOT EJ-GODK-EMIL-LEVNR)                                        
157600        ACCEPT W1-TIKLOCK FROM TIME                                       
157700        COMPUTE W1-TIKLOCK-9KOMPL =                                       
157800                +999999999 - W1-TIKLOCK                                   
157900        MOVE W1-DAREGDAT-9KOMPL TO LAK-DAREGDAT-9KOMPL                    
158000        MOVE W1-TIKLOCK-9KOMPL  TO LAK-TIKLOCK-9KOMPL                     
158100        MOVE INLA-ART-IDARTNR   TO LAK-IDARTNR                            
158200        MOVE INLA-INL-IDDC      TO LAK-IDDC                               
158300        MOVE WS-ARTC-IDANSK     TO LAK-IDANSK                             
158400                                   W-IDANSK-L                             
158500        PERFORM IMS-GET-XXBX-2232                                         
158600        IF SEGMENT-FINNS                                                  
158700          MOVE XXBX-2232-IDANSK-LARM                                      
158800                                TO LAK-IDANSK                             
158900        END-IF                                                            
159000        MOVE INLA-INL-IDLEVNR   TO LAK-IDLEVNR                            
159100        MOVE '*'                TO LAK-FLNYLARM                           
159200        MOVE INLA-ART-KVAVIS    TO LAK-KVAVIS                             
159300        MOVE INLB31-KVAVROP-AVB TO LAK-KVAVROP                            
159400        MOVE WS-TIAVRDAT-INL    TO LAK-TIAAMMDD                           
159500        MOVE ZERO               TO LAK-TIAAMMDD-AVS                       
159600                                                                          
159700        PERFORM IMS-ISRT-WDD401                                           
159800       END-IF                                                             
159900     .                                                                    
160000     EJECT                                                                
160100 DCB-BOKA-LEVPL-LBESK SECTION.                                            
160200                                                                          
160300     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
160400                                                                          
160500     PERFORM IMS-GHNP-INLB24                                              
160600                                                                          
160700     PERFORM UNTIL (SEGMENT-SAKNAS                                        
160800                OR  WS-KV-OBOK   = ZERO)                                  
160900                                                                          
161000       MOVE DAGENS-DATUM     TO INLB24-LEV-TIREGDAT                       
161100       MOVE DAGENS-TID-6-POS TO INLB24-LEV-TIREGTID                       
161200                                                                          
161300       IF  INLB24-LEV-KVAVIS-BSKKVAR < WS-KV-OBOK                         
161400*      -- CL BOKAS NED HELT                                               
161500         SUBTRACT INLB24-LEV-KVAVIS-BSKKVAR                               
161600                                   FROM WS-KV-OBOK                        
161700         MOVE ZERO TO INLB24-LEV-KVAVIS-BSKKVAR                           
161800       ELSE                                                               
161900*      -- CL BOKAS NED DELVIS                                             
162000         SUBTRACT WS-KV-OBOK                                              
162100             FROM INLB24-LEV-KVAVIS-BSKKVAR                               
162200         MOVE ZERO                 TO WS-KV-OBOK                          
162300       END-IF                                                             
162400                                                                          
162500       COMPUTE WS-BSKKVAR-GGR-10                                          
162600         = INLB24-LEV-KVAVIS-BSKKVAR                                      
162700         * 10                                                             
162800       END-COMPUTE                                                        
162900                                                                          
163000       IF WS-BSKKVAR-GGR-10 < INLB24-LEV-KVAVIS-BSKURS                    
163100         PERFORM IMS-DLET-INLB                                            
163200       ELSE                                                               
163300         PERFORM IMS-REPL-INLB                                            
163400       END-IF                                                             
163500                                                                          
163600       IF  WS-KV-OBOK            > ZERO                                   
163700         PERFORM IMS-GHNP-INLB24                                          
163800       END-IF                                                             
163900     END-PERFORM                                                          
164000     .                                                                    
164100     EJECT                                                                
164200 DCC-BOKA-LEVPL-BREST SECTION.                                            
164300                                                                          
164400     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
164500                                                                          
164600     PERFORM IMS-GHU-INLB11                                               
164700                                                                          
164800     IF  INLB11-KVBR             <= WS-KV-OBOK                            
164900       MOVE ZERO                 TO INLB11-KVBR                           
165000     ELSE                                                                 
165100       SUBTRACT WS-KV-OBOK       FROM INLB11-KVBR                         
165200     END-IF                                                               
165300                                                                          
165400     PERFORM IMS-REPL-INLB                                                
165500     .                                                                    
165600     EJECT                                                                
165700 DD-UPPD-INLB-LEVPL-LOKAL SECTION.                                        
165800     MOVE 'DD-UPPD-INLB-LEVPL-LOKAL ' TO CURRENT-SECTION                  
165900                                                                          
166000     MOVE JA    TO AVROP-LOKAL-SW                                         
166100                                                                          
166200     IF INLA-ART-KVAVIS    > ZERO                                         
166300                                                                          
166400       MOVE INLA-INL-IDLEVNR   TO W-INLB11-IDLEVNR                        
166500                                                                          
166600       MOVE INLA-ART-IDARTNR   TO W-IDARTNR-D9                            
166700       MOVE INLA-ART-IDDC      TO W-IDDC-D9                               
166800       PERFORM IMS-GU-INLB11                                              
166900                                                                          
167000       IF  SEGMENT-FINNS                                                  
167100                                                                          
167200         MOVE NEJ TO SW-WDF1-FINNS                                        
167300         IF  INLA-ART-KDRT     NOT = 10                                   
167400           PERFORM DDA-BOKA-LEVPL-AVROP-LOKAL                             
167500         END-IF                                                           
167600                                                                          
167700         PERFORM DDB-BOKA-LEVPL-LBESK-LOKAL                               
167800       ELSE                                                               
167900         MOVE NEJ          TO AVROP-LOKAL-SW                              
168000       END-IF                                                             
168100     END-IF                                                               
168200                                                                          
168300     IF AVROP-LOKAL-SAKNAS                                                
168400       PERFORM DDC-SKAPA-LARM-235-LOKAL                                   
168500     END-IF                                                               
168600                                                                          
168700     .                                                                    
168800     EJECT                                                                
168900 DDA-BOKA-LEVPL-AVROP-LOKAL SECTION.                                      
169000     MOVE 'DDA-BOKA-LEVPL-AVROP-LOKAL ' TO CURRENT-SECTION                
169100                                                                          
169200     MOVE INLA-ART-KVAVIS        TO WS-KV-OBOK                            
169300     MOVE ZERO                   TO WS-INLB23-TIAVROP-INL                 
169400                                                                          
169500     MOVE +2                   TO W-KDAVROP                               
169600     PERFORM IMS-GHNP-INLB23-KD                                           
169700     IF SEGMENT-SAKNAS                                                    
169800       MOVE NEJ       TO AVROP-LOKAL-SW                                   
169900     END-IF                                                               
170000     PERFORM UNTIL (SEGMENT-SAKNAS                                        
170100                OR WS-KV-OBOK  = ZERO)                                    
170200       MOVE INLB23-DAAVROP-AVS TO W-DAAVROP-LARM                          
170300       MOVE INLB23-TILEVDAG    TO W-TILEVDAG-LARM                         
170400       MOVE +2                 TO W-KDAVROP-LARM                          
170500                                                                          
170600       PERFORM DDAA-BOKA-ETT-AVROP-LOKAL                                  
170700                                                                          
170800       IF  WS-KV-OBOK          > ZERO                                     
170900         PERFORM IMS-GHNP-INLB23-KD                                       
171000       END-IF                                                             
171100     END-PERFORM                                                          
171200                                                                          
171300     .                                                                    
171400     EJECT                                                                
171500 DDAA-BOKA-ETT-AVROP-LOKAL SECTION.                                       
171600     MOVE 'DDAA-BOKA-ETT-AVROP-LOKAL ' TO CURRENT-SECTION                 
171700                                                                          
171800     IF  WS-INLB23-TIAVROP-INL  = ZERO                                    
171900       MOVE INLB23-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
172000       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
172100       CALL WDATKONV USING          DAT-KDDATFORM                         
172200                                    DAT-I-TIDATUM                         
172300                                    DAT-O-TIDATUM                         
172400                                    DAT-KDSVAR                            
172500       IF DAT-KDSVAR-FEL                                                  
172600         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                   
172700         CALL FELLOG                                                      
172800       ELSE                                                               
172900         MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                             
173000         MOVE WS-TIAAVV          TO WS-INLB23-TIAVROP-INL                 
173100       END-IF                                                             
173200     END-IF                                                               
173300                                                                          
173400     IF  INLB23-KVAVROP          <= WS-KV-OBOK                            
173500       SUBTRACT INLB23-KVAVROP   FROM WS-KV-OBOK                          
173600       MOVE INLB23-KVAVROP       TO INLB31-KVAVROP-AVB                    
173700       MOVE ZERO                 TO INLB23-KVAVROP                        
173800       MOVE +9                   TO INLB23-KDAVROP                        
173900     ELSE                                                                 
174000       SUBTRACT WS-KV-OBOK       FROM INLB23-KVAVROP                      
174100       MOVE WS-KV-OBOK           TO INLB31-KVAVROP-AVB                    
174200       MOVE ZERO                 TO WS-KV-OBOK                            
174300     END-IF                                                               
174400                                                                          
174500     MOVE WS-IDLOPNRM-AAVVDLLLL  TO INLB31-IDLOPNRM-PL                    
174600     MOVE INLB23-TIAVRDAT-INL    TO WS-TIAVRDAT-INL                       
174700                                                                          
174800     PERFORM IMS-REPL-INLB                                                
174900                                                                          
175000     PERFORM IMS-ISRT-INLB31                                              
175100                                                                          
175200     IF INLA-ART-KVAVIS > INLB31-KVAVROP-AVB                              
175300        PERFORM DDAAA-KOLL-LARM-230-235-LOKAL                             
175400     END-IF                                                               
175500                                                                          
175600     .                                                                    
175700     EJECT                                                                
175800 DDAAA-KOLL-LARM-230-235-LOKAL SECTION.                                   
175900     MOVE 'DDAAA-KOLL-LARM-230-235-LOKAL ' TO CURRENT-SECTION             
176000                                                                          
176100     PERFORM IMS-GU-WDD905-NEXT                                           
176200                                                                          
176300     IF SEGMENT-FINNS                                                     
176400*       SKALL EJ LARMA OM SLÄP                                            
176500        MOVE WS-TIAAMMDD TO JUST-TIAAMMDD                                 
176600        IF SW-WDF1-FINNS = NEJ                                            
176700           PERFORM IMS-GET-WDF101                                         
176800        END-IF                                                            
176900        IF WDF1-FINNS AND F1-LEV-KVDAGAR-INLAVV > ZERO                    
177000*         JUSTERING GÖRS MED ANTALET DAGAR (X) I KVDAGAR-INLAVV           
177100*         DVS ATT VI FÅR JÄMFÖRA MED DAGENS-DATUM + X                     
177200                                                                          
177300          MOVE 2                  TO WORK-KDCALL                          
177400          MOVE INLA-ART-IDDC      TO WORK-IDDC                            
177500          MOVE WS-TIAAMMDD        TO WORK-TIAAMMDD-FOM                    
177600          MOVE F1-LEV-KVDAGAR-INLAVV  TO WORK-KVWORKD                     
177700          ADD  +1                 TO WORK-KVWORKD                         
177800          CALL WORKDAY USING WORK-KDCALL                                  
177900                    WORK-DATE-AREA WORK-KDSVAR                            
178000          IF WORK-KDSVAR-OK                                               
178100            MOVE WORK-TIAAMMDD-TOM TO JUST-TIAAMMDD                       
178200          ELSE                                                            
178300            STRING 'FEL FRÅN WORKDAY : ' WORK-KDSVAR                      
178400            DELIMITED BY SIZE INTO FELTEXT                                
178500            CALL FELLOG                                                   
178600          END-IF                                                          
178700        END-IF                                                            
178800                                                                          
178900        IF WS-TIAVRDAT-INL >= JUST-TIAAMMDD                               
179000*          LARM 230  TOO EARLY                                            
179100           MOVE 230                TO LAK-KDLARM                          
179200           PERFORM DDAAAA-LARM-230-235-LOKAL                              
179300        END-IF                                                            
179400     ELSE                                                                 
179500*       LARM 235  NOT SCHEDULED                                           
179600        MOVE 235                TO LAK-KDLARM                             
179700        PERFORM DDAAAA-LARM-230-235-LOKAL                                 
179800     END-IF                                                               
179900     .                                                                    
180000     EJECT                                                                
180100 DDAAAA-LARM-230-235-LOKAL SECTION.                                       
180200     MOVE 'DDAAAA-LARM-230-235-LOKAL   ' TO CURRENT-SECTION               
180300                                                                          
180400     ACCEPT W1-TIKLOCK FROM TIME                                          
180500     COMPUTE W1-TIKLOCK-9KOMPL =                                          
180600             +999999999 - W1-TIKLOCK                                      
180700     MOVE W1-DAREGDAT-9KOMPL TO LAK-DAREGDAT-9KOMPL                       
180800     MOVE W1-TIKLOCK-9KOMPL  TO LAK-TIKLOCK-9KOMPL                        
180900     MOVE INLA-ART-IDARTNR   TO LAK-IDARTNR                               
181000     MOVE INLA-INL-IDDC      TO LAK-IDDC                                  
181100     MOVE WS-IDANSK-WDK722   TO LAK-IDANSK                                
181200                                W-IDANSK-L                                
181300     PERFORM IMS-GET-XXBX-2232                                            
181400     IF SEGMENT-FINNS                                                     
181500       MOVE XXBX-2232-IDANSK-LARM                                         
181600                             TO LAK-IDANSK                                
181700     END-IF                                                               
181800     MOVE INLA-INL-IDLEVNR   TO LAK-IDLEVNR                               
181900     MOVE '*'                TO LAK-FLNYLARM                              
182000     MOVE INLA-ART-KVAVIS    TO LAK-KVAVIS                                
182100     MOVE INLB31-KVAVROP-AVB TO LAK-KVAVROP                               
182200     MOVE WS-TIAVRDAT-INL    TO LAK-TIAAMMDD                              
182300     MOVE ZERO               TO LAK-TIAAMMDD-AVS                          
182400                                                                          
182500     PERFORM IMS-ISRT-WDD401                                              
182600     .                                                                    
182700     EJECT                                                                
182800 DDB-BOKA-LEVPL-LBESK-LOKAL SECTION.                                      
182900     MOVE 'DDB-BOKA-LEVPL-LBESK-LOKAL ' TO CURRENT-SECTION                
183000                                                                          
183100     MOVE INLA-ART-KVAVIS        TO WS-KV-OBOK                            
183200                                                                          
183300     PERFORM IMS-GHNP-INLB24                                              
183400                                                                          
183500     PERFORM UNTIL (SEGMENT-SAKNAS                                        
183600                OR  WS-KV-OBOK   = ZERO)                                  
183700                                                                          
183800       MOVE DAGENS-DATUM     TO INLB24-LEV-TIREGDAT                       
183900       MOVE DAGENS-TID-6-POS TO INLB24-LEV-TIREGTID                       
184000                                                                          
184100       IF  INLB24-LEV-KVAVIS-BSKKVAR < WS-KV-OBOK                         
184200*      -- NDC BOKAS NED HELT                                              
184300         SUBTRACT INLB24-LEV-KVAVIS-BSKKVAR                               
184400                                   FROM WS-KV-OBOK                        
184500         MOVE ZERO TO INLB24-LEV-KVAVIS-BSKKVAR                           
184600       ELSE                                                               
184700*      -- NDC BOKAS NED DELVIS                                            
184800         SUBTRACT WS-KV-OBOK                                              
184900             FROM INLB24-LEV-KVAVIS-BSKKVAR                               
185000         MOVE ZERO                 TO WS-KV-OBOK                          
185100       END-IF                                                             
185200                                                                          
185300       COMPUTE WS-BSKKVAR-GGR-10                                          
185400         = INLB24-LEV-KVAVIS-BSKKVAR                                      
185500         * 10                                                             
185600       END-COMPUTE                                                        
185700                                                                          
185800       IF WS-BSKKVAR-GGR-10 < INLB24-LEV-KVAVIS-BSKURS                    
185900         PERFORM IMS-DLET-INLB                                            
186000       ELSE                                                               
186100         PERFORM IMS-REPL-INLB                                            
186200       END-IF                                                             
186300                                                                          
186400       IF  WS-KV-OBOK            > ZERO                                   
186500         PERFORM IMS-GHNP-INLB24                                          
186600       END-IF                                                             
186700     END-PERFORM                                                          
186800     .                                                                    
186900     EJECT                                                                
187000 DDC-SKAPA-LARM-235-LOKAL SECTION.                                        
187100     MOVE 'DDC-SKAPA-LARM-235-LOKAL ' TO CURRENT-SECTION                  
187200                                                                          
187300     ACCEPT W1-TIKLOCK FROM TIME                                          
187400     COMPUTE W1-TIKLOCK-9KOMPL =                                          
187500             +999999999 - W1-TIKLOCK                                      
187600     MOVE W1-DAREGDAT-9KOMPL TO LAK-DAREGDAT-9KOMPL                       
187700     MOVE W1-TIKLOCK-9KOMPL  TO LAK-TIKLOCK-9KOMPL                        
187800     MOVE INLA-ART-IDARTNR   TO LAK-IDARTNR                               
187900     MOVE INLA-INL-IDDC      TO LAK-IDDC                                  
188000     MOVE WS-IDANSK-WDK722   TO LAK-IDANSK                                
188100                                W-IDANSK-L                                
188200     PERFORM IMS-GET-XXBX-2232                                            
188300     IF SEGMENT-FINNS                                                     
188400       MOVE XXBX-2232-IDANSK-LARM                                         
188500                             TO LAK-IDANSK                                
188600     END-IF                                                               
188700     MOVE INLA-INL-IDLEVNR   TO LAK-IDLEVNR                               
188800     MOVE 235                TO LAK-KDLARM                                
188900     MOVE '*'                TO LAK-FLNYLARM                              
189000     MOVE INLA-ART-KVAVIS    TO LAK-KVAVIS                                
189100     MOVE ZERO               TO LAK-KVAVROP                               
189200     MOVE WS-TIAAMMDD        TO LAK-TIAAMMDD                              
189300     MOVE ZERO               TO LAK-TIAAMMDD-AVS                          
189400                                                                          
189500     PERFORM IMS-ISRT-WDD401                                              
189600                                                                          
189700     .                                                                    
189800     EJECT                                                                
189900 DE-UPPD-HISTORIK SECTION.                                                
190000                                                                          
190100     IF CDC                                                               
190200       PERFORM DEA-UPPD-INLE-HIST                                         
190300     ELSE                                                                 
190400       PERFORM DEB-UPPD-WDL611-HIST                                       
190500       PERFORM DEC-UPPD-WDL612-HIST                                       
190600**NDC  SKA UPPDATERA SIN HISTORIK PÅ WDL6                                 
190700     END-IF                                                               
190800                                                                          
190900     .                                                                    
191000     EJECT                                                                
191100 DEA-UPPD-INLE-HIST SECTION.                                              
191200     SKIP2                                                                
191300     PERFORM IMS-GU-INLE-ART                                              
191400                                                                          
191500     IF  SEGMENT-SAKNAS                                                   
191600       MOVE INLA-ART-IDARTNR     TO INLE-ART-IDARTNR                      
191700       PERFORM IMS-ISRT-INLE-ART                                          
191800     END-IF                                                               
191900                                                                          
192000     MOVE WS-DAINLEV             TO INLE-INL-DAINLEV                      
192100     PERFORM IMS-ISRT-INLE-INL                                            
192200     PERFORM UNTIL SEGMENT-FINNS                                          
192300       SUBTRACT +1 FROM WS-DAINLEV                                        
192400       MOVE WS-DAINLEV             TO INLE-INL-DAINLEV                    
192500       PERFORM IMS-ISRT-INLE-INL                                          
192600     END-PERFORM                                                          
192700                                                                          
192800     MOVE INLA-INL-KDINL         TO INLE-MOT-IDPTYP                       
192900     MOVE WS-IDLOPNRM-VVDLLLLK   TO INLE-MOT-IDLOPNRM                     
193000     MOVE WS-IDAVINR             TO INLE-MOT-IDAVINR                      
193100     MOVE INLA-INL-IDKONTO       TO INLE-MOT-IDKONTO                      
193200     MOVE INLA-INL-IDANALYS      TO INLE-MOT-IDANALYS                     
193300     MOVE INLA-INL-IDKST         TO INLE-MOT-IDKST                        
193400     MOVE INLA-INL-IDLEVNR       TO INLE-MOT-IDLEVNR                      
193500     MOVE INLA-ART-ADLAGOMR      TO INLE-MOT-ADLAGOMR                     
193600     MOVE INLA-ART-ADGANG        TO INLE-MOT-ADGANG                       
193700     MOVE INLA-ART-ADPLATS       TO INLE-MOT-ADPLATS                      
193800     MOVE WS-IDDC                TO INLE-MOT-IDDC                         
193900     MOVE INLA-ART-KDRT          TO INLE-MOT-KDRT                         
194000     MOVE WS-IDFS                TO INLE-MOT-IDFS                         
194100     MOVE ZERO                   TO INLE-MOT-KDAVVANT                     
194200     MOVE ZERO                   TO INLE-MOT-KDAVVKV                      
194300     MOVE ZERO                   TO INLE-MOT-KVANTMOT                     
194400     MOVE INLA-ART-KVAVIS        TO INLE-MOT-KVAVIS                       
194500     MOVE ZERO                   TO INLE-MOT-KVFORDEL                     
194600     MOVE ZERO                   TO INLE-MOT-KVRETUR                      
194700     MOVE ZERO                   TO INLE-MOT-KVFORV                       
194800     MOVE INLA-INL-TIAVIDAT      TO INLE-MOT-TIAVIDAT                     
194900     MOVE ZERO                   TO INLE-MOT-TIUPPDAT                     
195000     MOVE ZERO                   TO INLE-MOT-IDSHIPM                      
195100                                                                          
195200     PERFORM IMS-ISRT-INLE-MOT                                            
195300     .                                                                    
195400     EJECT                                                                
195500 DEB-UPPD-WDL611-HIST SECTION.                                            
195600     SKIP2                                                                
195700     PERFORM IMS-GU-INLC01                                                
195800     IF SEGMENT-SAKNAS                                                    
195900         MOVE W-IDARTNR  TO ART-IDARTNR                                   
196000         PERFORM IMS-ISRT-INLC01                                          
196100     END-IF                                                               
196200                                                                          
196300     PERFORM S98-FIXA-LOKAL-TID                                           
196400     PERFORM S97-CALL-W218ETA                                             
196500                                                                          
196600     MOVE ARTS-SLAG-ADLAGOMR       TO INL-ADLAGOMR                        
196700     MOVE ARTS-SLAG-ADGANG         TO INL-ADGANG                          
196800     MOVE ARTS-SLAG-ADPLATS        TO INL-ADPLATS                         
196900     MOVE NEJ                      TO INL-FLMAKUL                         
197000                                      INL-FLSKAKOL                        
197100     MOVE SPACE                    TO INL-ADINLOMR                        
197200     MOVE INLA-INL-IDKONTO         TO INL-IDKONTO                         
197300     MOVE INLA-INL-IDANALYS        TO INL-IDANALYS                        
197400     MOVE INLA-INL-IDKST           TO INL-IDKST                           
197500     MOVE INLA-INL-IDDC            TO INL-IDDC                            
197600     MOVE INLA-ART-IDLOPNRM        TO INL-IDLOPNRM                        
197700     MOVE MID-IDLEVNR              TO INL-IDLEVNR                         
197800                                      INL-IDDISTR                         
197900     MOVE +0                       TO INL-IDKUNDNR                        
198000                                      INL-IDFAKT                          
198100                                      INL-IDKUNDNR                        
198200     MOVE MID-IDFS                 TO INL-IDKUNDRF                        
198300     MOVE +0                       TO INL-IDKOLLI                         
198400     MOVE 'R31'                    TO INL-IDPTYP                          
198500     MOVE +0                       TO INL-KDFRAKT                         
198600     MOVE SPACE                    TO INL-KDKOLLI                         
198700                                      INL-IDUSER-003                      
198800     MOVE INLA-ART-KDRT            TO INL-KDRT                            
198900     MOVE WS-ARTC21-KDVALISO       TO INL-KDVALISO                        
199000     MOVE +0                       TO INL-KVANTMOT                        
199100                                      INL-KVART-SKROT                     
199200     MOVE INLA-ART-KVAVIS          TO INL-KVAVIS                          
199300     MOVE WS-ARTC21-PRARTBEL-PR    TO INL-PRARTNTO                        
199400     MOVE ZERO                     TO INL-PRKURS                          
199500     MOVE LETA-TIAAMMDD-SVAR       TO INL-TIBERANK                        
199600     MOVE MSGI-TILOKDAT            TO INL-TIINLMOT                        
199700     MOVE +0                       TO INL-TIINLINL                        
199800     MOVE 'N'                      TO INL-FLPRIO                          
199900                                      INL-FLTULLST                        
200000     MOVE +0                       TO INL-TIINLMTI                        
200100     MOVE +0                       TO INL-TIINLITI                        
200200                                      INL-KVTULRET                        
200300                                      INL-KVRETUR                         
200400                                      INL-KDAVVANT                        
200500     MOVE INLA-INL-TIAVIDAT        TO INL-TIAVIDAT                        
200600     MOVE SPACE                    TO INL-IDDC-LEV                        
200700                                                                          
200800     MOVE WS-DAINLEV               TO INL-DAINLEV                         
200900     PERFORM IMS-ISRT-INLC11                                              
201000     PERFORM UNTIL SEGMENT-FINNS                                          
201100       SUBTRACT +1 FROM WS-DAINLEV                                        
201200       MOVE WS-DAINLEV             TO INL-DAINLEV                         
201300       PERFORM IMS-ISRT-INLC11                                            
201400     END-PERFORM                                                          
201500     .                                                                    
201600     EJECT                                                                
201700 DEC-UPPD-WDL612-HIST SECTION.                                            
201800     SKIP2                                                                
201900     MOVE +0              TO WS-ORDER-REST                                
202000                             WS-TOT-ORD-KVBEART                           
202100     MOVE INLA-ART-KVAVIS TO WS-KVAVIS-ANT                                
202200     MOVE NEJ             TO BOKAT-KLART-SW                               
202300     PERFORM IMS-GU-INLC01                                                
202400     PERFORM IMS-GHNP-INLC12                                              
202500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
202600                   BOKAT-KLART                                            
202700       IF ORD-IDLOPNRM = +0      AND                                      
202800          ORD-IDLEVNR  = MID-IDLEVNR AND                                  
202900          ORD-IDDC     = MID-IDDC                                         
203000*--                                             -- ORDER FINNS            
203100         MOVE JA TO ORDER-FINNS-SW                                        
203200                                                                          
203300         IF WS-KVAVIS-ANT   = ORD-KVBEART                                 
203400           MOVE INLA-ART-IDLOPNRM TO ORD-IDLOPNRM                         
203500           MOVE WS-KVAVIS-ANT     TO ORD-KVAVIS                           
203600           PERFORM IMS-REPL-INLC12                                        
203700           MOVE JA TO BOKAT-KLART-SW                                      
203800           ADD  WS-KVAVIS-ANT     TO WS-TOT-ORD-KVBEART                   
203900         ELSE                                                             
204000           IF WS-KVAVIS-ANT   < ORD-KVBEART                               
204100             MOVE INLA-ART-IDLOPNRM TO ORD-IDLOPNRM                       
204200             MOVE WS-KVAVIS-ANT     TO ORD-KVAVIS                         
204300             COMPUTE WS-ORDER-REST = ORD-KVBEART - WS-KVAVIS-ANT          
204400             END-COMPUTE                                                  
204500             COMPUTE ORD-KVBEART = ORD-KVBEART - WS-ORDER-REST            
204600             END-COMPUTE                                                  
204700             ADD  WS-KVAVIS-ANT     TO WS-TOT-ORD-KVBEART                 
204800             MOVE JA TO BOKAT-KLART-SW                                    
204900             MOVE ORD-DAREGDAT TO WS-SPAR-DAREGDAT                        
205000             MOVE ORD-TIREGTID TO WS-SPAR-TIREGTID                        
205100             PERFORM IMS-REPL-INLC12                                      
205200             PERFORM DECB-SKAPA-KOPIA-REST                                
205300             PERFORM IMS-ISRT-INLC12                                      
205400             PERFORM UNTIL SEGMENT-FINNS                                  
205500               ADD +1      TO ORD-TIREGTID                                
205600               PERFORM IMS-ISRT-INLC12                                    
205700             END-PERFORM                                                  
205800           ELSE                                                           
205900             IF WS-KVAVIS-ANT   > ORD-KVBEART                             
206000               MOVE INLA-ART-IDLOPNRM  TO ORD-IDLOPNRM                    
206100               COMPUTE WS-KVAVIS-ANT  = WS-KVAVIS-ANT   -                 
206200                                          ORD-KVBEART                     
206300               END-COMPUTE                                                
206400               MOVE ORD-KVBEART        TO ORD-KVAVIS                      
206500               PERFORM IMS-REPL-INLC12                                    
206600               ADD  ORD-KVBEART       TO WS-TOT-ORD-KVBEART               
206700             END-IF                                                       
206800           END-IF                                                         
206900         END-IF                                                           
207000       END-IF                                                             
207100       PERFORM IMS-GHNP-INLC12                                            
207200     END-PERFORM                                                          
207300                                                                          
207400     IF SEGMENT-SAKNAS AND INTE-BOKAT-KLART                               
207500       PERFORM DECA-SKAPA-NY-ORDER                                        
207600       PERFORM IMS-ISRT-INLC12                                            
207700       PERFORM UNTIL SEGMENT-FINNS                                        
207800         ADD +1      TO ORD-TIREGTID                                      
207900         PERFORM IMS-ISRT-INLC12                                          
208000       END-PERFORM                                                        
208100       MOVE JA TO BOKAT-KLART-SW                                          
208200     END-IF                                                               
208300     .                                                                    
208400     EJECT                                                                
208500 DECA-SKAPA-NY-ORDER SECTION.                                             
208600     SKIP2                                                                
208700                                                                          
208800     MOVE DAGENS-DATUM-Y2K   TO ORD-DAREGDAT                              
208900     MOVE DAGENS-TID-6-POS   TO ORD-TIREGTID                              
209000     MOVE MID-IDDC           TO ORD-IDDC                                  
209100     MOVE ZEROES             TO ORD-IDKUNDRF                              
209200     MOVE MID-IDLEVNR        TO ORD-IDLEVNR                               
209300     MOVE INLA-ART-IDLOPNRM  TO ORD-IDLOPNRM                              
209400     MOVE +0                 TO ORD-KVBEART                               
209500     MOVE WS-KVAVIS-ANT      TO ORD-KVAVIS                                
209600     MOVE MSGI-TILOKDAT      TO ORD-TIBERANK                              
209700                                                                          
209800     .                                                                    
209900     EJECT                                                                
210000 DECB-SKAPA-KOPIA-REST SECTION.                                           
210100     SKIP2                                                                
210200     MOVE WS-SPAR-DAREGDAT   TO ORD-DAREGDAT                              
210300     COMPUTE ORD-TIREGTID     = WS-SPAR-TIREGTID + 1                      
210400     MOVE WS-ORDER-REST      TO ORD-KVBEART                               
210500     MOVE +0                 TO ORD-IDLOPNRM                              
210600                                ORD-KVAVIS                                
210700     .                                                                    
210800     EJECT                                                                
210900 DF-UPPD-FILB-LOGG SECTION.                                               
211000                                                                          
211100     IF INLA-ART-KDRT = 0 OR 9 OR 10                                      
211200       IF NDC-CA                                                          
211300         PERFORM DFA-LOGG-LAB-NDC-WDR8                                    
211400       ELSE                                                               
211500         PERFORM DFB-LOGG-EKO-R31-WDR8-WDR9                               
211600       END-IF                                                             
211700     ELSE                                                                 
211800       IF INLA-ART-KDRT = 6                                               
211900         IF NDC-NA                                                        
212000           PERFORM DFA-LOGG-LAB-NDC-WDR8                                  
212100         ELSE                                                             
212200           PERFORM DFG-LOGG-EKO-R31-WDR8-WDR9-RT6                         
212300         END-IF                                                           
212400       ELSE                                                               
212500         IF NDC-NA                                                        
212600           PERFORM DFA-LOGG-LAB-NDC-WDR8                                  
212700         ELSE                                                             
212800           PERFORM DFC-LOGG-EKO-R31-WDR8-WDR9                             
212900         END-IF                                                           
213000       END-IF                                                             
213100     END-IF                                                               
213200     IF WS-ARTC-PRINK = WS-ARTC-PRARTSTD                                  
213300       CONTINUE                                                           
213400     ELSE                                                                 
213500       IF XDC-NON-VCC-OWNED OR NDC-US                                     
213600         IF INLA-ART-KVAVIS > 0 AND INLA-ART-KDRT NOT = 6                 
213700           PERFORM DFDB-LOGG-EKO-R31-WDR8-PALAGG                          
213800         END-IF                                                           
213900       ELSE                                                               
214000         IF INLA-ART-KVAVIS > 0 AND INLA-ART-KDRT NOT = 6                 
214100           PERFORM DFDA-LOGG-EKO-R31-WDR9-PALAGG                          
214200         END-IF                                                           
214300       END-IF                                                             
214400     END-IF                                                               
214500                                                                          
214600     IF CDC                                                               
214700         PERFORM DFE-LOGG-310                                             
214800                                                                          
214900         MOVE WS-INLB23-TIAVROP-INL   TO TMP1-YYWW                        
215000         MOVE WS-TIAAVVD-AAVV         TO TMP2-YYWW                        
215100         PERFORM WY2000P3                                                 
215200         IF  TMP1-YYWW > TMP2-YYWW                                        
215300         OR  INLA-ART-KDRT           = 10                                 
215400           PERFORM DFF-LOGG-RY7                                           
215500         END-IF                                                           
215600                                                                          
215700         IF  WS-ARTC-KDERS         > 9                                    
215800           PERFORM DFH-LOGG-092-M113                                      
215900         END-IF                                                           
216000                                                                          
216100         IF  ((INLA-ART-PRARTSTD     NOT = WS-ARTC-PRINK                  
216200           AND (NOT                                                       
216300               (INLA-ART-BEFT        >= 10                                
216400            AND INLA-ART-BEFT        <= 69)))                             
216500          OR  (INLA-ART-PRARTSTD     = WS-ARTC-PRINK                      
216600           AND (INLA-ART-BEFT        >= 10                                
216700            AND INLA-ART-BEFT        <= 69)))                             
216800           PERFORM DFI-LOGG-092-M117                                      
216900         END-IF                                                           
217000     END-IF                                                               
217100     .                                                                    
217200     EJECT                                                                
217300 DFA-LOGG-LAB-NDC-WDR8 SECTION.                                           
217400                                                                          
217500     MOVE SPACE                  TO LAB-W510A11                           
217600                                                                          
217700     MOVE 'L11'                  TO LAB-IDPTYP                            
217800     MOVE SPACE                  TO LAB-KDEKOHT                           
217900     MOVE ZERO                   TO LAB-IDFTG                             
218000                                    LAB-DAINLINL                          
218100     MOVE MID-IDDC               TO LAB-IDDC-SEND                         
218200                                    LAB-IDDC-REC                          
218300     MOVE INLA-INL-IDLEVNR       TO LAB-IDLEVNR                           
218400     MOVE INLA-INL-IDFS          TO LAB-IDFS                              
218500     MOVE WS-INLC-IDFAKT         TO LAB-IDORDNR7                          
218600     MOVE INLA-ART-IDARTNR       TO LAB-IDARTNR                           
218700     MOVE WS-ARTC-KDPRODSL       TO LAB-KDPRODSL                          
218800     MOVE WS-ARTC-KDPSLLOC       TO LAB-KDPSLLOC                          
218900     MOVE ZERO                   TO LAB-PRARTBEU                          
219000     MOVE INLA-ART-KVAVIS        TO LAB-KVAVIS                            
219100     MOVE ZERO                   TO LAB-KVANTMOT                          
219200                                    LAB-PRAVCOST                          
219300                                    LAB-PRAVCOST-OLD                      
219400                                    LAB-KVLS-OLD                          
219500                                    LAB-REMARKUP                          
219600                                                                          
219700     MOVE 'W6019200'        TO EKO-FIL-IDPGM                              
219800     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
219900     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
220000     MOVE +1                TO EKO-FIL-IDSEKVNR                           
220100     MOVE 'W510'            TO EKO-FIL-CT-IDSYSTEM                        
220200     MOVE 'A11'             TO EKO-FIL-CT-IDPTYP                          
220300     MOVE ' '               TO EKO-FIL-CT-IDVTYP                          
220400                                                                          
220500     PERFORM IMS-ISRT-EKOTRANS                                            
220600                                                                          
220700     PERFORM UNTIL SEGMENT-FINNS                                          
220800       ADD +1 TO EKO-FIL-IDSEKVNR                                         
220900       PERFORM IMS-ISRT-EKOTRANS                                          
221000     END-PERFORM                                                          
221100     .                                                                    
221200     EJECT                                                                
221300 DFB-LOGG-EKO-R31-WDR8-WDR9 SECTION.                                      
221400                                                                          
221500     MOVE SPACE                      TO EKO-W51080                        
221600     MOVE SPACE                      TO WS-SAP-MM-POST                    
221700                                                                          
221800     MOVE INLA-ART-IDARTNR           TO EKO-IDARTNR                       
221900     MOVE MID-IDDC                   TO EKO-IDDC                          
222000     MOVE INLA-ART-KDRT              TO EKO-KDRT                          
222100     MOVE INLA-ART-KVAVIS            TO EKO-KVAVIS                        
222200     IF NDC-CN OR NDC-US                                                  
222300       PERFORM DFBAA-GET-PRARTBES                                         
222400       MOVE ZERO                     TO EKO-PRINK                         
222500       MOVE ZERO                     TO EKO-PRHEMTAG                      
222600       IF NDC-CN                                                          
222700         MOVE 60                     TO EKO-IDFTG                         
222800         MOVE WS-KDVALISO-HUV-CN     TO CURR-KDVALISO-HUV                 
222900       ELSE                                                               
223000         MOVE 53                     TO EKO-IDFTG                         
223100         MOVE WS-KDVALISO-HUV-US     TO CURR-KDVALISO-HUV                 
223200       END-IF                                                             
223300       MOVE INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD                
223400       MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                   
223500       MOVE WS-DAAVIDAT-YYMMDD(3:2)  TO W-DATE-AAMM(3:2)                  
223600       MOVE WS-ARTC21-KDVALISO       TO CURR-KDVALISO-ROW                 
223700       MOVE W-DATE-AAMM              TO CURR-TIAAMM                       
223800       MOVE 'M'                      TO CURR-KDVALTYP                     
223900       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
224000       IF CURR-KDSVAR = ' '                                               
224100         MOVE CURR-PRKURS-NEW        TO WS-PRKURS                         
224200         MOVE CURR-REVALUTA-TO       TO WS-REVALUTA                       
224300       ELSE                                                               
224400         MOVE 1                      TO WS-PRKURS                         
224500         MOVE 1                      TO WS-REVALUTA                       
224600       END-IF                                                             
224700       MOVE WS-ARTC21-KDVALISO       TO EKO-KDVALISO                      
224800**** AGREE PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                
224900**** TO COUNTRY CURRENCY                                                  
225000       COMPUTE WS-ARTC21-PRARTBES-PR = WS-ARTC21-PRARTBEL-PR              
225100                                       * WS-PRKURS / WS-REVALUTA          
225200       IF WS-ARTC21-PRARTBES-PR  > ZERO                                   
225300         MOVE WS-ARTC21-PRARTBES-PR  TO EKO-PRARTBES                      
225400       ELSE                                                               
225500         MOVE 0.1                    TO EKO-PRARTBES                      
225600       END-IF                                                             
225700     ELSE                                                                 
225800       MOVE WS-ARTC-PRINK            TO EKO-PRINK                         
225900       MOVE WS-ARTC-PRHEMTAG         TO EKO-PRHEMTAG                      
226000       IF WS-ARTC21-PRARTBES-PR  > ZERO                                   
226100         MOVE WS-ARTC21-PRARTBES-PR  TO EKO-PRARTBES                      
226200       ELSE                                                               
226300         MOVE 0.1                    TO EKO-PRARTBES                      
226400       END-IF                                                             
226500       MOVE 57                       TO EKO-IDFTG                         
226600     END-IF                                                               
226700     MOVE INLA-INL-IDKONTO           TO EKO-IDKONTO                       
226800     MOVE INLA-ART-IDLOPNRM          TO EKO-IDLOPNRM                      
226900     MOVE INLA-INL-IDFS              TO EKO-IDFS                          
227000     MOVE INLA-INL-IDLEVNR           TO EKO-IDLEVNR                       
227100     MOVE WS-ARTC-KDTIPPR            TO EKO-KDTIPPR                       
227200     MOVE INLA-INL-TIAVIDAT          TO EKO-TIAVIDAT                      
227300     IF INLA-INL-IDLEVNR = '1441'                                         
227400       MOVE NEJ                      TO WS-SAP-MM-POST                    
227500     END-IF                                                               
227600     IF  WS-ARTC21-PRARTBEL-PR   > ZERO                                   
227700       MOVE WS-ARTC21-PRARTBEL-PR    TO EKO-PRARTBEL-PR                   
227800       MOVE WS-ARTC21-KDVALISO       TO EKO-KDVALISO                      
227900     ELSE                                                                 
228000*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
228100       MOVE 0.1                      TO EKO-PRARTBEL-PR                   
228200       MOVE 'XXX'                    TO EKO-KDVALISO                      
228300     END-IF                                                               
228400     MOVE ZERO                       TO EKO-RETULF                        
228500                                                                          
228600     MOVE WS-ARTC-KDPRODSL           TO EKO-KDPRODSL                      
228700     MOVE WS-ARTC-IDINK-X            TO EKO-IDINK                         
228800     MOVE WS-ARTC-KDSORT             TO EKO-KDSORT                        
228900     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
229000     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
229100     MOVE JA                         TO EKO-FLLSBOK                       
229200     MOVE ZERO                       TO EKO-IDDISTR                       
229300     MOVE NEJ                        TO EKO-FLDIRLEV                      
229400     MOVE NEJ                        TO EKO-FLAVVINL                      
229500     MOVE ' '                        TO EKO-KDINLAVV                      
229600     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
229700                                                                          
229800     MOVE 'W6019200'                 TO EKO-FIL-IDPGM                     
229900     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
230000     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
230100     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
230200     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
230300     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
230400     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
230500                                                                          
230600     IF NOT (XDC-NON-VCC-OWNED OR NDC-US)                                 
230700       IF WS-SAP-MM-POST NOT = NEJ                                        
230800         PERFORM IMS-ISRT-EKOTRANS                                        
230900                                                                          
231000         PERFORM UNTIL SEGMENT-FINNS                                      
231100           ADD +1 TO EKO-FIL-IDSEKVNR                                     
231200           PERFORM IMS-ISRT-EKOTRANS                                      
231300         END-PERFORM                                                      
231400       END-IF                                                             
231500     ELSE                                                                 
231600       IF NDC-US OR NDC-CN                                                
231700         IF WS-SAP-MM-POST NOT = NEJ                                      
231800           PERFORM IMS-ISRT-EKOTRANS                                      
231900                                                                          
232000           PERFORM UNTIL SEGMENT-FINNS                                    
232100             ADD +1 TO EKO-FIL-IDSEKVNR                                   
232200             PERFORM IMS-ISRT-EKOTRANS                                    
232300           END-PERFORM                                                    
232400         END-IF                                                           
232500       END-IF                                                             
232600     END-IF                                                               
232700                                                                          
232800     IF XDC-NON-VCC-OWNED OR NDC-US                                       
232900       MOVE 'W6019200'                  TO EKO-FIL-IDPGM                  
233000       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
233100                                           EKO-EKH-DAVERDAT               
233200       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
233300       MOVE 1                           TO EKO-FIL-IDSEKVNR               
233400       PERFORM IMS-GU-WDB601                                              
233500       EVALUATE TRUE                                                      
233600        WHEN NDC-CN                                                       
233700          MOVE 'W570'                   TO EKO-FIL-IDCPYTXT(1:4)          
233800        WHEN NDC-IN                                                       
233900          MOVE 'W515'                   TO EKO-FIL-IDCPYTXT(1:4)          
234000        WHEN NDC-US                                                       
234100          MOVE 'W561'                   TO EKO-FIL-IDCPYTXT(1:4)          
234200        WHEN OTHER                                                        
234300          MOVE DCS-KDTRADP              TO EKO-FIL-IDCPYTXT(1:4)          
234400       END-EVALUATE                                                       
234500       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
234600       PERFORM DFBA-LOGG-EKO-R31-WDR8-DET                                 
234700**** HEMT SHOULD ONLY BE USED WHEN LOCAL SOURCING                         
234800       IF NDC-CN OR NDC-US                                                
234900         PERFORM DFBA-LOGG-EKO-R31-WDR8-HEMT                              
235000       END-IF                                                             
235100       IF  WS-ARTC-PRINK = WS-ARTC-PRARTSTD                               
235200       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
235300         CONTINUE                                                         
235400       ELSE                                                               
235500         PERFORM IMS-GU-WDB601                                            
235600         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
235700         IF SEGMENT-FINNS                                                 
235800           PERFORM IMS-GNP-WDB617                                         
235900           IF SEGMENT-FINNS                                               
236000             IF NDC-US                                                    
236100               CONTINUE                                                   
236200             ELSE                                                         
236300               PERFORM DFBA-LOGG-EKO-R31-WDR8-KALK                        
236400             END-IF                                                       
236500           END-IF                                                         
236600         END-IF                                                           
236700       END-IF                                                             
236800       IF NDC-US                                                          
236900         CONTINUE                                                         
237000       ELSE                                                               
237100         PERFORM DFBA-LOGG-EKO-R31-WDR8-SUM                               
237200       END-IF                                                             
237300     ELSE                                                                 
237400       MOVE 'W6019200'                  TO FIL-IDPGM                      
237500       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
237600                                           EKH-DAVERDAT                   
237700       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
237800       MOVE 1                           TO FIL-IDSEKVNR                   
237900       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
238000       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
238100       PERFORM DFBB-LOGG-EKO-R31-WDR9                                     
238200     END-IF                                                               
238300     .                                                                    
238400     EJECT                                                                
238500                                                                          
238600 DFBA-LOGG-EKO-R31-WDR8-DET SECTION.                                      
238700     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
238800     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
238900     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
239000     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
239100     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
239200     MOVE +0                          TO EKO-EKH-IDDISTR                  
239300                                         EKO-EKH-IDKUNDNR                 
239400*******************************                                           
239500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
239600     MOVE ZERO TO NOLL-RAKNARE                                            
239700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
239800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
239900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
240000          FOR LEADING ZERO                                                
240100     ADD +1 TO NOLL-RAKNARE                                               
240200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
240300          WITH POINTER NOLL-RAKNARE                                       
240400*******************************                                           
240500     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
240600                                                                          
240700     IF NDC-CN OR NDC-US                                                  
240800       PERFORM DFBAA-GET-PRARTBES                                         
240900     END-IF                                                               
241000     PERFORM DDAB-GET-CURRENCY-RATE                                       
241100*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
241200*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
241300     IF XDC-NON-VCC-OWNED                                                 
241400     AND NOT (NDC-CN OR NDC-US)                                           
241500       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
241600     END-IF                                                               
241700***                                                                       
241800     IF WS-ARTC21-PRARTBEL-PR > ZERO                                      
241900       MOVE WS-ARTC21-PRARTBEL-PR     TO EKO-EKH-PRARTSTD                 
242000       MOVE WS-ARTC21-KDVALISO        TO EKO-EKH-KDVALISO                 
242100       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
242200     ELSE                                                                 
242300       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
242400       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
242500       MOVE 1.00                      TO EKO-EKH-PRKURS                   
242600     END-IF                                                               
242700     MOVE ZERO                        TO EKO-EKH-PRINK                    
242800     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
242900     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
243000                                         EKO-EKH-PRDMTRL                  
243100                                         EKO-EKH-PROVRPAL                 
243200     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
243300     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
243400                                         EKO-EKH-PRARTNTO                 
243500                                         EKO-EKH-PRARTSJK                 
243600                                         EKO-EKH-PRLANDCO                 
243700                                         EKO-EKH-SUBEL                    
243800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
243900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
244000                                                                          
244100                                                                          
244200     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVANTAL                  
244300     COMPUTE WS-SUARTSTD = WS-ARTC21-PRARTBEL-SUM *                       
244400                           EKO-EKH-KVANTAL                                
244500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
244600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
244700                                         EKO-EKH-KDANMORS                 
244800                                         EKO-EKH-IDKST                    
244900     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
245000                                         EKO-EKH-KDFRAKT                  
245100                                         EKO-EKH-SUVAT                    
245200                                                                          
245300     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
245400     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
245500       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
245600     ELSE                                                                 
245700       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
245800     END-IF                                                               
245900     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
246000     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
246100     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
246200     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
246300     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
246400     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
246500     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
246600     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
246700     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
246800     MOVE SPACE                       TO EKO-EKH-FLDCET                   
246900     MOVE MID-IDFS                    TO EKO-EKH-IDKUNDRF                 
247000     MOVE 'REC'                       TO EKO-EKH-CMD                      
247100                                                                          
247200     IF EKO-EKH-KVANTAL NOT = +0                                          
247300       PERFORM IMS-ISRT-EKOTRANS                                          
247400       PERFORM UNTIL SEGMENT-FINNS                                        
247500         ADD +1 TO EKO-FIL-IDSEKVNR                                       
247600         PERFORM IMS-ISRT-EKOTRANS                                        
247700       END-PERFORM                                                        
247800     END-IF                                                               
247900     .                                                                    
248000     EJECT                                                                
248100                                                                          
248200 DFBA-LOGG-EKO-R31-WDR8-HEMT SECTION.                                     
248300     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
248400     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
248500     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
248600     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
248700     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
248800     MOVE +0                          TO EKO-EKH-IDDISTR                  
248900                                         EKO-EKH-IDKUNDNR                 
249000*******************************                                           
249100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
249200     MOVE ZERO TO NOLL-RAKNARE                                            
249300     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
249400     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
249500     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
249600          FOR LEADING ZERO                                                
249700     ADD +1 TO NOLL-RAKNARE                                               
249800     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
249900          WITH POINTER NOLL-RAKNARE                                       
250000*******************************                                           
250100     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
250200                                                                          
250300     IF NDC-CN OR NDC-US                                                  
250400       PERFORM DFBAA-GET-PRARTBES                                         
250500     END-IF                                                               
250600     PERFORM DDAB-GET-CURRENCY-RATE                                       
250700*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
250800*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
250900     IF XDC-NON-VCC-OWNED                                                 
251000     AND NOT (NDC-CN OR NDC-US)                                           
251100       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
251200     END-IF                                                               
251300***                                                                       
251400     IF WS-ARTC21-PRARTBEL-PR > ZERO                                      
251500       MOVE WS-ARTC21-PRARTBEL-PR     TO EKO-EKH-PRARTSTD                 
251600       MOVE WS-ARTC21-KDVALISO        TO EKO-EKH-KDVALISO                 
251700       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
251800     ELSE                                                                 
251900       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
252000       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
252100       MOVE 1.00                      TO EKO-EKH-PRKURS                   
252200     END-IF                                                               
252300     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
252400                          EKO-EKH-PRARTSTD * INLA-ART-KVAVIS              
252500     MOVE EKO-EKH-PRHEMTAG            TO EKO-EKH-SUBEL                    
252600     MOVE EKO-EKH-PRHEMTAG            TO WS-SUHEMT                        
252700     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
252800     MOVE ZERO                        TO EKO-EKH-PRINK                    
252900     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
253000                                         EKO-EKH-PRDMTRL                  
253100                                         EKO-EKH-PROVRPAL                 
253200     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
253300     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
253400                                         EKO-EKH-PRARTNTO                 
253500                                         EKO-EKH-PRARTSJK                 
253600                                         EKO-EKH-PRLANDCO                 
253700     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
253800     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
253900                                                                          
254000     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
254100     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
254200     MOVE SPACE                       TO EKO-EKH-BEVAT                    
254300                                         EKO-EKH-KDANMORS                 
254400                                         EKO-EKH-IDKST                    
254500     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
254600                                         EKO-EKH-KDFRAKT                  
254700                                         EKO-EKH-SUVAT                    
254800                                                                          
254900     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
255000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
255100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
255200     ELSE                                                                 
255300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
255400     END-IF                                                               
255500     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
255600     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
255700     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
255800     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
255900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
256000     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
256100     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
256200     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
256300     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
256400     MOVE SPACE                       TO EKO-EKH-FLDCET                   
256500     MOVE MID-IDFS                    TO EKO-EKH-IDKUNDRF                 
256600     MOVE 'REC'                       TO EKO-EKH-CMD                      
256700                                                                          
256800     IF EKO-EKH-PRHEMTAG > ZERO                                           
256900       PERFORM IMS-ISRT-EKOTRANS                                          
257000       PERFORM UNTIL SEGMENT-FINNS                                        
257100         ADD +1 TO EKO-FIL-IDSEKVNR                                       
257200         PERFORM IMS-ISRT-EKOTRANS                                        
257300       END-PERFORM                                                        
257400     END-IF                                                               
257500     .                                                                    
257600     EJECT                                                                
257700                                                                          
257800 DFBA-LOGG-EKO-R31-WDR8-KALK SECTION.                                     
257900     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
258000     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
258100     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
258200     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
258300     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
258400     MOVE +0                          TO EKO-EKH-IDDISTR                  
258500                                         EKO-EKH-IDKUNDNR                 
258600*******************************                                           
258700*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
258800     MOVE ZERO TO NOLL-RAKNARE                                            
258900     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
259000     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
259100     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
259200          FOR LEADING ZERO                                                
259300     ADD +1 TO NOLL-RAKNARE                                               
259400     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
259500          WITH POINTER NOLL-RAKNARE                                       
259600*******************************                                           
259700     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
259800                                                                          
259900     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
260000     MOVE ZERO                        TO EKO-EKH-PRINK                    
260100     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
260200     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
260300                                         EKO-EKH-PRDMTRL                  
260400                                         EKO-EKH-PROVRPAL                 
260500     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
260600     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
260700                                         EKO-EKH-PRARTNTO                 
260800                                         EKO-EKH-PRARTSJK                 
260900                                         EKO-EKH-PRLANDCO                 
261000                                         EKO-EKH-SUBEL                    
261100                                                                          
261200     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
261300     MOVE WS-DAAVIDAT-YYMMDD(1:2)     TO W-DATE-AAMM(1:2)                 
261400     MOVE 01                          TO W-DATE-AAMM(3:2)                 
261500     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
261600     MOVE DCS-KDVALISO                TO CURR-KDVALISO-ROW                
261700     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
261800     MOVE 'A'                         TO CURR-KDVALTYP                    
261900     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
262000     IF CURR-KDSVAR = ' '                                                 
262100       MOVE CURR-PRKURS-NEW           TO W-PRKURS                         
262200       MOVE CURR-REVALUTA-TO          TO W-REVALUTA                       
262300     ELSE                                                                 
262400       MOVE 1                         TO W-PRKURS                         
262500       MOVE 1                         TO W-REVALUTA                       
262600     END-IF                                                               
262700                                                                          
262800     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
262900       PROC-REDIRLON * W-REVALUTA / W-PRKURS * INLA-ART-KVAVIS            
263000     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
263100       PROC-REDMTRL  * W-REVALUTA / W-PRKURS * INLA-ART-KVAVIS            
263200     MOVE EKO-EKH-PRDMTRL             TO WS-SUDIRMTRL                     
263300     MOVE EKO-EKH-PRDIRLON            TO WS-SUDIRLON                      
263400     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
263500                             EKO-EKH-PRDMTRL                              
263600                                                                          
263700     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
263800     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
263900                                                                          
264000     MOVE WS-ARTC21-KDVALISO          TO EKO-EKH-KDVALISO                 
264100     MOVE SPAR-PRKURS                 TO EKO-EKH-PRKURS                   
264200     MOVE 1.00                        TO EKO-EKH-PRKURS                   
264300                                                                          
264400     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
264500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
264600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
264700                                         EKO-EKH-KDANMORS                 
264800                                         EKO-EKH-IDKST                    
264900     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
265000                                         EKO-EKH-KDFRAKT                  
265100                                         EKO-EKH-SUVAT                    
265200                                                                          
265300     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
265400     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
265500       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
265600     ELSE                                                                 
265700       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
265800     END-IF                                                               
265900     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
266000     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
266100     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
266200     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
266300     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
266400     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
266500     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
266600     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
266700     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
266800     MOVE SPACE                       TO EKO-EKH-FLDCET                   
266900     MOVE MID-IDFS                    TO EKO-EKH-IDKUNDRF                 
267000     MOVE 'REC'                       TO EKO-EKH-CMD                      
267100                                                                          
267200     IF EKO-EKH-SUBEL > ZERO                                              
267300       PERFORM IMS-ISRT-EKOTRANS                                          
267400       PERFORM UNTIL SEGMENT-FINNS                                        
267500         ADD +1 TO EKO-FIL-IDSEKVNR                                       
267600         PERFORM IMS-ISRT-EKOTRANS                                        
267700       END-PERFORM                                                        
267800     END-IF                                                               
267900     .                                                                    
268000     EJECT                                                                
268100                                                                          
268200 DFBA-LOGG-EKO-R31-WDR8-SUM SECTION.                                      
268300     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
268400     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
268500     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
268600     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
268700     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
268800     MOVE +0                          TO EKO-EKH-IDDISTR                  
268900                                         EKO-EKH-IDKUNDNR                 
269000*******************************                                           
269100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
269200     MOVE ZERO TO NOLL-RAKNARE                                            
269300     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
269400     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
269500     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
269600          FOR LEADING ZERO                                                
269700     ADD +1 TO NOLL-RAKNARE                                               
269800     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
269900          WITH POINTER NOLL-RAKNARE                                       
270000*******************************                                           
270100     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
270200                                                                          
270300     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
270400     MOVE ZERO                        TO EKO-EKH-PRINK                    
270500     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
270600     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
270700                                         EKO-EKH-PRDMTRL                  
270800                                         EKO-EKH-PROVRPAL                 
270900     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
271000     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
271100                                         EKO-EKH-PRARTNTO                 
271200                                         EKO-EKH-PRARTSJK                 
271300                                         EKO-EKH-PRLANDCO                 
271400                                         EKO-EKH-SUBEL                    
271500*    COMPUTE EKO-EKH-SUBEL = WS-SUDIRLON + WS-SUDIRMTRL +                 
271600*                            WS-SUHEMT + WS-SUARTSTD                      
271700     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
271800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
271900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
272000                                                                          
272100     MOVE WS-ARTC21-KDVALISO          TO EKO-EKH-KDVALISO                 
272200     MOVE SPAR-PRKURS                 TO EKO-EKH-PRKURS                   
272300                                                                          
272400     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
272500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
272600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
272700                                         EKO-EKH-KDANMORS                 
272800                                         EKO-EKH-IDKST                    
272900     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
273000                                         EKO-EKH-KDFRAKT                  
273100                                         EKO-EKH-SUVAT                    
273200                                                                          
273300     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
273400     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
273500       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
273600     ELSE                                                                 
273700       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
273800     END-IF                                                               
273900     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
274000     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
274100     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
274200     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
274300     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
274400     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
274500     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
274600     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
274700     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
274800     MOVE SPACE                       TO EKO-EKH-FLDCET                   
274900     MOVE MID-IDFS                    TO EKO-EKH-IDKUNDRF                 
275000     MOVE 'REC'                       TO EKO-EKH-CMD                      
275100                                                                          
275200     IF EKO-EKH-SUBEL > ZERO                                              
275300       PERFORM IMS-ISRT-EKOTRANS                                          
275400       PERFORM UNTIL SEGMENT-FINNS                                        
275500         ADD +1 TO EKO-FIL-IDSEKVNR                                       
275600         PERFORM IMS-ISRT-EKOTRANS                                        
275700       END-PERFORM                                                        
275800     END-IF                                                               
275900     .                                                                    
276000     EJECT                                                                
276100                                                                          
276200 DFBAA-GET-PRARTBES SECTION.                                              
276300     MOVE ZERO              TO WS-ARTC21-PRARTBEL-PR                      
276400     MOVE ZERO              TO WS-ARTC21-PRARTBEL-SUM                     
276500     MOVE ZERO              TO WS-ARTC21-PRARTBES-PR                      
276600                                                                          
276700     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
276800     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
276900       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
277000     ELSE                                                                 
277100       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
277200     END-IF                                                               
277300     MOVE NEJ TO PRIS-FINNS-SW                                            
277400                                                                          
277500*    -- WDK711                                                            
277600     PERFORM IMS-GHU-WDK711                                               
277700     IF SEGMENT-FINNS                                                     
277800                                                                          
277900       PERFORM IMS-GNP-WDK723                                             
278000       IF SEGMENT-FINNS                                                   
278100         MOVE SAVT-IDAVTAL    TO WS-ARTC23-IDAVTAL                        
278200       ELSE                                                               
278300         MOVE ZERO            TO WS-ARTC23-IDAVTAL                        
278400       END-IF                                                             
278500                                                                          
278600*    -- WDK724                                                            
278700       MOVE INLA-INL-IDLEVNR    TO W-IDLEVNR-PR                           
278800       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAAVIDAT                     
278900       PERFORM IMS-GHNP-WDK724                                            
279000                                                                          
279100       IF SEGMENT-FINNS                                                   
279200         MOVE JA                TO PRIS-FINNS-SW                          
279300         MOVE SPRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-PR                  
279400         MOVE SPRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-SUM                 
279500         MOVE SPRL-PRARTBES-PR  TO WS-ARTC21-PRARTBES-PR                  
279600         MOVE SPRL-KDVALISO     TO WS-ARTC21-KDVALISO                     
279700       END-IF                                                             
279800     END-IF                                                               
279900     .                                                                    
280000     EJECT                                                                
280100                                                                          
280200 DDAB-GET-CURRENCY-RATE SECTION.                                          
280300     MOVE WS-IDDC         TO W-IDDC-B6                                    
280400     PERFORM IMS-GU-WDB601                                                
280500     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
280600     IF W-KDVALISO-HUV = WS-ARTC21-KDVALISO                               
280700       MOVE 1 TO SPAR-PRKURS                                              
280800       MOVE 1 TO W-REVALUTA                                               
280900     ELSE                                                                 
281000       MOVE WS-ARTC21-KDVALISO TO W-KDVALISO-ROW                          
281100       PERFORM IMS-GU-WDGX9306                                            
281200       IF SEGMENT-SAKNAS                                                  
281300         MOVE 1               TO SPAR-PRKURS                              
281400         MOVE 1               TO W-REVALUTA                               
281500       ELSE                                                               
281600         MOVE WS-ARTC21-KDVALISO TO W-KDVALISO-ROW                        
281700         COMPUTE W-TISTADAT-9KOMPL =                                      
281800                 9999999 - WS-DATE-YYMMDD                                 
281900         PERFORM IMS-GNP-WDGX9308                                         
282000         IF SEGMENT-SAKNAS                                                
282100           PERFORM IMS-GNP-WDGX9308-FIRST                                 
282200           IF SEGMENT-SAKNAS                                              
282300             MOVE 1               TO SPAR-PRKURS                          
282400             MOVE 1               TO W-REVALUTA                           
282500           ELSE                                                           
282600             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
282700             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
282800           END-IF                                                         
282900         ELSE                                                             
283000           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
283100           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
283200         END-IF                                                           
283300       END-IF                                                             
283400     END-IF                                                               
283500     .                                                                    
283600     EJECT                                                                
283700                                                                          
283800 DDAC-GET-CURR-RATE-LOCAL SECTION.                                        
283900                                                                          
284000     IF W-KDVALISO-HUV = WS-ARTC21-KDVALISO                               
284100       CONTINUE                                                           
284200     ELSE                                                                 
284300       MOVE WS-ARTC-KDPRODSL           TO TEST-KDPRODSL                   
284400       IF KDPRODSL-LOCAL                                                  
284500         MOVE INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD              
284600         MOVE WS-DAAVIDAT-YYMMDD(1:2)  TO W-DATE-AAMM(1:2)                
284700         MOVE WS-DAAVIDAT-YYMMDD(3:2)  TO W-DATE-AAMM(3:2)                
284800         MOVE W-KDVALISO-HUV           TO CURR-KDVALISO-HUV               
284900         MOVE WS-ARTC21-KDVALISO       TO CURR-KDVALISO-ROW               
285000         MOVE W-DATE-AAMM              TO CURR-TIAAMM                     
285100         MOVE 'M'                      TO CURR-KDVALTYP                   
285200         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
285300         IF CURR-KDSVAR = ' '                                             
285400           MOVE CURR-PRKURS-NEW        TO WS-PRKURS                       
285500                                          SPAR-PRKURS                     
285600           MOVE CURR-REVALUTA-TO       TO WS-REVALUTA                     
285700         ELSE                                                             
285800           MOVE 1                      TO WS-PRKURS                       
285900                                          SPAR-PRKURS                     
286000           MOVE 1                      TO WS-REVALUTA                     
286100         END-IF                                                           
286200**** PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                      
286300**** TO COUNTRY CURRENCY                                                  
286400         COMPUTE WS-ARTC21-PRARTBEL-PR = WS-ARTC21-PRARTBEL-PR            
286500                                   * WS-PRKURS / WS-REVALUTA              
286600       END-IF                                                             
286700     END-IF                                                               
286800     .                                                                    
286900     EJECT                                                                
287000 DFBB-LOGG-EKO-R31-WDR9 SECTION.                                          
287100     MOVE '103'                       TO EKH-KDEKHHT                      
287200     MOVE '102'                       TO EKH-KDEKSHT                      
287300     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
287400     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
287500     MOVE SPACE                       TO EKH-IDDC-REC                     
287600     MOVE +0                          TO EKH-IDDISTR                      
287700                                         EKH-IDKUNDNR                     
287800*******************************                                           
287900*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
288000     MOVE ZERO TO NOLL-RAKNARE                                            
288100     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
288200     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
288300     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
288400          FOR LEADING ZERO                                                
288500     ADD +1 TO NOLL-RAKNARE                                               
288600     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
288700          WITH POINTER NOLL-RAKNARE                                       
288800*******************************                                           
288900     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
289000     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
289100     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
289200     IF INLA-INL-IDLEVNR = '1002 '                                        
289300       MOVE ZERO                      TO EKH-PRHEMTAG                     
289400     ELSE                                                                 
289500       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
289600     END-IF                                                               
289700     MOVE ZERO                        TO EKH-PRDIRLON                     
289800                                         EKH-PRDMTRL                      
289900                                         EKH-PROVRPAL                     
290000     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
290100     MOVE ZERO                        TO EKH-KDPSLLOC                     
290200                                         EKH-PRARTNTO                     
290300                                         EKH-PRARTSJK                     
290400                                         EKH-PRLANDCO                     
290500                                         EKH-SUBEL                        
290600     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
290700     MOVE SPACE                       TO EKH-FLLSBOK                      
290800                                                                          
290900     IF EKO-KDVALISO = 'XXX'                                              
291000       MOVE 'SEK'                     TO EKH-KDVALISO                     
291100     ELSE                                                                 
291200       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
291300     END-IF                                                               
291400     MOVE 1.00                        TO EKH-PRKURS                       
291500                                                                          
291600     MOVE INLA-ART-KVAVIS             TO EKH-KVANTAL                      
291700     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
291800     MOVE SPACE                       TO EKH-BEVAT                        
291900                                         EKH-KDANMORS                     
292000                                         EKH-IDKST                        
292100     MOVE ZERO                        TO EKH-IDKONTO                      
292200                                         EKH-KDFRAKT                      
292300                                         EKH-SUVAT                        
292400                                                                          
292500     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
292600     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
292700       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
292800     ELSE                                                                 
292900       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
293000     END-IF                                                               
293100     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
293200     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
293300     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
293400     MOVE ZERO                        TO EKH-KDAVVTYP                     
293500     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
293600     MOVE ZERO                        TO EKH-KVANTMOT                     
293700     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
293800     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
293900     MOVE SPACE                       TO EKH-KDTRADP                      
294000     MOVE SPACE                       TO EKH-FLDCET                       
294100     MOVE SPACE                       TO EKH-IDKUNDRF                     
294200     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
294300                                                                          
294400                                                                          
294500     IF EKH-KVANTAL NOT = +0                                              
294600       PERFORM IMS-ISRT-WLSAPA01                                          
294700       PERFORM UNTIL SEGMENT-FINNS                                        
294800         ADD +1 TO FIL-IDSEKVNR                                           
294900         PERFORM IMS-ISRT-WLSAPA01                                        
295000       END-PERFORM                                                        
295100     END-IF                                                               
295200     .                                                                    
295300     EJECT                                                                
295400                                                                          
295500 DFC-LOGG-EKO-R31-WDR8-WDR9 SECTION.                                      
295600     IF XDC-NON-VCC-OWNED                                                 
295700       MOVE 'W6019200'                  TO EKO-FIL-IDPGM                  
295800       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
295900       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
296000       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
296100       MOVE 1                           TO EKO-FIL-IDSEKVNR               
296200       PERFORM IMS-GU-WDB601                                              
296300       EVALUATE TRUE                                                      
296400         WHEN NDC-CN                                                      
296500           MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)           
296600         WHEN NDC-IN                                                      
296700           MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)           
296800         WHEN OTHER                                                       
296900           MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)           
297000       END-EVALUATE                                                       
297100       MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)           
297200       PERFORM DFCA-LOGG-EKO-R31-WDR8                                     
297300     ELSE                                                                 
297400       MOVE 'W6019200'                  TO FIL-IDPGM                      
297500       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
297600                                           EKH-DAVERDAT                   
297700       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
297800       MOVE 1                           TO FIL-IDSEKVNR                   
297900       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
298000       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
298100       PERFORM DFCB-LOGG-EKO-R31-WDR9                                     
298200     END-IF                                                               
298300     .                                                                    
298400     EJECT                                                                
298500                                                                          
298600 DFCA-LOGG-EKO-R31-WDR8 SECTION.                                          
298700     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
298800     MOVE '105'                       TO EKO-EKH-KDEKSHT                  
298900     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
299000     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
299100     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
299200     MOVE +0                          TO EKO-EKH-IDDISTR                  
299300                                         EKO-EKH-IDKUNDNR                 
299400*******************************                                           
299500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
299600     MOVE ZERO TO NOLL-RAKNARE                                            
299700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
299800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
299900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
300000          FOR LEADING ZERO                                                
300100     ADD +1 TO NOLL-RAKNARE                                               
300200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
300300          WITH POINTER NOLL-RAKNARE                                       
300400*******************************                                           
300500     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
300600     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
300700                                         EKO-EKH-PRARTNTO                 
300800                                         EKO-EKH-PRARTSJK                 
300900                                         EKO-EKH-PRHEMTAG                 
301000                                         EKO-EKH-PRLANDCO                 
301100                                         EKO-EKH-PRDIRLON                 
301200                                         EKO-EKH-PRDMTRL                  
301300                                         EKO-EKH-PROVRPAL                 
301400                                         EKO-EKH-SUBEL                    
301500     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
301600     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
301700                                                                          
301800     MOVE 1.00                        TO EKO-EKH-PRKURS                   
301900                                                                          
302000     MOVE ZERO                        TO EKO-EKH-PRINK                    
302100     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
302200     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVANTAL                  
302300     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
302400     MOVE SPACE                       TO EKO-EKH-BEVAT                    
302500                                         EKO-EKH-KDANMORS                 
302600                                         EKO-EKH-IDANALYS                 
302700                                         EKO-EKH-IDKST                    
302800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
302900                                         EKO-EKH-KDFRAKT                  
303000                                         EKO-EKH-SUVAT                    
303100                                                                          
303200     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
303300     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
303400       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
303500     ELSE                                                                 
303600       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
303700     END-IF                                                               
303800     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
303900     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
304000     MOVE MID-IDLEVNR                 TO EKO-EKH-IDLEVNR                  
304100     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
304200     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
304300     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
304400     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
304500     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
304600     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
304700     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
304800     MOVE SPACE                       TO EKO-EKH-FLDCET                   
304900     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
305000     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
305100                                                                          
305200     IF EKO-EKH-KVANTAL NOT = +0                                          
305300       PERFORM IMS-ISRT-EKOTRANS                                          
305400       PERFORM UNTIL SEGMENT-FINNS                                        
305500         ADD +1 TO EKO-FIL-IDSEKVNR                                       
305600         PERFORM IMS-ISRT-EKOTRANS                                        
305700       END-PERFORM                                                        
305800     END-IF                                                               
305900     .                                                                    
306000     EJECT                                                                
306100                                                                          
306200 DFCB-LOGG-EKO-R31-WDR9 SECTION.                                          
306300     MOVE '102'                       TO EKH-KDEKHHT                      
306400     MOVE '105'                       TO EKH-KDEKSHT                      
306500     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
306600     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
306700     MOVE SPACE                       TO EKH-IDDC-REC                     
306800     MOVE +0                          TO EKH-IDDISTR                      
306900                                         EKH-IDKUNDNR                     
307000*******************************                                           
307100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
307200     MOVE ZERO TO NOLL-RAKNARE                                            
307300     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
307400     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
307500     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
307600          FOR LEADING ZERO                                                
307700     ADD +1 TO NOLL-RAKNARE                                               
307800     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
307900          WITH POINTER NOLL-RAKNARE                                       
308000*******************************                                           
308100     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
308200     MOVE ZERO                        TO EKH-KDPSLLOC                     
308300                                         EKH-PRARTNTO                     
308400                                         EKH-PRARTSJK                     
308500                                         EKH-PRHEMTAG                     
308600                                         EKH-PRLANDCO                     
308700                                         EKH-PRDIRLON                     
308800                                         EKH-PRDMTRL                      
308900                                         EKH-PROVRPAL                     
309000                                         EKH-SUBEL                        
309100     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
309200     MOVE SPACE                       TO EKH-FLLSBOK                      
309300                                                                          
309400     MOVE 'SEK'                       TO EKH-KDVALISO                     
309500     MOVE 1.00                        TO EKH-PRKURS                       
309600                                                                          
309700     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
309800     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
309900     MOVE INLA-ART-KVAVIS             TO EKH-KVANTAL                      
310000     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
310100     MOVE SPACE                       TO EKH-BEVAT                        
310200                                         EKH-KDANMORS                     
310300                                         EKH-IDANALYS                     
310400                                         EKH-IDKST                        
310500     MOVE ZERO                        TO EKH-IDKONTO                      
310600                                         EKH-KDFRAKT                      
310700                                         EKH-SUVAT                        
310800                                                                          
310900     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
311000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
311100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
311200     ELSE                                                                 
311300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
311400     END-IF                                                               
311500     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
311600     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
311700     MOVE MID-IDLEVNR                 TO EKH-IDLEVNR                      
311800     MOVE ZERO                        TO EKH-KDAVVTYP                     
311900     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
312000     MOVE ZERO                        TO EKH-KVANTMOT                     
312100     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
312200     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
312300     MOVE 'SEPV'                      TO EKH-KDTRADP                      
312400     MOVE SPACE                       TO EKH-FLDCET                       
312500     MOVE SPACE                       TO EKH-IDKUNDRF                     
312600     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
312700                                                                          
312800                                                                          
312900     IF EKH-KVANTAL NOT = +0                                              
313000       PERFORM IMS-ISRT-WLSAPA01                                          
313100       PERFORM UNTIL SEGMENT-FINNS                                        
313200         ADD +1 TO FIL-IDSEKVNR                                           
313300         PERFORM IMS-ISRT-WLSAPA01                                        
313400       END-PERFORM                                                        
313500     END-IF                                                               
313600     .                                                                    
313700     EJECT                                                                
313800 DFDA-LOGG-EKO-R31-WDR9-PALAGG  SECTION.                                  
313900                                                                          
314000     MOVE 'W6019200'                  TO FIL-IDPGM                        
314100     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
314200                                         EKH-DAVERDAT                     
314300     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
314400     MOVE 1                           TO FIL-IDSEKVNR                     
314500     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
314600     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
314700     MOVE '103'                       TO EKH-KDEKHHT                      
314800     MOVE '101'                       TO EKH-KDEKSHT                      
314900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
315000     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
315100     MOVE SPACE                       TO EKH-IDDC-REC                     
315200     MOVE +0                          TO EKH-IDDISTR                      
315300                                         EKH-IDKUNDNR                     
315400*******************************                                           
315500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
315600     MOVE ZERO TO NOLL-RAKNARE                                            
315700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
315800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
315900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
316000          FOR LEADING ZERO                                                
316100     ADD +1 TO NOLL-RAKNARE                                               
316200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
316300          WITH POINTER NOLL-RAKNARE                                       
316400*******************************                                           
316500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
316600     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
316700     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
316800     MOVE WS-ARTC-PRDIRLON            TO EKH-PRDIRLON                     
316900     MOVE WS-ARTC-PRDMTRL             TO EKH-PRDMTRL                      
317000     MOVE WS-ARTC-PROVRPAL            TO EKH-PROVRPAL                     
317100     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
317200     MOVE ZERO                        TO EKH-KDPSLLOC                     
317300                                         EKH-PRARTNTO                     
317400                                         EKH-PRARTSJK                     
317500                                         EKH-PRHEMTAG                     
317600                                         EKH-PRLANDCO                     
317700                                         EKH-SUBEL                        
317800     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
317900     MOVE SPACE                       TO EKH-FLLSBOK                      
318000                                                                          
318100     MOVE 'SEK'                       TO EKH-KDVALISO                     
318200     MOVE 1.00                        TO EKH-PRKURS                       
318300                                                                          
318400     MOVE INLA-ART-KVAVIS             TO EKH-KVANTAL                      
318500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
318600     MOVE SPACE                       TO EKH-BEVAT                        
318700                                         EKH-KDANMORS                     
318800                                         EKH-IDKST                        
318900     MOVE ZERO                        TO EKH-IDKONTO                      
319000                                         EKH-KDFRAKT                      
319100                                         EKH-SUVAT                        
319200                                                                          
319300     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
319400     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
319500       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
319600     ELSE                                                                 
319700       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
319800     END-IF                                                               
319900     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
320000     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
320100     MOVE MID-IDLEVNR                 TO EKH-IDLEVNR                      
320200     MOVE ZERO                        TO EKH-KDAVVTYP                     
320300     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
320400     MOVE ZERO                        TO EKH-KVANTMOT                     
320500     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
320600     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
320700     MOVE SPACE                       TO EKH-KDTRADP                      
320800     MOVE SPACE                       TO EKH-FLDCET                       
320900     MOVE SPACE                       TO EKH-IDKUNDRF                     
321000     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
321100                                                                          
321200                                                                          
321300     PERFORM IMS-ISRT-WLSAPA01                                            
321400                                                                          
321500     PERFORM UNTIL SEGMENT-FINNS                                          
321600       ADD +1 TO FIL-IDSEKVNR                                             
321700       PERFORM IMS-ISRT-WLSAPA01                                          
321800     END-PERFORM                                                          
321900     .                                                                    
322000     EJECT                                                                
322100                                                                          
322200 DFDB-LOGG-EKO-R31-WDR8-PALAGG  SECTION.                                  
322300     MOVE 'W6019200'                  TO EKO-FIL-IDPGM                    
322400     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
322500     MOVE FUNCTION CURRENT-DATE (9:8) TO EKO-FIL-TIKLOCK                  
322600     MOVE 1                           TO EKO-FIL-IDSEKVNR                 
322700     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
322800     MOVE '101'                       TO EKO-EKH-KDEKSHT                  
322900     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
323000     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
323100     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
323200     MOVE +0                          TO EKO-EKH-IDDISTR                  
323300                                         EKO-EKH-IDKUNDNR                 
323400*******************************                                           
323500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
323600     MOVE ZERO TO NOLL-RAKNARE                                            
323700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
323800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
323900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
324000          FOR LEADING ZERO                                                
324100     ADD +1 TO NOLL-RAKNARE                                               
324200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
324300          WITH POINTER NOLL-RAKNARE                                       
324400*******************************                                           
324500     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
324600     MOVE WS-ARTC-PRARTSTD            TO EKO-EKH-PRARTSTD                 
324700     MOVE ZERO                        TO EKO-EKH-PRINK                    
324800                                                                          
324900     MOVE DAGENS-DATUM-Y2K(3:2)       TO W-DATE-AAMM(1:2)                 
325000     MOVE 01                          TO W-DATE-AAMM(3:2)                 
325100     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
325200     MOVE DCS-KDVALISO                TO CURR-KDVALISO-ROW                
325300     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
325400     MOVE 'A'                         TO CURR-KDVALTYP                    
325500     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
325600     IF CURR-KDSVAR = ' '                                                 
325700       MOVE CURR-PRKURS-NEW           TO W-PRKURS                         
325800       MOVE CURR-REVALUTA-TO          TO W-REVALUTA                       
325900     ELSE                                                                 
326000       MOVE 1                         TO W-PRKURS                         
326100       MOVE 1                         TO W-REVALUTA                       
326200     END-IF                                                               
326300     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
326400                   PROC-REDIRLON /  W-PRKURS / W-REVALUTA                 
326500     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
326600                   PROC-REDMTRL  /  W-PRKURS / W-REVALUTA                 
326700     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
326800     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
326900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
327000                                         EKO-EKH-PRARTNTO                 
327100                                         EKO-EKH-PRARTSJK                 
327200                                         EKO-EKH-PRHEMTAG                 
327300                                         EKO-EKH-PRLANDCO                 
327400                                         EKO-EKH-SUBEL                    
327500     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
327600     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
327700                                                                          
327800     MOVE 1.00                        TO EKO-EKH-PRKURS                   
327900                                                                          
328000     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVANTAL                  
328100     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
328200     MOVE SPACE                       TO EKO-EKH-BEVAT                    
328300                                         EKO-EKH-KDANMORS                 
328400                                         EKO-EKH-IDKST                    
328500     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
328600                                         EKO-EKH-KDFRAKT                  
328700                                         EKO-EKH-SUVAT                    
328800                                                                          
328900     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
329000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
329100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
329200     ELSE                                                                 
329300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
329400     END-IF                                                               
329500     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
329600     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
329700     MOVE MID-IDLEVNR                 TO EKO-EKH-IDLEVNR                  
329800     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
329900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
330000     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
330100     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
330200     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
330300     MOVE SPACE                       TO EKO-EKH-FLDCET                   
330400     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
330500     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
330600     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
330700     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
330800     EVALUATE TRUE                                                        
330900       WHEN NDC-CN                                                        
331000         MOVE 'W570'                  TO EKO-FIL-IDCPYTXT(1:4)            
331100       WHEN NDC-IN                                                        
331200         MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)            
331300       WHEN NDC-US                                                        
331400         MOVE 'W561'                  TO EKO-FIL-IDCPYTXT(1:4)            
331500       WHEN OTHER                                                         
331600         MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)            
331700     END-EVALUATE                                                         
331800     MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)            
331900                                                                          
332000     IF EKO-EKH-PRDIRLON > ZERO                                           
332100     OR EKO-EKH-PRDMTRL  > ZERO                                           
332200       PERFORM IMS-ISRT-EKOTRANS                                          
332300                                                                          
332400       PERFORM UNTIL SEGMENT-FINNS                                        
332500         ADD +1 TO EKO-FIL-IDSEKVNR                                       
332600         PERFORM IMS-ISRT-EKOTRANS                                        
332700       END-PERFORM                                                        
332800     END-IF                                                               
332900     .                                                                    
333000     EJECT                                                                
333100 DFE-LOGG-310 SECTION.                                                    
333200                                                                          
333300     MOVE SPACE                  TO W211310-W211310                       
333400                                                                          
333500     MOVE '221'                  TO W211310-IDTTYP                        
333600     MOVE INLA-ART-IDARTNR       TO W211310-IDARTNR-S                     
333700     MOVE ZERO                   TO W211310-SORTFLT1                      
333800     MOVE W-KDCLAGER             TO W211310-KDCLAGER-S                    
333900                                    W211310-KDCLAGER                      
334000     MOVE WS-ARTC-IDANSK         TO W211310-IDANSKNR                      
334100     MOVE INLA-ART-PRARTSTD      TO W211310-PRARTSTD                      
334200     MOVE WS-IDLOPNRM-VVDLLLLK   TO W211310-IDLOPNR                       
334300     MOVE ZERO                   TO W211310-KDAVVANT                      
334400     MOVE 9                      TO W211310-POSTLGD                       
334500     MOVE '310'                  TO W211310-IDPTYP                        
334600     MOVE ZERO                   TO W211310-NOLLOR-20                     
334700     MOVE INLA-ART-IDARTNR       TO W211310-IDARTNR                       
334800     MOVE ZERO                   TO W211310-KVMOTANT                      
334900     MOVE ZERO                   TO W211310-NOLLOR-41                     
335000     MOVE INLA-INL-IDLEVNR       TO W211310-IDLEVNR-INL                   
335100     MOVE INLA-INL-TIAVIDAT      TO W211310-TIAVSDAT                      
335200     MOVE ZERO                   TO W211310-NOLLOR-21                     
335300     MOVE INLA-ART-KDRT          TO W211310-KDRT                          
335400     MOVE INLA-ART-KVAVIS        TO W211310-KVAVIS                        
335500     MOVE WS-IDAVINR             TO W211310-IDAVINR                       
335600     MOVE WS-ARTC-IDINK          TO W211310-KDPKINR                       
335700                                                                          
335800     MOVE W211310-W211310        TO WS-ZZAC01-LOGGPOST                    
335900     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
336000     PERFORM S02-SKAPA-ZZAC01                                             
336100     .                                                                    
336200     EJECT                                                                
336300 DFF-LOGG-RY7 SECTION.                                                    
336400                                                                          
336500     MOVE SPACE                  TO FILC-FIL-WDR301-DATA                  
336600                                                                          
336700     MOVE 'RY7'                  TO FILC-RY7-IDPTYP                       
336800     MOVE INLA-ART-IDARTNR       TO FILC-RY7-IDARTNR                      
336900     MOVE INLA-INL-IDLEVNR       TO FILC-RY7-IDLEVNR                      
337000     MOVE WS-INLB23-TIAVROP-INL  TO FILC-RY7-TIAVROP-INL                  
337100     MOVE WS-TIAAVVD-AAVV        TO FILC-RY7-TIANKDAG                     
337200     MOVE INLA-ART-KVAVIS        TO FILC-RY7-KVAVIS                       
337300                                                                          
337400     MOVE 'W236RY7 '             TO FILC-FIL-IDCPYTXT                     
337500     PERFORM S02-SKAPA-FILC01                                             
337600     .                                                                    
337700     EJECT                                                                
337800                                                                          
337900 DFG-LOGG-EKO-R31-WDR8-WDR9-RT6 SECTION.                                  
338000     IF XDC-NON-VCC-OWNED                                                 
338100       MOVE 'W6019200'                  TO EKO-FIL-IDPGM                  
338200       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
338300       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
338400       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
338500       MOVE 1                           TO EKO-FIL-IDSEKVNR               
338600       PERFORM IMS-GU-WDB601                                              
338700       EVALUATE TRUE                                                      
338800        WHEN NDC-CN                                                       
338900          MOVE 'W570'                   TO EKO-FIL-IDCPYTXT(1:4)          
339000        WHEN NDC-IN                                                       
339100          MOVE 'W515'                   TO EKO-FIL-IDCPYTXT(1:4)          
339200        WHEN OTHER                                                        
339300          MOVE DCS-KDTRADP              TO EKO-FIL-IDCPYTXT(1:4)          
339400       END-EVALUATE                                                       
339500       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
339600       PERFORM DFGA-LOGG-EKO-R31-WDR8-RT6                                 
339700     ELSE                                                                 
339800       MOVE 'W6019200'                  TO FIL-IDPGM                      
339900       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
340000                                           EKH-DAVERDAT                   
340100       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
340200       MOVE 1                           TO FIL-IDSEKVNR                   
340300       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
340400       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
340500       PERFORM DFGB-LOGG-EKO-R31-WDR9-RT6                                 
340600     END-IF                                                               
340700     .                                                                    
340800     EJECT                                                                
340900                                                                          
341000 DFGA-LOGG-EKO-R31-WDR8-RT6 SECTION.                                      
341100     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
341200     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
341300     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
341400     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
341500     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
341600     MOVE +0                          TO EKO-EKH-IDDISTR                  
341700                                         EKO-EKH-IDKUNDNR                 
341800*******************************                                           
341900*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
342000     MOVE ZERO TO NOLL-RAKNARE                                            
342100     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
342200     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
342300     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
342400          FOR LEADING ZERO                                                
342500     ADD +1 TO NOLL-RAKNARE                                               
342600     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
342700          WITH POINTER NOLL-RAKNARE                                       
342800*******************************                                           
342900     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
343000     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
343100                                         EKO-EKH-PRARTNTO                 
343200                                         EKO-EKH-PRARTSJK                 
343300                                         EKO-EKH-PRHEMTAG                 
343400                                         EKO-EKH-PRLANDCO                 
343500                                         EKO-EKH-PRDIRLON                 
343600                                         EKO-EKH-PRDMTRL                  
343700                                         EKO-EKH-PROVRPAL                 
343800                                         EKO-EKH-SUBEL                    
343900     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
344000     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
344100                                                                          
344200     MOVE 1.00                        TO EKO-EKH-PRKURS                   
344300                                                                          
344400     MOVE ZERO                        TO EKO-EKH-PRINK                    
344500     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
344600     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVANTAL                  
344700     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
344800     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
344900     MOVE INLA-INL-IDKONTO            TO EKO-EKH-IDKONTO                  
345000     MOVE INLA-INL-IDKST              TO EKO-EKH-IDKST                    
345100     MOVE SPACE                       TO EKO-EKH-BEVAT                    
345200                                         EKO-EKH-KDANMORS                 
345300     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
345400                                         EKO-EKH-SUVAT                    
345500                                                                          
345600     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
345700     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
345800       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
345900     ELSE                                                                 
346000       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
346100     END-IF                                                               
346200     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
346300     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
346400     MOVE MID-IDLEVNR                 TO EKO-EKH-IDLEVNR                  
346500     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
346600     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
346700     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
346800     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
346900     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
347000     MOVE SPACE                       TO EKO-EKH-FLDCET                   
347100     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
347200     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
347300     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
347400     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
347500                                                                          
347600                                                                          
347700     IF EKO-EKH-KVANTAL NOT = +0                                          
347800       PERFORM IMS-ISRT-EKOTRANS                                          
347900       PERFORM UNTIL SEGMENT-FINNS                                        
348000         ADD +1 TO EKO-FIL-IDSEKVNR                                       
348100         PERFORM IMS-ISRT-EKOTRANS                                        
348200       END-PERFORM                                                        
348300     END-IF                                                               
348400     .                                                                    
348500     EJECT                                                                
348600                                                                          
348700 DFGB-LOGG-EKO-R31-WDR9-RT6 SECTION.                                      
348800     MOVE '102'                       TO EKH-KDEKHHT                      
348900     MOVE '102'                       TO EKH-KDEKSHT                      
349000     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
349100     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
349200     MOVE SPACE                       TO EKH-IDDC-REC                     
349300     MOVE +0                          TO EKH-IDDISTR                      
349400                                         EKH-IDKUNDNR                     
349500*******************************                                           
349600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
349700     MOVE ZERO TO NOLL-RAKNARE                                            
349800     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
349900     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
350000     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
350100          FOR LEADING ZERO                                                
350200     ADD +1 TO NOLL-RAKNARE                                               
350300     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
350400          WITH POINTER NOLL-RAKNARE                                       
350500*******************************                                           
350600     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
350700     MOVE ZERO                        TO EKH-KDPSLLOC                     
350800                                         EKH-PRARTNTO                     
350900                                         EKH-PRARTSJK                     
351000                                         EKH-PRHEMTAG                     
351100                                         EKH-PRLANDCO                     
351200                                         EKH-PRDIRLON                     
351300                                         EKH-PRDMTRL                      
351400                                         EKH-PROVRPAL                     
351500                                         EKH-SUBEL                        
351600     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
351700     MOVE SPACE                       TO EKH-FLLSBOK                      
351800                                                                          
351900     MOVE 'SEK'                       TO EKH-KDVALISO                     
352000     MOVE 1.00                        TO EKH-PRKURS                       
352100                                                                          
352200     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
352300     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
352400     MOVE INLA-ART-KVAVIS             TO EKH-KVANTAL                      
352500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
352600     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
352700     MOVE INLA-INL-IDKONTO            TO EKH-IDKONTO                      
352800     MOVE INLA-INL-IDKST              TO EKH-IDKST                        
352900     MOVE SPACE                       TO EKH-BEVAT                        
353000                                         EKH-KDANMORS                     
353100     MOVE ZERO                        TO EKH-KDFRAKT                      
353200                                         EKH-SUVAT                        
353300                                                                          
353400     MOVE MID-TIAVIDAT                TO WS-DAAVIDAT-YYMMDD               
353500     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
353600       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
353700     ELSE                                                                 
353800       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
353900     END-IF                                                               
354000     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
354100     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
354200     MOVE MID-IDLEVNR                 TO EKH-IDLEVNR                      
354300     MOVE ZERO                        TO EKH-KDAVVTYP                     
354400     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
354500     MOVE ZERO                        TO EKH-KVANTMOT                     
354600     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
354700     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
354800     MOVE 'SEPV'                      TO EKH-KDTRADP                      
354900     MOVE SPACE                       TO EKH-FLDCET                       
355000     MOVE SPACE                       TO EKH-IDKUNDRF                     
355100     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
355200                                                                          
355300     IF EKH-KVANTAL NOT = +0                                              
355400       PERFORM IMS-ISRT-WLSAPA01                                          
355500       PERFORM UNTIL SEGMENT-FINNS                                        
355600         ADD +1 TO FIL-IDSEKVNR                                           
355700         PERFORM IMS-ISRT-WLSAPA01                                        
355800       END-PERFORM                                                        
355900     END-IF                                                               
356000     .                                                                    
356100     EJECT                                                                
356200 DFH-LOGG-092-M113 SECTION.                                               
356300                                                                          
356400     MOVE WS-IDLOPNRM-VVDLLLLK   TO M113-IDLOPNRM                         
356500     MOVE WS-IDAVINR             TO M113-IDAVINR                          
356600     MOVE INLA-INL-IDLEVNR       TO M113-IDLEVNR                          
356700     MOVE INLA-ART-KDRT          TO M113-KDRT                             
356800     MOVE INLA-ART-KVAVIS        TO M113-KVANTAL                          
356900     MOVE WS-ARTC-KDERS        TO M113-KDERS                              
357000     MOVE WS-ARTC-KDLTK        TO M113-KDLTK                              
357100     MOVE WS-ARTC-IDANSK       TO M113-IDANSKNR                           
357200                                                                          
357300     PERFORM S03-RED-W211FEL-GNRL                                         
357400     MOVE WS-ARTC-IDANSK       TO W211FEL-IDKUNDNR-S                      
357500     MOVE '113'                  TO W211FEL-IDFELKODX                     
357600     MOVE M113-M113              TO W211FEL-FELMED                        
357700                                                                          
357800     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
357900     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
358000     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
358100     PERFORM S02-SKAPA-ZZAC01                                             
358200     .                                                                    
358300     EJECT                                                                
358400 DFI-LOGG-092-M117 SECTION.                                               
358500                                                                          
358600     MOVE INLA-ART-BEFT          TO M117-BEFT                             
358700     MOVE WS-ARTC-PRDIRLON     TO M117-PRDIRLON                           
358800     MOVE WS-ARTC-PRDMTRL      TO M117-PRDMTRL                            
358900     MOVE WS-ARTC-PROVRPAL     TO M117-PROVRPAL                           
359000     MOVE WS-ARTC-KDVTH        TO M117-KDVTH                              
359100                                                                          
359200     PERFORM S03-RED-W211FEL-GNRL                                         
359300     MOVE '117'                  TO W211FEL-IDFELKODX                     
359400     MOVE M117-M117              TO W211FEL-FELMED                        
359500                                                                          
359600     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
359700     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
359800     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
359900     PERFORM S02-SKAPA-ZZAC01                                             
360000     .                                                                    
360100     EJECT                                                                
360200 DG-UPPD-ARTS-KVBEART SECTION.                                            
360300     SKIP2                                                                
360400     PERFORM IMS-GHU-ARTS11                                               
360500                                                                          
360600     SUBTRACT WS-TOT-ORD-KVBEART FROM ARTS-SLAG-KVBEART                   
360700     END-SUBTRACT                                                         
360800     PERFORM IMS-REPL-ARTS                                                
360900     .                                                                    
361000     EJECT                                                                
361100                                                                          
361200 DH-UPPD-REMAN SECTION.                                                   
361300     IF BYT02-RENOV                                                       
361400       IF BYT16-BYTES                                                     
361500         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
361600                                  6000                                    
361700       ELSE                                                               
361800         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
361900                                  1000                                    
362000       END-IF                                                             
362100     END-IF                                                               
362200     PERFORM IMS-GHU-WDA901                                               
362300                                                                          
362400     IF SEGMENT-SAKNAS                                                    
362500       PERFORM IMS-GU-ARTC01-REM                                          
362600       MOVE W-IDARTNR-WDA9         TO UPB-IDARTNR                         
362700       MOVE REM-ART-IDFKNGRP       TO UPB-IDFKNGRP                        
362800       PERFORM IMS-ISRT-WDA901                                            
362900     END-IF                                                               
363000                                                                          
363100     MOVE INLA-INL-IDLEVNR         TO W-IDLEVNR-WDB2B                     
363200     PERFORM IMS-GU-WDB201-BSEQ                                           
363300                                                                          
363400     MOVE ZERO                       TO WS-IDDISTR-DISP                   
363500     IF SEGMENT-FINNS                                                     
363600** BARA AKTUELLT MED DISTRIKT: 99XX                                       
363700       MOVE GMT-IDDISTR              TO WS-IDDISTR-DISP                   
363800       PERFORM UNTIL WS-IDDISTR-DISP(1:2) = 99 OR SEGMENT-SAKNAS          
363900         PERFORM IMS-GN-WDB201-BSEQ                                       
364000         IF SEGMENT-FINNS                                                 
364100           MOVE GMT-IDDISTR          TO WS-IDDISTR-DISP                   
364200         END-IF                                                           
364300       END-PERFORM                                                        
364400                                                                          
364500       IF WS-IDDISTR-DISP(1:2) = 99                                       
364600                                                                          
364700         MOVE GMT-IDDISTR              TO W-IDDISTR-WDA9                  
364800         PERFORM IMS-GHU-WDA911                                           
364900                                                                          
365000         IF SEGMENT-SAKNAS                                                
365100           MOVE W-IDDISTR-WDA9         TO   UPD-IDDISTR                   
365200           MOVE ZERO                   TO   UPD-KVLS-REM                  
365300           SUBTRACT INLA-ART-KVAVIS    FROM UPD-KVLS-REM                  
365400           MOVE ZERO                   TO   UPD-DAREGDAT                  
365500                                            UPD-TIREGTID                  
365600           MOVE SPACE                  TO   UPD-IDUSER                    
365700           IF UPD-KVLS-REM < ZERO                                         
365800              MOVE ZERO TO UPD-KVLS-REM                                   
365900           END-IF                                                         
366000           PERFORM IMS-ISRT-WDA911                                        
366100         ELSE                                                             
366200           SUBTRACT INLA-ART-KVAVIS    FROM UPD-KVLS-REM                  
366300           IF UPD-KVLS-REM < ZERO                                         
366400              MOVE ZERO TO UPD-KVLS-REM                                   
366500           END-IF                                                         
366600           PERFORM IMS-REPL-WDA911                                        
366700         END-IF                                                           
366800       END-IF                                                             
366900     END-IF                                                               
367000     .                                                                    
367100     EJECT                                                                
367200 DJ-SKAPA-LARM SECTION.                                                   
367300                                                                          
367400     IF AVROP-SAKNAS                                                      
367500       PERFORM IMS-GU-XXCS01                                              
367600       IF SEGMENT-SAKNAS                                                  
367700         MOVE W-IDHTYP-2239-X TO WLXXCS01                                 
367800         PERFORM IMS-ISRT-XXCS01                                          
367900       END-IF                                                             
368000                                                                          
368100       MOVE SPACE              TO 2240-WDGX2240                           
368200       MOVE INLA-ART-IDARTNR   TO 2240-IDARTNR                            
368300       MOVE INLA-INL-IDLEVNR   TO 2240-IDLEVNR                            
368400       MOVE WS-TIAAVVD         TO 2240-TIANKDAG                           
368500       MOVE INLA-ART-KVAVIS    TO 2240-KVAVIS                             
368600                                                                          
368700       PERFORM IMS-ISRT-XXCS11                                            
368800                                                                          
368900*      LARM-235                                                           
369000                                                                          
369100*******IF WS-ARTC-ADLAGOMR > 13 AND WS-ARTC-ADLAGOMR < 17                 
369200******* KDEFFMAN ?                                                        
369300                                                                          
369400       MOVE INLA-INL-IDLEVNR   TO WS-IDLEVNR-EMIL                         
369500       IF (NOT EJ-GODK-EMIL-LEVNR)                                        
369600        ACCEPT W1-TIKLOCK FROM TIME                                       
369700        COMPUTE W1-TIKLOCK-9KOMPL =                                       
369800                +999999999 - W1-TIKLOCK                                   
369900        MOVE W1-DAREGDAT-9KOMPL TO LAK-DAREGDAT-9KOMPL                    
370000        MOVE W1-TIKLOCK-9KOMPL  TO LAK-TIKLOCK-9KOMPL                     
370100        MOVE INLA-ART-IDARTNR   TO LAK-IDARTNR                            
370200        MOVE INLA-INL-IDDC      TO LAK-IDDC                               
370300        MOVE WS-ARTC-IDANSK     TO LAK-IDANSK                             
370400                                   W-IDANSK-L                             
370500        PERFORM IMS-GET-XXBX-2232                                         
370600        IF SEGMENT-FINNS                                                  
370700          MOVE XXBX-2232-IDANSK-LARM                                      
370800                                TO LAK-IDANSK                             
370900        END-IF                                                            
371000        MOVE INLA-INL-IDLEVNR   TO LAK-IDLEVNR                            
371100        MOVE 235                TO LAK-KDLARM                             
371200        MOVE '*'                TO LAK-FLNYLARM                           
371300        MOVE INLA-ART-KVAVIS    TO LAK-KVAVIS                             
371400        MOVE ZERO               TO LAK-KVAVROP                            
371500        MOVE WS-TIAAMMDD        TO LAK-TIAAMMDD                           
371600        MOVE ZERO               TO LAK-TIAAMMDD-AVS                       
371700                                                                          
371800        PERFORM IMS-ISRT-WDD401                                           
371900       END-IF                                                             
372000     END-IF                                                               
372100                                                                          
372200     PERFORM IMS-GU-XXCT01                                                
372300                                                                          
372400     IF SEGMENT-SAKNAS                                                    
372500       MOVE W-IDHTYP-2241-X    TO WLXXCT01                                
372600       PERFORM IMS-ISRT-XXCT01                                            
372700     END-IF                                                               
372800                                                                          
372900     MOVE SPACE              TO 2242-WDGX2242                             
373000     MOVE INLA-ART-IDARTNR   TO 2242-IDARTNR                              
373100                                                                          
373200     PERFORM IMS-ISRT-XXCT11                                              
373300     .                                                                    
373400     EJECT                                                                
373500 E-STARTA-OM SECTION.                                                     
373600                                                                          
373700     MOVE MAX-MOD-LAENGD         TO MSG-KVLL                              
373800     MOVE 'W6T192X '             TO MSG-KDTRANS-1                         
373900     MOVE '6192'                 TO MSG-IDTRANS-1                         
374000     MOVE MFS-KDMFSFOR           TO MSG-KDMFSFOR-1                        
374100                                                                          
374200     MOVE MID-IDDC               TO MOD-MID-IDDC                          
374300     MOVE MID-IDLEVNR            TO MOD-MID-IDLEVNR                       
374400     MOVE MID-IDFS               TO MOD-MID-IDFS                          
374500     MOVE MID-TIAVIDAT           TO MOD-MID-TIAVIDAT                      
374600     MOVE INLA-ART-IDRADNR-INL   TO MOD-MID-IDRADNR-INL                   
374700                                                                          
374800     PERFORM IMS-ISRT-6192-MSG                                            
374900     .                                                                    
375000     EJECT                                                                
375100 F-FINIT-SANDNING SECTION.                                                
375200                                                                          
375300     MOVE INF-UPDATE-DONE        TO MSG-KOM-IDMFSMED                      
375400                                                                          
375500     PERFORM IMS-ISRT-DISP-MSG                                            
375600     .                                                                    
375700     EJECT                                                                
375800 G-UPPDATERA-KVAKS SECTION.                                               
375900                                                                          
376000     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
376100     PERFORM IMS-GHU-ARTC11                                               
376200                                                                          
376300     SUBTRACT INLA-ART-KVAVIS    FROM ARTC-CLAG-KVAKS-PAV                 
376400     ADD INLA-ART-KVAVIS         TO   ARTC-CLAG-KVAKS-CDC                 
376500                                                                          
376600     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC + ARTC-CLAG-KVAKS-T         
376700     MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                           
376800     MOVE '+' TO LOGG-IDTECKEN-KVAKS                                      
376900     MOVE '-' TO LOGG-IDTECKEN-KVAKS-PAV                                  
377000                                                                          
377100     PERFORM IMS-REPL-ARTC                                                
377200     PERFORM S05-SKAPA-SALDOLOGG-WDK6                                     
377300                                                                          
377400     MOVE ARTC-CLAG-IDARTNR-EMBQ3 TO WS-ARTC-IDARTNR-EMBQ3                
377500     MOVE INLA-ART-IDLOPNRM      TO WS-IDLOPNRM-VVDLLLLK                  
377600     .                                                                    
377700     EJECT                                                                
377800                                                                          
377900 S02-SKAPA-ZZAC01 SECTION.                                                
378000                                                                          
378100     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
378200     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
378300                                                                          
378400     ADD +1                      TO WS-IDLOGLOP                           
378500     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
378600                                                                          
378700     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
378800     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
378900                                                                          
379000     PERFORM IMS-ISRT-ZZAC01                                              
379100     PERFORM UNTIL SEGMENT-FINNS                                          
379200       ACCEPT ZZAC01-TIAAMMDD      FROM DATE                              
379300       ACCEPT ZZAC01-TIKLOCK       FROM TIME                              
379400       ADD +1                      TO WS-IDLOGLOP                         
379500       MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                     
379600       PERFORM IMS-ISRT-ZZAC01                                            
379700     END-PERFORM                                                          
379800     .                                                                    
379900     EJECT                                                                
380000 S02-SKAPA-FILC01 SECTION.                                                
380100                                                                          
380200     MOVE IDPGM                  TO FILC-FIL-IDPGM                        
380300     ACCEPT FILC-FIL-TIREGDAT    FROM DATE                                
380400     ACCEPT FILC-FIL-TIKLOCK     FROM TIME                                
380500     ADD +1                      TO WS-IDSEKVNR                           
380600     MOVE WS-IDSEKVNR            TO FILC-FIL-IDSEKVNR                     
380700                                                                          
380800     PERFORM IMS-ISRT-FILC-TRANS                                          
380900     PERFORM UNTIL SEGMENT-FINNS                                          
381000       ACCEPT FILC-FIL-TIREGDAT    FROM DATE                              
381100       ACCEPT FILC-FIL-TIKLOCK     FROM TIME                              
381200       ADD +1                      TO WS-IDSEKVNR                         
381300       MOVE WS-IDSEKVNR            TO FILC-FIL-IDSEKVNR                   
381400                                                                          
381500       PERFORM IMS-ISRT-FILC-TRANS                                        
381600     END-PERFORM                                                          
381700     .                                                                    
381800     EJECT                                                                
381900 S03-RED-W211FEL-GNRL SECTION.                                            
382000                                                                          
382100     MOVE ZERO                   TO W211FEL-SORT-FLT                      
382200     MOVE SPACE                  TO W211FEL-FILLER2                       
382300                                                                          
382400     MOVE 'R31'                  TO W211FEL-IDPTYP-S                      
382500     MOVE INLA-ART-IDARTNR       TO W211FEL-SORTBGP                       
382600     MOVE 1                      TO W211FEL-KDFELMRK                      
382700     .                                                                    
382800     EJECT                                                                
382900 S05-SKAPA-SALDOLOGG-WDK6 SECTION.                                        
383000     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
383100     MOVE 9                         TO LOGG-IDSEKVNR                      
383200     IF CDC-TR                                                            
383300       MOVE WC-CDC-SE               TO LOGG-IDDC                          
383400     ELSE                                                                 
383500       MOVE MID-IDDC                TO LOGG-IDDC                          
383600     END-IF                                                               
383700     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
383800     MOVE 'R31'                     TO LOGG-IDSUBTYP                      
383900     MOVE 'W6019200'                TO LOGG-IDPGM                         
384000     MOVE '6115'                    TO LOGG-IDTRANS                       
384100     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
384200     MOVE SPACE                     TO LOGG-REF                           
384300     MOVE MID-IDLEVNR               TO LOGG-IDLEVNR                       
384400     MOVE MID-IDFS                  TO LOGG-IDFS                          
384500     MOVE WS-IDLOPNRM-VVDLLLLK      TO LOGG-IDLOPNRM                      
384600     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
384700     MOVE SPACE                     TO LOGG-IDTECKEN-KVLS                 
384800     MOVE INLA-ART-KVAVIS           TO LOGG-KVART-SALDO                   
384900     MOVE ARTC-CLAG-KVEFRS          TO LOGG-KVEFRS                        
385000     MOVE ARTC-CLAG-KVLS            TO LOGG-KVLS                          
385100     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
385200     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
385300     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
385400     ACCEPT WLOGG-TID FROM TIME                                           
385500     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
385600                                                                          
385700     PERFORM IMS-ISRT-WDL9                                                
385800     IF SEGMENT-FINNS-REDAN                                               
385900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
386000          ADD -1 TO LOGG-IDSEKVNR                                         
386100          PERFORM IMS-ISRT-WDL9                                           
386200       END-PERFORM                                                        
386300     END-IF                                                               
386400     .                                                                    
386500     EJECT                                                                
386600                                                                          
386700 S06-SKAPA-SALDOLOGG-WDK7 SECTION.                                        
386800     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
386900     MOVE 9                         TO LOGG-IDSEKVNR                      
387000     IF CDC-TR                                                            
387100       MOVE WC-CDC-SE               TO LOGG-IDDC                          
387200     ELSE                                                                 
387300       MOVE MID-IDDC                TO LOGG-IDDC                          
387400     END-IF                                                               
387500     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
387600     MOVE 'R31'                     TO LOGG-IDSUBTYP                      
387700     MOVE 'W6019200'                TO LOGG-IDPGM                         
387800     MOVE '6115'                    TO LOGG-IDTRANS                       
387900     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
388000     MOVE SPACE                     TO LOGG-REF                           
388100     MOVE WS-IDLOPNRM-VVDLLLLK      TO LOGG-IDLOPNRM                      
388200     MOVE MID-IDLEVNR               TO LOGG-IDLEVNR                       
388300     MOVE MID-IDFS                  TO LOGG-IDFS                          
388400     MOVE '+'                       TO LOGG-IDTECKEN-KVAKS                
388500     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
388600     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
388700     MOVE SPACE                     TO LOGG-IDTECKEN-KVLS                 
388800     MOVE INLA-ART-KVAVIS           TO LOGG-KVART-SALDO                   
388900     MOVE ARTS-SLAG-KVAKS-SDC       TO LOGG-KVAKS                         
389000     MOVE ARTS-SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                     
389100     MOVE ARTS-SLAG-KVEFRS          TO LOGG-KVEFRS                        
389200     MOVE ARTS-SLAG-KVLS            TO LOGG-KVLS                          
389300     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
389400     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
389500     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
389600     ACCEPT WLOGG-TID FROM TIME                                           
389700     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
389800                                                                          
389900     PERFORM IMS-ISRT-WDL9                                                
390000     IF SEGMENT-FINNS-REDAN                                               
390100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
390200          ADD -1 TO LOGG-IDSEKVNR                                         
390300          PERFORM IMS-ISRT-WDL9                                           
390400       END-PERFORM                                                        
390500     END-IF                                                               
390600     .                                                                    
390700     EJECT                                                                
390800 S97-CALL-W218ETA SECTION.                                                
390900                                                                          
391000     MOVE '608'          TO LETA-KDCALL                                   
391100     MOVE SPACE          TO LETA-IDDC-SEND                                
391200     MOVE MID-IDDC       TO LETA-IDDC-REC                                 
391300     MOVE W-IDARTNR      TO LETA-IDARTNR                                  
391400     MOVE SPACE          TO LETA-IDLEVNR                                  
391500     MOVE 0              TO LETA-KDFRAKT                                  
391600     MOVE MSGI-TILOKDAT  TO LETA-TIAAMMDD-ANROP                           
391700                            WS-LOKALTID                                   
391800     IF WS-LOKAL-SEKEL = '9'                                              
391900        MOVE 19          TO LETA-TISEKEL-ANROP                            
392000     ELSE                                                                 
392100        MOVE 20          TO LETA-TISEKEL-ANROP                            
392200     END-IF                                                               
392300                                                                          
392400     CALL W218ETA USING LETA-W218LETA                                     
392500                        ETA-ARTC-PCB                                      
392600                        ETA-ARTS-PCB                                      
392700                        ETA-INLC-PCB                                      
392800                        ETA-LEVA-PCB                                      
392900                        ETA-WDB6-PCB                                      
393000                                                                          
393100     IF LETA-SVAR-OK = 'F' OR 'N'                                         
393200        MOVE 'FEL RETURKOD FRÅN ETA' TO FELTEXT                           
393300        DISPLAY FELTEXT                                                   
393400        CALL FELLOG                                                       
393500     END-IF                                                               
393600     .                                                                    
393700     EJECT                                                                
393800 S98-FIXA-LOKAL-TID SECTION.                                              
393900                                                                          
394000     MOVE ALL '+'         TO MSGI-WMSGINIT                                
394100     MOVE '013'           TO MSGI-KDCALL                                  
394200     MOVE MID-IDDC        TO WS-IDDC-LOCAL-DATE                           
394300                                                                          
394400     MOVE WS-IDDC-LOCAL   TO MSGI-IDUSER                                  
394500                                                                          
394600     CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                         
394700     .                                                                    
394800     EJECT                                                                
394900                                                                          
395000* --- IMS SEKTIONER ---                                                   
395100     SKIP3                                                                
395200 IMS-GET-MSG SECTION.                                                     
395300                                                                          
395400     MOVE '  QC' TO GODK-STATUSKODER                                      
395500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
395600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
395700     PERFORM IMS-STATUSKONTROLL                                           
395800     .                                                                    
395900     SKIP3                                                                
396000 IMS-GN-MSG SECTION.                                                      
396100                                                                          
396200     MOVE '  QD' TO GODK-STATUSKODER                                      
396300     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
396400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
396500     PERFORM IMS-STATUSKONTROLL                                           
396600     .                                                                    
396700     EJECT                                                                
396800 IMS-ISRT-6192-MSG SECTION.                                               
396900                                                                          
397000     MOVE    LOW-VALUE        TO MSG-KDZ1 MSG-KDZ2                        
397100     MOVE    '  '             TO GODK-STATUSKODER                         
397200     CALL    CBLTDLI          USING ISRT 6192-PCB MSG-IO-AREA             
397300     MOVE    6192-STATUS-CODE TO STATUS-WS                                
397400     PERFORM IMS-STATUSKONTROLL                                           
397500     CALL    CBLTDLI          USING ISRT 6192-PCB MSG-KOM-WMSGKOM         
397600     MOVE    6192-STATUS-CODE TO STATUS-WS                                
397700     PERFORM IMS-STATUSKONTROLL                                           
397800     .                                                                    
397900     SKIP3                                                                
398000 IMS-ISRT-DISP-MSG SECTION.                                               
398100                                                                          
398200     MOVE    '  '             TO GODK-STATUSKODER                         
398300     CALL    CBLTDLI          USING ISRT DISP-PCB MSG-KOM-WMSGKOM         
398400     MOVE    DISP-STATUS-CODE TO STATUS-WS                                
398500     PERFORM IMS-STATUSKONTROLL                                           
398600     .                                                                    
398700     EJECT                                                                
398800 IMS-GU-INLA-INL SECTION.                                                 
398900     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
399000          DELIMITED BY SIZE INTO SSA1                                     
399100     MOVE '  ' TO GODK-STATUSKODER                                        
399200     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1                     
399300     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
399400     PERFORM IMS-STATUSKONTROLL                                           
399500     .                                                                    
399600     SKIP3                                                                
399700 IMS-GNP-INLA-ART-GQ SECTION.                                             
399800     STRING 'W6INLA11(IDRADNRI>=' W-IDRADNR-INL-X ')'                     
399900          DELIMITED BY SIZE INTO SSA1                                     
400000     MOVE '  ' TO GODK-STATUSKODER                                        
400100     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA3 SSA1                    
400200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
400300     PERFORM IMS-STATUSKONTROLL                                           
400400     .                                                                    
400500     SKIP3                                                                
400600 IMS-GNP-INLA-ART SECTION.                                                
400700     MOVE 'W6INLA11 ' TO SSA1                                             
400800     MOVE '  GE' TO GODK-STATUSKODER                                      
400900     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA3 SSA1                    
401000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
401100     PERFORM IMS-STATUSKONTROLL                                           
401200     .                                                                    
401300     EJECT                                                                
401400 IMS-GU-ARTC01-REM SECTION.                                               
401500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
401600          DELIMITED BY SIZE INTO SSA1                                     
401700     MOVE '  ' TO GODK-STATUSKODER                                        
401800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK601 SSA1                    
401900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
402000     PERFORM IMS-STATUSKONTROLL                                           
402100     .                                                                    
402200     SKIP3                                                                
402300 IMS-GU-ARTC01 SECTION.                                                   
402400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
402500          DELIMITED BY SIZE INTO SSA1                                     
402600     MOVE '  ' TO GODK-STATUSKODER                                        
402700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
402800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
402900     PERFORM IMS-STATUSKONTROLL                                           
403000     .                                                                    
403100     SKIP3                                                                
403200 IMS-GHU-ARTC11 SECTION.                                                  
403300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
403400          DELIMITED BY SIZE INTO SSA1                                     
403500     MOVE 'WLARTC11'       TO SSA2                                        
403600     MOVE '  ' TO GODK-STATUSKODER                                        
403700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
403800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
403900     PERFORM IMS-STATUSKONTROLL                                           
404000     .                                                                    
404100     SKIP3                                                                
404200 IMS-GNP-ARTC23 SECTION.                                                  
404300                                                                          
404400     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
404500     MOVE 'WLARTC23'              TO SSA2                                 
404600     MOVE '  GE' TO GODK-STATUSKODER                                      
404700     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
404800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
404900     PERFORM IMS-STATUSKONTROLL                                           
405000     .                                                                    
405100     SKIP3                                                                
405200 IMS-GHNP-ARTC11 SECTION.                                                 
405300     MOVE 'WLARTC11 ' TO SSA1                                             
405400     MOVE '  ' TO GODK-STATUSKODER                                        
405500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
405600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
405700     PERFORM IMS-STATUSKONTROLL                                           
405800     .                                                                    
405900     SKIP3                                                                
406000 IMS-GHNP-ARTC21 SECTION.                                                 
406100     MOVE SPACE TO SSA1                                                   
406200                                                                          
406300     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X ')'                      
406400          DELIMITED BY SIZE INTO SSA1                                     
406500     MOVE '  GE' TO GODK-STATUSKODER                                      
406600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
406700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
406800     PERFORM IMS-STATUSKONTROLL                                           
406900     .                                                                    
407000     SKIP3                                                                
407100 IMS-REPL-ARTC SECTION.                                                   
407200                                                                          
407300     MOVE '  ' TO GODK-STATUSKODER                                        
407400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
407500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
407600     PERFORM IMS-STATUSKONTROLL                                           
407700     .                                                                    
407800     EJECT                                                                
407900 IMS-GHU-ARTS11 SECTION.                                                  
408000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
408100          DELIMITED BY SIZE INTO SSA1                                     
408200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
408300          DELIMITED BY SIZE INTO SSA2                                     
408400     MOVE '  ' TO GODK-STATUSKODER                                        
408500     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2         
408600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
408700     PERFORM IMS-STATUSKONTROLL                                           
408800     .                                                                    
408900     SKIP3                                                                
409000 IMS-REPL-ARTS SECTION.                                                   
409100                                                                          
409200     MOVE '  ' TO GODK-STATUSKODER                                        
409300     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-ARTS11                  
409400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
409500     PERFORM IMS-STATUSKONTROLL                                           
409600     .                                                                    
409700     SKIP3                                                                
409800 IMS-GU-ARTS11 SECTION.                                                   
409900     MOVE 'IMS-GU-ARTS11      '   TO CURRENT-IMS-SECTION                  
410000                                                                          
410100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
410200          DELIMITED BY SIZE INTO SSA1                                     
410300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
410400          DELIMITED BY SIZE INTO SSA2                                     
410500     MOVE '  ' TO GODK-STATUSKODER                                        
410600     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2          
410700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
410800     PERFORM IMS-STATUSKONTROLL                                           
410900     .                                                                    
411000     SKIP3                                                                
411100 IMS-GNP-WDK722 SECTION.                                                  
411200     MOVE 'IMS-GNP-WDK722     '   TO CURRENT-IMS-SECTION                  
411300                                                                          
411400     MOVE 'WDK722  ' TO SSA1                                              
411500     MOVE '  GE' TO GODK-STATUSKODER                                      
411600     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WDK722 SSA1                   
411700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
411800     PERFORM IMS-STATUSKONTROLL                                           
411900     .                                                                    
412000     SKIP3                                                                
412100 IMS-GHU-WDK711 SECTION.                                                  
412200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
412300          DELIMITED BY SIZE INTO SSA1                                     
412400     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
412500          DELIMITED BY SIZE INTO SSA2                                     
412600     MOVE '  GE' TO GODK-STATUSKODER                                      
412700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711                        
412800          SSA1 SSA2                                                       
412900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
413000     PERFORM IMS-STATUSKONTROLL                                           
413100     .                                                                    
413200     SKIP3                                                                
413300 IMS-GHU-WDK712 SECTION.                                                  
413400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
413500          DELIMITED BY SIZE INTO SSA1                                     
413600     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
413700          DELIMITED BY SIZE INTO SSA2                                     
413800     MOVE '    ' TO GODK-STATUSKODER                                      
413900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
414000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
414100     PERFORM IMS-STATUSKONTROLL                                           
414200     .                                                                    
414300 IMS-REPL-WDK712 SECTION.                                                 
414400     MOVE '  ' TO GODK-STATUSKODER                                        
414500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
414600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
414700     PERFORM IMS-STATUSKONTROLL                                           
414800     .                                                                    
414900     SKIP3                                                                
415000 IMS-GNP-WDK723 SECTION.                                                  
415100     MOVE   'WDK723'    TO SSA1                                           
415200     MOVE '  GE' TO GODK-STATUSKODER                                      
415300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
415400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
415500     PERFORM IMS-STATUSKONTROLL                                           
415600     .                                                                    
415700     SKIP3                                                                
415800 IMS-GHNP-WDK724 SECTION.                                                 
415900     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
416000                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
416100          DELIMITED BY SIZE INTO SSA1                                     
416200     MOVE '  GE' TO GODK-STATUSKODER                                      
416300     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK724 SSA1                  
416400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
416500     PERFORM IMS-STATUSKONTROLL                                           
416600     .                                                                    
416700     SKIP3                                                                
416800 IMS-REPL-WDK724 SECTION.                                                 
416900     MOVE '  ' TO GODK-STATUSKODER                                        
417000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK724                       
417100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
417200     PERFORM IMS-STATUSKONTROLL                                           
417300     .                                                                    
417400     EJECT                                                                
417500 IMS-GU-INLC01 SECTION.                                                   
417600     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
417700          DELIMITED BY SIZE INTO SSA1                                     
417800     MOVE '  GE' TO GODK-STATUSKODER                                      
417900     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA-INLC01 SSA1               
418000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
418100     PERFORM IMS-STATUSKONTROLL                                           
418200     .                                                                    
418300     SKIP3                                                                
418400 IMS-ISRT-INLC01 SECTION.                                                 
418500     STRING 'WLINLC01 '                                                   
418600          DELIMITED BY SIZE INTO SSA1                                     
418700     MOVE '  II' TO GODK-STATUSKODER                                      
418800     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-INLC01 SSA1             
418900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
419000     PERFORM IMS-STATUSKONTROLL                                           
419100     .                                                                    
419200     SKIP3                                                                
419300 IMS-ISRT-INLC11 SECTION.                                                 
419400     STRING 'WLINLC11 '                                                   
419500          DELIMITED BY SIZE INTO SSA1                                     
419600     MOVE '  II' TO GODK-STATUSKODER                                      
419700     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-INLC11 SSA1             
419800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
419900     PERFORM IMS-STATUSKONTROLL                                           
420000     .                                                                    
420100     SKIP3                                                                
420200 IMS-GHNP-INLC12 SECTION.                                                 
420300     STRING 'WLINLC12 '                                                   
420400          DELIMITED BY SIZE INTO SSA1                                     
420500     MOVE '  GE' TO GODK-STATUSKODER                                      
420600     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-AREA-INLC12 SSA1             
420700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
420800     PERFORM IMS-STATUSKONTROLL                                           
420900     .                                                                    
421000     SKIP3                                                                
421100 IMS-REPL-INLC12 SECTION.                                                 
421200     STRING 'WLINLC12 '                                                   
421300          DELIMITED BY SIZE INTO SSA1                                     
421400     MOVE '  ' TO GODK-STATUSKODER                                        
421500     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-INLC12 SSA1             
421600     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
421700     PERFORM IMS-STATUSKONTROLL                                           
421800     .                                                                    
421900     SKIP3                                                                
422000 IMS-ISRT-INLC12 SECTION.                                                 
422100     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
422200          DELIMITED BY SIZE INTO SSA1                                     
422300     STRING 'WLINLC12 '                                                   
422400          DELIMITED BY SIZE INTO SSA2                                     
422500     MOVE '  II' TO GODK-STATUSKODER                                      
422600     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-INLC12 SSA1 SSA2        
422700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
422800     PERFORM IMS-STATUSKONTROLL                                           
422900     .                                                                    
423000     SKIP3                                                                
423100 IMS-GU-INLB11 SECTION.                                                   
423200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
423300          DELIMITED BY SIZE INTO SSA1                                     
423400     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
423500          DELIMITED BY SIZE INTO SSA2                                     
423600     MOVE '  GE' TO GODK-STATUSKODER                                      
423700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
423800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
423900     PERFORM IMS-STATUSKONTROLL                                           
424000     .                                                                    
424100     EJECT                                                                
424200 IMS-GHU-INLB11 SECTION.                                                  
424300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
424400          DELIMITED BY SIZE INTO SSA1                                     
424500     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
424600          DELIMITED BY SIZE INTO SSA2                                     
424700     MOVE '  ' TO GODK-STATUSKODER                                        
424800     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
424900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
425000     PERFORM IMS-STATUSKONTROLL                                           
425100     .                                                                    
425200     SKIP3                                                                
425300 IMS-GHNP-INLB23-KD SECTION.                                              
425400     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
425500          DELIMITED BY SIZE INTO SSA1                                     
425600     MOVE '  GE' TO GODK-STATUSKODER                                      
425700     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
425800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
425900     PERFORM IMS-STATUSKONTROLL                                           
426000     .                                                                    
426100     EJECT                                                                
426200 IMS-GHNP-INLB23-TI-F SECTION.                                            
426300* ?  STRING 'WLINLB23*F(DAAVROP  =' W-DAAVROP-X                           
426400     STRING 'WLINLB23*F(WDD905KY =' W-WDD905KY-X                          
426500                      '&KDAVROP  =' W-KDAVROP-X ')'                       
426600          DELIMITED BY SIZE INTO SSA1                                     
426700     MOVE '  ' TO GODK-STATUSKODER                                        
426800     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
426900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
427000     PERFORM IMS-STATUSKONTROLL                                           
427100     .                                                                    
427200     SKIP3                                                                
427300 IMS-GNP-INLB32 SECTION.                                                  
427400     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
427500          DELIMITED BY SIZE INTO SSA1                                     
427600     STRING 'WLINLB32(IDORDNSB =' W-IDORDNSB-X ')'                        
427700          DELIMITED BY SIZE INTO SSA2                                     
427800     MOVE '  ' TO GODK-STATUSKODER                                        
427900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
428000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
428100     PERFORM IMS-STATUSKONTROLL                                           
428200     .                                                                    
428300     EJECT                                                                
428400 IMS-GHNP-INLB24 SECTION.                                                 
428500     MOVE 'WLINLB24 ' TO SSA1                                             
428600     MOVE '  GE' TO GODK-STATUSKODER                                      
428700     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
428800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
428900     PERFORM IMS-STATUSKONTROLL                                           
429000     .                                                                    
429100     SKIP3                                                                
429200 IMS-ISRT-INLB31 SECTION.                                                 
429300     MOVE 'WLINLB31 ' TO SSA1                                             
429400     MOVE '  ' TO GODK-STATUSKODER                                        
429500     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA4 SSA1                   
429600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
429700     PERFORM IMS-STATUSKONTROLL                                           
429800     .                                                                    
429900     SKIP3                                                                
430000 IMS-REPL-INLB SECTION.                                                   
430100     MOVE '  ' TO GODK-STATUSKODER                                        
430200     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
430300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
430400     PERFORM IMS-STATUSKONTROLL                                           
430500     .                                                                    
430600     SKIP3                                                                
430700 IMS-DLET-INLB SECTION.                                                   
430800     MOVE '  ' TO GODK-STATUSKODER                                        
430900     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
431000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
431100     PERFORM IMS-STATUSKONTROLL                                           
431200     .                                                                    
431300     EJECT                                                                
431400 IMS-GU-INLE-ART SECTION.                                                 
431500     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
431600          DELIMITED BY SIZE INTO SSA1                                     
431700     MOVE '  GE' TO GODK-STATUSKODER                                      
431800     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA SSA1                      
431900     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
432000     PERFORM IMS-STATUSKONTROLL                                           
432100     .                                                                    
432200     SKIP3                                                                
432300 IMS-ISRT-INLE-ART SECTION.                                               
432400     MOVE 'WLINLE01 ' TO SSA1                                             
432500     MOVE '  ' TO GODK-STATUSKODER                                        
432600     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
432700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
432800     PERFORM IMS-STATUSKONTROLL                                           
432900     .                                                                    
433000     SKIP3                                                                
433100 IMS-ISRT-INLE-INL SECTION.                                               
433200     MOVE 'WLINLE11 ' TO SSA1                                             
433300     MOVE '  II' TO GODK-STATUSKODER                                      
433400     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
433500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
433600     PERFORM IMS-STATUSKONTROLL                                           
433700     .                                                                    
433800     SKIP3                                                                
433900 IMS-ISRT-INLE-MOT SECTION.                                               
434000     MOVE 'WLINLE21 ' TO SSA1                                             
434100     MOVE '  ' TO GODK-STATUSKODER                                        
434200     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
434300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
434400     PERFORM IMS-STATUSKONTROLL                                           
434500     .                                                                    
434600     EJECT                                                                
434700 IMS-ISRT-ZZAC01 SECTION.                                                 
434800     MOVE 'WLZZAC01 ' TO SSA1                                             
434900     MOVE '  II' TO GODK-STATUSKODER                                      
435000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
435100     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
435200     PERFORM IMS-STATUSKONTROLL                                           
435300     .                                                                    
435400     SKIP3                                                                
435500 IMS-GU-XXCS01      SECTION.                                              
435600     STRING 'WLXXCS01(WDG3KEY  =' W-IDHTYP-2239-X ')'                     
435700          DELIMITED BY SIZE INTO SSA1                                     
435800     MOVE '  GE' TO GODK-STATUSKODER                                      
435900     CALL CBLTDLI USING GU   XXCS-PCB DLI-IO-AREA6 SSA1                   
436000     MOVE XXCS-STATUS-CODE TO STATUS-WS                                   
436100     PERFORM IMS-STATUSKONTROLL                                           
436200     .                                                                    
436300     EJECT                                                                
436400 IMS-ISRT-XXCS01    SECTION.                                              
436500     MOVE 'WLXXCS01' TO SSA1                                              
436600     MOVE '  ' TO GODK-STATUSKODER                                        
436700     CALL CBLTDLI USING ISRT XXCS-PCB DLI-IO-AREA6 SSA1                   
436800     MOVE XXCS-STATUS-CODE TO STATUS-WS                                   
436900     PERFORM IMS-STATUSKONTROLL                                           
437000     .                                                                    
437100     EJECT                                                                
437200 IMS-ISRT-XXCS11    SECTION.                                              
437300     MOVE 'WLXXCS11' TO SSA1                                              
437400     MOVE '  ' TO GODK-STATUSKODER                                        
437500     CALL CBLTDLI USING ISRT XXCS-PCB DLI-IO-AREA6 SSA1                   
437600     MOVE XXCS-STATUS-CODE TO STATUS-WS                                   
437700     PERFORM IMS-STATUSKONTROLL                                           
437800     .                                                                    
437900     EJECT                                                                
438000 IMS-GU-XXCT01      SECTION.                                              
438100     STRING 'WLXXCT01(WDG3KEY  =' W-IDHTYP-2241-X ')'                     
438200          DELIMITED BY SIZE INTO SSA1                                     
438300     MOVE '  GE' TO GODK-STATUSKODER                                      
438400     CALL CBLTDLI USING GU   XXCT-PCB DLI-IO-AREA6 SSA1                   
438500     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
438600     PERFORM IMS-STATUSKONTROLL                                           
438700     .                                                                    
438800     EJECT                                                                
438900 IMS-ISRT-XXCT11    SECTION.                                              
439000     MOVE 'WLXXCT11' TO SSA1                                              
439100     MOVE '  ' TO GODK-STATUSKODER                                        
439200     CALL CBLTDLI USING ISRT XXCT-PCB DLI-IO-AREA6 SSA1                   
439300     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
439400     PERFORM IMS-STATUSKONTROLL                                           
439500     .                                                                    
439600     EJECT                                                                
439700 IMS-ISRT-XXCT01    SECTION.                                              
439800     MOVE 'WLXXCT01' TO SSA1                                              
439900     MOVE '  ' TO GODK-STATUSKODER                                        
440000     CALL CBLTDLI USING ISRT XXCT-PCB DLI-IO-AREA6 SSA1                   
440100     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
440200     PERFORM IMS-STATUSKONTROLL                                           
440300     .                                                                    
440400     EJECT                                                                
440500 IMS-ISRT-FILC-TRANS  SECTION.                                            
440600     MOVE 'WLFILC01 ' TO SSA1                                             
440700     MOVE '  II' TO GODK-STATUSKODER                                      
440800     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA  SSA1                   
440900     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
441000     PERFORM IMS-STATUSKONTROLL                                           
441100     .                                                                    
441200     EJECT                                                                
441300 IMS-ISRT-WDL9   SECTION.                                                 
441400     MOVE 'WLLOGA01 ' TO SSA1                                             
441500     MOVE '  II' TO GODK-STATUSKODER                                      
441600     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
441700     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
441800     PERFORM IMS-STATUSKONTROLL                                           
441900     .                                                                    
442000     SKIP3                                                                
442100 IMS-ISRT-EKOTRANS  SECTION.                                              
442200     MOVE 'WLFILB01 ' TO SSA1                                             
442300     MOVE '  II' TO GODK-STATUSKODER                                      
442400     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-FILB01 SSA1             
442500     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
442600     PERFORM IMS-STATUSKONTROLL                                           
442700     .                                                                    
442800     EJECT                                                                
442900 IMS-ISRT-WLSAPA01 SECTION.                                               
443000     MOVE 'WLSAPA01 ' TO SSA1                                             
443100     MOVE '  II' TO GODK-STATUSKODER                                      
443200     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
443300     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
443400     PERFORM IMS-STATUSKONTROLL                                           
443500     .                                                                    
443600     EJECT                                                                
443700                                                                          
443800 IMS-GHU-WDA901 SECTION.                                                  
443900     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
444000          DELIMITED BY SIZE INTO SSA1                                     
444100     MOVE '  GE'           TO GODK-STATUSKODER                            
444200     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA901 SSA1                   
444300     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
444400     PERFORM IMS-STATUSKONTROLL                                           
444500     .                                                                    
444600     EJECT                                                                
444700 IMS-GHU-WDA911 SECTION.                                                  
444800     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
444900          DELIMITED BY SIZE INTO SSA1                                     
445000     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-WDA9-X ')'                    
445100          DELIMITED BY SIZE INTO SSA2                                     
445200     MOVE '  GE'           TO GODK-STATUSKODER                            
445300     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA911 SSA1                   
445400                                                   SSA2                   
445500     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
445600     PERFORM IMS-STATUSKONTROLL                                           
445700     .                                                                    
445800                                                                          
445900 IMS-ISRT-WDA901 SECTION.                                                 
446000     MOVE 'WDA901   '      TO SSA1                                        
446100     MOVE '  '             TO GODK-STATUSKODER                            
446200     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
446300     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
446400     PERFORM IMS-STATUSKONTROLL                                           
446500     .                                                                    
446600                                                                          
446700 IMS-ISRT-WDA911 SECTION.                                                 
446800     MOVE 'WDA911   '      TO SSA1                                        
446900     MOVE '  '             TO GODK-STATUSKODER                            
447000     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA911 SSA1                  
447100     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
447200     PERFORM IMS-STATUSKONTROLL                                           
447300     .                                                                    
447400                                                                          
447500 IMS-REPL-WDA911 SECTION.                                                 
447600     MOVE '  '             TO GODK-STATUSKODER                            
447700     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA911                       
447800     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
447900     PERFORM IMS-STATUSKONTROLL                                           
448000     .                                                                    
448100     EJECT                                                                
448200 IMS-GU-WDB201-BSEQ SECTION.                                              
448300     STRING 'WDB201  (WDB2BSEQ =' W-WDB2BSEQ-X ')'                        
448400          DELIMITED BY SIZE INTO SSA1                                     
448500     MOVE '  GE'            TO GODK-STATUSKODER                           
448600     CALL CBLTDLI USING GU WDB2B-PCB DLI-IO-WDB201 SSA1                   
448700     MOVE WDB2B-STATUS-CODE TO STATUS-WS                                  
448800     PERFORM IMS-STATUSKONTROLL                                           
448900     .                                                                    
449000     EJECT                                                                
449100 IMS-GN-WDB201-BSEQ SECTION.                                              
449200     STRING 'WDB201  (WDB2BSEQ =' W-WDB2BSEQ-X ')'                        
449300          DELIMITED BY SIZE INTO SSA1                                     
449400     MOVE '  GE'            TO GODK-STATUSKODER                           
449500     CALL CBLTDLI USING GN WDB2B-PCB DLI-IO-WDB201 SSA1                   
449600     MOVE WDB2B-STATUS-CODE TO STATUS-WS                                  
449700     PERFORM IMS-STATUSKONTROLL                                           
449800     .                                                                    
449900     EJECT                                                                
450000 IMS-ISRT-WDD401 SECTION.                                                 
450100     MOVE 'IMS-ISRT-WDD401        '   TO CURRENT-IMS-SECTION              
450200                                                                          
450300     STRING 'WDD401     '                                                 
450400          DELIMITED BY SIZE INTO SSA1                                     
450500     MOVE '  II'            TO GODK-STATUSKODER                           
450600     CALL CBLTDLI USING ISRT WDD4-PCB DLI-IO-WDD401 SSA1                  
450700     MOVE WDD4-STATUS-CODE  TO STATUS-WS                                  
450800     PERFORM IMS-STATUSKONTROLL                                           
450900     .                                                                    
451000     EJECT                                                                
451100 IMS-GET-XXBX-2232 SECTION.                                               
451200                                                                          
451300     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
451400          DELIMITED BY SIZE INTO SSA1                                     
451500     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
451600          DELIMITED BY SIZE INTO SSA2                                     
451700     MOVE '  GE' TO GODK-STATUSKODER                                      
451800     CALL CBLTDLI USING GU  XXBX-PCB DLI-IO-AREA-BX SSA1 SSA2             
451900     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
452000     PERFORM IMS-STATUSKONTROLL                                           
452100     .                                                                    
452200     EJECT                                                                
452300 IMS-GU-WDD905-NEXT SECTION.                                              
452400                                                                          
452500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
452600          DELIMITED BY SIZE INTO SSA1                                     
452700     STRING 'WDD902  (IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
452800          DELIMITED BY SIZE INTO SSA2                                     
452900     STRING 'WDD905  (WDD905KY >' W-WDD905KY-LARM-X                       
453000                    '&KDAVROP  =' W-KDAVROP-LARM-X ')'                    
453100          DELIMITED BY SIZE INTO SSA3                                     
453200     MOVE '  GE' TO GODK-STATUSKODER                                      
453300     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-AREA5 SSA1 SSA2 SSA3           
453400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
453500     PERFORM IMS-STATUSKONTROLL                                           
453600     .                                                                    
453700     EJECT                                                                
453800 IMS-GET-WDF101 SECTION.                                                  
453900                                                                          
454000     STRING 'WDF101  (IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
454100          DELIMITED BY SIZE INTO SSA1                                     
454200     MOVE '  GE' TO GODK-STATUSKODER                                      
454300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
454400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
454500     PERFORM IMS-STATUSKONTROLL                                           
454600     IF SEGMENT-FINNS                                                     
454700        MOVE JA  TO SW-WDF1-FINNS                                         
454800     ELSE                                                                 
454900        MOVE NEJ TO SW-WDF1-FINNS                                         
455000     END-IF                                                               
455100     .                                                                    
455200     EJECT                                                                
455300                                                                          
455400 IMS-GU-WDF101   SECTION.                                                 
455500     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
455600     DELIMITED BY SIZE INTO SSA1                                          
455700     MOVE '  GE' TO GODK-STATUSKODER                                      
455800     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
455900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
456000     PERFORM IMS-STATUSKONTROLL                                           
456100     .                                                                    
456200     SKIP3                                                                
456300                                                                          
456400 IMS-GNP-WDF102   SECTION.                                                
456500     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
456600     DELIMITED BY SIZE INTO SSA1                                          
456700     MOVE '  GE' TO GODK-STATUSKODER                                      
456800     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA-F102 SSA1                
456900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
457000     PERFORM IMS-STATUSKONTROLL                                           
457100     .                                                                    
457200     EJECT                                                                
457300                                                                          
457400 IMS-GU-WDB601    SECTION.                                                
457500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
457600          DELIMITED BY SIZE INTO SSA1                                     
457700     MOVE '  GE' TO GODK-STATUSKODER                                      
457800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
457900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
458000     PERFORM IMS-STATUSKONTROLL                                           
458100     IF SEGMENT-SAKNAS                                                    
458200         MOVE SPACE TO DCS-KDDC                                           
458300     END-IF                                                               
458400     .                                                                    
458500                                                                          
458600 IMS-GNP-WDB617    SECTION.                                               
458700     MOVE 'WDB617   ' TO SSA1                                             
458800     MOVE '  GE' TO GODK-STATUSKODER                                      
458900     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
459000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
459100     PERFORM IMS-STATUSKONTROLL                                           
459200     .                                                                    
459300                                                                          
459400 IMS-GU-WDGX9306 SECTION.                                                 
459500     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
459600             DELIMITED BY SIZE INTO SSA1                                  
459700     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
459800             DELIMITED BY SIZE INTO SSA2                                  
459900     MOVE '  GE'   TO GODK-STATUSKODER                                    
460000     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
460100     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
460200     PERFORM IMS-STATUSKONTROLL                                           
460300     .                                                                    
460400     SKIP3                                                                
460500                                                                          
460600 IMS-GNP-WDGX9308 SECTION.                                                
460700     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
460800             DELIMITED BY SIZE INTO SSA1                                  
460900     MOVE '  GE'   TO GODK-STATUSKODER                                    
461000     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
461100     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
461200     PERFORM IMS-STATUSKONTROLL                                           
461300     .                                                                    
461400     SKIP3                                                                
461500                                                                          
461600 IMS-GNP-WDGX9308-FIRST SECTION.                                          
461700     MOVE 'WDGX9308*F' TO SSA1                                            
461800     MOVE '  GE'   TO GODK-STATUSKODER                                    
461900     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
462000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
462100     PERFORM IMS-STATUSKONTROLL                                           
462200     .                                                                    
462300     SKIP3                                                                
462400                                                                          
462500 IMS-STATUSKONTROLL SECTION.                                              
462600                                                                          
462700     SET STATUS-IX TO 1                                                   
462800     SEARCH GODK-STATUS                                                   
462900       AT END                                                             
463000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
463100         DELIMITED BY SIZE INTO FELTEXT                                   
463200         CALL FELLOG                                                      
463300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
463400         CONTINUE                                                         
463500     END-SEARCH                                                           
463600     .                                                                    
463700     EJECT                                                                
463800*    -COPY WY2000P3                                                       
463900                                                                          
