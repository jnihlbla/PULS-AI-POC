000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W222BHDC.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   12/11/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET BERÄKNAR BEHOV FÖR LOKAL ANSKAFFNING I NDC-CN         
001000*        OCH MOTSVARAR SUB-PGM W2222200 FÖR CDC ANSKAFFNING.              
001100*        ÄVEN OCH NDC-NA (USA).RÄKNAR MED BÅDE REFILLBEHOV INOM           
001200*        LANDET OCH GLOBAL EXPORT.                                        
001300*                                                                         
001400*        PROGRAMMET ÄR ETT SUBPROGRAM FÖR BERÄKNING AV                    
001500*        ARTIKEL-BEHOV. EN BEHOVSTABELL ANGER PER VECKA                   
001600*        EN ARTIKELS FÖRVÄNTADE BEHOV. BEHOVEN BESTÅR AV                  
001700*            - PROGNOS   BASERAT PÅ PB-SEP(PB-REF),SÄSONGSINDEX,          
001800*                        TRENDER, PB-JUSTERINGAR                          
001900*                                                                         
002000*        BERÄKNINGS-OMFATTNING OCH -RESULTAT FÖRMEDDLAS                   
002100*        VIA LÄNKAREAN W222BHDC                                           
002200*                                                                         
002300*        PROGRAMMET LÄSER      WDK6 WDK7 WDB6 WDD7A                       
002400*                              WDR2 (WDGX2502) REFILL PARAMETERTAB        
002500*                                                                         
002600*    SUBPROGRAM.                                                          
002700*        W009VADD    ADD AV VECKOR TILL DATUM                             
002800*        PERADD      ADD AV PERIODER TILL DATUM                           
002900*        W272REFL    PÅFYLLNADS-PUNKT,-KVANTITET OCH                      
003000*                    ÖVERLAGERPUNKT FÖR CDC-REFILL.                       
003100*                                                                         
003200*                                                                         
003300*    INPUT                                                                
003400*        BHDC-KDBEHOV: 01 = ENDAST SEPARATBEHOV PÅ ETT DC SOM HAR         
003500*                           EN LOKAL LEVERANTÖR.                          
003600*                           KVPB-REF =                                    
003700*                           LAGERSALDO + REFILL ORDER + AK SALDO +        
003800*                           OKS SALDO + RO SALDO + KVALITETSSPÄRR-        
003900*                           SALDO. TA ÄVEN MED SÄSONGSFAKTORN,            
004000*                           TRENDER, PB-JUSTERING OCH ARBETSDAGAR.        
004100*                                                                         
004200*                      02 = ENDAST XDC-BEHOV.RÄKNAR FRAM BEHOV FÖR        
004300*                           ALLA LDC'R OCH NDC'R SOM REFILLAS FRÅN        
004400*                           OVAN NDC.                                     
004500*                           KVPB-REFILL =                                 
004600*                           LAGERSALDO + REFILL ORDER + AK SALDO +        
004700*                           OKS SALDO + RO SALDO + KVALITETSSPÄRR-        
004800*                           SALDO.PROGNOS (KVPB-REF + KVPBREOI).          
004900*                           TA ÄVEN MED SÄSONGSFAKTORN OCH ARBETS-        
005000*                           DAGAR.                                        
005100*                                                                         
005200*                      03 = PB TOTAL SEP-BEHOV + XDC-BEHOV (UNDER-        
005300*                           LIGGANDE REFILL DC) INKL. TREND.              
005400*                                                                         
005500*                      04 = ENDAST CDC-BEHOV.RÄKNAR FRAM BEHOV FÖR        
005600*                           CDC OCH ALLA CHILD DC SOM REFILLAS            
005700*                           FRÅN CDC(XDC MED IDDC-REF = 11).CDC           
005800*                           ANVÄNDER PB-PLAN ISTÄLLET FÖR PBREOI.         
005900*                           EJ PB-SEP+PBREOI SOM FÖR NDC-BEHOVEN.         
006000*                           TILLGÅNGAR PÅ CDC RÄKNAS EJ LIKA SOM          
006100*                           FÖR NDC.KVLS-TOTAL LAGRAS PÅ WDK611.          
006200*                           FÖR CDC GÄLLER 5-DAGARS ARBETSVECKA.          
006300*                                                                         
006400*                      05 = XDC-BEHOV + CDC-BEHOV.ALLA UNDER-             
006500*                           LIGGANDE DC SOM REFILLAS FRÅN KINA.           
006600*                                                                         
006700*                                                                         
006800*    OUTPUT                                                               
006900*        BHDC-FLJANEJ-ANROP: J = ANROP OK,  N = ANROP FEL                 
007000*                                                                         
007100*    ABENDKODER:                                                          
007200*        U0016 -  . . . .                                                 
007300*        U1000 -  . . . .                                                 
007400*                                                                         
007500***---------------------------------------------------------------        
007600*    ÄNDRINGAR:                                                           
007700*                                                                         
007800*    2014-10-21  E'TRACKER 10243380                                       
007900*                RÄTTA SCR 8616110 FEL LÄSNINGAR WDD7                     
008000*                                                                         
008100*    2015-09-28  E'TRACKER 10243132 CHINA EXPORT PROJECT                  
008200*                                                                         
008300*    2017-08-08  E'TRACKER 10299286 LOCAL SOURCING US                     
008400*                SAMMA CCID SOM REFILLEN: 10302687                        
008500*                                                                         
008600*    2018-05-22  JIRA PULS-976 GLOBAL EXPORT ANSKAFFNING                  
008700*                PROJECT ID:2558 GLOBAL EXPORT 2018                       
008800*                                                                         
008900                                                                          
009000     SKIP3                                                                
009100 ENVIRONMENT DIVISION.                                                    
009200     SKIP2                                                                
009300 INPUT-OUTPUT SECTION.                                                    
009400                                                                          
009500 FILE-CONTROL.                                                            
009600     EJECT                                                                
009700 DATA DIVISION.                                                           
009800     SKIP2                                                                
009900 FILE SECTION.                                                            
010000     EJECT                                                                
010100 WORKING-STORAGE SECTION.                                                 
010200     SKIP2                                                                
010300*    -COPY WY2000W1                                                       
010400     SKIP2                                                                
010500*    -COPY WY2000W3                                                       
010600     SKIP2                                                                
010700*    -COPY WY2000W9                                                       
010800     SKIP3                                                                
010900 77  IDPGM                       PIC X(8)    VALUE 'W222BHDC'.            
011000 77  JA                          PIC X       VALUE 'J'.                   
011100 77  NEJ                         PIC X       VALUE 'N'.                   
011200 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
011300 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
011400     SKIP1                                                                
011500 77  ANTAL-SEASONINDEX         PIC S9(9)   VALUE +12   COMP SYNC.         
011600 77  JUST-PB-FINNS-SW          PIC X       VALUE 'J'.                     
011700 77  SEP-SEASON-FINNS-SW       PIC X       VALUE 'N'.                     
011800 77  CDCBEHOV-SW               PIC X       VALUE 'N'.                     
011900 77  PERIOD-ANTAL              PIC S9(9)   VALUE +12   COMP-3.            
012000 77  MAX-BEHOVS-VECKOR         PIC S9(9)   VALUE +156  COMP SYNC.         
012100 77  TREND-SW                  PIC X       VALUE 'N'.                     
012200 77  WS-ERS-FINNS-K611         PIC X       VALUE 'N'.                     
012300 77  WS-TIERSDAT-PREL-AAVVD    PIC 9(5)    VALUE ZERO.                    
012400 77  WS-TIERSDAT-PREL-AAVV     PIC 9(4)    VALUE ZERO.                    
012500 77  CDC-ERSDAT-PREL-OK-SW     PIC X       VALUE 'J'.                     
012600 77  6ARBDAG-SW                PIC X       VALUE 'N'.                     
012700 77  WS-SATS-TPO-BEHOV-AAVV    PIC 9(4)    VALUE ZERO.                    
012800 77  WS-BEHOVSVECKA-AAVV       PIC 9(4)    VALUE ZERO.                    
012900 77  WS-FLFLYG                 PIC X       VALUE 'N'.                     
013000                                                                          
013100 77  WDK629-SW                 PIC X       VALUE 'N'.                     
013200     88  WDK629-FINNS                      VALUE 'J'.                     
013300     88  WDK629-SAKNAS                     VALUE 'N'.                     
013400                                                                          
013500 77  SW-DEMAND-ADJUST          PIC X       VALUE 'N'.                     
013600     88  DEMAND-ADJUST                     VALUE 'J'.                     
013700                                                                          
013800 01  FILLER                    PIC X(16)                                  
013900                                       VALUE 'SPARA LINK IDDC '.          
014000 01  WS-SPAR-FAELT.                                                       
014100     03  SPAR-XLAG-KVPB-TREND  PIC S9(6)V9(1)                             
014200                                           VALUE ZERO COMP-3.             
014300     03  SPAR-XLAG-KVVECKOR-TREND  PIC S9(3)                              
014400                                           VALUE ZERO COMP-3.             
014500     03  SPAR-XLAG-DAPBPLAN        PIC 9(8)    VALUE ZERO.                
014600     03  SPAR-XLAG-DASEASON        PIC 9(8)    VALUE ZERO.                
014700     03  SPAR-XLAG-KVPB-PLAN       PIC S9(6)V9(1)                         
014800                                           VALUE ZERO COMP-3.             
014900     03  SPAR-XLAG-RESEASON-PLAN   OCCURS 12                              
015000                                   PIC S9V9(2) VALUE ZERO COMP-3.         
015100                                                                          
015200*---                                                                      
015300 01  FILLER                    PIC X(16) VALUE 'KDBEHOV W222BHDC'.        
015400 01  KDBEHOV-W222BHDC.                                                    
015500     03  ENDAST-SEPARATBEHOV   PIC  X(2)   VALUE '01'.                    
015600     03  ENDAST-XDCBEHOV       PIC  X(2)   VALUE '02'.                    
015700     03  PB-TOTAL-SEP-LEV-XDC  PIC  X(2)   VALUE '03'.                    
015800     03  ENDAST-CDCBEHOV       PIC  X(2)   VALUE '04'.                    
015900     03  XDC-CDC-BEHOV         PIC  X(2)   VALUE '05'.                    
016000     03  ENDAST-GLOBALBEHOV    PIC  X(2)   VALUE '06'.                    
016100     03  ENDAST-LOKALBEHOV     PIC  X(2)   VALUE '07'.                    
016200     EJECT                                                                
016300*                                                                         
016400 01  FILLER                    PIC X(16) VALUE 'KDBEHOV W2222200'.        
016500 01  KDBEHOV-W2222200.                                                    
016600     03  SEP-SATS-TPO-LEV-SDC-NDC   PIC  X(2)   VALUE '19'.               
016700     EJECT                                                                
016800                                                                          
016900 01  FILLER                    PIC X(16) VALUE 'WS-JUST-SEAS-TAB'.        
017000 01  WS-JUST-RESEASON-TAB.                                                
017100     03  WS-JUST-RESEASON  OCCURS 12                                      
017200                               PIC S9V9(2) VALUE ZERO  COMP-3.            
017300                                                                          
017400     EJECT                                                                
017500 01  DYNAMISKA-SUBPROGRAM.                                                
017600*                                                                         
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
018100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018200     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
018300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
018400     03  W271LTPB                PIC X(8)    VALUE 'W271LTPB'.            
018500     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
018600     03  W272REFL                PIC X(8)    VALUE 'W272REFL'.            
018700     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
018800     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
018900     03  W22222                  PIC X(8)    VALUE 'W22222'.              
019000     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
019100     SKIP2                                                                
019200*    --- PARAMETRAR TILL ABEND                                            
019300                                                                          
019400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
019500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019700     SKIP2                                                                
019800 01  FILLER                  PIC X(16) VALUE 'WDATAREA        '.          
019900*    ---PARAMETRAR TILL DATKONV                                           
020000*01  -COPY WDATAREA                                                       
020100     EJECT                                                                
020200 01  FILLER                  PIC X(16) VALUE 'WDAGAREA        '.          
020300*    --- PARAMETRAR TILL DAGKONV                                          
020400*01  -COPY WDAGAREA                                                       
020500     EJECT                                                                
020600 01  FILLER                  PIC X(16) VALUE 'WORKAREA        '.          
020700*    ---PARAMETRAR TILL WORKDAY                                           
020800*01  -COPY WORKAREA                                                       
020900     EJECT                                                                
021000 01  FILLER                  PIC X(16) VALUE 'WZ20DAYS        '.          
021100*    ---PARAMETRAR TILL WZ20DAYS                                          
021200*01  -COPY WZ20DAYS                                                       
021300     EJECT                                                                
021400 01  FILLER                  PIC X(16) VALUE 'W271REFL        '.          
021500*    ---PARAMETRAR TILL W271REFL                                          
021600*01  -COPY W271REFL    -PRE REFL1-                                        
021700     EJECT                                                                
021800 01  FILLER                  PIC X(16) VALUE 'W272REFL        '.          
021900*    ---PARAMETRAR TILL W272REFL                                          
022000*01  -COPY W272REFL                                                       
022100     EJECT                                                                
022200 01  FILLER                  PIC X(16) VALUE 'W271UTIL        '.          
022300*    ---PARAMETRAR TILL W271UTIL                                          
022400*01 -COPY W271UTIL                                                        
022500     EJECT                                                                
022600*    --- PARAMETRAR TILL W271UTUP                                         
022700*01 -COPY W271UTUP                                                        
022800     EJECT                                                                
022900 01  FILLER                  PIC X(16) VALUE 'W2222200        '.          
023000*    ---PARAMETRAR TILL W2222200                                          
023100*01  AREA  -COPY W222L222   -PRE L222-.                                   
023200     EJECT                                                                
023300 01  FILLER                  PIC X(16) VALUE 'W271LTPB        '.          
023400*    ---PARAMETRAR TILL W271LTPB                                          
023500*01  -COPY W271LTPB                                                       
023600     EJECT                                                                
023700*    ---KONSTANTER FÖR ALLA VALIDA DC                                     
023800*01  -COPY WWDCKONS                                                       
023900     EJECT                                                                
024000* VARIABLES TO SUBPROGRAM W009VADD                                        
024100 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
024200 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
024300     EJECT                                                                
024400*    ---VALID IDDC CODES                                                  
024500*01  -COPY WWDC99 -PRE LOC-                                               
024600*01  -COPY WWDC99 -PRE REF-                                               
024700     EJECT                                                                
024800                                                                          
024900 01  FELTEXT.                                                             
025000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
025100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
025200     EJECT                                                                
025300 01  FILLER                  PIC X(16) VALUE 'ARBETSAREOR     '.          
025400 01  ARBETSAREOR.                                                         
025500     SKIP1                                                                
025600*                                                                         
025700*-- PER-TABELL ÄR EN RULLANDE TABELL DÄR                                  
025800*-- IX = 1  ÄR JANUARI                                                    
025900*-- IX = 12 ÄR DECEMBER                                                   
026000*                                                                         
026100     03 PER-TABELL OCCURS 12.                                             
026200        05 PER-PERIOD            PIC  9(2)   VALUE ZERO.                  
026300        05 PER-START-VV          PIC  9(2)   VALUE ZERO.                  
026400        05 PER-SLUT-VV           PIC  9(2)   VALUE ZERO.                  
026500     SKIP3                                                                
026600*                                                                         
026700*   DAPUBL-TABELL                                                         
026800*   LADDAS FRÅN WDK712 FÖR GIVET LINK-IDARTNR                             
026900*                                                                         
027000     03 DAPUBL-TAB.                                                       
027100        05 DAPUBL-TABELL OCCURS 100.                                      
027200           07 PUBL-IDLANDX2      PIC  X(2)   VALUE SPACE.                 
027300           07 PUBL-DAPUBL        PIC  9(8)   VALUE ZERO.                  
027400           07 PUBL-PRMATRL       PIC  S9(7)V9(2)  COMP-3.                 
027500           07 PUBL-KVQPACK-3     PIC S9(5)   VALUE ZERO COMP-3.           
027600     03 PUBL-IX                  PIC  9(3)   VALUE ZERO.                  
027700     03 PUBL-IX-MAX              PIC  9(3)   VALUE 100.                   
027800     SKIP3                                                                
027900     03  IX                  PIC S9(9)               COMP SYNC.           
028000     03  IX-PER              PIC S9(9)               COMP SYNC.           
028100     03  IX-FRAN             PIC S9(9)               COMP-3               
028200                                                    VALUE ZERO.           
028300     03  IX-TILL             PIC S9(9)               COMP-3               
028400                                                    VALUE ZERO.           
028500     03  IX-VECKA            PIC S9(9)               COMP-3               
028600                                                    VALUE ZERO.           
028700     03  INDX-T              PIC S9(4)   VALUE +0    COMP SYNC.           
028800     03  IY                  PIC S9(9)               COMP SYNC.           
028900     03  INDX-L              PIC S9(9)   VALUE +0    COMP SYNC.           
029000     03  INDX1-L             PIC S9(9)   VALUE +0    COMP SYNC.           
029100     03  INDX1-L-MAX         PIC S9(9)   VALUE +53   COMP SYNC.           
029200     03  INDX1-LT-MAX        PIC S9(9)   VALUE +157  COMP SYNC.           
029300     SKIP1                                                                
029400     03  W-DATUM-AAVV        PIC S9(5)               COMP-3.              
029500     03  W-AAVV              PIC S9(5)               COMP-3.              
029600     03  W-AAVVD             PIC S9(5)              VALUE ZERO.           
029700     03  W-TIAAVV            PIC 9(4)               VALUE ZERO.           
029800     03  W-TIAAVVD           PIC 9(5)               VALUE ZERO.           
029900     03  W-RED-NUM-4         PIC 9(4)               VALUE ZERO.           
030000                                                                          
030100     03  W-DATUM-FORSTA-NDC  PIC S9(5)               COMP-3.              
030200     03  W-DATUM-FORSTA-GDC  PIC S9(5)               COMP-3.              
030300     03  W-DATUM-FORSTA-CDC  PIC S9(5)               COMP-3.              
030400     03  W-TIFINLV-AAVV-MINUS-2  PIC S9(5)           COMP-3.              
030500     03  W-TIFINLV-AAVV-MINUS-LT PIC S9(5)           COMP-3.              
030600     03  WS-LTID                 PIC S9(5)           COMP-3.              
030700     03  WS-LTID-A               PIC S9(5)           COMP-3.              
030800     03  WS-LTID-B               PIC S9(5)           COMP-3.              
030900     03  WS-LT-WEEKS             PIC S9(3)           COMP-3.              
031000     03  WS-LT-WEEKS-DAYS        PIC S9(3)           COMP-3.              
031100     03  WS-REMN-DAYS            PIC  9(2) VALUE ZERO.                    
031200                                                                          
031300     03  W-DATUM             PIC 9(4).                                    
031400     03  W-DAT             REDEFINES W-DATUM.                             
031500         05  W-DATUM-AA      PIC 9(2).                                    
031600         05  W-DATUM-VV      PIC 9(2).                                    
031700                                                                          
031800     03  WS-START-AAVV       PIC 9(4).                                    
031900     03  WS-START2-AAVV    REDEFINES WS-START-AAVV.                       
032000         05  WS-START-AA     PIC 9(2).                                    
032100         05  WS-START-VV      PIC 9(2).                                   
032200                                                                          
032300     SKIP1                                                                
032400     03  FILLER              PIC X(16) VALUE 'W-ANTAL-VECKOR  '.          
032500     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
032600     03  W-PER-ANT           PIC S9(3)               COMP-3.              
032700                                                                          
032800     SKIP1                                                                
032900     03  W-BER-START-AA      PIC S9(3)               COMP-3.              
033000     03  W-BER-START-VV      PIC S9(3)               COMP-3.              
033100     03  W-BER-SLUT-DATUM    PIC S9(5)               COMP-3.              
033200     03  W-BER-START-DATUM   PIC S9(5)               COMP-3.              
033300     03  W-BER-DATUM         PIC S9(5)               COMP-3.              
033400     03  W-BER-DATUM-AAVV    PIC 9(4)            VALUE ZERO.              
033500     03  WS-DAGENS-DATUM-AAVV                                             
033600                             PIC 9(4)            VALUE ZERO.              
033700     03  WS-DAGENS-DATUM-AAMMDD                                           
033800                             PIC 9(6)            VALUE ZERO.              
033900     03  WS-DAG-ONE-AAMMDD   PIC 9(6)            VALUE ZERO.              
034000     03  FILLER REDEFINES  WS-DAG-ONE-AAMMDD.                             
034100         05 WS-DAG-ONE-AAMM  PIC 9(4).                                    
034200         05 WS-DAG-ONE-DD    PIC 9(2).                                    
034300                                                                          
034400                                                                          
034500     SKIP1                                                                
034600     03  W-KVPB-SEP          PIC S9(6)V9(3)          COMP-3.              
034700     03  W-KVPB-REF-SUM      PIC S9(6)V9(3)          COMP-3.              
034800     03  W-KVPB-JUST1        PIC S9(6)V9(3)          COMP-3.              
034900     03  W-KVPB-JUST2        PIC S9(6)V9(3)          COMP-3.              
035000     03  W-KVBEHOV-SUMMA     PIC S9(7)V9(2)          COMP-3.              
035100     03  W-TIFINLV-AAVV      PIC S9(5)               COMP-3.              
035200     03  W-PER               PIC 9(2)                COMP-3.              
035300                                                                          
035400     SKIP1                                                                
035500******----------------------------------------------------------          
035600****** AREOR FÖR BERÄKNING AV REFILL BEHOV TILL LDC,NDC OCH CDC.          
035700******----------------------------------------------------------          
035800     03  FILLER              PIC X(16) VALUE 'W-TIME          '.          
035900     03  W-TIME                   PIC 9(8)   VALUE ZERO.                  
036000*-----                                                                    
036100     03  W-KVBEHOV-INNEV-VECKA    PIC S9(6)V9  VALUE ZERO COMP-3.         
036200     03  W-KVBEHOV-52V            PIC S9(6)V9  VALUE ZERO COMP-3.         
036300     03  W-DC-REFILL-KVANT        PIC S9(7)    VALUE ZERO COMP-3.         
036400     03  W-DAGAR-KVAR           PIC S9(1)      VALUE ZERO COMP-3.         
036500     03  W-VECKODEL             PIC S9(1)V9(2) VALUE ZERO COMP-3.         
036600*-----                                                                    
036700     03  W-NDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
036800     03  W-NDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
036900     03  W-NDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
037000     03  W-NDC-TILLGANG           PIC S9(7)V9  VALUE ZERO COMP-3.         
037100     03  W-NDC-TILLGANG-ERS       PIC S9(7)V9  VALUE ZERO COMP-3.         
037200     03  W-NDC-KVAR-EFT-VECKA     PIC S9(7)V9  VALUE ZERO COMP-3.         
037300     03  W-NDC-KVAR-EFT-DAG       PIC S9(7)V9  VALUE ZERO COMP-3.         
037400     03  W-NDC-KVBEHOV-DESSUTOM   PIC S9(7)V9  VALUE ZERO COMP-3.         
037500     03  W-NDC-DIFF               PIC S9(7)V9  VALUE ZERO COMP-3.         
037600     03  W-NDC-ACC-KVBEHOV-VECKA  PIC S9(7)V9  VALUE ZERO COMP-3.         
037700     03  W-NDC-KVAR-KVBEHOV-VECKA PIC S9(7)V9  VALUE ZERO COMP-3.         
037800*-----                                                                    
037900*----- GLOBALA EXPORT DC'N SOM REFILLAS FRÅN NDC TILL NDC.                
038000*-----                                                                    
038100     03  W-GDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
038200     03  W-GDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
038300     03  W-GDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
038400     03  W-GDC-TILLGANG           PIC S9(7)V9  VALUE ZERO COMP-3.         
038500     03  W-GDC-TILLGANG-ERS       PIC S9(7)V9  VALUE ZERO COMP-3.         
038600     03  W-GDC-KVAR-EFT-VECKA     PIC S9(7)V9  VALUE ZERO COMP-3.         
038700     03  W-GDC-KVAR-EFT-DAG       PIC S9(7)V9  VALUE ZERO COMP-3.         
038800     03  W-GDC-KVBEHOV-DESSUTOM   PIC S9(7)V9  VALUE ZERO COMP-3.         
038900     03  W-GDC-DIFF               PIC S9(7)V9  VALUE ZERO COMP-3.         
039000     03  W-GDC-ACC-KVBEHOV-VECKA  PIC S9(7)V9  VALUE ZERO COMP-3.         
039100     03  W-GDC-KVAR-KVBEHOV-VECKA PIC S9(7)V9  VALUE ZERO COMP-3.         
039200*-----                                                                    
039300*----- CDC-BEHOV VID REFILL FRÅN NDC'T TILL CDC.KINA,USA....              
039400*-----                                                                    
039500     03  W-CDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
039600     03  W-CDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
039700     03  W-CDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
039800     03  W-CDC-TILLGANG           PIC S9(7)    VALUE ZERO COMP-3.         
039900     03  W-CDC-TILLGANG-REST      PIC S9(7)    VALUE ZERO COMP-3.         
040000     03  W-CDC-KVAR-EFT-VECKA     PIC S9(7)V9  VALUE ZERO COMP-3.         
040100     03  W-CDC-KVAR-EFT-DAG       PIC S9(7)V9  VALUE ZERO COMP-3.         
040200     03  W-CDC-KVBEHOV-DESSUTOM   PIC S9(7)V9  VALUE ZERO COMP-3.         
040300     03  W-CDC-DIFF               PIC S9(7)V9  VALUE ZERO COMP-3.         
040400     03  W-CDC-ACC-KVBEHOV-VECKA  PIC S9(7)V9  VALUE ZERO COMP-3.         
040500     03  W-CDC-KVAR-KVBEHOV-VECKA PIC S9(7)V9  VALUE ZERO COMP-3.         
040600                                                                          
040700     03  W-CDC-KVPB-PLAN          PIC S9(6)V9(1)                          
040800                                               VALUE ZERO COMP-3.         
040900                                                                          
041000*-----                                                                    
041100     03  W-JUST-PB-FINNS          PIC X        VALUE 'N'.                 
041200     03  W-CDC-JUST-PB-FINNS      PIC X        VALUE 'N'.                 
041300     03  W-CDC-KVPB-JUST OCCURS 2                                         
041400                                PIC S9(6)V9(1) VALUE ZERO COMP-3.         
041500                                                                          
041600     03  W-CDC-TIPBJUST  OCCURS 2                                         
041700                                  PIC S9(5)    VALUE ZERO COMP-3.         
041800                                                                          
041900     03  WS-CDC-TIPBJUST          PIC 9(4)     VALUE ZERO.                
042000                                                                          
042100*-----                                                                    
042200                                                                          
042300     03  SPAR-KVBEHOV-VECKA       PIC S9(7)V9(2)                          
042400                                               VALUE ZERO COMP-3.         
042500     03  SPAR-KVBEHOV-DESSUTOM    PIC S9(7)V9(2)                          
042600                                               VALUE ZERO COMP-3.         
042700     03  W-BINNDAY                PIC 9(6)     VALUE ZERO.                
042800     03  W-BINNDAY-AAVV           PIC S9(5)    VALUE ZERO COMP-3.         
042900     03  WS-FIXA-AAR              PIC 9(2)     VALUE ZERO.                
043000                                                                          
043100*-----                                                                    
043200     03  WS-LART-KVQPACK-3        PIC S9(5)     VALUE ZERO COMP-3.        
043300     03  WS-KVQPACK-3             PIC S9(5)     VALUE ZERO COMP-3.        
043400     03  WS-QX-BRYTNING           PIC 9(2)      VALUE ZERO.               
043500     03  WS-KVANT-QX              PIC 9(5)V9(2) VALUE ZERO.               
043600     03  WS-KVANT-QX-DELAR    REDEFINES WS-KVANT-QX.                      
043700         05 WS-KVANT-QX-HELTAL PIC 9(5).                                  
043800         05 WS-KVANT-QX-DECTAL PIC 9(2).                                  
043900     03  WS-ANTAL-QX              PIC S9(7)  VALUE ZERO COMP-3.           
044000*-----                                                                    
044100                                                                          
044200     03  WS-KVPB-TREND            PIC S9(6)V9.                            
044300     03  MAX-KVVECKOR-TREND       PIC S9(3)  VALUE ZERO.                  
044400                                                                          
044500     03  WS-TIAAPER.                                                      
044600         05 TIAA                  PIC 9(2)   VALUE ZERO.                  
044700         05 PER                   PIC 9(2)   VALUE ZERO.                  
044800     03  TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                           
044900                                                                          
045000     03  WS-BER-IY           PIC S9(9)   VALUE ZERO  COMP SYNC.           
045100     03  WS-BER-IX           PIC S9(9)   VALUE ZERO  COMP SYNC.           
045200     03  WS-BER-TAB-NOLL.                                                 
045300         05  FILLER          OCCURS 4.                                    
045400             07  FILLER      OCCURS 52.                                   
045500                 09  FILLER                                               
045600                             PIC S9(6)V9(3)                               
045700                                         VALUE ZERO  COMP-3.              
045800                                                                          
045900     SKIP1                                                                
046000     03  FILLER              PIC X(16) VALUE 'WS-CURRENT-DATE '.          
046100     03  WS-CURRENT-DATE.                                                 
046200         05  WS-DAGENS-TIAAAA    PIC 9(4)    VALUE ZERO.                  
046300         05  FILLER              PIC 9(4)    VALUE ZERO.                  
046400         05  FILLER              PIC 9(6)    VALUE ZERO.                  
046500     03  FILLER REDEFINES WS-CURRENT-DATE.                                
046600*-----   INKLUSIVE SEKEL                                                  
046700         05  WS-DAGENS-DATUM     PIC 9(8).                                
046800         05  WS-DAGENS-TID.                                               
046900             07 WS-DAGENS-TIMME  PIC 9(2).                                
047000             07 WS-DAGENS-MINUT  PIC 9(2).                                
047100             07 WS-DAGENS-SEKUND PIC 9(2).                                
047200                                                                          
047300     03  WS-DAGENS-VECKA         PIC 9(2)    VALUE ZERO.                  
047400     03  WS-DAGENS-AAVVD         PIC 9(5)    VALUE ZERO.                  
047500     03  WS-DAGENS-DAGNR         PIC 9       VALUE ZERO.                  
047600     03  WS-DAPUBL               PIC 9(8)    VALUE ZERO.                  
047700     03  WS-DAPUBL-AAVV          PIC 9(4)    VALUE ZERO.                  
047800     03  WS-CALL-KVVECKOR-BEHOV  PIC S9(3)   VALUE ZERO  COMP-3.          
047900     03  WS-KVPB-PLAN-VECKA      PIC S9(6)V9(1)                           
048000                                             VALUE ZERO  COMP-3.          
048100     03  WS-ARSTOTAL             PIC S9(9)V9(2)                           
048200                                             COMP-3  VALUE ZERO.          
048300     03  WS-KVPB                 PIC S9(9)V9(2)                           
048400                                             COMP-3  VALUE ZERO.          
048500     03  WS-KVBEHOV-PER          PIC S9(7)V9(2)                           
048600                                             COMP-3  VALUE ZERO.          
048700*--TEST                                                                   
048800     03  WS-KVBEHOV-PER-TEST     OCCURS 12                                
048900                                 PIC S9(7)V9(2)                           
049000                                             COMP-3  VALUE ZERO.          
049100                                                                          
049200     03  WS-JUSTERA              PIC S9V9(2) COMP-3  VALUE ZERO.          
049300     03  WS-RESEASON-TOT         PIC S9(2)V9(2)                           
049400                                             COMP-3  VALUE ZERO.          
049500                                                                          
049600****                                                                      
049700**** LEADTIME CALCULATION FOR DEMAND FROM W2222200                        
049800     03  WS-LEADTIME-WEEKS       PIC 9(3)     VALUE ZERO.                 
049900     03  WS-LEADTIME-WEEKS-PLUS1 PIC 9(3)     VALUE ZERO.                 
050000     03  WS-LEADTIME-WEEKS-VECKA PIC 9(3)     VALUE ZERO.                 
050100     03  WS-CALC-AAVV            PIC 9(4)     VALUE ZERO.                 
050200     03  WS-REST-DAYS            PIC 9(2)     VALUE ZERO.                 
050300     03  WS-REST-DAYS-LT-F       PIC 9(2)     VALUE ZERO.                 
050400     03  WS-REST-DAYS-LT-V       PIC 9(2)     VALUE ZERO.                 
050500     03  WS-REST-DAYS-ADJ        PIC 9(2)     VALUE ZERO.                 
050600     03  WS-TIVV-TEMP            PIC 9(2)     VALUE ZERO.                 
050700     03  WS-TIAAVV-L222          PIC 9(4)     VALUE ZERO.                 
050800     03  WS-LTDATE-AAMMDD        PIC 9(6)     VALUE ZERO.                 
050900     03  WS-LTDATE-VECKA-AAMMDD  PIC 9(6)     VALUE ZERO.                 
051000     03  WS-TIAAVV-PLUS1         PIC 9(4)     VALUE ZERO.                 
051100     03  WS-TIAAVVD-PLUS1        PIC 9(4)     VALUE ZERO.                 
051200     03  WS-ANTAL-DAG-LT         PIC 9(3)     VALUE ZERO.                 
051300     03  WS-DAT-TID              PIC 9        VALUE ZERO.                 
051400     03  WS-DAT-TID-VECKA        PIC 9        VALUE ZERO.                 
051500     03  WS-NONEED-DAYS          PIC 9        VALUE ZERO.                 
051600     03  WS-LAST-MINUS-NEED      PIC S9(7)V9  VALUE ZERO COMP-3.          
051700     03  WS-DAY-NEED-LAST        PIC S9(7)V9  VALUE ZERO COMP-3.          
051800     03  WS-KVDAGAR-KVAR         PIC 9(3)     VALUE ZERO COMP-3.          
051900     03  WS-VECKO-SEP-BEHOV      PIC S9(7)V9(2)                           
052000                                              VALUE ZERO COMP-3.          
052100     03  WS-DAG-SEP-BEHOV        PIC S9(7)V9(2)                           
052200                                              VALUE ZERO COMP-3.          
052300     03  WS-KVPB-DESSUTOM        PIC S9(8)V9(2)                           
052400                                          VALUE ZERO COMP-3.              
052500     03  WS-FAKTOR               PIC S9(1)V9(3)          COMP-3.          
052600                                                                          
052700****                                                                      
052800****                                                                      
052900**** PERIODERNA 1,2,3,4,6,7,8 INNEHÅLLER 6 VECKOR                         
053000**** PERIOD 5 INNEHÅLLER 10 VECKOR (25-34)                                
053100****                                                                      
053200     03 FILLER                PIC X(16) VALUE 'WS-KVBEHOV-TAB  '.         
053300     03 WS-KVBEHOV-TABELL.                                                
053400        05 WS-KVBEHOV-VECKA         OCCURS 52                             
053500                              PIC S9(7)V9(2) COMP-3  VALUE ZERO.          
053600                                                                          
053700     03 FILLER                PIC X(16) VALUE 'WS-RESEASON-TAB '.         
053800     03 WS-RESEASON-TABELL.                                               
053900        05 WS-RESEASON              OCCURS 12                             
054000                              PIC S9(2)V9(4) COMP-3  VALUE ZERO.          
054100                                                                          
054200     03 FILLER                PIC X(16) VALUE 'WS-RESEASON-AVR '.         
054300     03 WS-RESEASON-AVR-TABELL.                                           
054400        05 WS-RESEASON-AVR          OCCURS 12                             
054500                              PIC S9(2)V9(2) COMP-3  VALUE ZERO.          
054600                                                                          
054700     03 WS-NOLLA-KVBEHOV.                                                 
054800        05 FILLER                   OCCURS 52                             
054900                              PIC S9(7)V9(2) COMP-3  VALUE ZERO.          
055000                                                                          
055100     03 WS-NOLLA-RESEASON.                                                
055200        05 FILLER                   OCCURS 12                             
055300                              PIC S9(2)V9(4) COMP-3  VALUE ZERO.          
055400                                                                          
055500     03 WS-NOLLA-RESEASON-AVR.                                            
055600       05 FILLER                    OCCURS 12                             
055700                              PIC S9(2)V9(2) COMP-3  VALUE ZERO.          
055800     SKIP3                                                                
055900                                                                          
056000********************************************************                  
056100*                                                      *                  
056200*   W271REF2                                           *                  
056300*                                                      *                  
056400*   SUBPROGRAM W271REF2 INLAGD I W222BHDC FÖR ATT      *                  
056500*   TJÄNA TID OCH PENGAR (PGA TIDSÖDANDE CALL ANROP)   *                  
056600*   SE PROGRAM W2222200 FÖR CDC ANSKAFFNINGEN.         *                  
056700*                                                      *                  
056800********************************************************                  
056900                                                                          
057000 01  FILLER                  PIC X(16) VALUE 'ARBETSFAELT     '.          
057100 01  ARBETSFAELT.                                                         
057200*                                                                         
057300     03  DAGENS-DATUM            PIC 9(8)     VALUE ZERO.                 
057400     03  FILLER REDEFINES DAGENS-DATUM.                                   
057500         05 DAGENS-AAR           PIC 9(4).                                
057600         05 DAGENS-MAANAD        PIC 9(2).                                
057700         05 DAGENS-DAG           PIC 9(2).                                
057800     03  DAGENS-AAR-PLUS2        PIC 9(4)     VALUE ZERO.                 
057900     03  JMF-AAAA                PIC 9(4)     VALUE ZERO.                 
058000                                                                          
058100*                                                                         
058200     03  WS-SNITT-VECKOR      PIC 9(1)V9(2)   VALUE 4.33.                 
058300*                                                                         
058400     03  WS-N                 PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
058500     03  WS-NP                PIC S9(10)V9(5) VALUE ZERO COMP-3.          
058600     03  WS-UB                PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
058700     03  WS-NP-UB             PIC S9(10)V9(5) VALUE ZERO COMP-3.          
058800     03  WS-LEDTIDSBEHOV      PIC S9(7)V9(1)  VALUE ZERO COMP-3.          
058900     03  WS-TABELLBEHOV       PIC S9(6)V9(3)  VALUE ZERO COMP-3.          
059000     03  WS-PAFYLLNPKT        PIC S9(7)V9(5)  VALUE ZERO COMP-3.          
059100     03  WS-PAFYLLNKVANT      PIC S9(7)V9(5)  VALUE ZERO COMP-3.          
059200     03  WS-OVERLAGERPKT      PIC S9(7)       VALUE ZERO COMP-3.          
059300     03  WS-KVPB-REF-DAY-BINN PIC S9(6)V9(5)  VALUE ZERO COMP-3.          
059400                                                                          
059500     03  W222-BEHOV-TAB.                                                  
059600         05  W-BEHOV-LT                      OCCURS 157.                  
059700             07  W-BEHOV-LT-VECKA                                         
059800                              PIC S9(8)V9(1) COMP-3 VALUE ZERO.           
059900             07  W-BEHOV-LT-TOT                                           
060000                              PIC S9(8)V9(1) COMP-3 VALUE ZERO.           
060100                                                                          
060200                                                                          
060300     03  WS-SP-IDDC              PIC X(2)    VALUE SPACE.                 
060400                                                                          
060500*---                                                                      
060600     03  WS-PRMATRL                PIC S9(7)V9(2) VALUE ZERO              
060700                                                   COMP-3.                
060800     03  WS-PRARTBES               PIC S9(7)V9(2) VALUE ZERO              
060900                                                   COMP-3.                
061000     03  WS-REFPB-TOT              PIC S9(6)V9(1) VALUE ZERO              
061100                                                   COMP-3.                
061200     03  WS-KVPB-TOT-REF2          PIC S9(6)V9(1) VALUE ZERO              
061300                                                   COMP-3.                
061400*** TA BORT WS-KVPB-REF, WS-KVPBREOI?                                     
061500     03  WS-KVPB-REF               PIC S9(6)V9(3) VALUE ZERO              
061600                                                   COMP-3.                
061700     03  WS-KVPBREOI               PIC S9(6)V9(1) VALUE ZERO              
061800                                                   COMP-3.                
061900     03  WS-KVPBREOI-DAY           PIC S9(6)V9(5) VALUE ZERO              
062000                                                   COMP-3.                
062100     03  WS-KVPB-REF-DAY-PER-I     PIC S9(6)V9(5) VALUE ZERO              
062200                                                   COMP-3.                
062300     03  WS-KVPB-REF-DAY-PER-II    PIC S9(6)V9(5) VALUE ZERO              
062400                                                   COMP-3.                
062500     03  WS-KVPB-REF-DAY-PER-III   PIC S9(6)V9(5) VALUE ZERO              
062600                                                   COMP-3.                
062700     03  WS-KVPB-REF-DAY-PER-IV    PIC S9(6)V9(5) VALUE ZERO              
062800                                                   COMP-3.                
062900     03  WS-KVPB-REF-DAY-PER-V     PIC S9(6)V9(5) VALUE ZERO              
063000                                                   COMP-3.                
063100     03  WS-KVPB-REF-DAY-PER-VI    PIC S9(6)V9(5) VALUE ZERO              
063200                                                   COMP-3.                
063300     03  WS-KVREFLIM               PIC S9(5)      VALUE ZERO              
063400                                                   COMP-3.                
063500     03  WS-KVREFKVA               PIC S9(5)      VALUE ZERO              
063600                                                   COMP-3.                
063700     03  WS-KVARBDAG               PIC 9(3)    VALUE ZERO.                
063800     03  WS-BINNDAY-TIAAMMDD       PIC 9(6)    VALUE ZERO.                
063900     03  WS-TIAAMMDD-FOM           PIC 9(6)    VALUE ZERO.                
064000     03  FILLER REDEFINES WS-TIAAMMDD-FOM.                                
064100         05 WS-TIAA            PIC 9(2).                                  
064200         05 WS-TIMM            PIC 9(2).                                  
064300         05 WS-TIDD            PIC 9(2).                                  
064400                                                                          
064500                                                                          
064600     03  WS-RADIX                  PIC 9(2)    VALUE ZERO.                
064700     03  WS-PR-KOLIX               PIC 9(2)    VALUE ZERO.                
064800     03  PER-IX                    PIC 9(2)    VALUE ZERO.                
064900     03  TAB-KVARBDAG              PIC 9(3)    VALUE ZERO                 
065000                                   OCCURS 12.                             
065100                                                                          
065200     03  WS-SP-GRUNDTABELL-DC    PIC X(2)  VALUE SPACE.                   
065300                                                                          
065400     03  SPARAREOR.                                                       
065500                                                                          
065600         05  SPARAREA-AKTUELL-TABELL.                                     
065700*            07 -COPY WDGX2502 -PRE AKTTAB-                               
065800                                                                          
065900         05  SPARAREA-GRUNDTABELL.                                        
066000*            07 -COPY WDGX2502 -PRE GRUNDTAB-                             
066100                                                                          
066200     03  TE-TABELL-SAKNAS.                                                
066300         05 TABELL-SAKNAS-TEXT    PIC X(22)                               
066400                           VALUE 'TABELL SAKNAS FÖR DC '.                 
066500         05 TABELL-SAKNAS-DC PIC X(2).                                    
066600         05 FILLER           PIC X(11)  VALUE ' I W222BHDC'.              
066700                                                                          
066800                                                                          
066900 01  FILLER                    PIC X(24)  VALUE 'SWITCHAR'.               
067000                                                                          
067100 77  GRUNDTABELL-SW              PIC X       VALUE 'N'.                   
067200     88  GRUNDTABELL                         VALUE 'J'.                   
067300                                                                          
067400                                                                          
067500     SKIP3                                                                
067600****************************************** BERAKNINGSTABELL               
067700*                                          ARTKIKELBEHOV PER VECKA        
067800*                                          UNDER MAX 4ÅR                  
067900 01  FILLER                  PIC X(16) VALUE 'BERAKNINGS-TAB  '.          
068000 01  BERAKNINGS-TABELL.                                                   
068100     03  BER-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
068200     03  BER-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
068300     03  CDC-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
068400     03  CDC-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
068500     03  BER-MAX-VV          PIC S9(9)   VALUE +52   COMP SYNC.           
068600     03  BER-MAX-AA          PIC S9(9)   VALUE +4    COMP SYNC.           
068700     03  BER-ANTAL-VV        PIC S9(9)   VALUE ZERO  COMP SYNC.           
068800     03  BER-ANTAL-AA        PIC S9(9)   VALUE ZERO  COMP SYNC.           
068900     03  FILLER              PIC X(16) VALUE 'BER-TAB         '.          
069000     03  BER-TAB.                                                         
069100         05  BER-AA          OCCURS 4.                                    
069200             07  BER-VV      OCCURS 52.                                   
069300                 09  BER-BEHOV                                            
069400                             PIC S9(6)V9(3)                               
069500                                         VALUE ZERO  COMP-3.              
069600     SKIP3                                                                
069700****************************************** RESULTAT-TABELL *****          
069800*                                          TOTALT ARTIKELBEHOV            
069900*                                          PER VECKA MAX 4 ÅR             
070000 01  FILLER                  PIC X(16) VALUE 'RESULTAT-TABELL '.          
070100 01  RESULTAT-TABELL.                                                     
070200     03  RES-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
070300     03  RES-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
070400     03  RES-MAX-VV          PIC S9(9)   VALUE +52   COMP SYNC.           
070500     03  RES-MAX-AA          PIC S9(9)   VALUE +4    COMP SYNC.           
070600     03  RES-ANTAL-VV        PIC S9(9)   VALUE ZERO  COMP SYNC.           
070700     03  RES-ANTAL-AA        PIC S9(9)   VALUE ZERO  COMP SYNC.           
070800     03  FILLER              PIC X(16) VALUE 'RES-TAB         '.          
070900     03  RES-TAB.                                                         
071000         05  RES-AA          OCCURS 4.                                    
071100             07  REX-VV      OCCURS 52.                                   
071200                 09  RES-BEHOV                                            
071300                             PIC S9(6)V9(3)                               
071400                                         VALUE ZERO  COMP-3.              
071500*                                                                         
071600*                                                                         
071700****************************************************************          
071800     EJECT                                                                
071900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
072000*                                                                         
072100     EJECT                                                                
072200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
072300     SKIP3                                                                
072400 01  NYCKLAR-TILL-DLI.                                                    
072500     03  W-IDARTNR-X.                                                     
072600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
072700                                                                          
072800     03  W-IDARTNR-ERS-X.                                                 
072900         05  W-IDARTNR-ERS       PIC S9(9)   VALUE ZERO COMP-3.           
073000                                                                          
073100     03  W-IDARTNR-K7-ERS-X.                                              
073200         05  W-IDARTNR-K7-ERS    PIC S9(9)   VALUE ZERO COMP-3.           
073300                                                                          
073400     03  W-KDERS-0-X.                                                     
073500         05  W-KDERS-0           PIC S9(3)   VALUE ZERO COMP-3.           
073600                                                                          
073700     03  W-IDDC-X.                                                        
073800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
073900                                                                          
074000     03  W-IDDC-ERS-X.                                                    
074100         05  W-IDDC-ERS          PIC X(2)    VALUE SPACE.                 
074200                                                                          
074300     03  W-IDDC-B6-X.                                                     
074400         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
074500                                                                          
074600     03  W-IDDC-B616-X.                                                   
074700         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
074800                                                                          
074900     03  W-IDDC-REF-X.                                                    
075000         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
075100                                                                          
075200     03  W-IDDC-NON-X.                                                    
075300         05  W-IDDC-NON          PIC X(1)    VALUE SPACE.                 
075400                                                                          
075500     03  W-IDDC1-X.                                                       
075600         05  W-IDDC1             PIC X(1)    VALUE SPACE.                 
075700                                                                          
075800     03  W-WDGXKEY-2501-X.                                                
075900         05  W-IDHTYP-2501       PIC X(4)    VALUE '2501'.                
076000         05  W-IDDC-2501         PIC X(2)    VALUE SPACE.                 
076100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
076200                                                                          
076300     03  W-IDREFTAB-X.                                                    
076400         05  W-IDREFTAB-2502     PIC X(1)    VALUE SPACE.                 
076500                                                                          
076600     03  W-WDD7A1KY-MIN.                                                  
076700         05  W-IDARTNR-D7-MIN    PIC S9(9)  COMP-3 VALUE ZERO.            
076800         05  FILLER              PIC S9(9)  COMP-3 VALUE ZERO.            
076900         05  FILLER              PIC S9(3)  COMP-3 VALUE ZERO.            
077000                                                                          
077100     03  W-WDD7A1KY-MAX.                                                  
077200         05  W-IDARTNR-D7-MAX    PIC S9(9)  COMP-3 VALUE ZERO.            
077300         05  FILLER        PIC S9(9)  COMP-3 VALUE +999999999.            
077400         05  FILLER        PIC S9(3)  COMP-3 VALUE +999.                  
077500                                                                          
077600                                                                          
077700 77  WS-IDREFTAB-ID              PIC X(1).                                
077800     88  VALID-IDREFTAB-ID-CN                VALUE 'A' THRU 'Z'.          
077900     88  VALID-IDREFTAB-ID-REF               VALUE '0' THRU '9'.          
078000                                                                          
078100                                                                          
078200     SKIP2                                                                
078300*    --- STATUS-KOD FRÅN IMS                                              
078400 01  STATUS-WS                   PIC XX.                                  
078500     88  SEGMENT-FINNS                       VALUE '  '.                  
078600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
078700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
078800     SKIP2                                                                
078900 01  GODK-STATUSKODER.                                                    
079000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
079100     SKIP3                                                                
079200 01  ALL-SSA.                                                             
079300     03  SSA1                    PIC X(96).                               
079400     03  SSA2                    PIC X(64).                               
079500     EJECT                                                                
079600*    --- IMS FUNKTIONSKODER                                               
079700*01  -COPY W0003                                                          
079800     EJECT                                                                
079900*    ---  DLI INPUT-OUTPUT AREA                                           
080000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
080100 01  DLI-IO-WDK601.                                                       
080200*    03  -COPY WDK601                                                     
080300     EJECT                                                                
080400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
080500 01  DLI-IO-WDK611.                                                       
080600*    03  -COPY WDK611                                                     
080700     EJECT                                                                
080800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK624'.                      
080900 01  DLI-IO-WDK624.                                                       
081000*    03  -COPY WDK624                                                     
081100     EJECT                                                                
081200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK626'.                      
081300 01  DLI-IO-WDK626.                                                       
081400*    03  -COPY WDK626                                                     
081500     EJECT                                                                
081600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
081700 01  DLI-IO-WDK629.                                                       
081800*    03  -COPY WDK629                                                     
081900     EJECT                                                                
082000 01  FILLER         PIC X(16) VALUE 'DLI-IO-K601-ERS'.                    
082100 01  DLI-IO-WDK601-ERS.                                                   
082200*    03  -COPY WDK601    -PRE ERS-                                        
082300     EJECT                                                                
082400 01  FILLER         PIC X(16) VALUE 'DLI-IO-K611-ERS'.                    
082500 01  DLI-IO-WDK611-ERS.                                                   
082600*    03  -COPY WDK611    -PRE ERS-                                        
082700     EJECT                                                                
082800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
082900 01  DLI-IO-WDK701.                                                       
083000*    03  -COPY WDK701                                                     
083100     EJECT                                                                
083200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
083300 01  DLI-IO-WDK711.                                                       
083400*    03  -COPY WDK711                                                     
083500     EJECT                                                                
083600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711-E'.                    
083700 01  DLI-IO-WDK711-ERS.                                                   
083800*    03  -COPY WDK711  -PRE ERS-                                          
083900     EJECT                                                                
084000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
084100 01  DLI-IO-WDK712.                                                       
084200*    03  -COPY WDK712                                                     
084300     EJECT                                                                
084400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
084500 01  DLI-IO-WDK722.                                                       
084600*    03  -COPY WDK722                                                     
084700     EJECT                                                                
084800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
084900 01  DLI-IO-WDB601.                                                       
085000*    03  -COPY WDB601                                                     
085100                                                                          
085200 01  FILLER               PIC X(16)   VALUE 'B601-TABELL'.                
085300 01  B601-IX              PIC S9(4)   COMP SYNC VALUE ZERO.               
085400 01  MAX-B601-IX          PIC S9(4)   COMP SYNC VALUE +90.                
085500 01  B601-TABELL.                                                         
085600     03  FILLER OCCURS 90.                                                
085700*      05  -COPY WDB601  -PRE TAB-                                        
085800     EJECT                                                                
085900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
086000 01  DLI-IO-WDB616.                                                       
086100*    03  -COPY WDB616                                                     
086200                                                                          
086300 01  FILLER               PIC X(16)   VALUE 'B616-TABELL'.                
086400 01  B616-IX              PIC S9(4)   COMP SYNC VALUE ZERO.               
086500 01  MAX-B616-IX          PIC S9(4)   COMP SYNC VALUE +180.               
086600 01  B616-TABELL.                                                         
086700     03  FILLER OCCURS 180.                                               
086800       05  TAB-IDDC       PIC XX.                                         
086900*      05  -COPY WDB616  -PRE TAB-                                        
087000     EJECT                                                                
087100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2502'.                    
087200 01  DLI-IO-WDGX2502.                                                     
087300*    03  -COPY WDGX2502                                                   
087400     EJECT                                                                
087500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7A1'.                      
087600 01  DLI-IO-WDD7A1.                                                       
087700*    03  -COPY WDD7A1                                                     
087800     EJECT                                                                
087900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD704'.                      
088000 01  DLI-IO-WDD704.                                                       
088100*    03  -COPY WDD704  -PRE D704-                                         
088200     EJECT                                                                
088300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
088400 01  DLI-IO-WDK901.                                                       
088500*    03  -COPY WDK901  -PRE WDK9-                                         
088600     EJECT                                                                
088700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK911'.                      
088800 01  DLI-IO-WDK911.                                                       
088900*    03  -COPY WDK911  -PRE WDK9-                                         
089000     EJECT                                                                
089100 LINKAGE SECTION.                                                         
089200     SKIP3                                                                
089300*01  AREA  -COPY W222BHDC   -PRE LINK-.                                   
089400     EJECT                                                                
089500*01  -COPY W0008  -PRE WDK6-                                              
089600     05  FILLER                  PIC X.                                   
089700     EJECT                                                                
089800*01  -COPY W0008  -PRE WDK7-                                              
089900     05  FILLER                  PIC X.                                   
090000     EJECT                                                                
090100*01  -COPY W0008  -PRE WDB6-                                              
090200     05  FILLER                  PIC X.                                   
090300     EJECT                                                                
090400*01  -COPY W0008  -PRE 2501-                                              
090500     05  FILLER                  PIC X.                                   
090600     EJECT                                                                
090700*01  -COPY W0008  -PRE WDD7A-                                             
090800     05  FILLER                  PIC X.                                   
090900     EJECT                                                                
091000*01  -COPY W0008  -PRE WDK7-ERS-                                          
091100     05  FILLER                  PIC X.                                   
091200     EJECT                                                                
091300*01  -COPY W0008  -PRE WDD7-                                              
091400     05  FILLER                  PIC X.                                   
091500     EJECT                                                                
091600*01  -COPY W0008  -PRE WDK9-                                              
091700     05  FILLER                  PIC X.                                   
091800     EJECT                                                                
091900 01  REFL1-2501-PCB              PIC X.                                   
092000 01  REFL1-WDB6-PCB              PIC X.                                   
092100 01  REFL1-WDK7-PCB              PIC X.                                   
092200 01  REFL1-UTIL-WDK6-PCB         PIC X.                                   
092300 01  REFL1-UTIL-WDK7-PCB         PIC X.                                   
092400 01  REFL1-UTIL-WDB6-PCB         PIC X.                                   
092500     EJECT                                                                
092600 01  REFL2-2501-PCB              PIC X.                                   
092700 01  REFL2-WDB6-PCB              PIC X.                                   
092800 01  REFL2-UTIL-WDK6-PCB         PIC X.                                   
092900 01  REFL2-UTIL-WDK7-PCB         PIC X.                                   
093000 01  REFL2-UTIL-WDB6-PCB         PIC X.                                   
093100     EJECT                                                                
093200 01  UTIL-WDK6-PCB               PIC X.                                   
093300 01  UTIL-WDK7-PCB               PIC X.                                   
093400 01  UTIL-WDB6-PCB               PIC X.                                   
093500     EJECT                                                                
093600 01  W222-WDK6-PCB               PIC X.                                   
093700 01  W222-WDK7-PCB               PIC X.                                   
093800 01  W222-ARTM-PCB               PIC X.                                   
093900 01  W222-2501-PCB               PIC X.                                   
094000 01  W222-WDB6R-PCB              PIC X.                                   
094100 01  W222-WDK7R-PCB              PIC X.                                   
094200 01  W222-WDB6-PCB               PIC X.                                   
094300 01  W222-WDD7-PCB               PIC X.                                   
094400 01  W222-WDK7E-PCB              PIC X.                                   
094500 01  W222-UTIL-WDK6-PCB          PIC X.                                   
094600 01  W222-UTIL-WDK7-PCB          PIC X.                                   
094700 01  W222-UTIL-WDB6-PCB          PIC X.                                   
094800 01  W222-UTUP-WDK7-PCB          PIC X.                                   
094900 01  W222-UTUP-WDB6-PCB          PIC X.                                   
095000 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
095100 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
095200 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
095300     EJECT                                                                
095400 01  UTUP-WDK7-PCB               PIC X.                                   
095500 01  UTUP-WDB6-PCB               PIC X.                                   
095600 01  UTUP-UTIL-WDK6-PCB          PIC X.                                   
095700 01  UTUP-UTIL-WDK7-PCB          PIC X.                                   
095800 01  UTUP-UTIL-WDB6-PCB          PIC X.                                   
095900     EJECT                                                                
096000                                                                          
096100 PROCEDURE DIVISION  USING LINK-AREA WDK6-PCB WDK7-PCB WDB6-PCB           
096200                                     2501-PCB WDD7A-PCB                   
096300                                     WDK7-ERS-PCB WDD7-PCB                
096400                                     WDK9-PCB                             
096500                                     REFL1-2501-PCB                       
096600                                     REFL1-WDB6-PCB                       
096700                                     REFL1-WDK7-PCB                       
096800                                     REFL1-UTIL-WDK6-PCB                  
096900                                     REFL1-UTIL-WDK7-PCB                  
097000                                     REFL1-UTIL-WDB6-PCB                  
097100                                     REFL2-2501-PCB                       
097200                                     REFL2-WDB6-PCB                       
097300                                     REFL2-UTIL-WDK6-PCB                  
097400                                     REFL2-UTIL-WDK7-PCB                  
097500                                     REFL2-UTIL-WDB6-PCB                  
097600                                     UTIL-WDK6-PCB  UTIL-WDK7-PCB         
097700                                     UTIL-WDB6-PCB                        
097800                                     W222-WDK6-PCB                        
097900                                     W222-WDK7-PCB  W222-ARTM-PCB         
098000                                     W222-2501-PCB  W222-WDB6R-PCB        
098100                                     W222-WDK7R-PCB W222-WDB6-PCB         
098200                                     W222-WDD7-PCB  W222-WDK7E-PCB        
098300                                     W222-UTIL-WDK6-PCB                   
098400                                     W222-UTIL-WDK7-PCB                   
098500                                     W222-UTIL-WDB6-PCB                   
098600                                     W222-UTUP-WDK7-PCB                   
098700                                     W222-UTUP-WDB6-PCB                   
098800                                     W222-UTUP-UTIL-WDK6-PCB              
098900                                     W222-UTUP-UTIL-WDK7-PCB              
099000                                     W222-UTUP-UTIL-WDB6-PCB              
099100                                     UTUP-WDK7-PCB                        
099200                                     UTUP-WDB6-PCB                        
099300                                     UTUP-UTIL-WDK6-PCB                   
099400                                     UTUP-UTIL-WDK7-PCB                   
099500                                     UTUP-UTIL-WDB6-PCB                   
099600                                     .                                    
099700 MAIN SECTION.                                                            
099800                                                                          
099900                                                                          
100000     PERFORM A-INIT                                                       
100100                                                                          
100200     MOVE LINK-IDARTNR TO W-IDARTNR                                       
100300                                                                          
100400     PERFORM IMS-GU-WDK601                                                
100500                                                                          
100600     IF SEGMENT-FINNS                                                     
100700       MOVE JA                TO LINK-FLJANEJ-ANROP                       
100800                                                                          
100900       PERFORM IMS-GNP-WDK611                                             
101000       IF SEGMENT-FINNS                                                   
101100                                                                          
101200         IF CLAG-IDDC-REF NOT = SPACE                                     
101300           PERFORM IMS-GNP-WDK629                                         
101400           IF SEGMENT-FINNS                                               
101500             MOVE JA  TO WDK629-SW                                        
101600           END-IF                                                         
101700         END-IF                                                           
101800                                                                          
101900         DIVIDE ART-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                   
102000                                                                          
102100         EVALUATE LINK-KDBEHOV                                            
102200             WHEN ENDAST-SEPARATBEHOV                                     
102300                  PERFORM B-BERAKNA-PROGNOS                               
102400                                                                          
102500             WHEN ENDAST-XDCBEHOV                                         
102600                  PERFORM C-BERAKNA-XDCBEHOV                              
102700                                                                          
102800             WHEN PB-TOTAL-SEP-LEV-XDC                                    
102900                  IF SPAR-XLAG-DAPBPLAN > WS-DAGENS-DATUM                 
103000                  OR SPAR-XLAG-DAPBPLAN = WS-DAGENS-DATUM                 
103100                                                                          
103200******    PLANERAT PB ÄR SATT SOM GÄLLER ISTÄLLET FÖR                     
103300******    DET TOTALA SEPARAT- OCH REFILLBEHOVEN.                          
103400                                                                          
103500                    IF SPAR-XLAG-DASEASON < WS-DAGENS-DATUM               
103600                                                                          
103700******    MASKINELLT BERÄKNADE SÄSONGSINDEX GÄLLER OCH                    
103800******    RÄKNAS FRAM NEDAN PÅ SAMMA SÄTT SOM W222PBTO GÖR.               
103900******    ALL KOD HAR FÅTT KOPIERATS IN I K-BER-PBTOT-SASONG              
104000******    SAMT I IB-BERAKNA-SASONG                                        
104100                                                                          
104200                      MOVE 52   TO LINK-KVVECKOR-BEHOV                    
104300                      PERFORM S07-INIT-DATUM                              
104400                      PERFORM B-BERAKNA-PROGNOS                           
104500                      PERFORM C-BERAKNA-XDCBEHOV                          
104600                      PERFORM D-BERAKNA-CDCBEHOV                          
104700                      PERFORM E-FLYTTA-RESULT-TILL-LINKAREA               
104800                      PERFORM K-BER-PBTOT-SASONG                          
104900                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
105000                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
105100                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
105200                                   LINK-TIBEHOV-FIRST                     
105300                      MOVE WS-CALL-KVVECKOR-BEHOV                         
105400                                TO LINK-KVVECKOR-BEHOV                    
105500                      PERFORM S07-INIT-DATUM                              
105600                    ELSE                                                  
105700******                                                                    
105800******    DETTA GÖRS FÖR ATT BERÄKNA FRAM BEHOVET I NÄRTID                
105900******    DVS INNEVARANDE PLUS TVÅ VECKOR                                 
106000******    EFTERSOM PBPLAN INTE SKA PÅVERKA NÄRTID                         
106100******    RESULTATET FRÅN I-BERAKNA-PB-PLAN FLYTTAS                       
106200******    I S10-ADD-TILL-RESULTAT-PBPLAN                                  
106300******                                                                    
106400                      MOVE 10   TO LINK-KVVECKOR-BEHOV                    
106500                      PERFORM S07-INIT-DATUM                              
106600                      PERFORM B-BERAKNA-PROGNOS                           
106700                      PERFORM C-BERAKNA-XDCBEHOV                          
106800                      PERFORM D-BERAKNA-CDCBEHOV                          
106900                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
107000                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
107100                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
107200                                   LINK-TIBEHOV-FIRST                     
107300                      MOVE WS-CALL-KVVECKOR-BEHOV                         
107400                                TO LINK-KVVECKOR-BEHOV                    
107500                      PERFORM S07-INIT-DATUM                              
107600                    END-IF                                                
107700                                                                          
107800                    PERFORM I-BERAKNA-PB-PLAN                             
107900                                                                          
108000                  ELSE                                                    
108100                    IF SPAR-XLAG-DASEASON > WS-DAGENS-DATUM               
108200                    OR SPAR-XLAG-DASEASON = WS-DAGENS-DATUM               
108300                                                                          
108400******    MANUELL SÄSONG ÄR SATT (GÄLLER FÖR DET TOTALA                   
108500******    SEPARAT- SAMT REFILLBEHOVEN).RÄKNA FÖRST FRAM                   
108600******    EN ÅRSTOTAL FÖR ATT FÅ FRAM EN MASKINELL PROGNOS                
108700******    SOM SKA PÅVERKAS AV MANUELLT SATTA SÄSONGSINDEX                 
108800                                                                          
108900                      MOVE 52   TO LINK-KVVECKOR-BEHOV                    
109000                      PERFORM S07-INIT-DATUM                              
109100                      PERFORM B-BERAKNA-PROGNOS                           
109200                      PERFORM C-BERAKNA-XDCBEHOV                          
109300                      PERFORM D-BERAKNA-CDCBEHOV                          
109400                      PERFORM E-FLYTTA-RESULT-TILL-LINKAREA               
109500                      PERFORM K-BER-PBTOT-SASONG                          
109600                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
109700                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
109800                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
109900                                   LINK-TIBEHOV-FIRST                     
110000                      MOVE WS-CALL-KVVECKOR-BEHOV                         
110100                                TO LINK-KVVECKOR-BEHOV                    
110200                      PERFORM S07-INIT-DATUM                              
110300                                                                          
110400                      PERFORM I-BERAKNA-PB-PLAN                           
110500                                                                          
110600                    ELSE                                                  
110700                      PERFORM B-BERAKNA-PROGNOS                           
110800                      PERFORM C-BERAKNA-XDCBEHOV                          
110900                      PERFORM D-BERAKNA-CDCBEHOV                          
111000                    END-IF                                                
111100                  END-IF                                                  
111200                                                                          
111300             WHEN ENDAST-GLOBALBEHOV                                      
111400                  PERFORM F-BERAKNA-GLOBAL-DCBEHOV                        
111500                                                                          
111600             WHEN ENDAST-LOKALBEHOV                                       
111700******---         URVALET I LÄSNINGEN SKILJER FRÅN XDCBEHOV               
111800                  PERFORM C-BERAKNA-XDCBEHOV                              
111900                                                                          
112000             WHEN ENDAST-CDCBEHOV                                         
112100                  PERFORM D-BERAKNA-CDCBEHOV                              
112200                                                                          
112300             WHEN XDC-CDC-BEHOV                                           
112400                  PERFORM C-BERAKNA-XDCBEHOV                              
112500                  PERFORM D-BERAKNA-CDCBEHOV                              
112600                                                                          
112700         END-EVALUATE                                                     
112800       ELSE                                                               
112900         MOVE NEJ TO LINK-FLJANEJ-ANROP                                   
113000       END-IF                                                             
113100     ELSE                                                                 
113200       MOVE NEJ TO LINK-FLJANEJ-ANROP                                     
113300     END-IF                                                               
113400                                                                          
113500     MOVE JA  TO TREND-SW                                                 
113600     PERFORM E-FLYTTA-RESULT-TILL-LINKAREA                                
113700     MOVE NEJ TO TREND-SW                                                 
113800                                                                          
113900     MOVE ZERO TO RETURN-CODE                                             
114000     GOBACK                                                               
114100     .                                                                    
114200     EJECT                                                                
114300 A-INIT SECTION.                                                          
114400     MOVE 'A-INIT SECTION'       TO CURRENT-SECTION                       
114500                                                                          
114600******************************************************************        
114700*                                                                *        
114800*    BERÄKNING AV START- OCH SLUT-TIDPUNKTER (ÅR OCH VECKA)      *        
114900*    FÖR BERÄKNING                                               *        
115000*    NOLLSTÄLLNING AV TABELLER                                   *        
115100*                                                                *        
115200******************************************************************        
115300     SKIP1                                                                
115400     MOVE NEJ                TO CDCBEHOV-SW                               
115500                                                                          
115600     MOVE FUNCTION CURRENT-DATE                                           
115700                             TO WS-CURRENT-DATE                           
115800                                                                          
115900     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
116000     MOVE WS-DAGENS-DATUM (3:6)                                           
116100                             TO DAT-I-TIDATUM                             
116200                                WS-DAGENS-DATUM-AAMMDD                    
116300                                                                          
116400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
116500                         DAT-O-TIDATUM DAT-KDSVAR                         
116600                                                                          
116700     IF DAT-KDSVAR-OK                                                     
116800                                                                          
116900       MOVE DAT-TIVV         TO WS-DAGENS-VECKA                           
117000       MOVE DAT-TIAAVVD      TO WS-DAGENS-AAVVD                           
117100       MOVE DAT-TID          TO WS-DAGENS-DAGNR                           
117200       MOVE DAT-TIAAVV-GRP   TO WS-DAGENS-DATUM-AAVV                      
117300                                                                          
117400     ELSE                                                                 
117500         STRING ' FEL FRÅN WDATKONV I W222BHDC'                           
117600                ' (A-INIT)'                                               
117700         DELIMITED BY SIZE INTO FELTEXT-STR                               
117800         DISPLAY FELTEXT                                                  
117900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
118000     END-IF                                                               
118100                                                                          
118200     IF  LINK-KVVECKOR-BEHOV > +97                                        
118300         MOVE +97  TO LINK-KVVECKOR-BEHOV                                 
118400     END-IF                                                               
118500     MOVE LINK-KVVECKOR-BEHOV                                             
118600                             TO WS-CALL-KVVECKOR-BEHOV                    
118700     PERFORM S07-INIT-DATUM                                               
118800     SKIP1                                                                
118900     PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                                 
119000     PERFORM S02-NOLLSTALL-RESULTAT-TAB                                   
119100     MOVE ZERO               TO LINK-KVBEHOV-SUMMA                        
119200                                LINK-KVBEHOV-DESSUTOM                     
119300                                LINK-TIBEHOV-FIRST                        
119400                                WS-ARSTOTAL                               
119500                                WS-KVPB-PLAN-VECKA                        
119600                                                                          
119700     MOVE WS-NOLLA-KVBEHOV   TO WS-KVBEHOV-TABELL                         
119800     MOVE WS-NOLLA-RESEASON     TO WS-RESEASON-TABELL                     
119900     MOVE WS-NOLLA-RESEASON-AVR TO WS-RESEASON-AVR-TABELL                 
120000                                                                          
120100     MOVE NEJ                TO WDK629-SW                                 
120200                                                                          
120300     PERFORM AA-HAMTA-VV-I-PER                                            
120400     PERFORM AB-HAMTA-DAPUBL                                              
120500     PERFORM AC-SPARA-DATA-LINK-DC                                        
120600     .                                                                    
120700     EJECT                                                                
120800 AA-HAMTA-VV-I-PER SECTION.                                               
120900     MOVE 'AA-HAMTA-VV-I-PER '   TO CURRENT-SECTION                       
121000                                                                          
121100     MOVE +1                 TO IX                                        
121200     MOVE DAT-TIAARP         TO WS-TIAAPER                                
121300     MOVE 1                  TO PER                                       
121400                                                                          
121500     PERFORM UNTIL IX        >  12                                        
121600       MOVE 'AARP  '         TO DAT-KDDATFORM                             
121700       MOVE TIAAPER          TO DAT-I-TIDATUM                             
121800       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
121900                           DAT-O-TIDATUM DAT-KDSVAR                       
122000       IF DAT-KDSVAR-OK                                                   
122100          MOVE DAT-TIVV      TO PER-START-VV (IX)                         
122200       ELSE                                                               
122300           STRING ' FEL DATUM - DATKONV2 I W222BHDC'                      
122400                  '(AA-HAMTA, TIAAPER)'                                   
122500           DELIMITED BY SIZE INTO FELTEXT-STR                             
122600           DISPLAY FELTEXT                                                
122700           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
122800       END-IF                                                             
122900       ADD +1                TO IX                                        
123000                                PER                                       
123100     END-PERFORM                                                          
123200                                                                          
123300     MOVE 1                  TO IX-TILL                                   
123400     MOVE 2                  TO IX-FRAN                                   
123500     PERFORM UNTIL IX-FRAN   >  12                                        
123600       COMPUTE PER-SLUT-VV (IX-TILL) =                                    
123700               PER-START-VV (IX-FRAN) - 1                                 
123800       ADD 1                 TO IX-TILL                                   
123900                                IX-FRAN                                   
124000     END-PERFORM                                                          
124100     MOVE 52                 TO PER-SLUT-VV (12)                          
124200                                                                          
124300     .                                                                    
124400     EJECT                                                                
124500 AB-HAMTA-DAPUBL   SECTION.                                               
124600     MOVE 'AB-HAMTA-DAPUBL '      TO CURRENT-SECTION                      
124700                                                                          
124800     INITIALIZE DAPUBL-TAB                                                
124900     MOVE 1      TO PUBL-IX                                               
125000                                                                          
125100     MOVE LINK-IDARTNR TO W-IDARTNR                                       
125200     PERFORM IMS-GU-WDK701                                                
125300     IF SEGMENT-FINNS                                                     
125400                                                                          
125500        PERFORM IMS-GNP-WDK712                                            
125600        PERFORM UNTIL SEGMENT-SAKNAS                                      
125700                                                                          
125800           MOVE LART-IDLANDX2  TO PUBL-IDLANDX2(PUBL-IX)                  
125900           MOVE LART-DAPUBL    TO PUBL-DAPUBL  (PUBL-IX)                  
126000           MOVE LART-PRMATRL   TO PUBL-PRMATRL (PUBL-IX)                  
126100           MOVE LART-KVQPACK-3 TO PUBL-KVQPACK-3(PUBL-IX)                 
126200                                                                          
126300           ADD 1 TO PUBL-IX                                               
126400           PERFORM IMS-GNP-WDK712                                         
126500        END-PERFORM                                                       
126600     END-IF                                                               
126700     .                                                                    
126800     EJECT                                                                
126900 AC-SPARA-DATA-LINK-DC   SECTION.                                         
127000     MOVE 'AC-SPARA-DATA-LINK-DC '  TO CURRENT-SECTION                    
127100                                                                          
127200     MOVE LINK-IDDC   TO W-IDDC                                           
127300                                                                          
127400     PERFORM IMS-GU-WDK711                                                
127500     IF SEGMENT-FINNS                                                     
127600       PERFORM IMS-GNP-WDK722                                             
127700                                                                          
127800       IF SEGMENT-FINNS                                                   
127900         MOVE XLAG-KVPB-TREND      TO SPAR-XLAG-KVPB-TREND                
128000         MOVE XLAG-KVVECKOR-TREND  TO SPAR-XLAG-KVVECKOR-TREND            
128100         MOVE XLAG-DAPBPLAN        TO SPAR-XLAG-DAPBPLAN                  
128200         MOVE XLAG-DASEASON        TO SPAR-XLAG-DASEASON                  
128300         MOVE XLAG-KVPB-PLAN       TO SPAR-XLAG-KVPB-PLAN                 
128400                                                                          
128500         MOVE +1   TO IX                                                  
128600         PERFORM UNTIL IX  > +12                                          
128700           MOVE XLAG-RESEASON-PLAN (IX) TO                                
128800                                   SPAR-XLAG-RESEASON-PLAN (IX)           
128900                                                                          
129000           ADD +1  TO IX                                                  
129100         END-PERFORM                                                      
129200       ELSE                                                               
129300         MOVE ZERO                 TO SPAR-XLAG-KVPB-TREND                
129400                                      SPAR-XLAG-KVVECKOR-TREND            
129500                                      SPAR-XLAG-DAPBPLAN                  
129600                                      SPAR-XLAG-DASEASON                  
129700                                      SPAR-XLAG-KVPB-PLAN                 
129800         MOVE +1   TO IX                                                  
129900         PERFORM UNTIL IX  > +12                                          
130000           MOVE ZERO               TO SPAR-XLAG-RESEASON-PLAN (IX)        
130100                                                                          
130200           ADD +1  TO IX                                                  
130300         END-PERFORM                                                      
130400       END-IF                                                             
130500     ELSE                                                                 
130600       MOVE ZERO                   TO SPAR-XLAG-KVPB-TREND                
130700                                      SPAR-XLAG-KVVECKOR-TREND            
130800                                      SPAR-XLAG-DAPBPLAN                  
130900                                      SPAR-XLAG-DASEASON                  
131000                                      SPAR-XLAG-KVPB-PLAN                 
131100       MOVE +1   TO IX                                                    
131200       PERFORM UNTIL IX  > +12                                            
131300         MOVE ZERO                 TO SPAR-XLAG-RESEASON-PLAN (IX)        
131400                                                                          
131500         ADD +1  TO IX                                                    
131600       END-PERFORM                                                        
131700     END-IF                                                               
131800                                                                          
131900     MOVE LINK-IDDC TO W-IDDC-B6                                          
132000     PERFORM S13-READ-OR-TAB-B601                                         
132100     PERFORM S77-KOLLA-DAPUBL                                             
132200     MOVE PUBL-KVQPACK-3(PUBL-IX) TO WS-LART-KVQPACK-3                    
132300                                                                          
132400     .                                                                    
132500     EJECT                                                                
132600 B-BERAKNA-PROGNOS SECTION.                                               
132700     MOVE 'B-BERAKNA-PROGNOS  '  TO CURRENT-SECTION                       
132800                                                                          
132900******************************************************************        
133000*                                                                *        
133100*    BERÄKNING AV PROGNOSBEHOV AV ARTIKEL FÖR RESPEKTIVE (NDC)   *        
133200*    LAGER SOM HAR LOKAL ANSKAFFNING.                            *        
133300*                                                                *        
133400*    KINA OCH JAPAN HAR 6 DAGARS ARBETSVECKA                     *        
133500******************************************************************        
133600     SKIP1                                                                
133700                                                                          
133800     MOVE LINK-IDDC   TO W-IDDC                                           
133900                         W-IDDC-B6                                        
134000     PERFORM S13-READ-OR-TAB-B601                                         
134100                                                                          
134200     MOVE NEJ TO 6ARBDAG-SW                                               
134300     IF DCS-CHINA OR DCS-JAPAN                                            
134400       MOVE JA TO 6ARBDAG-SW                                              
134500     END-IF                                                               
134600                                                                          
134700     PERFORM IMS-GU-WDK711                                                
134800     IF SEGMENT-FINNS                                                     
134900       MOVE NEJ     TO SEP-SEASON-FINNS-SW                                
135000       MOVE +1                 TO IX                                      
135100       PERFORM UNTIL IX > 12                                              
135200         IF SLAG-RESEASON (IX) NOT = 1.00                                 
135300           MOVE JA  TO SEP-SEASON-FINNS-SW                                
135400         END-IF                                                           
135500         ADD +1       TO IX                                               
135600       END-PERFORM                                                        
135700                                                                          
135800       PERFORM IMS-GNP-WDK722                                             
135900                                                                          
136000       IF SEGMENT-FINNS                                                   
136100         IF XLAG-KVPB-JUST1 > ZERO OR                                     
136200            XLAG-TIPBJUST-1 > ZERO OR                                     
136300            XLAG-KVPB-JUST2 > ZERO OR                                     
136400            XLAG-TIPBJUST-2 > ZERO OR                                     
136500            SEP-SEASON-FINNS-SW = JA                                      
136600                                                                          
136700           MOVE JA          TO JUST-PB-FINNS-SW                           
136800         ELSE                                                             
136900           MOVE NEJ         TO JUST-PB-FINNS-SW                           
137000         END-IF                                                           
137100         PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                             
137200                                                                          
137300         PERFORM S77-KOLLA-DAPUBL                                         
137400         MOVE PUBL-DAPUBL(PUBL-IX) TO WS-DAPUBL                           
137500                                                                          
137600         IF WS-DAPUBL > ZERO                                              
137700           MOVE 'AAMMDD'           TO DAT-KDDATFORM                       
137800           MOVE WS-DAPUBL(3:6)     TO DAT-I-TIDATUM                       
137900           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
138000                               DAT-O-TIDATUM DAT-KDSVAR                   
138100           IF DAT-KDSVAR-OK                                               
138200              MOVE DAT-TIAAVV-GRP  TO WS-DAPUBL-AAVV                      
138300           ELSE                                                           
138400               STRING ' FEL DATUM - DATKONV3 I W222BHDC'                  
138500                      '(B-BERAKNA DAPUBL-AAVV)'                           
138600               DELIMITED BY SIZE INTO FELTEXT-STR                         
138700               DISPLAY FELTEXT                                            
138800               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
138900           END-IF                                                         
139000         END-IF                                                           
139100                                                                          
139200         PERFORM BA-PROGNOSBERAKNING                                      
139300         PERFORM S03-ADD-TILL-RESULTAT-TAB                                
139400       END-IF                                                             
139500     END-IF                                                               
139600                                                                          
139700     .                                                                    
139800     EJECT                                                                
139900 BA-PROGNOSBERAKNING SECTION.                                             
140000     MOVE 'BA-PROGNOSBERAKNING '  TO CURRENT-SECTION                      
140100                                                                          
140200******************************************************************        
140300*                                                                *        
140400*    BERÄKNING AV ARTIKELPROGNOS PER VECKA. UTGÅNGSPUNKT FÖR     *        
140500*    BERÄKNINGEN ÄR PERIODBEHOVET (KVPB-SEP / KVPB-REF).         *        
140600*    OM PERIODBEHOVS-JUSTERINGAR FANNS  I ARTIKELREGISTRET       *        
140700*    JUSTERAS BEHOVET.                                           *        
140800*                                                                *        
140900* -- DIREKTLEVERANSANDEL KOLLAS BARA FÖR CDC ENLIGT PATRIK L.    *        
141000******************************************************************        
141100                                                                          
141200     PERFORM BAA-JUSTERA-PB-FAKTORER                                      
141300     MOVE +1                 TO BER-IY                                    
141400     MOVE W-BER-START-VV     TO BER-IX                                    
141500     MOVE LINK-TIBEHOV-START TO W-BER-DATUM                               
141600                                                                          
141700     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
141800                                                                          
141900         PERFORM UNTIL                                                    
142000            (BER-IY = BER-ANTAL-AA AND                                    
142100             BER-IX > BER-ANTAL-VV)                                       
142200         OR (BER-IY < BER-ANTAL-AA AND                                    
142300             BER-IX > 52)                                                 
142400                                                                          
142500             MOVE W-BER-DATUM       TO TMP1-YYWW                          
142600                                                                          
142700             IF  WS-DAPUBL > ZERO                                         
142800                                                                          
142900               MOVE WS-DAPUBL-AAVV  TO TMP2-YYWW                          
143000             ELSE                                                         
143100               MOVE W-TIFINLV-AAVV  TO TMP2-YYWW                          
143200             END-IF                                                       
143300             PERFORM WY2000P3                                             
143400             IF TMP1-YYWW < TMP2-YYWW                                     
143500               MOVE ZERO TO BER-BEHOV (BER-IY, BER-IX)                    
143600             ELSE                                                         
143700               MOVE W-KVPB-SEP TO BER-BEHOV (BER-IY, BER-IX)              
143800                                                                          
143900               IF JUST-PB-FINNS-SW = JA                                   
144000                  PERFORM BAB-PB-JUSTERINGAR                              
144100               END-IF                                                     
144200             END-IF                                                       
144300                                                                          
144400             IF BER-IY = 1 AND BER-IX = 1                                 
144500                 PERFORM BAC-JUSTERA-VA1                                  
144600             END-IF                                                       
144700                                                                          
144800             ADD 1           TO BER-IX                                    
144900                                W-BER-DATUM                               
145000         END-PERFORM                                                      
145100         ADD 1   TO BER-IY                                                
145200         MOVE 1  TO BER-IX                                                
145300                    IX-PER                                                
145400         SUBTRACT +52 FROM  W-BER-DATUM                                   
145500         IF W-BER-DATUM > 9900                                            
145600             MOVE 0001 TO W-BER-DATUM                                     
145700         ELSE                                                             
145800             ADD +100     TO    W-BER-DATUM                               
145900         END-IF                                                           
146000     END-PERFORM                                                          
146100     .                                                                    
146200     EJECT                                                                
146300 BAA-JUSTERA-PB-FAKTORER SECTION.                                         
146400     MOVE 'BAA-JUSTERA-PB-FAKTORER'  TO CURRENT-SECTION                   
146500                                                                          
146600     MOVE ZERO TO W-KVPB-SEP                                              
146700                  W-KVPB-JUST1                                            
146800                  W-KVPB-JUST2                                            
146900                                                                          
147000     IF  SLAG-KVPB-REF  > ZERO                                            
147100         DIVIDE SLAG-KVPB-REF   BY 4.33                                   
147200                                GIVING W-KVPB-SEP                         
147300     END-IF                                                               
147400     IF JUST-PB-FINNS-SW = JA                                             
147500        IF XLAG-KVPB-JUST1 > ZERO                                         
147600            DIVIDE XLAG-KVPB-JUST1 BY 4.33                                
147700                                   GIVING W-KVPB-JUST1                    
147800        END-IF                                                            
147900        IF XLAG-KVPB-JUST2 > ZERO                                         
148000            DIVIDE XLAG-KVPB-JUST2 BY 4.33                                
148100                                   GIVING W-KVPB-JUST2                    
148200        END-IF                                                            
148300     END-IF                                                               
148400     .                                                                    
148500     EJECT                                                                
148600 BAB-PB-JUSTERINGAR SECTION.                                              
148700     MOVE 'BAB-PB-JUSTERINGAR '   TO CURRENT-SECTION                      
148800                                                                          
148900******************************************************************        
149000*                                                                *        
149100*    FRAMRÄKNAT PERIODBEHOV PER VECKA JUSTERAS MED               *        
149200*    FÖLJANDE FAKTORER:                                          *        
149300*                                                                *        
149400*          - PERIODBEHOVSJUST 1 OCH 2  (KVPB-JUST)  WDK722       *        
149500*          - TREND VID XDC-BEHOV  (KVPB-TREND ETC)  WDK722       *        
149600*          - SÄSONG-INDEX              (RESEASON)   WDK711       *        
149700*                                                                *        
149800******************************************************************        
149900                                                                          
150000     MOVE XLAG-TIPBJUST-1         TO TMP1-YYWW                            
150100     MOVE W-BER-DATUM             TO TMP2-YYWW                            
150200     PERFORM WY2000P3                                                     
150300                                                                          
150400     IF  TMP1-YYWW         NOT > TMP2-YYWW   AND                          
150500         TMP1-YYWW         > ZERO                                         
150600         MOVE W-KVPB-JUST1  TO BER-BEHOV (BER-IY, BER-IX)                 
150700     END-IF                                                               
150800                                                                          
150900     MOVE XLAG-TIPBJUST-2         TO TMP1-YYWW                            
151000     MOVE W-BER-DATUM             TO TMP2-YYWW                            
151100     PERFORM WY2000P3                                                     
151200                                                                          
151300     IF  TMP1-YYWW         NOT > TMP2-YYWW   AND                          
151400         TMP1-YYWW         > ZERO                                         
151500         MOVE W-KVPB-JUST2  TO BER-BEHOV (BER-IY, BER-IX)                 
151600     END-IF                                                               
151700                                                                          
151800     MOVE XLAG-TIPBJUST-1         TO TMP1-YYWW                            
151900     MOVE XLAG-TIPBJUST-2         TO TMP2-YYWW                            
152000     MOVE W-BER-DATUM             TO TMP3-YYWW                            
152100     PERFORM WY2000Q3                                                     
152200                                                                          
152300     IF  TMP1-YYWW        NOT > TMP3-YYWW  AND                            
152400         TMP2-YYWW        NOT > TMP3-YYWW  AND                            
152500         TMP1-YYWW        > ZERO            AND                           
152600         TMP2-YYWW        > ZERO                                          
152700         MOVE XLAG-TIPBJUST-1     TO TMP1-YYWW                            
152800         MOVE XLAG-TIPBJUST-2     TO TMP2-YYWW                            
152900         PERFORM WY2000P3                                                 
153000                                                                          
153100         IF  TMP1-YYWW < TMP2-YYWW                                        
153200             MOVE W-KVPB-JUST2  TO BER-BEHOV (BER-IY, BER-IX)             
153300         ELSE                                                             
153400             MOVE W-KVPB-JUST1  TO BER-BEHOV (BER-IY, BER-IX)             
153500         END-IF                                                           
153600     END-IF                                                               
153700                                                                          
153800     MOVE 1 TO IX                                                         
153900     MOVE W-BER-DATUM TO W-DATUM                                          
154000     PERFORM UNTIL IX > PERIOD-ANTAL                                      
154100         IF  W-DATUM-VV NOT < PER-START-VV (IX)                           
154200         AND W-DATUM-VV NOT > PER-SLUT-VV  (IX)                           
154300             MULTIPLY SLAG-RESEASON (IX)                                  
154400                              BY BER-BEHOV (BER-IY, BER-IX)               
154500         END-IF                                                           
154600         ADD 1 TO IX                                                      
154700     END-PERFORM                                                          
154800     .                                                                    
154900     EJECT                                                                
155000 BAC-JUSTERA-VA1 SECTION.                                                 
155100     MOVE 'BAC-JUSTERA-VA1 '      TO CURRENT-SECTION                      
155200                                                                          
155300******************************************************************        
155400*    SEPARATBEHOVEN JUSTERAS I INNEVARANDE VECKA                          
155500*                                                                         
155600*--  OBS! KINA OCH JAPAN HAR 6 DAGARS ARBETSVECKA                         
155700*    IF NDC-CN OR LDC-CN OR NDC-JP                                        
155800*      MOVE JA TO 6ARBDAG-SW                                              
155900*    END-IF                                                               
156000*****************************************************************         
156100                                                                          
156200     IF LINK-TIAAVV-AKTUELL = LINK-TIBEHOV-START                          
156300       IF 6ARBDAG-SW = JA                                                 
156400         IF LINK-TID-AKTUELL > 0 AND < 7                                  
156500            ACCEPT W-TIME FROM TIME                                       
156600            COMPUTE W-DAGAR-KVAR = 6 - LINK-TID-AKTUELL                   
156700            IF W-TIME(1:2) > 05  AND                                      
156800               W-TIME(1:2) < 17                                           
156900               ADD +1 TO W-DAGAR-KVAR                                     
157000            END-IF                                                        
157100            COMPUTE W-VECKODEL = W-DAGAR-KVAR / 6                         
157200            IF W-VECKODEL < 0                                             
157300               MOVE +0 TO W-VECKODEL                                      
157400            END-IF                                                        
157500            COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =                  
157600            W-VECKODEL * BER-BEHOV (BER-IY, BER-IX)                       
157700         END-IF                                                           
157800       ELSE                                                               
157900         IF LINK-TID-AKTUELL > 0 AND < 6                                  
158000            ACCEPT W-TIME FROM TIME                                       
158100            COMPUTE W-DAGAR-KVAR = 5 - LINK-TID-AKTUELL                   
158200            IF W-TIME(1:2) > 05  AND                                      
158300               W-TIME(1:2) < 17                                           
158400               ADD +1 TO W-DAGAR-KVAR                                     
158500            END-IF                                                        
158600            COMPUTE W-VECKODEL = W-DAGAR-KVAR / 5                         
158700            IF W-VECKODEL < 0                                             
158800               MOVE +0 TO W-VECKODEL                                      
158900            END-IF                                                        
159000            COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =                  
159100            W-VECKODEL * BER-BEHOV (BER-IY, BER-IX)                       
159200         END-IF                                                           
159300       END-IF                                                             
159400     END-IF                                                               
159500     .                                                                    
159600     EJECT                                                                
159700 C-BERAKNA-XDCBEHOV   SECTION.                                            
159800     MOVE 'C-BERAKNA-XDCBEHOV  ' TO CURRENT-SECTION                       
159900                                                                          
160000******************************************************************        
160100*                                                                *        
160200*    BERÄKNING AV LDC/NDC'ERNAS BEHOV AV EN ARTIKEL              *        
160300*    BEHOVET VISAS DEN VECKA NDC (MED LOKAL ANSKAFFNING) BEHÖVER *        
160400*    LEVERERA, DVS BEHOVSDAG PÅ XDC MINUS LEDTID                 *        
160500*                                                                *        
160600*    BEHOVET AV ALLA UNDERLIGGANDE DC SOM REFILLAS FRÅN ETT NDC. *        
160700*    MAN GÅR NER EN NIVÅ BARA.                                   *        
160800******************************************************************        
160900                                                                          
161000     MOVE NEJ TO WS-ERS-FINNS-K611                                        
161100                                                                          
161200     MOVE WS-BER-TAB-NOLL     TO BER-TAB                                  
161300                                                                          
161400     ACCEPT W-TIME          FROM TIME                                     
161500                                                                          
161600     IF CLAG-REDIRLEV < 1.00                                              
161700       IF ART-FLERS = JA                                                  
161800         MOVE ART-IDARTNR     TO W-IDARTNR-D7-MIN                         
161900                                 W-IDARTNR-D7-MAX                         
162000         PERFORM IMS-GU-WDD7A1                                            
162100         IF SEGMENT-FINNS                                                 
162200           MOVE ERS-IDARTNR   TO W-IDARTNR-ERS                            
162300           PERFORM IMS-GU-WDK601-ERS                                      
162400           IF SEGMENT-FINNS                                               
162500             PERFORM IMS-GNP-WDK611-ERS                                   
162600                                                                          
162700             MOVE JA          TO WS-ERS-FINNS-K611                        
162800           END-IF                                                         
162900                                                                          
163000*----   VI BEHÖVER ÅTERSTÄLLA POSITIONEN I BASEN                          
163100           MOVE LINK-IDARTNR  TO W-IDARTNR                                
163200           PERFORM IMS-GU-WDK601                                          
163300         END-IF                                                           
163400       END-IF                                                             
163500                                                                          
163600       PERFORM IMS-GU-WDK701                                              
163700       IF SEGMENT-FINNS                                                   
163800         MOVE LINK-IDDC(1:1)  TO W-IDDC1                                  
163900         MOVE LINK-IDDC       TO W-IDDC-REF                               
164000         PERFORM CC-LAES-WDK711-REF                                       
164100                                                                          
164200         PERFORM UNTIL SEGMENT-SAKNAS                                     
164300                                                                          
164400           PERFORM S21-NOLLSTALL-W-NDC-BEHOV-REF                          
164500           PERFORM S77-KOLLA-DAPUBL                                       
164600           MOVE PUBL-DAPUBL(PUBL-IX)  TO WS-DAPUBL                        
164700           MOVE PUBL-PRMATRL(PUBL-IX) TO WS-PRMATRL                       
164800           MOVE SLAG-FLFLYG           TO WS-FLFLYG                        
164900           PERFORM S80-CALC-LT-ADJ-PUBWK                                  
165000                                                                          
165100           IF  WS-DAPUBL > ZERO                                           
165200           AND WS-DAPUBL > WS-DAGENS-DATUM                                
165300             MOVE WS-LT-WEEKS-DAYS   TO DAG-KVKALDAG                      
165400             MOVE WS-DAPUBL (3:6)                                         
165500                              TO DAG-TIAAMMDD-TOM                         
165600             MOVE 003         TO DAG-KDCALL                               
165700             CALL WDAGKONV USING DAG-KDCALL                               
165800                                 DAG-DATUM-AREA                           
165900                                 DAG-KDSVAR                               
166000             IF DAG-KDSVAR = SPACE                                        
166100               CONTINUE                                                   
166200             ELSE                                                         
166300               STRING 'FEL FRÅN WDAGKONV I W222BHDC '                     
166400               'C- SECTION (WS-DAPUBL)' DELIMITED BY SIZE                 
166500                         INTO         FELTEXT-STR                         
166600                DISPLAY FELTEXT                                           
166700                CALL ABEND USING RKOD-ABEND-UTAN-DUMP                     
166800             END-IF                                                       
166900                                                                          
167000             MOVE DAG-TIAAMMDD-FOM                                        
167100                              TO DAT-I-TIDATUM                            
167200             MOVE 'AAMMDD'    TO DAT-KDDATFORM                            
167300                                                                          
167400             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
167500                                 DAT-O-TIDATUM DAT-KDSVAR                 
167600                                                                          
167700             IF DAT-KDSVAR-OK                                             
167800                MOVE DAT-TIAAVV-GRP                                       
167900                              TO W-TIAAVV                                 
168000                MOVE W-TIAAVV                                             
168100                              TO W-DATUM-FORSTA-NDC                       
168200             ELSE                                                         
168300                 STRING ' FEL FRÅN WDATKONV4 I W222BHDC '                 
168400                        '(C-, DAG-TIAAMMDD)'                              
168500                 DELIMITED BY SIZE INTO FELTEXT-STR                       
168600                 DISPLAY FELTEXT                                          
168700                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
168800             END-IF                                                       
168900           ELSE                                                           
169000             MOVE W-TIFINLV-AAVV-MINUS-LT                                 
169100                              TO W-DATUM-FORSTA-NDC                       
169200           END-IF                                                         
169300                                                                          
169400           MOVE 'AAVV  '      TO DAT-KDDATFORM                            
169500           MOVE LINK-TIAAVV-AKTUELL                                       
169600                              TO DAT-I-TIDATUM                            
169700                                                                          
169800           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
169900                           DAT-O-TIDATUM DAT-KDSVAR                       
170000                                                                          
170100           IF DAT-KDSVAR-OK                                               
170200             MOVE DAT-TIAARP(3:2) TO W-PER                                
170300             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
170400           ELSE                                                           
170500                 STRING ' FEL FRÅN WDATKONV5 I W222BHDC '                 
170600                        '(C-, LINK-TIAAVVD)'                              
170700                 DELIMITED BY SIZE INTO FELTEXT-STR                       
170800                 DISPLAY FELTEXT                                          
170900                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
171000           END-IF                                                         
171100*                                                                         
171200           PERFORM S23-GET-WEEK-DEMAND-XDC                                
171300                                                                          
171400           MOVE +1                TO INDX-L                               
171500                                                                          
171600           COMPUTE W-NDC-TILLGANG = SLAG-KVLS +                           
171700                                    SLAG-KVAKS-PAV +                      
171800                                    SLAG-KVAKS-SDC +                      
171900                                    SLAG-KVBEART -                        
172000                                    SLAG-KVOKS-BULK -                     
172100                                    SLAG-KVOKS-DAG -                      
172200                                    SLAG-KVROS-BULK -                     
172300                                    SLAG-KVROS-DAG -                      
172400                                    SLAG-KVSPARR-KVAL                     
172500           COMPUTE W-KVBEHOV-52V ROUNDED =                                
172600                   (SLAG-KVPB-REF + SLAG-KVPBREOI) * 12                   
172700                                                                          
172800           PERFORM CD-JUSTERA-NDC-TILLGANG                                
172900           MOVE ZERO         TO W-NDC-KVBEHOV-DESSUTOM                    
173000                                                                          
173100           IF LINK-TID-AKTUELL = 5                                        
173200           OR LINK-TID-AKTUELL > 5                                        
173300              CONTINUE                                                    
173400           ELSE                                                           
173500              PERFORM CA-BERAKNA-INNEV-VECKA-NDC                          
173600              ADD W-NDC-KVBEHOV-DESSUTOM                                  
173700                             TO LINK-KVBEHOV-DESSUTOM                     
173800           END-IF                                                         
173900                                                                          
174000           PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                           
174100           PERFORM CB-NDCBEHOV                                            
174200           PERFORM S03-ADD-TILL-RESULTAT-TAB                              
174300                                                                          
174400           PERFORM CC-LAES-WDK711-REF                                     
174500         END-PERFORM                                                      
174600       END-IF                                                             
174700     END-IF                                                               
174800     .                                                                    
174900     EJECT                                                                
175000 CA-BERAKNA-INNEV-VECKA-NDC SECTION.                                      
175100     MOVE 'CA-BERAKNA-INNEV-VECKA-NDC ' TO CURRENT-SECTION                
175200                                                                          
175300******************************************************************        
175400*                                                                *        
175500*    BERÄKNING AV XDC'ERNAS BEHOV AV EN ARTIKEL I INNEVARANDE    *        
175600*    VECKA.       (REFILLADE LDC/NDC)                            *        
175700*                                                                *        
175800******************************************************************        
175900                                                                          
176000     MOVE W-DATUM-FORSTA-NDC    TO TMP1-YYWW                              
176100     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
176200     PERFORM WY2000P3                                                     
176300     IF TMP1-YYWW <= TMP2-YYWW                                            
176400*                                                                         
176500*****   WEEKLY DEMAND ALREADY ADJUSTED FOR NUMBER OF DAYS                 
176600*****   IN CURRENT WEEK. THIS IS DONE W271UTUP                            
176700*                                                                         
176800        COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED                             
176900                                 = (UTUP-KVBEHOV-V (INDX-L) * 1)          
177000                                                                          
177100        COMPUTE W-NDC-KVAR-EFT-VECKA ROUNDED =                            
177200                         W-NDC-TILLGANG - W-KVBEHOV-INNEV-VECKA           
177300                                                                          
177400        IF W-JUST-PB-FINNS = NEJ                                          
177500                                                                          
177600          IF W-NDC-KVAR-EFT-VECKA    < SLAG-KVREFPKT                      
177700             COMPUTE W-NDC-DIFF      = SLAG-KVREFPKT -                    
177800                                       W-NDC-TILLGANG                     
177900             COMPUTE W-DC-REFILL-KVANT ROUNDED =                          
178000                                     W-NDC-DIFF + SLAG-KVREFBER           
178100                                                                          
178200             PERFORM S24-ADJUST-LOW-DEMAND                                
178300             IF DEMAND-ADJUST                                             
178400                MOVE +1             TO W-DC-REFILL-KVANT                  
178500             ELSE                                                         
178600*                                                                         
178700                PERFORM S08NDC-JUSTERA-REFILLKVANT                        
178800                PERFORM S06-SATT-QX-PROCENT-BRYTNING                      
178900                PERFORM S05-BERAKNA-Q-KVANT                               
179000             END-IF                                                       
179100             MOVE W-DC-REFILL-KVANT TO W-NDC-KVBEHOV-DESSUTOM             
179200          ELSE                                                            
179300             MOVE ZERO              TO W-NDC-KVBEHOV-DESSUTOM             
179400          END-IF                                                          
179500        ELSE                                                              
179600                                                                          
179700*******  GET REFILLING POINT AND REFILL QUANTITY                          
179800                                                                          
179900          INITIALIZE REFL1-REFL-W271REFL                                  
180000                                                                          
180100          MOVE LINK-IDARTNR    TO REFL1-REFL-IDARTNR                      
180200          MOVE SLAG-IDDC       TO REFL1-REFL-IDDC                         
180300          MOVE SLAG-IDDC-REF   TO REFL1-REFL-IDDC-REF                     
180400          MOVE SLAG-IDREFTAB   TO REFL1-REFL-IDREFTAB                     
180500          MOVE SLAG-FLWILSON   TO REFL1-REFL-FLWILSON                     
180600                                                                          
180700*******  BÅDE USA OCH CANADA SKALL HA MATERIALPRIS                        
180800          IF DCS-NDC-CN OR                                                
180900             DCS-NDC-NA                                                   
181000            MOVE WS-PRMATRL    TO REFL1-REFL-PRARTBES                     
181100          ELSE                                                            
181200            MOVE CLAG-PRARTSTD TO REFL1-REFL-PRARTBES                     
181300          END-IF                                                          
181400                                                                          
181500          MOVE SLAG-IDLEVNR    TO REFL1-REFL-IN-IDLEVNR-DC                
181600*---- GÄLLER EJ FÖR DCS-SDC OR DCS-NDC-CN, BARA NDC-NA/PF                 
181700          MOVE ZERO            TO REFL1-REFL-NDC-KVDAGAR-TBT-DC           
181800          MOVE SLAG-FLFLYG     TO REFL1-REFL-FLFLYG                       
181900                                                                          
182000          MOVE W-BINNDAY       TO REFL1-REFL-BINNDAY-TIAAMMDD             
182100          MOVE ZERO            TO REFL1-REFL-KVREFPKT                     
182200                                  REFL1-REFL-KVREFBER                     
182300          MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                             
182400          MOVE W-BINNDAY       TO TMP2-YYMMDD                             
182500*                                                                         
182600*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
182700*                                                                         
182800          PERFORM WY2000P1                                                
182900          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
183000*                                                                         
183100*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
183200*                                                                         
183300            MOVE SLAG-KVREFBER TO REFL1-REFL-IN-KVREFBER                  
183400          ELSE                                                            
183500            MOVE +0            TO REFL1-REFL-IN-KVREFBER                  
183600          END-IF                                                          
183700                                                                          
183800          MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                             
183900          MOVE W-BINNDAY       TO TMP2-YYMMDD                             
184000*                                                                         
184100*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
184200*                                                                         
184300          PERFORM WY2000P1                                                
184400          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
184500*                                                                         
184600*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
184700*                                                                         
184800            MOVE SLAG-KVREFPKT TO REFL1-REFL-IN-KVREFPKT                  
184900          ELSE                                                            
185000            MOVE +0            TO REFL1-REFL-IN-KVREFPKT                  
185100          END-IF                                                          
185200*                                                                         
185300          MOVE UTUP-LT-BEHOV-V (INDX-L)                                   
185400                               TO REFL1-REFL-IN-LEADTID-BEHOV             
185500                                                                          
185600          CALL W271REFL USING REFL1-REFL-W271REFL                         
185700                              REFL1-2501-PCB                              
185800                              REFL1-WDB6-PCB                              
185900                              REFL1-WDK7-PCB                              
186000                              REFL1-UTIL-WDK6-PCB                         
186100                              REFL1-UTIL-WDK7-PCB                         
186200                              REFL1-UTIL-WDB6-PCB                         
186300                                                                          
186400          IF W-NDC-KVAR-EFT-VECKA    < REFL1-REFL-KVREFPKT                
186500             COMPUTE W-NDC-DIFF      = (REFL1-REFL-KVREFPKT               
186600                                          -  W-NDC-TILLGANG )             
186700             COMPUTE W-DC-REFILL-KVANT ROUNDED =                          
186800                             W-NDC-DIFF + REFL1-REFL-KVREFBER             
186900                                                                          
187000             PERFORM S24-ADJUST-LOW-DEMAND                                
187100             IF DEMAND-ADJUST                                             
187200                MOVE +1             TO W-DC-REFILL-KVANT                  
187300             ELSE                                                         
187400                PERFORM S06-SATT-QX-PROCENT-BRYTNING                      
187500                PERFORM S05-BERAKNA-Q-KVANT                               
187600             END-IF                                                       
187700             MOVE W-DC-REFILL-KVANT TO W-NDC-KVBEHOV-DESSUTOM             
187800          ELSE                                                            
187900             MOVE ZERO              TO W-NDC-KVBEHOV-DESSUTOM             
188000          END-IF                                                          
188100        END-IF                                                            
188200     ELSE                                                                 
188300        MOVE ZERO                   TO W-NDC-KVBEHOV-DESSUTOM             
188400     END-IF                                                               
188500     .                                                                    
188600     EJECT                                                                
188700 CB-NDCBEHOV  SECTION.                                                    
188800     MOVE 'CB-NDCBEHOV    '    TO CURRENT-SECTION                         
188900                                                                          
189000******************************************************************        
189100*                                                                *        
189200*    BERÄKNING AV XDC'ERNAS BEHOV AV EN ARTIKEL I KOMMANDE       *        
189300*    VECKOR.  (REFILLADE LDC/NDC)                                *        
189400*                                                                *        
189500******************************************************************        
189600                                                                          
189700     MOVE ZERO                  TO W-NDC-ACC-KVBEHOV                      
189800     ADD W-NDC-KVBEHOV-DESSUTOM TO W-NDC-TILLGANG                         
189900     ADD W-KVBEHOV-INNEV-VECKA  TO W-NDC-ACC-KVBEHOV                      
190000                                                                          
190100     MOVE +1                    TO BER-IY                                 
190200     MOVE W-BER-START-VV        TO BER-IX                                 
190300     MOVE LINK-TIBEHOV-START    TO W-BER-DATUM                            
190400                                   W-AAVV                                 
190500     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
190600     MOVE W-AAVV                TO DAT-I-TIDATUM                          
190700                                                                          
190800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
190900                         DAT-O-TIDATUM DAT-KDSVAR                         
191000                                                                          
191100     IF DAT-KDSVAR-OK                                                     
191200       MOVE DAT-TIAARP(3:2)     TO W-PER                                  
191300       MOVE DAT-TIAAMMDD        TO W-BINNDAY                              
191400     ELSE                                                                 
191500           STRING ' FEL FRÅN WDATKONV6 I W222BHDC '                       
191600                  '(CB-, LINK-TIBEHOV)'                                   
191700           DELIMITED BY SIZE INTO FELTEXT-STR                             
191800           DISPLAY FELTEXT                                                
191900           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
192000     END-IF                                                               
192100                                                                          
192200     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
192300                                                                          
192400         PERFORM UNTIL                                                    
192500            (BER-IY = BER-ANTAL-AA AND                                    
192600             BER-IX > BER-ANTAL-VV)                                       
192700         OR (BER-IY < BER-ANTAL-AA AND                                    
192800             BER-IX > 52)                                                 
192900                                                                          
193000           ADD  +1                   TO INDX-L                            
193100                                                                          
193200           COMPUTE W-NDC-KVBEHOV-VECKA ROUNDED                            
193300                                 = (UTUP-KVBEHOV-V (INDX-L) * 1)          
193400                                                                          
193500           MOVE W-DATUM-FORSTA-NDC   TO TMP1-YYWW                         
193600           MOVE W-BER-DATUM          TO TMP2-YYWW                         
193700           PERFORM WY2000P3                                               
193800           IF TMP1-YYWW <= TMP2-YYWW                                      
193900                                                                          
194000             IF W-JUST-PB-FINNS = NEJ                                     
194100                                                                          
194200               COMPUTE W-NDC-KVBEHOV-DAG ROUNDED                          
194300                                      = W-NDC-KVBEHOV-VECKA / 5           
194400               ADD W-NDC-KVBEHOV-DAG                                      
194500                                     TO W-NDC-ACC-KVBEHOV                 
194600               MOVE W-NDC-KVBEHOV-DAG                                     
194700                                     TO W-NDC-ACC-KVBEHOV-VECKA           
194800               COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                       
194900                             W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV           
195000                                                                          
195100               MOVE +1               TO IX                                
195200               PERFORM UNTIL IX > +4                                      
195300               OR W-NDC-KVAR-EFT-DAG  < SLAG-KVREFPKT                     
195400                 ADD +1              TO IX                                
195500                 ADD W-NDC-KVBEHOV-DAG                                    
195600                                     TO W-NDC-ACC-KVBEHOV                 
195700                                        W-NDC-ACC-KVBEHOV-VECKA           
195800                 COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                     
195900                             W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV           
196000                                                                          
196100               END-PERFORM                                                
196200               COMPUTE W-NDC-KVAR-KVBEHOV-VECKA ROUNDED =                 
196300                             W-NDC-KVBEHOV-VECKA -                        
196400                                        W-NDC-ACC-KVBEHOV-VECKA           
196500               ADD W-NDC-KVAR-KVBEHOV-VECKA                               
196600                                     TO W-NDC-ACC-KVBEHOV                 
196700                                                                          
196800               IF W-NDC-KVAR-EFT-DAG  < SLAG-KVREFPKT                     
196900                  COMPUTE W-NDC-DIFF  = SLAG-KVREFPKT -                   
197000                                             W-NDC-KVAR-EFT-DAG           
197100                  COMPUTE W-DC-REFILL-KVANT ROUNDED =                     
197200                                     W-NDC-DIFF + SLAG-KVREFBER           
197300                                                                          
197400                  PERFORM S24-ADJUST-LOW-DEMAND                           
197500                  IF DEMAND-ADJUST                                        
197600                     MOVE +1         TO W-DC-REFILL-KVANT                 
197700                  ELSE                                                    
197800                     PERFORM S08NDC-JUSTERA-REFILLKVANT                   
197900                     PERFORM S06-SATT-QX-PROCENT-BRYTNING                 
198000                     PERFORM S05-BERAKNA-Q-KVANT                          
198100                  END-IF                                                  
198200                  IF WS-DAPUBL      > ZERO                                
198300                    MOVE W-BINNDAY        TO TMP1-YYMMDD                  
198400                    MOVE WS-DAPUBL (3:6)  TO TMP2-YYMMDD                  
198500                    PERFORM WY2000P1                                      
198600                    IF TMP1-YYMMDD >= TMP2-YYMMDD                         
198700                      MOVE W-DC-REFILL-KVANT                              
198800                                     TO BER-BEHOV(BER-IY, BER-IX)         
198900                    ELSE                                                  
199000                      MOVE BER-IY                                         
199100                                     TO WS-BER-IY                         
199200                      MOVE BER-IX                                         
199300                                     TO WS-BER-IX                         
199400                      IF WS-BER-IX > 1                                    
199500                        SUBTRACT 1 FROM WS-BER-IX                         
199600                      ELSE                                                
199700                        IF WS-BER-IY  > 1                                 
199800*CC*                    IF WS-BER-IY  > ZERO                              
199900                          SUBTRACT 1 FROM WS-BER-IY                       
200000                          MOVE 52      TO WS-BER-IX                       
200100                        END-IF                                            
200200                      END-IF                                              
200300                      MOVE W-DC-REFILL-KVANT                              
200400                              TO BER-BEHOV(WS-BER-IY, WS-BER-IX)          
200500                    END-IF                                                
200600                                                                          
200700                  ELSE                                                    
200800                    MOVE W-DC-REFILL-KVANT                                
200900                              TO BER-BEHOV(BER-IY, BER-IX)                
201000                  END-IF                                                  
201100                  ADD W-DC-REFILL-KVANT                                   
201200                              TO W-NDC-TILLGANG                           
201300               END-IF                                                     
201400             ELSE                                                         
201500                                                                          
201600*******  GET REFILLING POINT AND REFILL QUANTITY                          
201700                                                                          
201800               INITIALIZE REFL1-REFL-W271REFL                             
201900                                                                          
202000               MOVE LINK-IDARTNR    TO REFL1-REFL-IDARTNR                 
202100               MOVE SLAG-IDDC       TO REFL1-REFL-IDDC                    
202200               MOVE SLAG-IDDC-REF   TO REFL1-REFL-IDDC-REF                
202300               MOVE SLAG-IDREFTAB   TO REFL1-REFL-IDREFTAB                
202400               MOVE SLAG-FLWILSON   TO REFL1-REFL-FLWILSON                
202500                                                                          
202600*******  BÅDE USA OCH CANADA SKALL HA MATERIALPRIS                        
202700               IF DCS-NDC-CN OR                                           
202800                  DCS-NDC-NA                                              
202900                 MOVE WS-PRMATRL    TO REFL1-REFL-PRARTBES                
203000               ELSE                                                       
203100                 MOVE CLAG-PRARTSTD TO REFL1-REFL-PRARTBES                
203200               END-IF                                                     
203300                                                                          
203400               MOVE SLAG-IDLEVNR    TO REFL1-REFL-IN-IDLEVNR-DC           
203500*---- GÄLLER EJ FÖR DCS-SDC OR DCS-NDC-CN, BARA NDC-NA/PF                 
203600               MOVE ZERO       TO REFL1-REFL-NDC-KVDAGAR-TBT-DC           
203700                                                                          
203800               MOVE SLAG-FLFLYG     TO REFL1-REFL-FLFLYG                  
203900                                                                          
204000               MOVE W-BINNDAY       TO REFL1-REFL-BINNDAY-TIAAMMDD        
204100               MOVE ZERO            TO REFL1-REFL-KVREFPKT                
204200                                       REFL1-REFL-KVREFBER                
204300               MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                        
204400               MOVE W-BINNDAY       TO TMP2-YYMMDD                        
204500                                                                          
204600               PERFORM WY2000P1                                           
204700               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
204800*                                                                         
204900*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
205000*                                                                         
205100                 MOVE SLAG-KVREFBER TO REFL1-REFL-IN-KVREFBER             
205200               ELSE                                                       
205300                 MOVE +0            TO REFL1-REFL-IN-KVREFBER             
205400               END-IF                                                     
205500                                                                          
205600               MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                        
205700               MOVE W-BINNDAY       TO TMP2-YYMMDD                        
205800*                                                                         
205900*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
206000*                                                                         
206100               PERFORM WY2000P1                                           
206200               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
206300*                                                                         
206400*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
206500*                                                                         
206600                 MOVE SLAG-KVREFPKT TO REFL1-REFL-IN-KVREFPKT             
206700               ELSE                                                       
206800                 MOVE +0            TO REFL1-REFL-IN-KVREFPKT             
206900               END-IF                                                     
207000*                                                                         
207100               MOVE UTUP-LT-BEHOV-V (INDX-L)                              
207200                                    TO REFL1-REFL-IN-LEADTID-BEHOV        
207300                                                                          
207400               CALL W271REFL USING REFL1-REFL-W271REFL                    
207500                                   REFL1-2501-PCB                         
207600                                   REFL1-WDB6-PCB                         
207700                                   REFL1-WDK7-PCB                         
207800                                   REFL1-UTIL-WDK6-PCB                    
207900                                   REFL1-UTIL-WDK7-PCB                    
208000                                   REFL1-UTIL-WDB6-PCB                    
208100                                                                          
208200               COMPUTE W-NDC-KVBEHOV-DAG ROUNDED =                        
208300                                         W-NDC-KVBEHOV-VECKA / 5          
208400               ADD W-NDC-KVBEHOV-DAG                                      
208500                                    TO W-NDC-ACC-KVBEHOV                  
208600               MOVE W-NDC-KVBEHOV-DAG                                     
208700                                    TO W-NDC-ACC-KVBEHOV-VECKA            
208800               COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                       
208900                              W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV          
209000                                                                          
209100               MOVE +1              TO IX                                 
209200               PERFORM UNTIL IX > +4                                      
209300               OR W-NDC-KVAR-EFT-DAG < REFL1-REFL-KVREFPKT                
209400                 ADD +1             TO IX                                 
209500                 ADD W-NDC-KVBEHOV-DAG                                    
209600                                    TO W-NDC-ACC-KVBEHOV                  
209700                                       W-NDC-ACC-KVBEHOV-VECKA            
209800                 COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                     
209900                              W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV          
210000               END-PERFORM                                                
210100               COMPUTE W-NDC-KVAR-KVBEHOV-VECKA ROUNDED =                 
210200                              W-NDC-KVBEHOV-VECKA -                       
210300                                         W-NDC-ACC-KVBEHOV-VECKA          
210400               ADD W-NDC-KVAR-KVBEHOV-VECKA                               
210500                                    TO W-NDC-ACC-KVBEHOV                  
210600                                                                          
210700               IF W-NDC-KVAR-EFT-DAG < REFL1-REFL-KVREFPKT                
210800                  COMPUTE W-NDC-DIFF = REFL1-REFL-KVREFPKT -              
210900                                              W-NDC-KVAR-EFT-DAG          
211000                  COMPUTE W-DC-REFILL-KVANT ROUNDED  =                    
211100                                W-NDC-DIFF + REFL1-REFL-KVREFBER          
211200                                                                          
211300                  PERFORM S24-ADJUST-LOW-DEMAND                           
211400                  IF DEMAND-ADJUST                                        
211500                     MOVE +1              TO W-DC-REFILL-KVANT            
211600                  ELSE                                                    
211700                     PERFORM S06-SATT-QX-PROCENT-BRYTNING                 
211800                     PERFORM S05-BERAKNA-Q-KVANT                          
211900                  END-IF                                                  
212000                  IF WS-DAPUBL   > ZERO                                   
212100                     MOVE W-BINNDAY       TO TMP1-YYMMDD                  
212200                     MOVE WS-DAPUBL (3:6) TO TMP2-YYMMDD                  
212300                     PERFORM WY2000P1                                     
212400                                                                          
212500                     IF TMP1-YYMMDD >= TMP2-YYMMDD                        
212600                       MOVE W-DC-REFILL-KVANT                             
212700                                     TO BER-BEHOV(BER-IY, BER-IX)         
212800                     ELSE                                                 
212900                       MOVE BER-IY                                        
213000                                     TO WS-BER-IY                         
213100                       MOVE BER-IX                                        
213200                                     TO WS-BER-IX                         
213300                       IF WS-BER-IX > 1                                   
213400                         SUBTRACT 1 FROM WS-BER-IX                        
213500                       ELSE                                               
213600                         IF WS-BER-IY > 1                                 
213700*AD                      IF WS-BER-IY > ZERO                              
213800                           SUBTRACT 1 FROM WS-BER-IY                      
213900                           MOVE 52      TO WS-BER-IX                      
214000                         END-IF                                           
214100                       END-IF                                             
214200                       MOVE W-DC-REFILL-KVANT                             
214300                               TO BER-BEHOV(WS-BER-IY, WS-BER-IX)         
214400                     END-IF                                               
214500                                                                          
214600                  ELSE                                                    
214700                     MOVE W-DC-REFILL-KVANT                               
214800                             TO BER-BEHOV(BER-IY, BER-IX)                 
214900                  END-IF                                                  
215000                  ADD W-DC-REFILL-KVANT                                   
215100                                        TO W-NDC-TILLGANG                 
215200               END-IF                                                     
215300             END-IF                                                       
215400           END-IF                                                         
215500                                                                          
215600           ADD 1                        TO BER-IX                         
215700                                           W-BER-DATUM                    
215800           MOVE +1                      TO W-ANTAL-VECKOR                 
215900           CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                      
216000                                                                          
216100           MOVE 'AAVV  '                TO DAT-KDDATFORM                  
216200           MOVE W-AAVV                  TO DAT-I-TIDATUM                  
216300                                                                          
216400           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
216500                           DAT-O-TIDATUM DAT-KDSVAR                       
216600                                                                          
216700           IF DAT-KDSVAR-OK                                               
216800             MOVE DAT-TIAARP(3:2)       TO W-PER                          
216900             MOVE DAT-TIAAMMDD          TO W-BINNDAY                      
217000           ELSE                                                           
217100               STRING ' FEL FRÅN WDATKONV7 I W222BHDC '                   
217200                      '(CB-, W-AAVV, BER-IY )'                            
217300               DELIMITED BY SIZE INTO FELTEXT-STR                         
217400               DISPLAY FELTEXT                                            
217500               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
217600           END-IF                                                         
217700                                                                          
217800         END-PERFORM                                                      
217900         ADD 1                     TO BER-IY                              
218000         MOVE 1                    TO BER-IX                              
218100                                      IX-PER                              
218200         SUBTRACT +52              FROM  W-BER-DATUM                      
218300         ADD +100                  TO    W-BER-DATUM                      
218400     END-PERFORM                                                          
218500     .                                                                    
218600     EJECT                                                                
218700 CC-LAES-WDK711-REF     SECTION.                                          
218800     MOVE 'CC-LAES-WDK711-REF  '    TO CURRENT-SECTION                    
218900                                                                          
219000******************************************************************        
219100*                                                                *        
219200*    LÄSER WDK711  FÖR DE LAGER SOM REFILLAS FRÅN AKTUELLT NDC.  *        
219300*    SLAG-IDDC-REF = NDC-CN  (KINA NDC'R)                        *        
219400*    SLAG-IDDC-REF = NDC-NA  (USA NDC'R EJ CANADA)               *        
219500*    VID ENDAST-LOKALBEHOV = BARA REFILL INOM EGNA LANDET.       *        
219600******************************************************************        
219700                                                                          
219800     IF LINK-KDBEHOV = ENDAST-LOKALBEHOV                                  
219900       PERFORM IMS-GNP-WDK711-REF                                         
220000                                                                          
220100       PERFORM UNTIL SEGMENT-SAKNAS                                       
220200       OR (SLAG-KDREFSTA = 'A'                                            
220300       AND SLAG-FLREFILL = JA)                                            
220400*--     LÄS FRAM TILL EN GILTIG LDC-/NDC-POST INOM LANDET                 
220500*--                                                                       
220600          PERFORM IMS-GNP-WDK711-REF                                      
220700       END-PERFORM                                                        
220800     ELSE                                                                 
220900       PERFORM IMS-GNP-WDK711-REF-XDC                                     
221000                                                                          
221100       PERFORM UNTIL SEGMENT-SAKNAS                                       
221200       OR (SLAG-KDREFSTA = 'A'                                            
221300       AND SLAG-FLREFILL = JA)                                            
221400*--     LÄS FRAM TILL EN GILTIG NDC-POST ALLA REFILL-DC                   
221500*--                                                                       
221600          PERFORM IMS-GNP-WDK711-REF-XDC                                  
221700       END-PERFORM                                                        
221800     END-IF                                                               
221900                                                                          
222000     IF SEGMENT-FINNS                                                     
222100*** FIX FÖR NEGATIVA KVOKS-BULK                                           
222200        IF SLAG-KVOKS-BULK < 0                                            
222300          MOVE ZERO          TO SLAG-KVOKS-BULK                           
222400        END-IF                                                            
222500*** FIX FÖR NEGATIVA KVOKS-DAG                                            
222600        IF SLAG-KVOKS-DAG < 0                                             
222700          MOVE ZERO          TO SLAG-KVOKS-DAG                            
222800        END-IF                                                            
222900                                                                          
223000        MOVE SLAG-IDDC TO W-IDDC-B6                                       
223100        PERFORM S13-READ-OR-TAB-B601                                      
223200                                                                          
223300*** KINA OCH JAPAN HAR 6 DAGARS ARBETSVECKA                               
223400        MOVE NEJ TO 6ARBDAG-SW                                            
223500        IF DCS-CHINA OR DCS-JAPAN                                         
223600          MOVE JA TO 6ARBDAG-SW                                           
223700        END-IF                                                            
223800     END-IF                                                               
223900     .                                                                    
224000     EJECT                                                                
224100 CD-JUSTERA-NDC-TILLGANG  SECTION.                                        
224200     MOVE 'CD-JUSTERA-NDC-TILLGANG' TO CURRENT-SECTION                    
224300                                                                          
224400     IF WS-ERS-FINNS-K611 = JA                                            
224500       IF ERS-CLAG-KDERS = 01 OR 11 OR 17 OR 21 OR 27                     
224600          MOVE ERS-ART-IDARTNR TO W-IDARTNR-K7-ERS                        
224700          MOVE SLAG-IDDC       TO W-IDDC-ERS                              
224800          PERFORM IMS-GU-WDK711-ERS                                       
224900          IF  SEGMENT-FINNS                                               
225000             COMPUTE W-NDC-TILLGANG-ERS ROUNDED =                         
225100                     ERS-SLAG-KVLS                                        
225200                  +  ERS-SLAG-KVAKS-PAV                                   
225300                  +  ERS-SLAG-KVAKS-SDC                                   
225400                  +  ERS-SLAG-KVBEART                                     
225500                  -  ERS-SLAG-KVROS-BULK                                  
225600                  -  ERS-SLAG-KVROS-DAG                                   
225700                  -  ERS-SLAG-KVRESS                                      
225800                  -  ERS-SLAG-KVOKS-BULK                                  
225900                  -  ERS-SLAG-KVOKS-DAG                                   
226000             ADD W-NDC-TILLGANG-ERS TO W-NDC-TILLGANG                     
226100          END-IF                                                          
226200       END-IF                                                             
226300     END-IF                                                               
226400     .                                                                    
226500     EJECT                                                                
226600 D-BERAKNA-CDCBEHOV  SECTION.                                             
226700     MOVE 'D-BERAKNA-CDCBEHOV '  TO CURRENT-SECTION                       
226800                                                                          
226900******************************************************************        
227000*                                                                *        
227100*    BERÄKNING AV CDC'S BEHOV AV EN ARTIKEL SOM REFILLAS FRÅN    *        
227200*    ETT NDC.IDAG KINA NDC-CN/USA NDC-US, MEN KAN KOMMA FLER.    *        
227300*    BEHOVET VISAS DEN VECKA NDC (MED LOKAL ANSKAFFNING) BEHÖVER *        
227400*    LEVERERA, DVS BEHOVSDAG PÅ CDC MINUS LEDTID                 *        
227500*                                                                *        
227600*    BEHOVET AV ALLA UNDERLIGGANDE LDC, SDC OCH NDC SOM REFILLAS *        
227700*    FRÅN CDC.                                                   *        
227800*    MAN GÅR NER EN NIVÅ BARA.ANVÄND PB-PLAN FÖR DC11(=REOI DC71)*        
227900*                                                                *        
228000*    CDC REFILLAS FRÅN ETT NDC => REFILLSEGMENT WDK629 FINNS.    *        
228100*    PUB-DATUM KOLLAS PÅ NDC KINA/US FÖR REFILL-ARTIKLAR PÅ CDC. *        
228200*    MAN MÅSTE STÄLLA TILLBAKA LÄSNING AV B601 TILL CDC IGEN.    *        
228300******************************************************************        
228400                                                                          
228500     PERFORM S20-NOLLSTALL-W-CDC-BEHOV-REF                                
228600     MOVE WS-BER-TAB-NOLL     TO BER-TAB                                  
228700                                                                          
228800     MOVE JA                  TO CDCBEHOV-SW                              
228900     ACCEPT W-TIME         FROM TIME                                      
229000                                                                          
229100     IF (CLAG-FLREFILL = JA                                               
229200     OR (CLAG-FLREFILL = NEJ AND ART-FLIART = JA))                        
229300     AND CLAG-REDIRLEV < 1.00                                             
229400     AND CLAG-KDERS    < 07                                               
229500                                                                          
229600       IF WDK629-FINNS                                                    
229700       AND CREF-IDDC-REF = LINK-IDDC                                      
229800       AND CREF-FLREFILL = JA                                             
229900       AND (CREF-KDREFSTA = 'A'                                           
230000       OR  (CREF-KDREFSTA = 'P' AND ART-FLIART = 'J'))                    
230100                                                                          
230200         MOVE LINK-IDDC TO W-IDDC-B6                                      
230300         PERFORM S13-READ-OR-TAB-B601                                     
230400         PERFORM S77-KOLLA-DAPUBL                                         
230500         MOVE ZERO                 TO WS-DAPUBL                           
230600         MOVE WC-CDC-SE TO W-IDDC-B6                                      
230700         PERFORM S13-READ-OR-TAB-B601                                     
230800                                                                          
230900         PERFORM DD-HAEMTA-TIERS-PREL                                     
231000                                                                          
231100         MOVE JA TO  CDC-ERSDAT-PREL-OK-SW                                
231200         IF WS-TIERSDAT-PREL-AAVV > ZERO                                  
231300           PERFORM DE-KOLLA-BEHOV-ERSDAT-PREL                             
231400         END-IF                                                           
231500         IF CDC-ERSDAT-PREL-OK-SW = JA                                    
231600                                                                          
231700           MOVE CREF-FLFLYG           TO WS-FLFLYG                        
231800           PERFORM S80-CALC-LT-ADJ-PUBWK                                  
231900                                                                          
232000           MOVE W-TIFINLV-AAVV-MINUS-LT                                   
232100                             TO W-DATUM-FORSTA-CDC                        
232200                                                                          
232300           MOVE 'AAVV  '     TO DAT-KDDATFORM                             
232400           MOVE LINK-TIAAVV-AKTUELL                                       
232500                                 TO DAT-I-TIDATUM                         
232600                                    W-BINNDAY-AAVV                        
232700                                                                          
232800           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
232900                           DAT-O-TIDATUM DAT-KDSVAR                       
233000                                                                          
233100           IF DAT-KDSVAR-OK                                               
233200             MOVE DAT-TIAARP(3:2) TO W-PER                                
233300             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
233400             MOVE DAT-TIAAVVD     TO W-TIAAVVD                            
233500           ELSE                                                           
233600             STRING ' FEL FRÅN WDATKONV3 I W222BHDC '                     
233700                    '(D-, LINK-TIAAVVD)'                                  
233800             DELIMITED BY SIZE INTO FELTEXT-STR                           
233900             DISPLAY FELTEXT                                              
234000             CALL ABEND USING RKOD-ABEND-UTAN-DUMP                        
234100           END-IF                                                         
234200                                                                          
234300*----- TILLGÅNGAR TOTALT PÅ CDC.                                          
234400           IF LINK-KVTILLG-TOT-CDC = ZERO                                 
234500              MOVE CLAG-KVTILLG-TOT     TO W-CDC-TILLGANG                 
234600           ELSE                                                           
234700              MOVE LINK-KVTILLG-TOT-CDC TO W-CDC-TILLGANG                 
234800           END-IF                                                         
234900                                                                          
235000***        CHECK IF FFC OR SEASON EXISTS FOR ANY DC                       
235100***        CALL W271UTIL                                                  
235200           PERFORM S25-FINNS-JUST-PB-CDC                                  
235300*                                                                         
235400***        CALL DEMAND MODULE W22222 TO GET WEEKLY DEMAND                 
235500***        AND CALCULATE CUMULATIVE DEMAND                                
235600***        FROM CURR WEEK                                                 
235700           PERFORM S27-BEHOV-VECKA                                        
235800*                                                                         
235900           MOVE +1        TO INDX-L                                       
236000           MOVE ZERO      TO INDX1-L                                      
236100*                                                                         
236200           MOVE ZERO      TO W-CDC-KVBEHOV-DESSUTOM                       
236300           IF LINK-TID-AKTUELL = 5                                        
236400           OR LINK-TID-AKTUELL > 5                                        
236500              COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED =                     
236600                                  W-BEHOV-LT-VECKA(INDX-L) * 1            
236700           ELSE                                                           
236800              PERFORM DA-BERAKNA-INNEV-VECKA-CDC                          
236900              ADD W-CDC-KVBEHOV-DESSUTOM                                  
237000                          TO LINK-KVBEHOV-DESSUTOM                        
237100           END-IF                                                         
237200                                                                          
237300           PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                           
237400           PERFORM DB-CDCBEHOV                                            
237500           PERFORM S03-ADD-TILL-RESULTAT-TAB                              
237600                                                                          
237700         END-IF                                                           
237800       END-IF                                                             
237900     END-IF                                                               
238000     .                                                                    
238100     EJECT                                                                
238200 DA-BERAKNA-INNEV-VECKA-CDC SECTION.                                      
238300     MOVE 'DA-BERAKNA-INNEV-VECKA-CDC ' TO CURRENT-SECTION                
238400                                                                          
238500******************************************************************        
238600*                                                                *        
238700*    BERÄKNING AV CDC'S BEHOV AV EN ARTIKEL I INNEVARANDE        *        
238800*    VECKA.       (REFILL FRÅN NDC-CN/NDC-US                     *        
238900*                                                                *        
239000*    DEFAULTVÄRDEN FÖR CREF-RESEASON-PLAN ÄR 1.00 ( REFILL-LOGIK)*        
239100******************************************************************        
239200                                                                          
239300     INITIALIZE REFL-W272REFL                                             
239400                                                                          
239500     MOVE W-DATUM-FORSTA-CDC    TO TMP1-YYWW                              
239600     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
239700     PERFORM WY2000P3                                                     
239800     IF TMP1-YYWW <= TMP2-YYWW                                            
239900        COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED =                           
240000                                  W-BEHOV-LT-VECKA(INDX-L) * 1            
240100                                                                          
240200        COMPUTE W-CDC-KVAR-EFT-VECKA ROUNDED                              
240300                                     = W-CDC-TILLGANG                     
240400                                     - W-KVBEHOV-INNEV-VECKA              
240500                                                                          
240600        MOVE ZERO                   TO WS-REST-DAYS-ADJ                   
240700        MOVE WS-REST-DAYS-LT-F      TO WS-REST-DAYS-ADJ                   
240800                                                                          
240900        IF W-JUST-PB-FINNS = NEJ                                          
241000                                                                          
241100          IF W-CDC-KVAR-EFT-VECKA    < CREF-KVREFPKT                      
241200             COMPUTE W-CDC-DIFF      = CREF-KVREFPKT                      
241300                                     - W-CDC-TILLGANG                     
241400             COMPUTE W-DC-REFILL-KVANT ROUNDED                            
241500                                     = W-CDC-DIFF + CLAG-KVQ              
241600             MOVE WC-CDC-SE         TO W-IDDC-B6                          
241700             PERFORM S13-READ-OR-TAB-B601                                 
241800             PERFORM S06-SATT-QX-PROCENT-BRYTNING                         
241900             PERFORM S05-BERAKNA-Q-KVANT                                  
242000                                                                          
242100             MOVE W-DC-REFILL-KVANT TO W-CDC-KVBEHOV-DESSUTOM             
242200          ELSE                                                            
242300             MOVE ZERO              TO W-CDC-KVBEHOV-DESSUTOM             
242400          END-IF                                                          
242500        ELSE                                                              
242600                                                                          
242700          ADD WS-LEADTIME-WEEKS-PLUS1                                     
242800                                TO INDX1-L                                
242900          PERFORM S28-TOTAL-LEATIME-NEED                                  
243000                                                                          
243100*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
243200          MOVE LINK-IDARTNR     TO REFL-IDARTNR                           
243300          MOVE WC-CDC-SE        TO REFL-IDDC                              
243400          MOVE CREF-IDDC-REF    TO REFL-IDDC-REF                          
243500          MOVE CREF-IDREFTAB    TO REFL-IDREFTAB                          
243600          MOVE CREF-FLREFBEO    TO REFL-FLREFBEO                          
243700          MOVE CREF-FLWILSON    TO REFL-FLWILSON                          
243800          MOVE CLAG-PRARTSTD    TO REFL-PRARTBES                          
243900                                                                          
244000          MOVE CREF-FLFLYG   TO REFL-FLFLYG                               
244100          MOVE W-BINNDAY     TO REFL-BINNDAY-TIAAMMDD                     
244200          MOVE ZERO          TO REFL-KVREFPKT                             
244300                                REFL-KVREFBER                             
244400                                                                          
244500          MOVE WS-LEDTIDSBEHOV                                            
244600                             TO REFL-IN-LEADTID-BEHOV                     
244700                                                                          
244800          MOVE CREF-TIREFPAF TO TMP1-YYMMDD                               
244900          MOVE W-BINNDAY     TO TMP2-YYMMDD                               
245000*                                                                         
245100*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
245200*                                                                         
245300          PERFORM WY2000P1                                                
245400          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
245500*                                                                         
245600*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
245700*                                                                         
245800            MOVE CLAG-KVQ    TO REFL-IN-KVREFBER                          
245900          ELSE                                                            
246000            MOVE +0          TO REFL-IN-KVREFBER                          
246100          END-IF                                                          
246200                                                                          
246300          MOVE CREF-TIREFPKT TO TMP1-YYMMDD                               
246400          MOVE W-BINNDAY     TO TMP2-YYMMDD                               
246500*                                                                         
246600*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
246700*                                                                         
246800          PERFORM WY2000P1                                                
246900          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
247000*                                                                         
247100*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
247200*                                                                         
247300            MOVE CREF-KVREFPKT TO REFL-IN-KVREFPKT                        
247400          ELSE                                                            
247500            MOVE +0            TO REFL-IN-KVREFPKT                        
247600          END-IF                                                          
247700                                                                          
247800          CALL W272REFL USING REFL-W272REFL REFL2-2501-PCB                
247900                              REFL2-WDB6-PCB                              
248000                              REFL2-UTIL-WDK6-PCB                         
248100                              REFL2-UTIL-WDK7-PCB                         
248200                              REFL2-UTIL-WDB6-PCB                         
248300                                                                          
248400          IF W-CDC-KVAR-EFT-VECKA    < REFL-KVREFPKT                      
248500             COMPUTE W-CDC-DIFF      = REFL-KVREFPKT                      
248600                                     - W-CDC-TILLGANG                     
248700             COMPUTE W-DC-REFILL-KVANT ROUNDED                            
248800                                     = W-CDC-DIFF                         
248900                                     + REFL-KVREFBER                      
249000             MOVE WC-CDC-SE         TO W-IDDC-B6                          
249100             PERFORM S13-READ-OR-TAB-B601                                 
249200             PERFORM S06-SATT-QX-PROCENT-BRYTNING                         
249300             PERFORM S05-BERAKNA-Q-KVANT                                  
249400             MOVE W-DC-REFILL-KVANT TO W-CDC-KVBEHOV-DESSUTOM             
249500          ELSE                                                            
249600             MOVE ZERO              TO W-CDC-KVBEHOV-DESSUTOM             
249700          END-IF                                                          
249800        END-IF                                                            
249900     ELSE                                                                 
250000        MOVE ZERO                   TO W-CDC-KVBEHOV-DESSUTOM             
250100     END-IF                                                               
250200     .                                                                    
250300     EJECT                                                                
250400 DB-CDCBEHOV  SECTION.                                                    
250500     MOVE 'DB-CDCBEHOV    '    TO CURRENT-SECTION                         
250600                                                                          
250700******************************************************************        
250800*                                                                *        
250900*    BERÄKNING AV CDC'S BEHOV AV EN ARTIKEL I KOMMANDE           *        
251000*    VECKOR.  (REFILL FRÅN NDC-CN/NDC-US)                        *        
251100*                                                                *        
251200******************************************************************        
251300     MOVE ZERO                  TO W-CDC-ACC-KVBEHOV                      
251400     ADD W-CDC-KVBEHOV-DESSUTOM TO W-CDC-TILLGANG                         
251500     ADD W-KVBEHOV-INNEV-VECKA  TO W-CDC-ACC-KVBEHOV                      
251600                                                                          
251700     MOVE ZERO                  TO WS-REST-DAYS-ADJ                       
251800     MOVE +1                    TO BER-IY                                 
251900     MOVE W-BER-START-VV        TO BER-IX                                 
252000     MOVE WS-REST-DAYS-LT-V     TO WS-REST-DAYS-ADJ                       
252100     MOVE WS-LEADTIME-WEEKS-VECKA                                         
252200                                TO WS-LEADTIME-WEEKS-PLUS1                
252300                                   INDX1-L                                
252400     MOVE LINK-TIBEHOV-START    TO W-BER-DATUM                            
252500                                   W-AAVV                                 
252600     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
252700     MOVE W-AAVV                TO DAT-I-TIDATUM                          
252800                                   W-BINNDAY-AAVV                         
252900                                                                          
253000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
253100                         DAT-O-TIDATUM DAT-KDSVAR                         
253200                                                                          
253300     IF DAT-KDSVAR-OK                                                     
253400       MOVE DAT-TIAARP(3:2)     TO W-PER                                  
253500       MOVE DAT-TIAAMMDD        TO W-BINNDAY                              
253600     ELSE                                                                 
253700       STRING ' FEL FRÅN WDATKONV1 I W222BHDC '                           
253800              '(DB-, LINK-TIBEHOV)'                                       
253900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
254000       DISPLAY FELTEXT                                                    
254100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
254200     END-IF                                                               
254300                                                                          
254400     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
254500                                                                          
254600         PERFORM UNTIL                                                    
254700            (BER-IY = BER-ANTAL-AA AND                                    
254800             BER-IX > BER-ANTAL-VV)                                       
254900         OR (BER-IY < BER-ANTAL-AA AND                                    
255000             BER-IX > 52)                                                 
255100                                                                          
255200           ADD  +1                   TO INDX-L                            
255300*                                                                         
255400           MOVE W-DATUM-FORSTA-CDC   TO TMP1-YYWW                         
255500           MOVE W-BER-DATUM          TO TMP2-YYWW                         
255600           PERFORM WY2000P3                                               
255700           IF TMP1-YYWW <= TMP2-YYWW                                      
255800                                                                          
255900             MOVE JA TO  CDC-ERSDAT-PREL-OK-SW                            
256000             IF WS-TIERSDAT-PREL-AAVV   > ZERO                            
256100               PERFORM DBA-KOLLA-BEHOV-ERSDAT-PREL                        
256200             END-IF                                                       
256300             IF CDC-ERSDAT-PREL-OK-SW = JA                                
256400                                                                          
256500               IF W-JUST-PB-FINNS = NEJ                                   
256600                 COMPUTE W-CDC-KVBEHOV-DAG ROUNDED                        
256700                                = W-BEHOV-LT-VECKA(INDX-L) / 5            
256800                 ADD  W-CDC-KVBEHOV-DAG                                   
256900                               TO W-CDC-ACC-KVBEHOV                       
257000                 MOVE W-CDC-KVBEHOV-DAG                                   
257100                               TO W-CDC-ACC-KVBEHOV-VECKA                 
257200                 COMPUTE W-CDC-KVAR-EFT-DAG ROUNDED =                     
257300                            W-CDC-TILLGANG - W-CDC-ACC-KVBEHOV            
257400                                                                          
257500                 MOVE +1       TO IX                                      
257600                 PERFORM UNTIL IX > +4                                    
257700                 OR W-CDC-KVAR-EFT-DAG < CREF-KVREFPKT                    
257800                   ADD +1      TO IX                                      
257900                   ADD W-CDC-KVBEHOV-DAG                                  
258000                               TO W-CDC-ACC-KVBEHOV                       
258100                                  W-CDC-ACC-KVBEHOV-VECKA                 
258200                   COMPUTE W-CDC-KVAR-EFT-DAG ROUNDED =                   
258300                            W-CDC-TILLGANG - W-CDC-ACC-KVBEHOV            
258400                                                                          
258500                 END-PERFORM                                              
258600                 COMPUTE W-CDC-KVAR-KVBEHOV-VECKA ROUNDED                 
258700                                = W-BEHOV-LT-VECKA(INDX-L)                
258800                                - W-CDC-ACC-KVBEHOV-VECKA                 
258900                 ADD W-CDC-KVAR-KVBEHOV-VECKA                             
259000                               TO W-CDC-ACC-KVBEHOV                       
259100                                                                          
259200                 IF W-CDC-KVAR-EFT-DAG   < CREF-KVREFPKT                  
259300                    COMPUTE W-CDC-DIFF   = CREF-KVREFPKT                  
259400                                         - W-CDC-KVAR-EFT-DAG             
259500                    COMPUTE W-DC-REFILL-KVANT ROUNDED                     
259600                                         = W-CDC-DIFF                     
259700                                         + CLAG-KVQ                       
259800                    MOVE WC-CDC-SE      TO W-IDDC-B6                      
259900                    PERFORM S13-READ-OR-TAB-B601                          
260000                    PERFORM S06-SATT-QX-PROCENT-BRYTNING                  
260100                    PERFORM S05-BERAKNA-Q-KVANT                           
260200                    IF WS-DAPUBL > ZERO                                   
260300                      MOVE W-BINNDAY        TO TMP1-YYMMDD                
260400                      MOVE WS-DAPUBL (3:6)  TO TMP2-YYMMDD                
260500                      PERFORM WY2000P1                                    
260600                      IF TMP1-YYMMDD >= TMP2-YYMMDD                       
260700                        MOVE W-DC-REFILL-KVANT                            
260800                              TO BER-BEHOV(BER-IY, BER-IX)                
260900                      ELSE                                                
261000                        MOVE BER-IY                                       
261100                              TO WS-BER-IY                                
261200                        MOVE BER-IX                                       
261300                              TO WS-BER-IX                                
261400                        IF WS-BER-IX > 1                                  
261500                          SUBTRACT 1 FROM WS-BER-IX                       
261600                        ELSE                                              
261700                          IF WS-BER-IY > 1                                
261800*AD                       IF WS-BER-IY > ZERO                             
261900                            SUBTRACT 1 FROM WS-BER-IY                     
262000                            MOVE 52     TO WS-BER-IX                      
262100                          END-IF                                          
262200                        END-IF                                            
262300                        IF WS-BER-IY > 0                                  
262400                        MOVE W-DC-REFILL-KVANT TO                         
262500                             BER-BEHOV(WS-BER-IY, WS-BER-IX)              
262600                        END-IF                                            
262700                      END-IF                                              
262800                                                                          
262900                    ELSE                                                  
263000                      MOVE W-DC-REFILL-KVANT                              
263100                              TO BER-BEHOV(BER-IY, BER-IX)                
263200                    END-IF                                                
263300                    ADD W-DC-REFILL-KVANT                                 
263400                               TO W-CDC-TILLGANG                          
263500                 END-IF                                                   
263600               ELSE                                                       
263700                                                                          
263800*******  GET TOTAL DEMAND FOR LEAD TIME - FROM W22222                     
263900                 ADD +1                TO INDX1-L                         
264000                 PERFORM S28-TOTAL-LEATIME-NEED                           
264100                                                                          
264200*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
264300                 MOVE LINK-IDARTNR     TO REFL-IDARTNR                    
264400                 MOVE WC-CDC-SE        TO REFL-IDDC                       
264500                 MOVE CREF-IDDC-REF    TO REFL-IDDC-REF                   
264600                 MOVE CREF-IDREFTAB    TO REFL-IDREFTAB                   
264700                 MOVE CREF-FLREFBEO    TO REFL-FLREFBEO                   
264800                 MOVE CREF-FLWILSON    TO REFL-FLWILSON                   
264900                 MOVE CLAG-PRARTSTD    TO REFL-PRARTBES                   
265000                                                                          
265100                 MOVE CREF-FLFLYG      TO REFL-FLFLYG                     
265200                 MOVE W-BINNDAY        TO REFL-BINNDAY-TIAAMMDD           
265300                                                                          
265400                 MOVE ZERO             TO REFL-KVREFPKT                   
265500                                          REFL-KVREFBER                   
265600                                                                          
265700                 MOVE WS-LEDTIDSBEHOV  TO REFL-IN-LEADTID-BEHOV           
265800                                                                          
265900                 MOVE CREF-TIREFPAF    TO TMP1-YYMMDD                     
266000                 MOVE W-BINNDAY        TO TMP2-YYMMDD                     
266100*                                                                         
266200*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
266300*                                                                         
266400                 PERFORM WY2000P1                                         
266500                 IF TMP1-YYMMDD >= TMP2-YYMMDD                            
266600*                                                                         
266700*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
266800*                                                                         
266900                   MOVE CLAG-KVQ        TO REFL-IN-KVREFBER               
267000                 ELSE                                                     
267100                   MOVE +0              TO REFL-IN-KVREFBER               
267200                 END-IF                                                   
267300                                                                          
267400                 MOVE CREF-TIREFPKT     TO TMP1-YYMMDD                    
267500                 MOVE W-BINNDAY         TO TMP2-YYMMDD                    
267600*                                                                         
267700*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
267800*                                                                         
267900                 PERFORM WY2000P1                                         
268000                 IF TMP1-YYMMDD >= TMP2-YYMMDD                            
268100*                                                                         
268200*-----   MANUELL PÅFYLLNADSPUNKT ÄR SATT                                  
268300*                                                                         
268400                   MOVE CREF-KVREFPKT   TO REFL-IN-KVREFPKT               
268500                                                                          
268600                 ELSE                                                     
268700                   MOVE +0              TO REFL-IN-KVREFPKT               
268800                 END-IF                                                   
268900                                                                          
269000                 CALL W272REFL USING REFL-W272REFL REFL2-2501-PCB         
269100                                     REFL2-WDB6-PCB                       
269200                                     REFL2-UTIL-WDK6-PCB                  
269300                                     REFL2-UTIL-WDK7-PCB                  
269400                                     REFL2-UTIL-WDB6-PCB                  
269500                                                                          
269600                 COMPUTE W-CDC-KVBEHOV-DAG ROUNDED =                      
269700                            W-BEHOV-LT-VECKA(INDX-L) / 5                  
269800                 ADD W-CDC-KVBEHOV-DAG                                    
269900                               TO W-CDC-ACC-KVBEHOV                       
270000                 MOVE W-CDC-KVBEHOV-DAG                                   
270100                               TO W-CDC-ACC-KVBEHOV-VECKA                 
270200                 COMPUTE W-CDC-KVAR-EFT-DAG ROUNDED =                     
270300                          W-CDC-TILLGANG - W-CDC-ACC-KVBEHOV              
270400                                                                          
270500                 MOVE +1       TO IX                                      
270600                 PERFORM UNTIL IX > +4                                    
270700                 OR W-CDC-KVAR-EFT-DAG < REFL-KVREFPKT                    
270800                   ADD +1      TO IX                                      
270900                   ADD W-CDC-KVBEHOV-DAG                                  
271000                               TO W-CDC-ACC-KVBEHOV                       
271100                                  W-CDC-ACC-KVBEHOV-VECKA                 
271200                   COMPUTE W-CDC-KVAR-EFT-DAG ROUNDED =                   
271300                         W-CDC-TILLGANG - W-CDC-ACC-KVBEHOV               
271400                                                                          
271500                 END-PERFORM                                              
271600                 COMPUTE W-CDC-KVAR-KVBEHOV-VECKA ROUNDED =               
271700                             W-BEHOV-LT-VECKA(INDX-L) -                   
271800                                    W-CDC-ACC-KVBEHOV-VECKA               
271900                 ADD W-CDC-KVAR-KVBEHOV-VECKA                             
272000                                         TO W-CDC-ACC-KVBEHOV             
272100                 IF W-CDC-KVAR-EFT-DAG    < REFL-KVREFPKT                 
272200                    COMPUTE W-CDC-DIFF    = REFL-KVREFPKT                 
272300                                          - W-CDC-KVAR-EFT-DAG            
272400                    COMPUTE W-DC-REFILL-KVANT ROUNDED                     
272500                                          = W-CDC-DIFF                    
272600                                          + REFL-KVREFBER                 
272700                                                                          
272800                    MOVE WC-CDC-SE       TO W-IDDC-B6                     
272900                    PERFORM S13-READ-OR-TAB-B601                          
273000                    PERFORM S06-SATT-QX-PROCENT-BRYTNING                  
273100                    PERFORM S05-BERAKNA-Q-KVANT                           
273200                    IF WS-DAPUBL   > ZERO                                 
273300                       MOVE W-BINNDAY       TO TMP1-YYMMDD                
273400                       MOVE WS-DAPUBL (3:6) TO TMP2-YYMMDD                
273500                       PERFORM WY2000P1                                   
273600                                                                          
273700                       IF TMP1-YYMMDD >= TMP2-YYMMDD                      
273800                         MOVE W-DC-REFILL-KVANT                           
273900                               TO BER-BEHOV(BER-IY, BER-IX)               
274000                       ELSE                                               
274100                         MOVE BER-IY                                      
274200                               TO WS-BER-IY                               
274300                         MOVE BER-IX                                      
274400                               TO WS-BER-IX                               
274500                         IF WS-BER-IX        > 1                          
274600                           SUBTRACT 1     FROM WS-BER-IX                  
274700                         ELSE                                             
274800                           IF WS-BER-IY      > 1                          
274900*AD                        IF WS-BER-IY      > ZERO                       
275000                             SUBTRACT 1   FROM WS-BER-IY                  
275100                             MOVE 52        TO WS-BER-IX                  
275200                           END-IF                                         
275300                         END-IF                                           
275400                         MOVE W-DC-REFILL-KVANT TO                        
275500                                BER-BEHOV(WS-BER-IY, WS-BER-IX)           
275600                       END-IF                                             
275700                                                                          
275800                    ELSE                                                  
275900                       MOVE W-DC-REFILL-KVANT                             
276000                               TO BER-BEHOV(BER-IY, BER-IX)               
276100                    END-IF                                                
276200                    ADD W-DC-REFILL-KVANT                                 
276300                                        TO W-CDC-TILLGANG                 
276400                 END-IF                                                   
276500               END-IF                                                     
276600             END-IF                                                       
276700           END-IF                                                         
276800                                                                          
276900           ADD 1                  TO BER-IX                               
277000                                     W-BER-DATUM                          
277100           MOVE +1           TO W-ANTAL-VECKOR                            
277200           CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                      
277300                                                                          
277400           MOVE 'AAVV  '     TO DAT-KDDATFORM                             
277500           MOVE W-AAVV       TO DAT-I-TIDATUM                             
277600                                W-BINNDAY-AAVV                            
277700                                                                          
277800           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
277900                           DAT-O-TIDATUM DAT-KDSVAR                       
278000                                                                          
278100           IF DAT-KDSVAR-OK                                               
278200             MOVE DAT-TIAARP(3:2) TO W-PER                                
278300             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
278400           ELSE                                                           
278500               STRING ' FEL FRÅN WDATKONV9 I W222BHDC '                   
278600                      '(DB-, W-AAVV, BER-IY )'                            
278700               DELIMITED BY SIZE INTO FELTEXT-STR                         
278800               DISPLAY FELTEXT                                            
278900               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
279000           END-IF                                                         
279100                                                                          
279200         END-PERFORM                                                      
279300         ADD 1                     TO BER-IY                              
279400         MOVE 1                    TO BER-IX                              
279500                                      IX-PER                              
279600         SUBTRACT +52              FROM  W-BER-DATUM                      
279700         ADD +100                  TO    W-BER-DATUM                      
279800     END-PERFORM                                                          
279900     .                                                                    
280000     EJECT                                                                
280100 DBA-KOLLA-BEHOV-ERSDAT-PREL SECTION.                                     
280200     MOVE 'DBA-KOLLA-BEHOV-ERSDAT-PREL '  TO CURRENT-SECTION              
280300                                                                          
280400     IF CLAG-KDERS = 03 OR 06                                             
280500       MOVE WS-TIERSDAT-PREL-AAVV   TO TMP1-YYWW                          
280600       MOVE W-BER-DATUM             TO TMP2-YYWW                          
280700       PERFORM WY2000P3                                                   
280800       IF TMP1-YYWW <= TMP2-YYWW                                          
280900         MOVE NEJ TO  CDC-ERSDAT-PREL-OK-SW                               
281000       END-IF                                                             
281100     END-IF                                                               
281200                                                                          
281300     IF CLAG-KDERS = 01 OR 02 OR 04 OR 05                                 
281400       PERFORM S34-BER-IX-AKTUELLT-DC                                     
281500       IF CREF-FLFLYG = JA                                                
281600         MOVE REF-KVDLTID-AIRETA TO DAYS-KVDAYS                           
281700       ELSE                                                               
281800         MOVE REF-KVDLTID-TOT    TO DAYS-KVDAYS                           
281900       END-IF                                                             
282000                                                                          
282100       MOVE WS-TIERSDAT-PREL-AAVVD   TO DAYS-TIDATE1                      
282200       MOVE 'YYWWD'                  TO DAYS-KDDATFMT1                    
282300       MOVE 'YYWWD'                  TO DAYS-KDDATFMT2                    
282400       MOVE SPACE                    TO DAYS-TIDATE2                      
282500                                        DAYS-IDCALEND                     
282600                                                                          
282700       CALL WZ20DAYS USING DAYS-WZ20DAYS                                  
282800                                                                          
282900       IF DAYS-KDRC = 8                                                   
283000         STRING 'FEL VID ANROP TILL WZ20DAYS 2'                           
283100         DELIMITED BY SIZE INTO FELTEXT-STR                               
283200         DISPLAY FELTEXT                                                  
283300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
283400       ELSE                                                               
283500         MOVE DAYS-TIDATE2(1:4)       TO TMP1-YYWW                        
283600         MOVE W-BER-DATUM             TO TMP2-YYWW                        
283700         PERFORM WY2000P3                                                 
283800         IF TMP1-YYWW <= TMP2-YYWW                                        
283900           MOVE NEJ TO  CDC-ERSDAT-PREL-OK-SW                             
284000         END-IF                                                           
284100       END-IF                                                             
284200     END-IF                                                               
284300                                                                          
284400     .                                                                    
284500     EJECT                                                                
284600 DD-HAEMTA-TIERS-PREL  SECTION.                                           
284700     MOVE 'DD-HAEMTA-TIERS-PREL '  TO CURRENT-SECTION                     
284800*--------------------------------------------------------------           
284900*---- WDD704-TIERSDAT-PREL HAR ALLTID D=1.                                
285000*--------------------------------------------------------------           
285100                                                                          
285200     MOVE ZERO     TO WS-TIERSDAT-PREL-AAVVD                              
285300                      WS-TIERSDAT-PREL-AAVV                               
285400                                                                          
285500     IF CLAG-KDERS = 03 OR 06                                             
285600       PERFORM IMS-GU-WDD704                                              
285700       IF SEGMENT-FINNS                                                   
285800         IF D704-TIERSDAT-PREL-C1 > ZERO                                  
285900           MOVE D704-TIERSDAT-PREL-C1  TO WS-TIERSDAT-PREL-AAVVD          
286000           MOVE WS-TIERSDAT-PREL-AAVVD(1:4) TO                            
286100                                          WS-TIERSDAT-PREL-AAVV           
286200         END-IF                                                           
286300       END-IF                                                             
286400     END-IF                                                               
286500     IF CLAG-KDERS = 01 OR 02 OR 04 OR 05                                 
286600       IF CLAG-TISTOREF > ZERO                                            
286700         MOVE CLAG-TISTOREF            TO WS-TIERSDAT-PREL-AAVVD          
286800         MOVE WS-TIERSDAT-PREL-AAVVD(1:4) TO                              
286900                                          WS-TIERSDAT-PREL-AAVV           
287000       END-IF                                                             
287100     END-IF                                                               
287200     .                                                                    
287300     EJECT                                                                
287400 DE-KOLLA-BEHOV-ERSDAT-PREL  SECTION.                                     
287500     MOVE 'DE-KOLLA-BEHOV-ERSDAT-PREL '  TO CURRENT-SECTION               
287600                                                                          
287700     IF CLAG-KDERS = 03 OR 06                                             
287800       MOVE WS-TIERSDAT-PREL-AAVV   TO TMP1-YYWW                          
287900       MOVE LINK-TIAAVV-AKTUELL     TO TMP2-YYWW                          
288000       PERFORM WY2000P3                                                   
288100       IF TMP1-YYWW <= TMP2-YYWW                                          
288200         MOVE NEJ TO  CDC-ERSDAT-PREL-OK-SW                               
288300       END-IF                                                             
288400     END-IF                                                               
288500                                                                          
288600     IF CLAG-KDERS = 01 OR 02 OR 04 OR 05                                 
288700       PERFORM S34-BER-IX-AKTUELLT-DC                                     
288800       IF CREF-FLFLYG = JA                                                
288900         MOVE REF-KVDLTID-AIRETA     TO DAYS-KVDAYS                       
289000       ELSE                                                               
289100         MOVE REF-KVDLTID-TOT        TO DAYS-KVDAYS                       
289200       END-IF                                                             
289300                                                                          
289400       MOVE WS-TIERSDAT-PREL-AAVVD   TO DAYS-TIDATE1                      
289500       MOVE 'YYWWD'                  TO DAYS-KDDATFMT1                    
289600       MOVE 'YYWWD'                  TO DAYS-KDDATFMT2                    
289700       MOVE SPACE                    TO DAYS-TIDATE2                      
289800                                        DAYS-IDCALEND                     
289900                                                                          
290000       CALL WZ20DAYS USING DAYS-WZ20DAYS                                  
290100                                                                          
290200       IF DAYS-KDRC = 8                                                   
290300         STRING 'FEL VID ANROP TILL WZ20DAYS 3'                           
290400         DELIMITED BY SIZE INTO FELTEXT-STR                               
290500         DISPLAY FELTEXT                                                  
290600         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
290700       ELSE                                                               
290800         MOVE DAYS-TIDATE2(1:4)       TO TMP1-YYWW                        
290900         MOVE LINK-TIAAVV-AKTUELL     TO TMP2-YYWW                        
291000         PERFORM WY2000P3                                                 
291100         IF TMP1-YYWW <= TMP2-YYWW                                        
291200           MOVE NEJ TO  CDC-ERSDAT-PREL-OK-SW                             
291300         END-IF                                                           
291400       END-IF                                                             
291500     END-IF                                                               
291600                                                                          
291700     .                                                                    
291800     EJECT                                                                
291900 DF-BERAKNA-CDC-SATSBEHOV SECTION.                                        
292000     MOVE 'DF-BERAKNA-CDC-SATSBEHOV '  TO CURRENT-SECTION                 
292100******************************************************************        
292200*                                                                *        
292300*    SATSBEHOV PER VECKA LÄSES                                   *        
292400*        - FÖRE BEGÄRD STARTVECKA: SUMMERING TILL                *        
292500*          LINK-KVBEHOV-DESSUTOM-SKALL EJ GÖRAS HÄR.SE W2222200  *        
292600*        - INOM BEGÄRT TIDSINTERVALL: FLYTTA TILL BER-TAB        *        
292700*                                                                *        
292800******************************************************************        
292900                                                                          
293000     PERFORM IMS-GNP-WDK624-FIRST                                         
293100                                                                          
293200     PERFORM UNTIL SEGMENT-SAKNAS                                         
293300       MOVE SATS-TIBEHOV-SATS          TO WS-SATS-TPO-BEHOV-AAVV          
293400       PERFORM S30-BER-SATS-TPO-LEDTIDSBEHOV                              
293500       MOVE WS-BEHOVSVECKA-AAVV        TO TMP1-YYWW                       
293600                                                                          
293700       MOVE W-BER-SLUT-DATUM           TO TMP2-YYWW                       
293800       PERFORM WY2000P3                                                   
293900       IF TMP1-YYWW <= TMP2-YYWW                                          
294000                                                                          
294100          MOVE WS-BEHOVSVECKA-AAVV     TO TMP1-YYWW                       
294200          MOVE W-TIFINLV-AAVV          TO TMP2-YYWW                       
294300          MOVE LINK-TIBEHOV-START      TO TMP3-YYWW                       
294400          PERFORM WY2000Q3                                                
294500          IF TMP1-YYWW < TMP3-YYWW                                        
294600          OR (TMP1-YYWW < TMP2-YYWW AND TMP2-YYWW < TMP3-YYWW)            
294700              CONTINUE                                                    
294800          ELSE                                                            
294900              MOVE WS-BEHOVSVECKA-AAVV TO W-DATUM                         
295000              PERFORM S04-BERAKNA-BERTAB-INDEX                            
295100              ADD SATS-KVBEHOV-TOTSATS TO                                 
295200                                   RES-BEHOV (BER-IY, BER-IX)             
295300          END-IF                                                          
295400       END-IF                                                             
295500                                                                          
295600       PERFORM IMS-GNP-WDK624-NEXT                                        
295700     END-PERFORM                                                          
295800     .                                                                    
295900     EJECT                                                                
296000 DG-BERAKNA-CDC-TPO-BEHOV SECTION.                                        
296100     MOVE 'DG-BERAKNA-CDC-TPO-BEHOV ' TO CURRENT-SECTION                  
296200******************************************************************        
296300*                                                                *        
296400*    BERÄKNING AV DIVERSEORDERBEHOV/TPO AV ARTIKEL               *        
296500*    SE D-BERAKNA-DIVERSEORDER-BEHOV FÖR CDC W2222200            *        
296600******************************************************************        
296700                                                                          
296800     PERFORM IMS-GU-WDK901                                                
296900                                                                          
297000     IF  SEGMENT-FINNS                                                    
297100     AND WDK9-ART-SUTPO-TOT > ZERO                                        
297200                                                                          
297300       PERFORM IMS-GNP-WDK911                                             
297400                                                                          
297500       IF SEGMENT-FINNS                                                   
297600           PERFORM DGA-DIVERSEORDER-BERAKNING                             
297700       END-IF                                                             
297800     END-IF                                                               
297900                                                                          
298000     .                                                                    
298100     EJECT                                                                
298200 DGA-DIVERSEORDER-BERAKNING SECTION.                                      
298300     MOVE 'DGA-DIVERSEORDER-BERAKNING'  TO CURRENT-SECTION                
298400******************************************************************        
298500*                                                                *        
298600*    DIVERSEORDER SALDO PER VECKA LÄSES                          *        
298700*        - FÖRE BEGÄRD STARTVECKA: SUMMERING TILL                *        
298800*          LINK-KVBEHOV-DESSUTOM                                 *        
298900*        - OM FÖRSTA INLEV.DAT < START.DATUM: SUMMERING TILL     *        
299000*          LINK-KVBEHOV-DESSUTOM - CDC KOLLAR BARA PÅ TIFINLV    *        
299100*          W2222200                                              *        
299200*          OM WDK9-ANT-DABEHOV < W-TIFINLV-AAVV: SUMMERING TILL  *        
299300*          LINK-KVBEHOV-DESSUTOM                                 *        
299400*        - INOM BEGÄRT TIDSINTERVALL: FLYTTA TILL BER-TAB        *        
299500*                                                                *        
299600******************************************************************        
299700     SKIP1                                                                
299800     PERFORM UNTIL SEGMENT-SAKNAS                                         
299900       MOVE WDK9-ANT-DABEHOV (3:4)   TO WS-SATS-TPO-BEHOV-AAVV            
300000       PERFORM S30-BER-SATS-TPO-LEDTIDSBEHOV                              
300100       MOVE WS-BEHOVSVECKA-AAVV      TO TMP1-YYWW                         
300200                                                                          
300300       MOVE W-BER-SLUT-DATUM         TO TMP2-YYWW                         
300400       PERFORM WY2000P3                                                   
300500       IF TMP1-YYWW <= TMP2-YYWW                                          
300600                                                                          
300700         MOVE WS-BEHOVSVECKA-AAVV    TO TMP1-YYWW                         
300800         MOVE W-TIFINLV-AAVV         TO TMP2-YYWW                         
300900         MOVE LINK-TIBEHOV-START     TO TMP3-YYWW                         
301000         PERFORM WY2000Q3                                                 
301100         IF  TMP1-YYWW   < TMP3-YYWW                                      
301200         OR (TMP1-YYWW   < TMP2-YYWW AND TMP2-YYWW < TMP3-YYWW)           
301300             ADD WDK9-ANT-SUTPO-PB                                        
301400                               TO LINK-KVBEHOV-DESSUTOM                   
301500             ADD WDK9-ANT-SUTPO-EJPB                                      
301600                               TO LINK-KVBEHOV-DESSUTOM                   
301700         ELSE                                                             
301800             MOVE WS-BEHOVSVECKA-AAVV    TO W-DATUM-AAVV                  
301900             MOVE -1                     TO W-ANTAL-VECKOR                
302000             CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR              
302100             MOVE W-DATUM-AAVV TO W-DATUM                                 
302200                                                                          
302300             PERFORM S04-BERAKNA-BERTAB-INDEX                             
302400             ADD WDK9-ANT-SUTPO-PB                                        
302500                                    TO RES-BEHOV (BER-IY, BER-IX)         
302600             ADD WDK9-ANT-SUTPO-EJPB                                      
302700                                    TO RES-BEHOV (BER-IY, BER-IX)         
302800         END-IF                                                           
302900       END-IF                                                             
303000                                                                          
303100       PERFORM IMS-GNP-WDK911                                             
303200     END-PERFORM                                                          
303300     .                                                                    
303400     EJECT                                                                
303500 E-FLYTTA-RESULT-TILL-LINKAREA SECTION.                                   
303600     MOVE 'E-FLYTTA-RESULT-TILL-LINKAREA' TO CURRENT-SECTION              
303700     SKIP3                                                                
303800                                                                          
303900     MOVE 1              TO IX                                            
304000                                                                          
304100*--- FÖR ATT KOMMA RÄTT I TRENDVÄRDE I BEHOVSVECKORNA SÅ BÖRJAR           
304200*--- MAN MED TRENDVÄRDET PÅ INNEVARANDE VECKA (KVBEHOV-DESSUTOM).         
304300*--- RAD 2 PÅ 2428 VISAR VÄRDET FÖR TRENDVECKA 2 OSV.                     
304400     IF LINK-TIAAVV-AKTUELL = LINK-TIBEHOV-START                          
304500       MOVE 1            TO INDX-T                                        
304600     ELSE                                                                 
304700       MOVE 2            TO INDX-T                                        
304800     END-IF                                                               
304900                                                                          
305000     MOVE +1             TO RES-IY                                        
305100     MOVE W-BER-START-VV TO RES-IX                                        
305200     MOVE ZERO TO W-KVBEHOV-SUMMA                                         
305300     MOVE SPAR-XLAG-KVVECKOR-TREND    TO MAX-KVVECKOR-TREND               
305400     SKIP1                                                                
305500     PERFORM UNTIL RES-IY > RES-ANTAL-AA                                  
305600     SKIP1                                                                
305700         PERFORM UNTIL                                                    
305800            (RES-IY = RES-ANTAL-AA AND                                    
305900             RES-IX > RES-ANTAL-VV)                                       
306000         OR (RES-IY < RES-ANTAL-AA AND                                    
306100             RES-IX > 52)                                                 
306200     SKIP1                                                                
306300                  MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                    
306400                  ADD  RES-BEHOV (RES-IY, RES-IX)                         
306500                              TO LINK-KVBEHOV-VECKA (IX) ROUNDED          
306600                                                                          
306700                  IF (LINK-KDBEHOV = PB-TOTAL-SEP-LEV-XDC)                
306800                     AND TREND-SW = JA                                    
306900                     AND SPAR-XLAG-KVPB-TREND NOT = ZERO                  
307000                     IF INDX-T = 2 AND IX = 1                             
307100                       PERFORM EB-ADDERA-TREND-INNEV-VECKA                
307200                       PERFORM EA-ADDERA-TREND                            
307300                     ELSE                                                 
307400                       PERFORM EA-ADDERA-TREND                            
307500                     END-IF                                               
307600                  END-IF                                                  
307700                  ADD  LINK-KVBEHOV-VECKA (IX)                            
307800                              TO W-KVBEHOV-SUMMA                          
307900                  ADD 1 TO IX                                             
308000                           RES-IX                                         
308100                           INDX-T                                         
308200         END-PERFORM                                                      
308300         ADD 1 TO RES-IY                                                  
308400         MOVE 1 TO RES-IX                                                 
308500     END-PERFORM                                                          
308600     SKIP1                                                                
308700     MOVE ZERO TO LINK-KVBEHOV-SUMMA                                      
308800     ADD W-KVBEHOV-SUMMA TO LINK-KVBEHOV-SUMMA                            
308900     PERFORM UNTIL IX > MAX-BEHOVS-VECKOR                                 
309000         MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                             
309100         ADD 1 TO IX                                                      
309200     END-PERFORM                                                          
309300     .                                                                    
309400     EJECT                                                                
309500 EA-ADDERA-TREND       SECTION.                                           
309600     MOVE 'EA-ADDERA-TREND   '  TO CURRENT-SECTION                        
309700                                                                          
309800******************************************************************        
309900*                                                                *        
310000*    BERÄKNING AV TRENDBEHOV                                     *        
310100*                                                                *        
310200*    OBS BEHOVET EFTER TREND EJ MINDRE ÄN HÄLFTEN FÖRE           *        
310300******************************************************************        
310400                                                                          
310500        COMPUTE SPAR-KVBEHOV-VECKA ROUNDED =                              
310600           LINK-KVBEHOV-VECKA (IX) / 2                                    
310700        IF INDX-T NOT < MAX-KVVECKOR-TREND                                
310800           MOVE MAX-KVVECKOR-TREND TO INDX-T                              
310900        END-IF                                                            
311000        IF WS-DAGENS-DAGNR > 5 OR                                         
311100          (WS-DAGENS-DAGNR = 5 AND WS-DAGENS-TIMME > 17)                  
311200*          VECKOHELG                                                      
311300           COMPUTE WS-KVPB-TREND ROUNDED =                                
311400              INDX-T * SPAR-XLAG-KVPB-TREND / 4.33                        
311500           END-COMPUTE                                                    
311600           ADD WS-KVPB-TREND  TO                                          
311700                    LINK-KVBEHOV-VECKA (IX)                               
311800        ELSE                                                              
311900*          MITT I VECKAN ELLER TRENDVÄRDET I VECKA 2                      
312000           COMPUTE WS-KVPB-TREND ROUNDED =                                
312100              INDX-T * SPAR-XLAG-KVPB-TREND / 4.33                        
312200           END-COMPUTE                                                    
312300           IF (IX = 1) AND (INDX-T = 1)                                   
312400              IF WS-DAGENS-DAGNR = 2                                      
312500                 COMPUTE WS-KVPB-TREND ROUNDED =                          
312600                         WS-KVPB-TREND * 0.80                             
312700              END-IF                                                      
312800              IF WS-DAGENS-DAGNR = 3                                      
312900                 COMPUTE WS-KVPB-TREND ROUNDED =                          
313000                         WS-KVPB-TREND * 0.60                             
313100              END-IF                                                      
313200              IF WS-DAGENS-DAGNR = 4                                      
313300                 COMPUTE WS-KVPB-TREND ROUNDED =                          
313400                         WS-KVPB-TREND * 0.40                             
313500              END-IF                                                      
313600              IF WS-DAGENS-DAGNR = 5                                      
313700                 COMPUTE WS-KVPB-TREND ROUNDED =                          
313800                         WS-KVPB-TREND * 0.20                             
313900              END-IF                                                      
314000           END-IF                                                         
314100           ADD WS-KVPB-TREND  TO                                          
314200                    LINK-KVBEHOV-VECKA (IX)                               
314300        END-IF                                                            
314400                                                                          
314500        IF LINK-KVBEHOV-VECKA (IX) < ZERO                                 
314600           MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                           
314700        END-IF                                                            
314800                                                                          
314900        IF LINK-KVBEHOV-VECKA (IX) < SPAR-KVBEHOV-VECKA                   
315000           MOVE SPAR-KVBEHOV-VECKA TO LINK-KVBEHOV-VECKA (IX)             
315100        END-IF                                                            
315200     .                                                                    
315300     EJECT                                                                
315400 EB-ADDERA-TREND-INNEV-VECKA  SECTION.                                    
315500     MOVE 'EB-ADDERA-TREND-INNEV-VECKA   '  TO CURRENT-SECTION            
315600                                                                          
315700******************************************************************        
315800*                                                                *        
315900*    BERÄKNING AV TRENDBEHOV FÖRSTA VECKAN PÅ 2428               *        
316000*                                                                *        
316100*    OBS BEHOVET EFTER TREND EJ MINDRE ÄN HÄLFTEN FÖRE           *        
316200******************************************************************        
316300                                                                          
316400     MOVE 1    TO INDX-T                                                  
316500                                                                          
316600     COMPUTE SPAR-KVBEHOV-DESSUTOM ROUNDED =                              
316700        LINK-KVBEHOV-DESSUTOM  / 2                                        
316800     IF INDX-T NOT < MAX-KVVECKOR-TREND                                   
316900        MOVE MAX-KVVECKOR-TREND TO INDX-T                                 
317000     END-IF                                                               
317100                                                                          
317200     IF WS-DAGENS-DAGNR > 5 OR                                            
317300       (WS-DAGENS-DAGNR = 5 AND WS-DAGENS-TIMME > 17)                     
317400*----   VECKOHELG                                                         
317500        COMPUTE WS-KVPB-TREND ROUNDED =                                   
317600           INDX-T * SPAR-XLAG-KVPB-TREND / 4.33                           
317700        END-COMPUTE                                                       
317800        ADD WS-KVPB-TREND  TO                                             
317900                 LINK-KVBEHOV-DESSUTOM                                    
318000     ELSE                                                                 
318100*----   MITT I VECKAN                                                     
318200        COMPUTE WS-KVPB-TREND ROUNDED =                                   
318300           INDX-T * SPAR-XLAG-KVPB-TREND / 4.33                           
318400        END-COMPUTE                                                       
318500        IF IX = 1                                                         
318600           IF WS-DAGENS-DAGNR = 2                                         
318700              COMPUTE WS-KVPB-TREND ROUNDED =                             
318800                      WS-KVPB-TREND * 0.80                                
318900           END-IF                                                         
319000           IF WS-DAGENS-DAGNR = 3                                         
319100              COMPUTE WS-KVPB-TREND ROUNDED =                             
319200                      WS-KVPB-TREND * 0.60                                
319300           END-IF                                                         
319400           IF WS-DAGENS-DAGNR = 4                                         
319500              COMPUTE WS-KVPB-TREND ROUNDED =                             
319600                      WS-KVPB-TREND * 0.40                                
319700           END-IF                                                         
319800           IF WS-DAGENS-DAGNR = 5                                         
319900              COMPUTE WS-KVPB-TREND ROUNDED =                             
320000                      WS-KVPB-TREND * 0.20                                
320100           END-IF                                                         
320200        END-IF                                                            
320300        ADD WS-KVPB-TREND  TO                                             
320400                 LINK-KVBEHOV-DESSUTOM                                    
320500     END-IF                                                               
320600                                                                          
320700     IF LINK-KVBEHOV-DESSUTOM < ZERO                                      
320800        MOVE ZERO TO LINK-KVBEHOV-DESSUTOM                                
320900     END-IF                                                               
321000                                                                          
321100     IF LINK-KVBEHOV-DESSUTOM < SPAR-KVBEHOV-DESSUTOM                     
321200        MOVE SPAR-KVBEHOV-DESSUTOM TO LINK-KVBEHOV-DESSUTOM               
321300     END-IF                                                               
321400                                                                          
321500*---- SÄTT TILLBAKA INDX-T TILL 2 IGEN FÖR ANDRA VECKANS BEHOV.           
321600     MOVE 2     TO INDX-T                                                 
321700                                                                          
321800     .                                                                    
321900     EJECT                                                                
322000 F-BERAKNA-GLOBAL-DCBEHOV SECTION.                                        
322100     MOVE 'F-BERAKNA-GLOBAL-DCBEHOV '  TO CURRENT-SECTION                 
322200******************************************************************        
322300*                                                                *        
322400*    BERÄKNING AV GLOBALA EXPORT NDC'ERNAS BEHOV AV EN ARTIKEL   *        
322500*    BEHOVET VISAS DEN VECKA NDC (MED LOKAL ANSKAFFNING) BEHÖVER *        
322600*    LEVERERA, DVS BEHOVSDAG PÅ GDC MINUS LEDTID                 *        
322700*                                                                *        
322800*    BEHOVET AV ALLA UNDERLIGGANDE DC SOM REFILLAS FRÅN ETT NDC. *        
322900*    MAN GÅR NER EN NIVÅ BARA.                                   *        
323000******************************************************************        
323100                                                                          
323200     MOVE NEJ TO WS-ERS-FINNS-K611                                        
323300                                                                          
323400     MOVE WS-BER-TAB-NOLL     TO BER-TAB                                  
323500                                                                          
323600     ACCEPT W-TIME          FROM TIME                                     
323700                                                                          
323800     IF CLAG-REDIRLEV < 1.00                                              
323900       IF ART-FLERS = JA                                                  
324000         MOVE ART-IDARTNR     TO W-IDARTNR-D7-MIN                         
324100                                 W-IDARTNR-D7-MAX                         
324200         PERFORM IMS-GU-WDD7A1                                            
324300         IF SEGMENT-FINNS                                                 
324400           MOVE ERS-IDARTNR   TO W-IDARTNR-ERS                            
324500           PERFORM IMS-GU-WDK601-ERS                                      
324600           IF SEGMENT-FINNS                                               
324700             PERFORM IMS-GNP-WDK611-ERS                                   
324800                                                                          
324900             MOVE JA          TO WS-ERS-FINNS-K611                        
325000           END-IF                                                         
325100                                                                          
325200*----   VI BEHÖVER ÅTERSTÄLLA POSITIONEN I BASEN                          
325300           MOVE LINK-IDARTNR  TO W-IDARTNR                                
325400           PERFORM IMS-GU-WDK601                                          
325500         END-IF                                                           
325600       END-IF                                                             
325700                                                                          
325800       PERFORM IMS-GU-WDK701                                              
325900       IF SEGMENT-FINNS                                                   
326000         MOVE LINK-IDDC(1:1)  TO W-IDDC-NON                               
326100         MOVE LINK-IDDC       TO W-IDDC-REF                               
326200         PERFORM FC-LAES-WDK711-REF-GLOBAL                                
326300                                                                          
326400         PERFORM UNTIL SEGMENT-SAKNAS                                     
326500                                                                          
326600           PERFORM S22-NOLLSTALL-W-GDC-BEHOV-REF                          
326700           PERFORM S77-KOLLA-DAPUBL                                       
326800           MOVE PUBL-DAPUBL(PUBL-IX)  TO WS-DAPUBL                        
326900           MOVE PUBL-PRMATRL(PUBL-IX) TO WS-PRMATRL                       
327000           MOVE SLAG-FLFLYG           TO WS-FLFLYG                        
327100           PERFORM S80-CALC-LT-ADJ-PUBWK                                  
327200                                                                          
327300           IF  WS-DAPUBL > ZERO                                           
327400           AND WS-DAPUBL > WS-DAGENS-DATUM                                
327500             MOVE WS-LT-WEEKS-DAYS TO DAG-KVKALDAG                        
327600             MOVE WS-DAPUBL (3:6)                                         
327700                       TO DAG-TIAAMMDD-TOM                                
327800             MOVE 003         TO DAG-KDCALL                               
327900             CALL WDAGKONV USING DAG-KDCALL                               
328000                                 DAG-DATUM-AREA                           
328100                                 DAG-KDSVAR                               
328200             IF DAG-KDSVAR = SPACE                                        
328300               CONTINUE                                                   
328400             ELSE                                                         
328500               STRING 'FEL FRÅN WDAGKONV I W222BHDC '                     
328600               'F- SECTION (WS-DAPUBL)' DELIMITED BY SIZE                 
328700                         INTO         FELTEXT-STR                         
328800                DISPLAY FELTEXT                                           
328900                CALL ABEND USING RKOD-ABEND-UTAN-DUMP                     
329000             END-IF                                                       
329100                                                                          
329200             MOVE DAG-TIAAMMDD-FOM                                        
329300                              TO DAT-I-TIDATUM                            
329400             MOVE 'AAMMDD'    TO DAT-KDDATFORM                            
329500                                                                          
329600             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
329700                                 DAT-O-TIDATUM DAT-KDSVAR                 
329800                                                                          
329900             IF DAT-KDSVAR-OK                                             
330000                MOVE DAT-TIAAVV-GRP                                       
330100                              TO W-TIAAVV                                 
330200                MOVE W-TIAAVV                                             
330300                              TO W-DATUM-FORSTA-GDC                       
330400             ELSE                                                         
330500                 STRING ' FEL FRÅN WDATKONV11 I W222BHDC '                
330600                        '(F-, DAG-TIAAMMDD)'                              
330700                 DELIMITED BY SIZE INTO FELTEXT-STR                       
330800                 DISPLAY FELTEXT                                          
330900                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
331000             END-IF                                                       
331100           ELSE                                                           
331200             MOVE W-TIFINLV-AAVV-MINUS-LT                                 
331300                              TO W-DATUM-FORSTA-GDC                       
331400           END-IF                                                         
331500                                                                          
331600           MOVE 'AAVV  '      TO DAT-KDDATFORM                            
331700           MOVE LINK-TIAAVV-AKTUELL                                       
331800                              TO DAT-I-TIDATUM                            
331900                                                                          
332000           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
332100                           DAT-O-TIDATUM DAT-KDSVAR                       
332200                                                                          
332300           IF DAT-KDSVAR-OK                                               
332400             MOVE DAT-TIAARP(3:2) TO W-PER                                
332500             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
332600           ELSE                                                           
332700                 STRING ' FEL FRÅN WDATKONV12 I W222BHDC '                
332800                        '(F-, LINK-TIAAVVD)'                              
332900                 DELIMITED BY SIZE INTO FELTEXT-STR                       
333000                 DISPLAY FELTEXT                                          
333100                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
333200           END-IF                                                         
333300*                                                                         
333400           PERFORM S23-GET-WEEK-DEMAND-XDC                                
333500                                                                          
333600           MOVE +1                TO INDX-L                               
333700*                                                                         
333800           COMPUTE W-GDC-TILLGANG = SLAG-KVLS +                           
333900                                    SLAG-KVAKS-PAV +                      
334000                                    SLAG-KVAKS-SDC +                      
334100                                    SLAG-KVBEART -                        
334200                                    SLAG-KVOKS-BULK -                     
334300                                    SLAG-KVOKS-DAG -                      
334400                                    SLAG-KVROS-BULK -                     
334500                                    SLAG-KVROS-DAG -                      
334600                                    SLAG-KVSPARR-KVAL                     
334700           COMPUTE W-KVBEHOV-52V ROUNDED =                                
334800                   (SLAG-KVPB-REF + SLAG-KVPBREOI) * 12                   
334900                                                                          
335000           PERFORM FD-JUSTERA-GDC-TILLGANG                                
335100           MOVE ZERO         TO W-GDC-KVBEHOV-DESSUTOM                    
335200                                                                          
335300           IF LINK-TID-AKTUELL = 5                                        
335400           OR LINK-TID-AKTUELL > 5                                        
335500              CONTINUE                                                    
335600           ELSE                                                           
335700              PERFORM FA-BERAKNA-INNEV-VECKA-GDC                          
335800              ADD W-GDC-KVBEHOV-DESSUTOM                                  
335900                             TO LINK-KVBEHOV-DESSUTOM                     
336000           END-IF                                                         
336100                                                                          
336200           PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                           
336300           PERFORM FB-GDCBEHOV                                            
336400           PERFORM S03-ADD-TILL-RESULTAT-TAB                              
336500                                                                          
336600           PERFORM FC-LAES-WDK711-REF-GLOBAL                              
336700         END-PERFORM                                                      
336800       END-IF                                                             
336900     END-IF                                                               
337000     .                                                                    
337100     EJECT                                                                
337200 FA-BERAKNA-INNEV-VECKA-GDC SECTION.                                      
337300     MOVE 'FA-BERAKNA-INNEV-VECKA-GDC ' TO CURRENT-SECTION                
337400                                                                          
337500******************************************************************        
337600*                                                                *        
337700*    BERÄKNING AV GDC'ERNAS BEHOV AV EN ARTIKEL I INNEVARANDE    *        
337800*    VECKA.       (REFILLADE GLOBALA EXPORT NDC'R                *        
337900*                                                                *        
338000******************************************************************        
338100                                                                          
338200     MOVE W-DATUM-FORSTA-GDC    TO TMP1-YYWW                              
338300     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
338400     PERFORM WY2000P3                                                     
338500     IF TMP1-YYWW <= TMP2-YYWW                                            
338600*                                                                         
338700*****   WEEKLY DEMAND ALREADY ADJUSTED FOR NUMBER OF DAYS                 
338800*****   IN CURRENT WEEK                                                   
338900*                                                                         
339000        COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED                             
339100                                = (UTUP-KVBEHOV-V (INDX-L) * 1)           
339200                                                                          
339300        COMPUTE W-GDC-KVAR-EFT-VECKA ROUNDED =                            
339400                          W-GDC-TILLGANG - W-KVBEHOV-INNEV-VECKA          
339500                                                                          
339600        IF W-JUST-PB-FINNS = NEJ                                          
339700                                                                          
339800          IF W-GDC-KVAR-EFT-VECKA   < SLAG-KVREFPKT                       
339900             COMPUTE W-GDC-DIFF     = SLAG-KVREFPKT -                     
340000                                                  W-GDC-TILLGANG          
340100             COMPUTE W-DC-REFILL-KVANT ROUNDED =                          
340200                     W-GDC-DIFF + SLAG-KVREFBER                           
340300                                                                          
340400             PERFORM S24-ADJUST-LOW-DEMAND                                
340500             IF DEMAND-ADJUST                                             
340600                MOVE +1             TO W-DC-REFILL-KVANT                  
340700             ELSE                                                         
340800                PERFORM S08GDC-JUSTERA-REFILLKVANT                        
340900                PERFORM S06-SATT-QX-PROCENT-BRYTNING                      
341000                PERFORM S05-BERAKNA-Q-KVANT                               
341100             END-IF                                                       
341200             MOVE W-DC-REFILL-KVANT TO W-GDC-KVBEHOV-DESSUTOM             
341300          ELSE                                                            
341400             MOVE ZERO              TO W-GDC-KVBEHOV-DESSUTOM             
341500          END-IF                                                          
341600        ELSE                                                              
341700                                                                          
341800*******  GET REFILLING POINT AND REFILL QUANTITY                          
341900                                                                          
342000          INITIALIZE REFL1-REFL-W271REFL                                  
342100                                                                          
342200          MOVE LINK-IDARTNR    TO REFL1-REFL-IDARTNR                      
342300          MOVE SLAG-IDDC       TO REFL1-REFL-IDDC                         
342400          MOVE SLAG-IDDC-REF   TO REFL1-REFL-IDDC-REF                     
342500          MOVE SLAG-IDREFTAB   TO REFL1-REFL-IDREFTAB                     
342600          MOVE SLAG-FLWILSON   TO REFL1-REFL-FLWILSON                     
342700                                                                          
342800*******  BÅDE USA OCH CANADA SKALL HA MATERIALPRIS                        
342900          IF DCS-NDC-CN OR                                                
343000             DCS-NDC-NA                                                   
343100            MOVE WS-PRMATRL    TO REFL1-REFL-PRARTBES                     
343200          ELSE                                                            
343300            MOVE CLAG-PRARTSTD TO REFL1-REFL-PRARTBES                     
343400          END-IF                                                          
343500                                                                          
343600          MOVE SLAG-IDLEVNR    TO REFL1-REFL-IN-IDLEVNR-DC                
343700*---- GÄLLER EJ FÖR DCS-SDC OR DCS-NDC-CN, BARA NDC-NA/PF                 
343800          MOVE ZERO            TO REFL1-REFL-NDC-KVDAGAR-TBT-DC           
343900          MOVE SLAG-FLFLYG     TO REFL1-REFL-FLFLYG                       
344000                                                                          
344100          MOVE W-BINNDAY       TO REFL1-REFL-BINNDAY-TIAAMMDD             
344200          MOVE ZERO            TO REFL1-REFL-KVREFPKT                     
344300                                  REFL1-REFL-KVREFBER                     
344400          MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                             
344500          MOVE W-BINNDAY       TO TMP2-YYMMDD                             
344600*                                                                         
344700*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
344800*                                                                         
344900          PERFORM WY2000P1                                                
345000          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
345100*                                                                         
345200*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
345300*                                                                         
345400            MOVE SLAG-KVREFBER TO REFL1-REFL-IN-KVREFBER                  
345500          ELSE                                                            
345600            MOVE +0            TO REFL-IN-KVREFBER                        
345700          END-IF                                                          
345800                                                                          
345900          MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                             
346000          MOVE W-BINNDAY       TO TMP2-YYMMDD                             
346100*                                                                         
346200*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
346300*                                                                         
346400          PERFORM WY2000P1                                                
346500          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
346600*                                                                         
346700*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
346800*                                                                         
346900            MOVE SLAG-KVREFPKT TO REFL1-REFL-IN-KVREFPKT                  
347000          ELSE                                                            
347100            MOVE +0            TO REFL1-REFL-IN-KVREFPKT                  
347200          END-IF                                                          
347300*                                                                         
347400          MOVE UTUP-LT-BEHOV-V (INDX-L)                                   
347500                               TO REFL1-REFL-IN-LEADTID-BEHOV             
347600                                                                          
347700          CALL W271REFL USING REFL1-REFL-W271REFL                         
347800                              REFL1-2501-PCB                              
347900                              REFL1-WDB6-PCB                              
348000                              REFL1-WDK7-PCB                              
348100                              REFL1-UTIL-WDK6-PCB                         
348200                              REFL1-UTIL-WDK7-PCB                         
348300                              REFL1-UTIL-WDB6-PCB                         
348400                                                                          
348500          IF W-GDC-KVAR-EFT-VECKA    < REFL1-REFL-KVREFPKT                
348600             COMPUTE W-GDC-DIFF      = REFL1-REFL-KVREFPKT -              
348700                                                 W-GDC-TILLGANG           
348800                                                                          
348900             COMPUTE W-DC-REFILL-KVANT ROUNDED =                          
349000                               W-GDC-DIFF + REFL1-REFL-KVREFBER           
349100                                                                          
349200             PERFORM S24-ADJUST-LOW-DEMAND                                
349300             IF DEMAND-ADJUST                                             
349400                MOVE +1             TO W-DC-REFILL-KVANT                  
349500             ELSE                                                         
349600                PERFORM S06-SATT-QX-PROCENT-BRYTNING                      
349700                PERFORM S05-BERAKNA-Q-KVANT                               
349800             END-IF                                                       
349900             MOVE W-DC-REFILL-KVANT TO W-GDC-KVBEHOV-DESSUTOM             
350000          ELSE                                                            
350100             MOVE ZERO              TO W-GDC-KVBEHOV-DESSUTOM             
350200          END-IF                                                          
350300        END-IF                                                            
350400     ELSE                                                                 
350500        MOVE ZERO                   TO W-GDC-KVBEHOV-DESSUTOM             
350600     END-IF                                                               
350700     .                                                                    
350800     EJECT                                                                
350900 FB-GDCBEHOV  SECTION.                                                    
351000     MOVE 'FB-GDCBEHOV    '    TO CURRENT-SECTION                         
351100                                                                          
351200******************************************************************        
351300*                                                                *        
351400*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL I KOMMANDE       *        
351500*    VECKOR.  (REFILLADE GLOBALA NDC'R EJ INOM LANDET)           *        
351600*                                                                *        
351700******************************************************************        
351800                                                                          
351900     MOVE ZERO                  TO W-GDC-ACC-KVBEHOV                      
352000     ADD W-GDC-KVBEHOV-DESSUTOM TO W-GDC-TILLGANG                         
352100     ADD W-KVBEHOV-INNEV-VECKA  TO W-GDC-ACC-KVBEHOV                      
352200                                                                          
352300     MOVE +1                    TO BER-IY                                 
352400     MOVE W-BER-START-VV        TO BER-IX                                 
352500     MOVE LINK-TIBEHOV-START    TO W-BER-DATUM                            
352600                                   W-AAVV                                 
352700     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
352800     MOVE W-AAVV                TO DAT-I-TIDATUM                          
352900                                                                          
353000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
353100                         DAT-O-TIDATUM DAT-KDSVAR                         
353200                                                                          
353300     IF DAT-KDSVAR-OK                                                     
353400       MOVE DAT-TIAARP(3:2)     TO W-PER                                  
353500       MOVE DAT-TIAAMMDD        TO W-BINNDAY                              
353600     ELSE                                                                 
353700           STRING ' FEL FRÅN WDATKONV13 I W222BHDC '                      
353800                  '(FB-, LINK-TIBEHOV)'                                   
353900           DELIMITED BY SIZE INTO FELTEXT-STR                             
354000           DISPLAY FELTEXT                                                
354100           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
354200     END-IF                                                               
354300                                                                          
354400     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
354500                                                                          
354600         PERFORM UNTIL                                                    
354700            (BER-IY = BER-ANTAL-AA AND                                    
354800             BER-IX > BER-ANTAL-VV)                                       
354900         OR (BER-IY < BER-ANTAL-AA AND                                    
355000             BER-IX > 52)                                                 
355100                                                                          
355200           ADD  +1                   TO INDX-L                            
355300                                                                          
355400           COMPUTE W-GDC-KVBEHOV-VECKA ROUNDED                            
355500                                 = (UTUP-KVBEHOV-V (INDX-L) * 1)          
355600                                                                          
355700           MOVE W-DATUM-FORSTA-GDC   TO TMP1-YYWW                         
355800           MOVE W-BER-DATUM          TO TMP2-YYWW                         
355900           PERFORM WY2000P3                                               
356000           IF TMP1-YYWW <= TMP2-YYWW                                      
356100                                                                          
356200             IF W-JUST-PB-FINNS = NEJ                                     
356300                                                                          
356400               COMPUTE W-GDC-KVBEHOV-DAG ROUNDED =                        
356500                                         W-GDC-KVBEHOV-VECKA / 5          
356600               ADD W-GDC-KVBEHOV-DAG                                      
356700                                     TO W-GDC-ACC-KVBEHOV                 
356800               MOVE W-GDC-KVBEHOV-DAG                                     
356900                                     TO W-GDC-ACC-KVBEHOV-VECKA           
357000               COMPUTE W-GDC-KVAR-EFT-DAG ROUNDED =                       
357100                             W-GDC-TILLGANG - W-GDC-ACC-KVBEHOV           
357200                                                                          
357300               MOVE +1               TO IX                                
357400               PERFORM UNTIL IX > +4                                      
357500               OR W-GDC-KVAR-EFT-DAG < SLAG-KVREFPKT                      
357600                 ADD +1              TO IX                                
357700                 ADD W-GDC-KVBEHOV-DAG                                    
357800                                     TO W-GDC-ACC-KVBEHOV                 
357900                                        W-GDC-ACC-KVBEHOV-VECKA           
358000                 COMPUTE W-GDC-KVAR-EFT-DAG ROUNDED =                     
358100                             W-GDC-TILLGANG - W-GDC-ACC-KVBEHOV           
358200                                                                          
358300               END-PERFORM                                                
358400               COMPUTE W-GDC-KVAR-KVBEHOV-VECKA ROUNDED =                 
358500                             W-GDC-KVBEHOV-VECKA -                        
358600                                        W-GDC-ACC-KVBEHOV-VECKA           
358700               ADD W-GDC-KVAR-KVBEHOV-VECKA                               
358800                                     TO W-GDC-ACC-KVBEHOV                 
358900                                                                          
359000               IF W-GDC-KVAR-EFT-DAG  < SLAG-KVREFPKT                     
359100                  COMPUTE W-GDC-DIFF  = SLAG-KVREFPKT -                   
359200                                              W-GDC-KVAR-EFT-DAG          
359300                  COMPUTE W-DC-REFILL-KVANT ROUNDED =                     
359400                                      W-GDC-DIFF + SLAG-KVREFBER          
359500                                                                          
359600                  PERFORM S24-ADJUST-LOW-DEMAND                           
359700                  IF DEMAND-ADJUST                                        
359800                     MOVE +1         TO W-DC-REFILL-KVANT                 
359900                  ELSE                                                    
360000                     PERFORM S08GDC-JUSTERA-REFILLKVANT                   
360100                     PERFORM S06-SATT-QX-PROCENT-BRYTNING                 
360200                     PERFORM S05-BERAKNA-Q-KVANT                          
360300                  END-IF                                                  
360400                  IF WS-DAPUBL > ZERO                                     
360500                    MOVE W-BINNDAY        TO TMP1-YYMMDD                  
360600                    MOVE WS-DAPUBL (3:6)  TO TMP2-YYMMDD                  
360700                    PERFORM WY2000P1                                      
360800                    IF TMP1-YYMMDD >= TMP2-YYMMDD                         
360900                      MOVE W-DC-REFILL-KVANT                              
361000                                     TO BER-BEHOV(BER-IY, BER-IX)         
361100                    ELSE                                                  
361200                      MOVE BER-IY                                         
361300                                     TO WS-BER-IY                         
361400                      MOVE BER-IX                                         
361500                                     TO WS-BER-IX                         
361600                      IF WS-BER-IX > 1                                    
361700                        SUBTRACT 1 FROM WS-BER-IX                         
361800                      ELSE                                                
361900                        IF WS-BER-IY > 1                                  
362000*AD                     IF WS-BER-IY > ZERO                               
362100                          SUBTRACT 1 FROM WS-BER-IY                       
362200                          MOVE 52      TO WS-BER-IX                       
362300                        END-IF                                            
362400                      END-IF                                              
362500                      IF WS-BER-IY > 0                                    
362600                      MOVE W-DC-REFILL-KVANT                              
362700                               TO BER-BEHOV(WS-BER-IY, WS-BER-IX)         
362800                      END-IF                                              
362900                    END-IF                                                
363000                                                                          
363100                  ELSE                                                    
363200                    MOVE W-DC-REFILL-KVANT                                
363300                               TO BER-BEHOV(BER-IY, BER-IX)               
363400                  END-IF                                                  
363500                  ADD W-DC-REFILL-KVANT                                   
363600                               TO W-GDC-TILLGANG                          
363700               END-IF                                                     
363800             ELSE                                                         
363900                                                                          
364000*******  GET REFILLING POINT AND REFILL QUANTITY                          
364100                                                                          
364200               INITIALIZE REFL1-REFL-W271REFL                             
364300                                                                          
364400               MOVE LINK-IDARTNR    TO REFL1-REFL-IDARTNR                 
364500               MOVE SLAG-IDDC       TO REFL1-REFL-IDDC                    
364600               MOVE SLAG-IDDC-REF   TO REFL1-REFL-IDDC-REF                
364700               MOVE SLAG-IDREFTAB   TO REFL1-REFL-IDREFTAB                
364800               MOVE SLAG-FLWILSON   TO REFL1-REFL-FLWILSON                
364900                                                                          
365000*******  BÅDE USA OCH CANADA SKALL HA MATERIALPRIS                        
365100               IF DCS-NDC-CN OR                                           
365200                  DCS-NDC-NA                                              
365300                 MOVE WS-PRMATRL    TO REFL1-REFL-PRARTBES                
365400               ELSE                                                       
365500                 MOVE CLAG-PRARTSTD TO REFL1-REFL-PRARTBES                
365600               END-IF                                                     
365700                                                                          
365800               MOVE SLAG-IDLEVNR    TO REFL1-REFL-IN-IDLEVNR-DC           
365900*---- GÄLLER EJ FÖR DCS-SDC OR DCS-NDC-CN, BARA NDC-NA/PF                 
366000               MOVE ZERO    TO REFL1-REFL-NDC-KVDAGAR-TBT-DC              
366100                                                                          
366200                                                                          
366300               MOVE SLAG-FLFLYG     TO REFL1-REFL-FLFLYG                  
366400                                                                          
366500               MOVE W-BINNDAY       TO REFL1-REFL-BINNDAY-TIAAMMDD        
366600               MOVE ZERO            TO REFL1-REFL-KVREFPKT                
366700                                       REFL1-REFL-KVREFBER                
366800               MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                        
366900               MOVE W-BINNDAY       TO TMP2-YYMMDD                        
367000                                                                          
367100               PERFORM WY2000P1                                           
367200               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
367300*                                                                         
367400*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
367500*                                                                         
367600                 MOVE SLAG-KVREFBER TO REFL1-REFL-IN-KVREFBER             
367700               ELSE                                                       
367800                 MOVE +0            TO REFL1-REFL-IN-KVREFBER             
367900               END-IF                                                     
368000                                                                          
368100               MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                        
368200               MOVE W-BINNDAY       TO TMP2-YYMMDD                        
368300*                                                                         
368400*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
368500*                                                                         
368600               PERFORM WY2000P1                                           
368700               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
368800*                                                                         
368900*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
369000*                                                                         
369100                 MOVE SLAG-KVREFPKT TO REFL1-REFL-IN-KVREFPKT             
369200               ELSE                                                       
369300                 MOVE +0            TO REFL1-REFL-IN-KVREFPKT             
369400               END-IF                                                     
369500*                                                                         
369600               MOVE UTUP-LT-BEHOV-V (INDX-L)                              
369700                                    TO REFL1-REFL-IN-LEADTID-BEHOV        
369800                                                                          
369900               CALL W271REFL USING REFL1-REFL-W271REFL                    
370000                                   REFL1-2501-PCB                         
370100                                   REFL1-WDB6-PCB                         
370200                                   REFL1-WDK7-PCB                         
370300                                   REFL1-UTIL-WDK6-PCB                    
370400                                   REFL1-UTIL-WDK7-PCB                    
370500                                   REFL1-UTIL-WDB6-PCB                    
370600                                                                          
370700               COMPUTE W-GDC-KVBEHOV-DAG ROUNDED =                        
370800                                       W-GDC-KVBEHOV-VECKA / 5            
370900               ADD W-GDC-KVBEHOV-DAG                                      
371000                                    TO W-GDC-ACC-KVBEHOV                  
371100               MOVE W-GDC-KVBEHOV-DAG                                     
371200                                    TO W-GDC-ACC-KVBEHOV-VECKA            
371300               COMPUTE W-GDC-KVAR-EFT-DAG ROUNDED =                       
371400                            W-GDC-TILLGANG - W-GDC-ACC-KVBEHOV            
371500                                                                          
371600               MOVE +1              TO IX                                 
371700               PERFORM UNTIL IX > +4                                      
371800               OR W-GDC-KVAR-EFT-DAG < REFL1-REFL-KVREFPKT                
371900                 ADD +1             TO IX                                 
372000                 ADD W-GDC-KVBEHOV-DAG                                    
372100                                    TO W-GDC-ACC-KVBEHOV                  
372200                                       W-GDC-ACC-KVBEHOV-VECKA            
372300                 COMPUTE W-GDC-KVAR-EFT-DAG ROUNDED =                     
372400                              W-GDC-TILLGANG - W-GDC-ACC-KVBEHOV          
372500                                                                          
372600               END-PERFORM                                                
372700               COMPUTE W-GDC-KVAR-KVBEHOV-VECKA ROUNDED =                 
372800                              W-GDC-KVBEHOV-VECKA -                       
372900                                         W-GDC-ACC-KVBEHOV-VECKA          
373000               ADD W-GDC-KVAR-KVBEHOV-VECKA                               
373100                                    TO W-GDC-ACC-KVBEHOV                  
373200                                                                          
373300               IF W-GDC-KVAR-EFT-DAG < REFL1-REFL-KVREFPKT                
373400                  COMPUTE W-GDC-DIFF = REFL1-REFL-KVREFPKT -              
373500                                              W-GDC-KVAR-EFT-DAG          
373600                  COMPUTE W-DC-REFILL-KVANT ROUNDED =                     
373700                                W-GDC-DIFF + REFL1-REFL-KVREFBER          
373800                                                                          
373900                  PERFORM S24-ADJUST-LOW-DEMAND                           
374000                  IF DEMAND-ADJUST                                        
374100                     MOVE +1        TO W-DC-REFILL-KVANT                  
374200                  ELSE                                                    
374300                     PERFORM S06-SATT-QX-PROCENT-BRYTNING                 
374400                     PERFORM S05-BERAKNA-Q-KVANT                          
374500                  END-IF                                                  
374600                  IF WS-DAPUBL > ZERO                                     
374700                     MOVE W-BINNDAY       TO TMP1-YYMMDD                  
374800                     MOVE WS-DAPUBL (3:6) TO TMP2-YYMMDD                  
374900                     PERFORM WY2000P1                                     
375000                                                                          
375100                     IF TMP1-YYMMDD >= TMP2-YYMMDD                        
375200                       MOVE W-DC-REFILL-KVANT                             
375300                                    TO BER-BEHOV(BER-IY, BER-IX)          
375400                     ELSE                                                 
375500                       MOVE BER-IY                                        
375600                                    TO WS-BER-IY                          
375700                       MOVE BER-IX                                        
375800                                    TO WS-BER-IX                          
375900                       IF WS-BER-IX > 1                                   
376000                         SUBTRACT 1 FROM WS-BER-IX                        
376100                       ELSE                                               
376200                         IF WS-BER-IY > 1                                 
376300*AD                      IF WS-BER-IY > ZERO                              
376400                           SUBTRACT 1 FROM WS-BER-IY                      
376500                           MOVE 52    TO WS-BER-IX                        
376600                         END-IF                                           
376700                       END-IF                                             
376800                       MOVE W-DC-REFILL-KVANT                             
376900                               TO BER-BEHOV(WS-BER-IY, WS-BER-IX)         
377000                     END-IF                                               
377100                                                                          
377200                  ELSE                                                    
377300                     MOVE W-DC-REFILL-KVANT                               
377400                               TO BER-BEHOV(BER-IY, BER-IX)               
377500                  END-IF                                                  
377600                  ADD W-DC-REFILL-KVANT                                   
377700                               TO W-GDC-TILLGANG                          
377800               END-IF                                                     
377900             END-IF                                                       
378000           END-IF                                                         
378100                                                                          
378200           ADD 1               TO BER-IX                                  
378300                                  W-BER-DATUM                             
378400           MOVE +1             TO W-ANTAL-VECKOR                          
378500           CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                      
378600                                                                          
378700           MOVE 'AAVV  '       TO DAT-KDDATFORM                           
378800           MOVE W-AAVV         TO DAT-I-TIDATUM                           
378900                                                                          
379000           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
379100                           DAT-O-TIDATUM DAT-KDSVAR                       
379200                                                                          
379300           IF DAT-KDSVAR-OK                                               
379400             MOVE DAT-TIAARP(3:2)  TO W-PER                               
379500             MOVE DAT-TIAAMMDD     TO W-BINNDAY                           
379600           ELSE                                                           
379700               STRING ' FEL FRÅN WDATKONV14 I W222BHDC '                  
379800                      '(FB-, W-AAVV, BER-IY )'                            
379900               DELIMITED BY SIZE INTO FELTEXT-STR                         
380000               DISPLAY FELTEXT                                            
380100               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
380200           END-IF                                                         
380300                                                                          
380400         END-PERFORM                                                      
380500         ADD 1                     TO BER-IY                              
380600         MOVE 1                    TO BER-IX                              
380700                                      IX-PER                              
380800         SUBTRACT +52            FROM W-BER-DATUM                         
380900         ADD +100                  TO W-BER-DATUM                         
381000     END-PERFORM                                                          
381100     .                                                                    
381200     EJECT                                                                
381300 FC-LAES-WDK711-REF-GLOBAL  SECTION.                                      
381400     MOVE 'FC-LAES-WDK711-REF-GLOBAL '    TO CURRENT-SECTION              
381500                                                                          
381600******************************************************************        
381700*                                                                *        
381800*    LÄSER WDK711  FÖR DE LAGER SOM REFILLAS FRÅN AKTUELLT NDC.  *        
381900*    SLAG-IDDC-REF = 71 , 72 .... NDC-CN.                        *        
382000*    SLAG-IDDC-REF = 4*      .... NDC-NA (USA),REFILLAR DC-51    *        
382100*    EJ REFILL INOM DET EGNA LANDET, GLOBALA REFILL-DC.          *        
382200******************************************************************        
382300                                                                          
382400     PERFORM IMS-GNP-WDK711-REF-G                                         
382500                                                                          
382600     PERFORM UNTIL SEGMENT-SAKNAS                                         
382700     OR (SLAG-KDREFSTA = 'A'                                              
382800     AND SLAG-FLREFILL = JA)                                              
382900*--   LÄS FRAM TILL EN GILTIG NDC-POST                                    
383000*--                                                                       
383100        PERFORM IMS-GNP-WDK711-REF-G                                      
383200     END-PERFORM                                                          
383300                                                                          
383400     IF SEGMENT-FINNS                                                     
383500*** FIX FÖR NEGATIVA KVOKS-BULK                                           
383600        IF SLAG-KVOKS-BULK < 0                                            
383700          MOVE ZERO          TO SLAG-KVOKS-BULK                           
383800        END-IF                                                            
383900*** FIX FÖR NEGATIVA KVOKS-DAG                                            
384000        IF SLAG-KVOKS-DAG < 0                                             
384100          MOVE ZERO          TO SLAG-KVOKS-DAG                            
384200        END-IF                                                            
384300                                                                          
384400        MOVE SLAG-IDDC TO W-IDDC-B6                                       
384500        PERFORM S13-READ-OR-TAB-B601                                      
384600                                                                          
384700*** KINA OCH JAPAN HAR 6 DAGARS ARBETSVECKA                               
384800        MOVE NEJ TO 6ARBDAG-SW                                            
384900        IF DCS-CHINA OR DCS-JAPAN                                         
385000          MOVE JA TO 6ARBDAG-SW                                           
385100        END-IF                                                            
385200     END-IF                                                               
385300     .                                                                    
385400     EJECT                                                                
385500 FD-JUSTERA-GDC-TILLGANG  SECTION.                                        
385600     MOVE 'FD-JUSTERA-GDC-TILLGANG' TO CURRENT-SECTION                    
385700                                                                          
385800     IF WS-ERS-FINNS-K611 = JA                                            
385900       IF ERS-CLAG-KDERS = 01 OR 11 OR 17 OR 21 OR 27                     
386000          MOVE ERS-ART-IDARTNR TO W-IDARTNR-K7-ERS                        
386100          MOVE SLAG-IDDC       TO W-IDDC-ERS                              
386200          PERFORM IMS-GU-WDK711-ERS                                       
386300          IF  SEGMENT-FINNS                                               
386400             COMPUTE W-GDC-TILLGANG-ERS ROUNDED =                         
386500                     ERS-SLAG-KVLS                                        
386600                  +  ERS-SLAG-KVAKS-PAV                                   
386700                  +  ERS-SLAG-KVAKS-SDC                                   
386800                  +  ERS-SLAG-KVBEART                                     
386900                  -  ERS-SLAG-KVROS-BULK                                  
387000                  -  ERS-SLAG-KVROS-DAG                                   
387100                  -  ERS-SLAG-KVRESS                                      
387200                  -  ERS-SLAG-KVOKS-BULK                                  
387300                  -  ERS-SLAG-KVOKS-DAG                                   
387400             ADD W-GDC-TILLGANG-ERS TO W-GDC-TILLGANG                     
387500          END-IF                                                          
387600       END-IF                                                             
387700     END-IF                                                               
387800     .                                                                    
387900     EJECT                                                                
388000 I-BERAKNA-PB-PLAN SECTION.                                               
388100     MOVE 'I-BERAKNA-PB-PLAN '   TO CURRENT-SECTION                       
388200                                                                          
388300     PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                                 
388400                                                                          
388500     MOVE LINK-IDDC TO W-IDDC-B6                                          
388600     PERFORM S13-READ-OR-TAB-B601                                         
388700                                                                          
388800***  KINA OCH JAPAN HAR 6 DAGARS ARBETSVECKA                              
388900     MOVE NEJ TO 6ARBDAG-SW                                               
389000     IF DCS-CHINA OR DCS-JAPAN                                            
389100       MOVE JA TO 6ARBDAG-SW                                              
389200     END-IF                                                               
389300                                                                          
389400     IF SPAR-XLAG-DAPBPLAN > WS-DAGENS-DATUM                              
389500     OR SPAR-XLAG-DAPBPLAN = WS-DAGENS-DATUM                              
389600******                                                                    
389700******    MANUELL PB-PLAN ÄR SATT (GÄLLER ISTÄLLET FÖR DET                
389800******    TOTALA SEPARAT- SAMT REFILLBEHOVEN)                             
389900******                                                                    
390000                                                                          
390100       COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                               
390200                                  SPAR-XLAG-KVPB-PLAN / 4.33              
390300     ELSE                                                                 
390400                                                                          
390500       COMPUTE WS-KVPB-PLAN-VECKA ROUNDED = WS-KVPB / 4.33                
390600     END-IF                                                               
390700                                                                          
390800     IF 6ARBDAG-SW = JA                                                   
390900       COMPUTE W-DAGAR-KVAR = 6 - LINK-TID-AKTUELL                        
391000       IF W-TIME(1:2)         > 05 AND                                    
391100          W-TIME(1:2)         < 17                                        
391200          ADD +1               TO W-DAGAR-KVAR                            
391300       END-IF                                                             
391400     ELSE                                                                 
391500       COMPUTE W-DAGAR-KVAR = 5 - LINK-TID-AKTUELL                        
391600       IF W-TIME(1:2)         > 05 AND                                    
391700          W-TIME(1:2)         < 17                                        
391800          ADD +1               TO W-DAGAR-KVAR                            
391900       END-IF                                                             
392000     END-IF                                                               
392100                                                                          
392200     MOVE W-BER-START-AA     TO W-DATUM-AA                                
392300     MOVE W-BER-START-VV     TO W-DATUM-VV                                
392400     MOVE W-DATUM            TO W-AAVV                                    
392500     MOVE -1                 TO W-ANTAL-VECKOR                            
392600     CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                            
392700                                                                          
392800     MOVE 'AAVV'             TO DAT-KDDATFORM                             
392900     MOVE W-AAVV             TO DAT-I-TIDATUM                             
393000                                                                          
393100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
393200                         DAT-O-TIDATUM DAT-KDSVAR                         
393300                                                                          
393400     IF DAT-KDSVAR-OK                                                     
393500****    HÄMTA DAT-TIRP                                                    
393600        CONTINUE                                                          
393700                                                                          
393800     ELSE                                                                 
393900         STRING ' FEL FRÅN WDATKONV8 I W222BHDC '                         
394000                '(I-BERAKNA, W-AAVV )'                                    
394100         DELIMITED BY SIZE INTO FELTEXT-STR                               
394200         DISPLAY FELTEXT                                                  
394300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
394400     END-IF                                                               
394500                                                                          
394600     MOVE +1                 TO BER-IY                                    
394700     MOVE W-BER-START-VV     TO BER-IX                                    
394800     MOVE LINK-TIBEHOV-START TO W-BER-DATUM                               
394900                                                                          
395000     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
395100                                                                          
395200         PERFORM UNTIL                                                    
395300            (BER-IY = BER-ANTAL-AA AND                                    
395400             BER-IX > BER-ANTAL-VV)                                       
395500         OR (BER-IY < BER-ANTAL-AA AND                                    
395600             BER-IX > 52)                                                 
395700                                                                          
395800             MOVE W-BER-DATUM       TO TMP1-YYWW                          
395900                                                                          
396000             PERFORM S77-KOLLA-DAPUBL                                     
396100             MOVE PUBL-DAPUBL(PUBL-IX) TO WS-DAPUBL                       
396200             IF  WS-DAPUBL > ZERO                                         
396300             AND WS-DAPUBL > WS-DAGENS-DATUM                              
396400                                                                          
396500               MOVE 'AAMMDD'         TO DAT-KDDATFORM                     
396600               MOVE WS-DAPUBL(3:6)   TO DAT-I-TIDATUM                     
396700               CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM            
396800                                   DAT-O-TIDATUM DAT-KDSVAR               
396900               IF DAT-KDSVAR-OK                                           
397000                  MOVE DAT-TIAAVV-GRP   TO WS-DAPUBL-AAVV                 
397100               ELSE                                                       
397200                   STRING ' FEL DATUM - DATKONV9 I W222BHDC'              
397300                          '(B-BERAKNA DAPUBL-AAVV)'                       
397400                   DELIMITED BY SIZE INTO FELTEXT-STR                     
397500                   DISPLAY FELTEXT                                        
397600                   CALL ABEND USING RKOD-ABEND-UTAN-DUMP                  
397700               END-IF                                                     
397800                                                                          
397900               MOVE WS-DAPUBL-AAVV  TO TMP2-YYWW                          
398000             ELSE                                                         
398100               MOVE W-TIFINLV-AAVV  TO TMP2-YYWW                          
398200             END-IF                                                       
398300             PERFORM WY2000P3                                             
398400             IF  TMP1-YYWW < TMP2-YYWW                                    
398500                 MOVE ZERO TO BER-BEHOV (BER-IY, BER-IX)                  
398600             ELSE                                                         
398700                                                                          
398800                 MOVE 'AAVV' TO DAT-KDDATFORM                             
398900                 MOVE W-BER-DATUM                                         
399000                             TO DAT-I-TIDATUM                             
399100                                                                          
399200                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
399300                                     DAT-O-TIDATUM DAT-KDSVAR             
399400                                                                          
399500                 IF DAT-KDSVAR-OK                                         
399600****                HÄMTA DAT-TIRP                                        
399700                    CONTINUE                                              
399800                                                                          
399900                 ELSE                                                     
400000                   STRING ' FEL FRÅN WDATKONV10 I W222BHDC '              
400100                          '(I-BERAKNA, W-BER-DATUM )'                     
400200                   DELIMITED BY SIZE INTO FELTEXT-STR                     
400300                   DISPLAY FELTEXT                                        
400400                   CALL ABEND USING RKOD-ABEND-UTAN-DUMP                  
400500                 END-IF                                                   
400600                                                                          
400700                 IF SPAR-XLAG-DASEASON > WS-DAGENS-DATUM                  
400800                 OR SPAR-XLAG-DASEASON = WS-DAGENS-DATUM                  
400900******                                                                    
401000******    MANUELL SÄSONG ÄR SATT (GÄLLER FÖR DET TOTALA                   
401100******    SEPARAT- SAMT REFILLBEHOVEN)                                    
401200                                                                          
401300                     COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =         
401400                               WS-KVPB-PLAN-VECKA *                       
401500                               SPAR-XLAG-RESEASON-PLAN (DAT-TIRP)         
401600                 ELSE                                                     
401700******                                                                    
401800******    INGEN MANUELL SÄSONG ÄR SATT UTAN BERÄKNAS UTIFRÅN              
401900******    DET TOTALA SEPARAT- SAMT REFILLBEHOVEN                          
402000                                                                          
402100                     COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =         
402200                               WS-KVPB-PLAN-VECKA * 1                     
402300*                                  WS-RESEASON-AVR (DAT-TIRP)             
402400                 END-IF                                                   
402500                                                                          
402600             END-IF                                                       
402700                                                                          
402800             ADD 1           TO BER-IX                                    
402900                                W-BER-DATUM                               
403000         END-PERFORM                                                      
403100         ADD 1   TO BER-IY                                                
403200         MOVE 1  TO BER-IX                                                
403300                    IX-PER                                                
403400         SUBTRACT +52 FROM  W-BER-DATUM                                   
403500         ADD +100     TO    W-BER-DATUM                                   
403600     END-PERFORM                                                          
403700     PERFORM S10-ADD-TILL-RESULTAT-PBPLAN                                 
403800     .                                                                    
403900     EJECT                                                                
404000 K-BER-PBTOT-SASONG SECTION.                                              
404100     MOVE 'K-BER-PBTOT-SASONG '  TO CURRENT-SECTION                       
404200                                                                          
404300****                                                                      
404400**** FLYTTA IN RESULTATET TILL EN                                         
404500**** ARBETSAREA DÄR VECKOBEHOVEN LIGGER FROM VECKA 1 TOM 52               
404600****                                                                      
404700                                                                          
404800     COMPUTE IX-TILL = WS-DAGENS-VECKA + 1                                
404900                                                                          
405000     MOVE +1                 TO IX-FRAN                                   
405100     PERFORM UNTIL IX-TILL   >  52                                        
405200        ADD  LINK-KVBEHOV-VECKA(IX-FRAN)                                  
405300                             TO WS-KVBEHOV-VECKA (IX-TILL)                
405400                                WS-ARSTOTAL                               
405500        ADD +1               TO IX-FRAN                                   
405600                                IX-TILL                                   
405700     END-PERFORM                                                          
405800                                                                          
405900     MOVE +1                 TO IX-TILL                                   
406000     PERFORM UNTIL IX-FRAN   >  52                                        
406100        ADD  LINK-KVBEHOV-VECKA(IX-FRAN)                                  
406200                             TO WS-KVBEHOV-VECKA (IX-TILL)                
406300                                WS-ARSTOTAL                               
406400        ADD +1               TO IX-FRAN                                   
406500                                IX-TILL                                   
406600     END-PERFORM                                                          
406700                                                                          
406800****                                                                      
406900**** PROGNOSBEHOVET SPEGLAR BEHOVET FÖR EN 6-VECKORSPERIOD                
407000****                                                                      
407100                                                                          
407200     COMPUTE WS-KVPB ROUNDED = WS-ARSTOTAL / 12                           
407300                                                                          
407400     MOVE ZERO               TO WS-RESEASON-TOT                           
407500                                                                          
407600     MOVE +1                 TO IX-VECKA                                  
407700                                IX-PER                                    
407800     PERFORM UNTIL IX-PER > +12                                           
407900                                                                          
408000       MOVE ZERO             TO WS-KVBEHOV-PER                            
408100                                                                          
408200       PERFORM UNTIL IX-VECKA > PER-SLUT-VV (IX-PER)                      
408300                                                                          
408400         ADD WS-KVBEHOV-VECKA (IX-VECKA)                                  
408500                             TO WS-KVBEHOV-PER                            
408600         ADD +1              TO IX-VECKA                                  
408700       END-PERFORM                                                        
408800                                                                          
408900****                                                                      
409000****   RÄKNA UT SÄSONGSINDEX                                              
409100****                                                                      
409200                                                                          
409300       COMPUTE WS-RESEASON (IX-PER) ROUNDED =                             
409400               WS-KVBEHOV-PER / WS-KVPB                                   
409500                ON SIZE ERROR                                             
409600                    MOVE ZERO     TO WS-RESEASON (IX-PER)                 
409700       END-COMPUTE                                                        
409800                                                                          
409900       COMPUTE WS-RESEASON-AVR (IX-PER) ROUNDED =                         
410000               WS-RESEASON (IX-PER) * +1                                  
410100       ADD WS-RESEASON-AVR (IX-PER)                                       
410200                             TO WS-RESEASON-TOT                           
410300                                                                          
410400****   WS-KVBEHOV-PER-TEST ANVÄNDS ENBART I TESTSYFTE                     
410500       MOVE WS-KVBEHOV-PER   TO WS-KVBEHOV-PER-TEST (IX-PER)              
410600                                                                          
410700       ADD +1                TO IX-PER                                    
410800     END-PERFORM                                                          
410900                                                                          
411000     IF WS-RESEASON-TOT < +10.00                                          
411100     OR WS-RESEASON-TOT > +14.00                                          
411200       MOVE +12.00           TO WS-RESEASON-TOT                           
411300       MOVE +1               TO IX-PER                                    
411400       PERFORM UNTIL IX-PER > +12                                         
411500         MOVE +1             TO WS-RESEASON-AVR (IX-PER)                  
411600         ADD +1              TO IX-PER                                    
411700       END-PERFORM                                                        
411800     ELSE                                                                 
411900                                                                          
412000       IF WS-RESEASON-TOT > +12.00                                        
412100         MOVE -0.01          TO WS-JUSTERA                                
412200       ELSE                                                               
412300         MOVE +0.01          TO WS-JUSTERA                                
412400       END-IF                                                             
412500                                                                          
412600       PERFORM UNTIL WS-RESEASON-TOT = +12.00                             
412700       OR WS-RESEASON-TOT = ZERO                                          
412800                                                                          
412900         MOVE +1             TO IX-PER                                    
413000         PERFORM UNTIL WS-RESEASON-TOT = +12.00                           
413100         OR IX-PER > +12                                                  
413200           IF WS-RESEASON-AVR (IX-PER) > +0.01                            
413300             ADD WS-JUSTERA  TO WS-RESEASON-AVR (IX-PER)                  
413400                                WS-RESEASON-TOT                           
413500           END-IF                                                         
413600           ADD +1            TO IX-PER                                    
413700         END-PERFORM                                                      
413800       END-PERFORM                                                        
413900     END-IF                                                               
414000     .                                                                    
414100     EJECT                                                                
414200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
414300*                                                               *         
414400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
414500 S01-NOLLSTALL-BERAKNINGS-TAB SECTION.                                    
414600     MOVE 'S01-NOLLSTALL-BERAKNINGS-TAB'  TO CURRENT-SECTION              
414700                                                                          
414800     MOVE 1               TO IY                                           
414900     PERFORM UNTIL IY     >  BER-MAX-AA                                   
415000     MOVE 1               TO IX                                           
415100         PERFORM UNTIL IX > BER-MAX-VV                                    
415200             MOVE ZERO    TO BER-BEHOV (IY, IX)                           
415300             ADD 1        TO IX                                           
415400         END-PERFORM                                                      
415500         ADD 1            TO IY                                           
415600     END-PERFORM                                                          
415700     .                                                                    
415800     EJECT                                                                
415900 S02-NOLLSTALL-RESULTAT-TAB SECTION.                                      
416000     MOVE 'S02-NOLLSTALL-RESULTAT-TAB'  TO CURRENT-SECTION                
416100                                                                          
416200     MOVE 1               TO IY                                           
416300     PERFORM UNTIL IY     >  RES-MAX-AA                                   
416400     MOVE 1 TO IX                                                         
416500         PERFORM UNTIL IX >  RES-MAX-VV                                   
416600             MOVE ZERO    TO RES-BEHOV (IY, IX)                           
416700             ADD 1        TO IX                                           
416800         END-PERFORM                                                      
416900         ADD 1            TO IY                                           
417000     END-PERFORM                                                          
417100     .                                                                    
417200     EJECT                                                                
417300 S03-ADD-TILL-RESULTAT-TAB SECTION.                                       
417400     MOVE 'S03-ADD-TILL-RESULTAT-TAB'  TO CURRENT-SECTION                 
417500                                                                          
417600     MOVE 1                TO IY                                          
417700     PERFORM UNTIL IY      >  BER-MAX-AA                                  
417800         MOVE 1            TO IX                                          
417900         PERFORM UNTIL IX  > BER-MAX-VV                                   
418000             ADD BER-BEHOV (IY, IX) TO RES-BEHOV (IY, IX)                 
418100             ADD 1         TO IX                                          
418200         END-PERFORM                                                      
418300         ADD 1             TO IY                                          
418400     END-PERFORM                                                          
418500     .                                                                    
418600     EJECT                                                                
418700 S04-BERAKNA-BERTAB-INDEX SECTION.                                        
418800     MOVE 'S04-BERAKNA-BERTAB-INDEX '  TO CURRENT-SECTION                 
418900******************************************************************        
419000*    DF-BERAKNA-CDC-SATSBEHOV/DG-BERAKNA-CDC-TPO-BEHOV           *        
419100*    MED UTGÅNGSPUNKT FRÅN DATUM I W-DATUM BERÄKNAS              *        
419200*    ÅRS- OCH VECKO-INDEX FÖR BERÄKNINGSTABELLEN                 *        
419300*                                                                *        
419400******************************************************************        
419500                                                                          
419600     MOVE W-BER-START-AA  TO TMP1-YY                                      
419700     MOVE W-DATUM-AA      TO TMP2-YY                                      
419800     PERFORM WY2000P9                                                     
419900     COMPUTE W-ANTAL-VECKOR = W-DATUM-VV                                  
420000                            + (TMP2-YY - TMP1-YY) * 52                    
420100                                                                          
420200     MOVE +100         TO W-DATUM-AAVV                                    
420300     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
420400     MOVE W-DATUM-AAVV TO W-DATUM                                         
420500     MOVE W-DATUM-AA   TO BER-IY                                          
420600     MOVE W-DATUM-VV   TO BER-IX                                          
420700     .                                                                    
420800     EJECT                                                                
420900 S05-BERAKNA-Q-KVANT    SECTION.                                          
421000     MOVE 'S05-BERAKNA-Q-KVANT '  TO CURRENT-SECTION                      
421100                                                                          
421200     MOVE ZERO               TO WS-KVANT-QX                               
421300     MOVE W-DC-REFILL-KVANT  TO WS-ANTAL-QX                               
421400                                                                          
421500     IF WS-LART-KVQPACK-3 > ZERO                                          
421600       MOVE WS-LART-KVQPACK-3  TO WS-KVQPACK-3                            
421700     ELSE                                                                 
421800       MOVE CLAG-KVQPACK-3     TO WS-KVQPACK-3                            
421900     END-IF                                                               
422000                                                                          
422100     IF CLAG-KVQPACK-4 > 1                                                
422200     OR WS-KVQPACK-3   > 1                                                
422300     OR CLAG-KVQPACK-2 > 1                                                
422400     OR CLAG-KVQPACK-1 > 1                                                
422500     OR CLAG-KVQPACK-0 > 1                                                
422600                                                                          
422700*KVQPACK-4                                                                
422800       IF CLAG-KVQPACK-4 > 1                                              
422900         IF DCS-SDC                                                       
423000           MOVE ZERO               TO WS-KVANT-QX-HELTAL                  
423100         ELSE                                                             
423200           COMPUTE WS-KVANT-QX =                                          
423300                   W-DC-REFILL-KVANT / CLAG-KVQPACK-4                     
423400           IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                         
423500             IF WS-KVANT-QX-HELTAL > ZERO                                 
423600               COMPUTE WS-ANTAL-QX =                                      
423700                       WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                
423800             END-IF                                                       
423900           ELSE                                                           
424000             COMPUTE WS-KVANT-QX-HELTAL =                                 
424100                     WS-KVANT-QX-HELTAL + 1                               
424200             COMPUTE WS-ANTAL-QX =                                        
424300                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                  
424400           END-IF                                                         
424500         END-IF                                                           
424600       END-IF                                                             
424700                                                                          
424800*KVQPACK-3                                                                
424900       IF WS-KVQPACK-3 > 1  AND                                           
425000          WS-KVANT-QX-HELTAL = ZERO                                       
425100         COMPUTE WS-KVANT-QX =                                            
425200                 W-DC-REFILL-KVANT / WS-KVQPACK-3                         
425300         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
425400           IF WS-KVANT-QX-HELTAL > ZERO                                   
425500             COMPUTE WS-ANTAL-QX =                                        
425600                     WS-KVANT-QX-HELTAL * WS-KVQPACK-3                    
425700           END-IF                                                         
425800         ELSE                                                             
425900           COMPUTE WS-KVANT-QX-HELTAL =                                   
426000                   WS-KVANT-QX-HELTAL + 1                                 
426100           COMPUTE WS-ANTAL-QX =                                          
426200                   WS-KVANT-QX-HELTAL * WS-KVQPACK-3                      
426300         END-IF                                                           
426400       END-IF                                                             
426500                                                                          
426600*KVQPACK-2                                                                
426700       IF CLAG-KVQPACK-2 > 1 AND                                          
426800          WS-KVANT-QX-HELTAL = ZERO                                       
426900         COMPUTE WS-KVANT-QX =                                            
427000                 W-DC-REFILL-KVANT / CLAG-KVQPACK-2                       
427100         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
427200           IF WS-KVANT-QX-HELTAL > ZERO                                   
427300             COMPUTE WS-ANTAL-QX =                                        
427400                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                  
427500           END-IF                                                         
427600         ELSE                                                             
427700           COMPUTE WS-KVANT-QX-HELTAL =                                   
427800                   WS-KVANT-QX-HELTAL + 1                                 
427900           COMPUTE WS-ANTAL-QX =                                          
428000                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                    
428100         END-IF                                                           
428200       END-IF                                                             
428300                                                                          
428400*KVQPACK-0                                                                
428500       IF CLAG-KVQPACK-0 > 1   AND                                        
428600          WS-KVANT-QX-HELTAL = ZERO                                       
428700         COMPUTE WS-KVANT-QX =                                            
428800                 W-DC-REFILL-KVANT / CLAG-KVQPACK-0                       
428900         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
429000           IF WS-KVANT-QX-HELTAL < 1                                      
429100             IF CLAG-KVQPACK-1 > 0                                        
429200               PERFORM S05B-KVQPACK-1                                     
429300             ELSE                                                         
429400               MOVE W-DC-REFILL-KVANT TO WS-ANTAL-QX                      
429500             END-IF                                                       
429600           ELSE                                                           
429700             COMPUTE WS-ANTAL-QX =                                        
429800                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                  
429900           END-IF                                                         
430000         ELSE                                                             
430100           COMPUTE WS-KVANT-QX-HELTAL =                                   
430200                   WS-KVANT-QX-HELTAL + 1                                 
430300           COMPUTE WS-ANTAL-QX =                                          
430400                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                    
430500         END-IF                                                           
430600       END-IF                                                             
430700                                                                          
430800*KVQPACK-1                                                                
430900       IF CLAG-KVQPACK-1 > 1                                              
431000          IF WS-KVANT-QX-HELTAL > ZERO                                    
431100             MOVE WS-ANTAL-QX TO W-DC-REFILL-KVANT                        
431200          END-IF                                                          
431300          PERFORM S05B-KVQPACK-1                                          
431400       END-IF                                                             
431500     END-IF                                                               
431600     MOVE WS-ANTAL-QX TO W-DC-REFILL-KVANT                                
431700                                                                          
431800     .                                                                    
431900     EJECT                                                                
432000                                                                          
432100 S05B-KVQPACK-1 SECTION.                                                  
432200     MOVE 'S05B-KVQPACK-1  '   TO CURRENT-SECTION                         
432300                                                                          
432400     COMPUTE WS-KVANT-QX =                                                
432500             W-DC-REFILL-KVANT / CLAG-KVQPACK-1                           
432600     IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                               
432700       IF WS-KVANT-QX-HELTAL < 1                                          
432800         COMPUTE WS-ANTAL-QX = 1 * CLAG-KVQPACK-1                         
432900       ELSE                                                               
433000         COMPUTE WS-ANTAL-QX =                                            
433100                 WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                      
433200       END-IF                                                             
433300     ELSE                                                                 
433400       COMPUTE WS-KVANT-QX-HELTAL =                                       
433500               WS-KVANT-QX-HELTAL + 1                                     
433600       COMPUTE WS-ANTAL-QX =                                              
433700               WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                        
433800     END-IF                                                               
433900     .                                                                    
434000     EJECT                                                                
434100 S06-SATT-QX-PROCENT-BRYTNING SECTION.                                    
434200     MOVE 'S06-SATT-QX-PROCENT-BRYTNING '   TO CURRENT-SECTION            
434300                                                                          
434400     MOVE DCS-REQXBRYT       TO WS-QX-BRYTNING                            
434500     .                                                                    
434600     EJECT                                                                
434700 S07-INIT-DATUM SECTION.                                                  
434800     MOVE 'S07-INIT-DATUM '  TO CURRENT-SECTION                           
434900                                                                          
435000     MOVE JA TO LINK-FLJANEJ-ANROP                                        
435100     SKIP1                                                                
435200     MOVE LINK-TIBEHOV-START TO W-DATUM                                   
435300                                W-BER-START-DATUM                         
435400     MOVE W-DATUM-AA TO W-BER-START-AA                                    
435500     MOVE W-DATUM-VV TO W-BER-START-VV                                    
435600     MOVE LINK-TIBEHOV-START  TO W-BER-SLUT-DATUM                         
435700     MOVE LINK-KVVECKOR-BEHOV TO W-ANTAL-VECKOR                           
435800     SUBTRACT 1 FROM W-ANTAL-VECKOR                                       
435900     CALL W009VADD USING W-BER-SLUT-DATUM W-ANTAL-VECKOR                  
436000     SKIP1                                                                
436100**** FIX FÖR ATT KUNNA HANTERA BERÄKNING VID ÅRSBRYTNING.                 
436200     MOVE +100 TO W-DATUM-AAVV                                            
436300     ADD  W-BER-START-VV TO W-DATUM-AAVV                                  
436400     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
436500     MOVE W-DATUM-AAVV   TO W-DATUM                                       
436600     MOVE W-DATUM-AA     TO RES-ANTAL-AA                                  
436700     MOVE W-DATUM-VV     TO RES-ANTAL-VV                                  
436800                                                                          
436900******************************************************************        
437000**** YTTERLIGARE +7 VECKOR TILLÄGG PGA AV LEDTIDEN TILL USA.              
437100**** GÄLLER EJ FÖR KINA. SE W2222200                                      
437200**** 2018-01-04 ALEXANDER:SPARA KODEN FÖR GLOBAL EXPORT VID REFILL        
437300**** TILL KINA ETC. ELLER KINA TILL CDC?                                  
437400*    ADD +7                  TO LINK-KVVECKOR-BEHOV                       
437500*    OBS! ADD ÄVEN TILL RES-ANTAL-AA  RES-ANTAL-VV                        
437600******************************************************************        
437700                                                                          
437800     IF  LINK-KVVECKOR-BEHOV > +156                                       
437900         MOVE +156 TO LINK-KVVECKOR-BEHOV                                 
438000     END-IF                                                               
438100                                                                          
438200     MOVE LINK-TIBEHOV-START  TO W-DATUM                                  
438300                                 W-BER-START-DATUM                        
438400     MOVE W-DATUM-AA TO W-BER-START-AA                                    
438500     MOVE W-DATUM-VV TO W-BER-START-VV                                    
438600     MOVE LINK-TIBEHOV-START  TO W-BER-SLUT-DATUM                         
438700     MOVE LINK-KVVECKOR-BEHOV TO W-ANTAL-VECKOR                           
438800     SUBTRACT 1 FROM W-ANTAL-VECKOR                                       
438900     CALL W009VADD USING W-BER-SLUT-DATUM W-ANTAL-VECKOR                  
439000     SKIP1                                                                
439100     MOVE +100 TO W-DATUM-AAVV                                            
439200     ADD  W-BER-START-VV  TO W-DATUM-AAVV                                 
439300     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
439400     MOVE W-DATUM-AAVV    TO W-DATUM                                      
439500     MOVE W-DATUM-AA      TO BER-ANTAL-AA                                 
439600     MOVE W-DATUM-VV      TO BER-ANTAL-VV                                 
439700                                                                          
439800     .                                                                    
439900     EJECT                                                                
440000 S08NDC-JUSTERA-REFILLKVANT   SECTION.                                    
440100     MOVE 'S08NDC-JUSTERA-REFILLKVANT'  TO CURRENT-SECTION                
440200                                                                          
440300*--- OM DET ÄR MINUSSALDO/LÅGT LS KVAR = OK ATT REFILLA                   
440400*--- MER ÄN 52-VECKORS BEHOV.                                             
440500     IF SLAG-KVREFPKT - W-NDC-TILLGANG > W-KVBEHOV-52V                    
440600        CONTINUE                                                          
440700     ELSE                                                                 
440800        IF W-DC-REFILL-KVANT < W-KVBEHOV-52V                              
440900           CONTINUE                                                       
441000        ELSE                                                              
441100           MOVE W-KVBEHOV-52V TO W-DC-REFILL-KVANT                        
441200        END-IF                                                            
441300     END-IF                                                               
441400     .                                                                    
441500     EJECT                                                                
441600 S08GDC-JUSTERA-REFILLKVANT   SECTION.                                    
441700     MOVE 'S08GDC-JUSTERA-REFILLKVANT'  TO CURRENT-SECTION                
441800                                                                          
441900     IF SLAG-KVREFPKT - W-GDC-TILLGANG > W-KVBEHOV-52V                    
442000        CONTINUE                                                          
442100     ELSE                                                                 
442200        IF W-DC-REFILL-KVANT < W-KVBEHOV-52V                              
442300           CONTINUE                                                       
442400        ELSE                                                              
442500           MOVE W-KVBEHOV-52V TO W-DC-REFILL-KVANT                        
442600        END-IF                                                            
442700     END-IF                                                               
442800     .                                                                    
442900     EJECT                                                                
443000 S09-NOLLSTALL-RESULTAT-PBPLAN SECTION.                                   
443100     MOVE 'S09-NOLLSTALL-RESULTAT-PBPLAN'  TO CURRENT-SECTION             
443200*------------------------------------------------------------             
443300*--- DET BLIR FEL IX OM MAN HAR VECKA 52 + 2V = 54.                       
443400*--- DET BLIR FEL IX OM MAN HAR VECKA 53 + 2V = 55.                       
443500*--- STARTVECKA 53 BLIR ALLTID AA +1 OCH VV=01, SE SDC-BEHOV.             
443600*------------------------------------------------------------             
443700                                                                          
443800     IF W-BER-START-VV = +53                                              
443900       MOVE 2             TO IY                                           
444000       MOVE 1             TO IX                                           
444100     ELSE                                                                 
444200       MOVE 1               TO IY                                         
444300       MOVE W-BER-START-VV  TO IX                                         
444400     END-IF                                                               
444500     ADD  2               TO IX                                           
444600     PERFORM UNTIL IY     >  RES-MAX-AA                                   
444700         PERFORM UNTIL IX >  RES-MAX-VV                                   
444800             MOVE ZERO    TO RES-BEHOV (IY, IX)                           
444900             ADD 1        TO IX                                           
445000         END-PERFORM                                                      
445100         ADD 1            TO IY                                           
445200         IF IX = 54                                                       
445300           MOVE 2         TO IX                                           
445400         ELSE                                                             
445500           MOVE 1         TO IX                                           
445600         END-IF                                                           
445700     END-PERFORM                                                          
445800     .                                                                    
445900     EJECT                                                                
446000 S10-ADD-TILL-RESULTAT-PBPLAN SECTION.                                    
446100     MOVE 'S10-ADD-TILL-RESULTAT-PBPLAN'  TO CURRENT-SECTION              
446200                                                                          
446300     IF W-BER-START-VV = +53                                              
446400       MOVE 2              TO IY                                          
446500       MOVE 1              TO IX                                          
446600     ELSE                                                                 
446700       MOVE 1               TO IY                                         
446800       MOVE W-BER-START-VV  TO IX                                         
446900     END-IF                                                               
447000     ADD  2                TO IX                                          
447100     PERFORM UNTIL IY      >  BER-MAX-AA                                  
447200         PERFORM UNTIL IX  > BER-MAX-VV                                   
447300             ADD BER-BEHOV (IY, IX) TO RES-BEHOV (IY, IX)                 
447400             ADD 1         TO IX                                          
447500         END-PERFORM                                                      
447600         ADD 1             TO IY                                          
447700         IF IX = 54                                                       
447800           MOVE 2          TO IX                                          
447900         ELSE                                                             
448000           MOVE 1          TO IX                                          
448100         END-IF                                                           
448200     END-PERFORM                                                          
448300     .                                                                    
448400     EJECT                                                                
448500 S13-READ-OR-TAB-B601 SECTION.                                            
448600                                                                          
448700     IF TAB-DCS-IDDC(1) = LOW-VALUE                                       
448800*--TAB IS EMPTY (FIRST CALL)                                              
448900       PERFORM IMS-GU-WDB601                                              
449000       MOVE DCS-WDB601 TO TAB-DCS-WDB601 (1)                              
449100     ELSE                                                                 
449200       MOVE +1 TO B601-IX                                                 
449300       PERFORM UNTIL B601-IX > MAX-B601-IX                                
449400         IF TAB-DCS-IDDC(B601-IX) = W-IDDC-B6                             
449500*--ALREADY SAVED. MOVE TAB TO DLI-IO-WDB601                               
449600           MOVE TAB-DCS-WDB601 (B601-IX) TO DCS-WDB601                    
449700           MOVE MAX-B601-IX TO B601-IX                                    
449800         ELSE                                                             
449900           IF TAB-DCS-IDDC(B601-IX) = LOW-VALUE                           
450000*--NO MATCH. SAVE A NEW IDDC IN TABEL                                     
450100             PERFORM IMS-GU-WDB601                                        
450200             MOVE DCS-WDB601 TO TAB-DCS-WDB601 (B601-IX)                  
450300             MOVE MAX-B601-IX TO B601-IX                                  
450400           END-IF                                                         
450500         END-IF                                                           
450600         ADD +1 TO B601-IX                                                
450700       END-PERFORM                                                        
450800       IF DCS-IDDC NOT = W-IDDC-B6                                        
450900*--NO MATCH. INDICATES THAT THE TABEL TO SMALL.                           
451000*--THERE ARE MORE THEN 80 XDC:S IN WDB601!!                               
451100         MOVE 'NO MATCH = TOO SMALL TABLE(80)' TO FELTEXT-STR             
451200         DISPLAY FELTEXT                                                  
451300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
451400       END-IF                                                             
451500     END-IF                                                               
451600     .                                                                    
451700     EJECT                                                                
451800 S20-NOLLSTALL-W-CDC-BEHOV-REF SECTION.                                   
451900     MOVE 'S20-NOLLSTALL-W-CDC-BEHOV-REF'  TO CURRENT-SECTION             
452000                                                                          
452100     MOVE ZERO     TO W-CDC-ACC-KVBEHOV                                   
452200                      W-CDC-KVBEHOV-VECKA                                 
452300                      W-CDC-KVBEHOV-DAG                                   
452400                      W-CDC-TILLGANG                                      
452500                      W-CDC-KVAR-EFT-VECKA                                
452600                      W-CDC-KVAR-EFT-DAG                                  
452700                      W-CDC-KVBEHOV-DESSUTOM                              
452800                      W-CDC-DIFF                                          
452900                      W-CDC-ACC-KVBEHOV-VECKA                             
453000                      W-CDC-KVAR-KVBEHOV-VECKA                            
453100                      W-KVBEHOV-INNEV-VECKA                               
453200                      W-KVBEHOV-52V                                       
453300                      W-DC-REFILL-KVANT                                   
453400                                                                          
453500     .                                                                    
453600     EJECT                                                                
453700 S21-NOLLSTALL-W-NDC-BEHOV-REF SECTION.                                   
453800     MOVE 'S21-NOLLSTALL-W-NDC-BEHOV-REF'  TO CURRENT-SECTION             
453900                                                                          
454000     MOVE ZERO     TO W-NDC-ACC-KVBEHOV                                   
454100                      W-NDC-KVBEHOV-VECKA                                 
454200                      W-NDC-KVBEHOV-DAG                                   
454300                      W-NDC-TILLGANG                                      
454400                      W-NDC-TILLGANG-ERS                                  
454500                      W-NDC-KVAR-EFT-VECKA                                
454600                      W-NDC-KVAR-EFT-DAG                                  
454700                      W-NDC-KVBEHOV-DESSUTOM                              
454800                      W-NDC-DIFF                                          
454900                      W-NDC-ACC-KVBEHOV-VECKA                             
455000                      W-NDC-KVAR-KVBEHOV-VECKA                            
455100                      W-KVBEHOV-INNEV-VECKA                               
455200                      W-KVBEHOV-52V                                       
455300                      W-DC-REFILL-KVANT                                   
455400                                                                          
455500                                                                          
455600     .                                                                    
455700     EJECT                                                                
455800 S22-NOLLSTALL-W-GDC-BEHOV-REF SECTION.                                   
455900     MOVE 'S22-NOLLSTALL-W-GDC-BEHOV-REF'  TO CURRENT-SECTION             
456000                                                                          
456100     MOVE ZERO     TO W-GDC-ACC-KVBEHOV                                   
456200                      W-GDC-KVBEHOV-VECKA                                 
456300                      W-GDC-KVBEHOV-DAG                                   
456400                      W-GDC-TILLGANG                                      
456500                      W-GDC-TILLGANG-ERS                                  
456600                      W-GDC-KVAR-EFT-VECKA                                
456700                      W-GDC-KVAR-EFT-DAG                                  
456800                      W-GDC-KVBEHOV-DESSUTOM                              
456900                      W-GDC-DIFF                                          
457000                      W-GDC-ACC-KVBEHOV-VECKA                             
457100                      W-GDC-KVAR-KVBEHOV-VECKA                            
457200                      W-KVBEHOV-INNEV-VECKA                               
457300                      W-KVBEHOV-52V                                       
457400                      W-DC-REFILL-KVANT                                   
457500                                                                          
457600                                                                          
457700     .                                                                    
457800     EJECT                                                                
457900 S23-GET-WEEK-DEMAND-XDC SECTION.                                         
458000     MOVE 'S23-GET-WEEK-DEMAND-XDC' TO CURRENT-SECTION                    
458100                                                                          
458200     INITIALIZE UTUP-W271UTUP                                             
458300     MOVE NEJ                       TO W-JUST-PB-FINNS                    
458400     MOVE LINK-IDARTNR              TO UTUP-IDARTNR                       
458500     MOVE SLAG-IDDC                 TO UTUP-IDDC                          
458600     MOVE SLAG-IDDC-REF             TO UTUP-IDDC-REF                      
458700     MOVE W-BINNDAY                 TO UTUP-TIAAMMDD                      
458800     MOVE 003                       TO UTUP-KDCALL                        
458900*                                                                         
459000     CALL W271UTUP USING UTUP-W271UTUP                                    
459100                         UTUP-WDK7-PCB                                    
459200                         UTUP-WDB6-PCB                                    
459300                         UTUP-UTIL-WDK6-PCB                               
459400                         UTUP-UTIL-WDK7-PCB                               
459500                         UTUP-UTIL-WDB6-PCB                               
459600     IF UTUP-KDSVAR-OK                                                    
459700        IF UTUP-FLPB-JUST = JA                                            
459800           MOVE JA                  TO W-JUST-PB-FINNS                    
459900        END-IF                                                            
460000     ELSE                                                                 
460100        DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                              
460200        CALL FELLOG                                                       
460300     END-IF                                                               
460400     .                                                                    
460500     EJECT                                                                
460600 S24-ADJUST-LOW-DEMAND  SECTION.                                          
460700                                                                          
460800     MOVE NEJ                       TO SW-DEMAND-ADJUST                   
460900     IF W-JUST-PB-FINNS = NEJ                                             
461000        IF  SLAG-KVREFPKT            = +1                                 
461100        AND SLAG-KVREFBER            = +1                                 
461200        AND CLAG-KVQPACK-1           < 2                                  
461300        AND W-DC-REFILL-KVANT        >= +1                                
461400            SET DEMAND-ADJUST       TO TRUE                               
461500        END-IF                                                            
461600     ELSE                                                                 
461700        IF  REFL1-REFL-KVREFPKT      = +1                                 
461800        AND REFL1-REFL-KVREFBER      = +1                                 
461900        AND CLAG-KVQPACK-1           < 2                                  
462000        AND W-DC-REFILL-KVANT        >= +1                                
462100            SET DEMAND-ADJUST       TO TRUE                               
462200        END-IF                                                            
462300     END-IF                                                               
462400     .                                                                    
462500     EJECT                                                                
462600 S25-FINNS-JUST-PB-CDC  SECTION.                                          
462700     MOVE 'S25-FINNS-JUST-PB '  TO CURRENT-SECTION                        
462800                                                                          
462900     INITIALIZE UTIL-W271UTIL                                             
463000     MOVE NEJ                       TO W-JUST-PB-FINNS                    
463100     MOVE LINK-IDARTNR              TO UTIL-IDARTNR                       
463200     MOVE WC-CDC-SE                 TO UTIL-IDDC                          
463300     MOVE CREF-IDDC-REF             TO UTIL-IDDC-REF                      
463400     MOVE 003                       TO UTIL-KDCALL                        
463500     CALL W271UTIL USING UTIL-W271UTIL                                    
463600                         UTIL-WDK6-PCB                                    
463700                         UTIL-WDK7-PCB                                    
463800                         UTIL-WDB6-PCB                                    
463900                                                                          
464000     IF UTIL-KDSVAR-OK                                                    
464100        IF UTIL-FLPB-JUST = JA                                            
464200           MOVE JA                  TO W-JUST-PB-FINNS                    
464300        END-IF                                                            
464400     ELSE                                                                 
464500        MOVE 'FEL FRÅN W271UTIL '                                         
464600                                TO FELTEXT-STR                            
464700        DISPLAY FELTEXT                                                   
464800        PERFORM S99-ABEND                                                 
464900     END-IF                                                               
465000     .                                                                    
465100     EJECT                                                                
465200 S27-BEHOV-VECKA   SECTION.                                               
465300     MOVE 'S27-BEHOV-VECKA  '         TO CURRENT-SECTION                  
465400                                                                          
465500     INITIALIZE L222-W222L222                                             
465600                                                                          
465700     PERFORM S27A-LEDTID-ADJMT                                            
465800     IF DCS-CDC                                                           
465900       MOVE LINK-IDARTNR              TO L222-IDARTNR                     
466000       MOVE WS-DAGENS-DAGNR           TO L222-TID-AKTUELL                 
466100       MOVE SPACE                     TO L222-IDDC                        
466200       MOVE NEJ                       TO L222-FLINKLDIRLEV                
466300       MOVE SEP-SATS-TPO-LEV-SDC-NDC  TO L222-KDBEHOV                     
466400       MOVE 52                        TO L222-KVVECKOR-BEHOV              
466500       MOVE WS-TIAAVV-L222            TO L222-TIAAVV-AKTUELL              
466600                                         L222-TIBEHOV-START               
466700                                         DATUM-AAVV                       
466800       MOVE 1                         TO W-ANTAL-VECKOR                   
466900                                                                          
467000       CALL W009VADD USING  DATUM-AAVV                                    
467100                            W-ANTAL-VECKOR                                
467200                                                                          
467300       MOVE DATUM-AAVV                TO L222-TIBEHOV-START               
467400                                                                          
467500       MOVE ZERO                      TO WS-LEDTIDSBEHOV                  
467600                                                                          
467700       CALL W22222 USING L222-AREA W222-WDK6-PCB W222-WDK7-PCB            
467800                         W222-ARTM-PCB                                    
467900                         W222-2501-PCB W222-WDB6R-PCB                     
468000                         W222-WDK7R-PCB W222-WDB6-PCB                     
468100                         W222-WDD7-PCB W222-WDK7E-PCB                     
468200                         W222-UTIL-WDK6-PCB                               
468300                         W222-UTIL-WDK7-PCB                               
468400                         W222-UTIL-WDB6-PCB                               
468500                         W222-UTUP-WDK7-PCB                               
468600                         W222-UTUP-WDB6-PCB                               
468700                         W222-UTUP-UTIL-WDK6-PCB                          
468800                         W222-UTUP-UTIL-WDK7-PCB                          
468900                         W222-UTUP-UTIL-WDB6-PCB                          
469000                                                                          
469100       IF L222-ANROP-FEL                                                  
469200          PERFORM S27B-NOLLA-W22222                                       
469300       ELSE                                                               
469400          MOVE +1                     TO INDX-L                           
469500                                         INDX1-L                          
469600          MOVE L222-KVBEHOV-DESSUTOM  TO W-BEHOV-LT-VECKA(INDX1-L)        
469700                                         W-BEHOV-LT-TOT(INDX1-L)          
469800          PERFORM S29-SEP-BEHOV-INNEV-VECKA                               
469900          ADD WS-KVPB-DESSUTOM        TO W-BEHOV-LT-VECKA(INDX1-L)        
470000                                         W-BEHOV-LT-TOT(INDX1-L)          
470100*                                                                         
470200          ADD +1                      TO INDX1-L                          
470300          PERFORM UNTIL INDX1-L > INDX1-LT-MAX                            
470400            MOVE L222-KVBEHOV-VECKA(INDX-L)                               
470500                                      TO W-BEHOV-LT-VECKA(INDX1-L)        
470600            MOVE W-BEHOV-LT-TOT(INDX1-L - 1)                              
470700                                      TO W-BEHOV-LT-TOT(INDX1-L)          
470800            ADD  L222-KVBEHOV-VECKA(INDX-L)                               
470900                                      TO W-BEHOV-LT-TOT(INDX1-L)          
471000            ADD +1                    TO INDX-L                           
471100                                         INDX1-L                          
471200          END-PERFORM                                                     
471300       END-IF                                                             
471400     END-IF                                                               
471500     .                                                                    
471600     EJECT                                                                
471700 S27A-LEDTID-ADJMT  SECTION.                                              
471800     MOVE 'S27A-LEDTID-ADJMT   '    TO CURRENT-SECTION                    
471900                                                                          
472000     PERFORM S34-BER-IX-AKTUELLT-DC                                       
472100                                                                          
472200     MOVE ZERO                    TO DAYS-KVDAYS                          
472300     IF DCS-CDC                                                           
472400       IF CREF-FLFLYG = JA                                                
472500         DIVIDE REF-KVDLTID-AIRETA                                        
472600                   BY 7                                                   
472700                   GIVING WS-LEADTIME-WEEKS                               
472800                   REMAINDER WS-REST-DAYS                                 
472900         MOVE REF-KVDLTID-AIRETA  TO DAYS-KVDAYS                          
473000       ELSE                                                               
473100         DIVIDE REF-KVDLTID-TOT                                           
473200                   BY 7                                                   
473300                   GIVING WS-LEADTIME-WEEKS                               
473400                   REMAINDER WS-REST-DAYS                                 
473500         MOVE REF-KVDLTID-TOT     TO DAYS-KVDAYS                          
473600       END-IF                                                             
473700     END-IF                                                               
473800                                                                          
473900     COMPUTE DAYS-KVDAYS = (DAYS-KVDAYS - 1)                              
474000***                                                                       
474100***  CALCULATE OF LEADTIME ADJUSTED FUTURE DATE                           
474200***                                                                       
474300     MOVE WS-DAGENS-DATUM-AAMMDD   TO DAYS-TIDATE1                        
474400     MOVE 'YYMMDD'                 TO DAYS-KDDATFMT1                      
474500     MOVE 'YYMMDD'                 TO DAYS-KDDATFMT2                      
474600     MOVE SPACE                    TO DAYS-TIDATE2                        
474700                                      DAYS-IDCALEND                       
474800                                                                          
474900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
475000                                                                          
475100     IF DAYS-KDRC = 8                                                     
475200        STRING 'FEL VID ANROP 1 TILL WZ20DAYS - SEC S27A'                 
475300        DELIMITED BY SIZE INTO FELTEXT-STR                                
475400        DISPLAY FELTEXT                                                   
475500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
475600     ELSE                                                                 
475700        MOVE DAYS-TIDATE2(1:6)     TO WS-LTDATE-AAMMDD                    
475800     END-IF                                                               
475900*                                                                         
476000     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
476100     MOVE WS-LTDATE-AAMMDD         TO DAT-I-TIDATUM                       
476200                                                                          
476300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
476400                         DAT-O-TIDATUM DAT-KDSVAR                         
476500                                                                          
476600     IF DAT-KDSVAR-OK                                                     
476700        MOVE DAT-TID               TO WS-DAT-TID                          
476800     ELSE                                                                 
476900        STRING ' FEL FRÅN WDATKONV I W222BHDC'                            
477000               ' (SEC - S27A ANROP 1)'                                    
477100        DELIMITED BY SIZE INTO FELTEXT-STR                                
477200        DISPLAY FELTEXT                                                   
477300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
477400     END-IF                                                               
477500*                                                                         
477600     COMPUTE WS-REST-DAYS          = WS-REST-DAYS                         
477700                                   + WS-DAGENS-DAGNR                      
477800     COMPUTE WS-REST-DAYS          = WS-REST-DAYS - 1                     
477900                                                                          
478000     IF  WS-REST-DAYS       > ZERO                                        
478100         COMPUTE WS-LEADTIME-WEEKS-PLUS1                                  
478200                                   = WS-LEADTIME-WEEKS + 1                
478300         IF WS-REST-DAYS    > 7                                           
478400            COMPUTE WS-LEADTIME-WEEKS-PLUS1                               
478500                                   = WS-LEADTIME-WEEKS + 1                
478600         END-IF                                                           
478700     ELSE                                                                 
478800       MOVE WS-LEADTIME-WEEKS     TO WS-LEADTIME-WEEKS-PLUS1              
478900     END-IF                                                               
479000                                                                          
479100     IF WS-LEADTIME-WEEKS-PLUS1 = ZERO                                    
479200        MOVE 1                    TO WS-LEADTIME-WEEKS-PLUS1              
479300     END-IF                                                               
479400                                                                          
479500***  BELOW IS THE DAY IN THE WEEK AFTER ADJUSTING CURRENT DATE            
479600***  WITH LEADTIME. CALCULATED ABOVE                                      
479700     IF WS-DAT-TID > ZERO                                                 
479800        DIVIDE WS-DAT-TID                                                 
479900                  BY 7                                                    
480000                  GIVING WS-TIVV-TEMP                                     
480100                  REMAINDER WS-REST-DAYS-LT-F                             
480200     END-IF                                                               
480300                                                                          
480400     MOVE WS-DAGENS-DATUM-AAVV    TO WS-TIAAVV-L222                       
480500***                                                                       
480600***  FOR ALL WEEKS EXCEPT CURRENT WEEK, WEEK SHOULD START DAY 1           
480700***  BELOW CALCULATION IS TO FIND THE DAY NO IN LT ADJ WEEK               
480800                                                                          
480900     MOVE 1                       TO WS-DAGENS-AAVVD(5:1)                 
481000     MOVE WS-DAGENS-AAVVD         TO DAYS-TIDATE1                         
481100     MOVE 'YYWWD'                 TO DAYS-KDDATFMT1                       
481200     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
481300     MOVE SPACE                   TO DAYS-TIDATE2                         
481400                                     DAYS-IDCALEND                        
481500                                                                          
481600     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
481700                                                                          
481800     IF DAYS-KDRC = 8                                                     
481900        STRING 'FEL VID ANROP 2 TILL WZ20DAYS - SEC S27A'                 
482000        DELIMITED BY SIZE INTO FELTEXT-STR                                
482100        DISPLAY FELTEXT                                                   
482200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
482300     ELSE                                                                 
482400        MOVE DAYS-TIDATE2(1:6)    TO WS-LTDATE-VECKA-AAMMDD               
482500     END-IF                                                               
482600*                                                                         
482700     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
482800     MOVE WS-LTDATE-VECKA-AAMMDD  TO DAT-I-TIDATUM                        
482900                                                                          
483000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
483100                         DAT-O-TIDATUM DAT-KDSVAR                         
483200                                                                          
483300     IF DAT-KDSVAR-OK                                                     
483400                                                                          
483500        MOVE DAT-TID              TO WS-DAT-TID-VECKA                     
483600     ELSE                                                                 
483700        STRING ' FEL FRÅN WDATKONV I W222BHDC'                            
483800               ' (SEC - S27A ANROP 2)'                                    
483900        DELIMITED BY SIZE INTO FELTEXT-STR                                
484000        DISPLAY FELTEXT                                                   
484100        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
484200     END-IF                                                               
484300                                                                          
484400***  BELOW IS THE DAY IN THE WEEK AFTER ADJUSTING CURRENT WEEK            
484500***  STARTING DAY 1, WITH LEADTIME.                                       
484600     IF WS-DAT-TID-VECKA > ZERO                                           
484700        DIVIDE WS-DAT-TID-VECKA                                           
484800                  BY 7                                                    
484900                  GIVING WS-TIVV-TEMP                                     
485000                  REMAINDER WS-REST-DAYS-LT-V                             
485100     END-IF                                                               
485200     IF WS-REST-DAYS-LT-V > ZERO                                          
485300        COMPUTE WS-LEADTIME-WEEKS-VECKA                                   
485400                                   = WS-LEADTIME-WEEKS + 1                
485500     ELSE                                                                 
485600        MOVE WS-LEADTIME-WEEKS    TO WS-LEADTIME-WEEKS-VECKA              
485700     END-IF                                                               
485800     .                                                                    
485900     EJECT                                                                
486000 S27B-NOLLA-W22222 SECTION.                                               
486100                                                                          
486200     MOVE +0                     TO L222-KVBEHOV-SUMMA                    
486300                                    L222-KVBEHOV-DESSUTOM                 
486400                                    L222-TIBEHOV-FIRST                    
486500                                                                          
486600     MOVE +1                     TO INDX-L                                
486700     PERFORM UNTIL INDX-L > 156                                           
486800       MOVE +0                   TO L222-KVBEHOV-VECKA(INDX-L)            
486900                                                                          
487000       ADD +1                    TO INDX-L                                
487100     END-PERFORM                                                          
487200     .                                                                    
487300     EJECT                                                                
487400 S28-TOTAL-LEATIME-NEED SECTION.                                          
487500                                                                          
487600     MOVE ZERO                     TO WS-LEDTIDSBEHOV                     
487700                                                                          
487800     MOVE W-BEHOV-LT-TOT (INDX1-L) TO WS-LEDTIDSBEHOV                     
487900     PERFORM S28A-LAST-WEEK-NEED                                          
488000     COMPUTE WS-LEDTIDSBEHOV        = WS-LEDTIDSBEHOV                     
488100                                    - WS-LAST-MINUS-NEED                  
488200     IF INDX-L > +1                                                       
488300        COMPUTE WS-LEDTIDSBEHOV     = WS-LEDTIDSBEHOV                     
488400                                    - W-BEHOV-LT-TOT (INDX-L - 1)         
488500     END-IF                                                               
488600     .                                                                    
488700     EJECT                                                                
488800 S28A-LAST-WEEK-NEED SECTION.                                             
488900                                                                          
489000     IF WS-REST-DAYS-ADJ > 0                                              
489100       IF WS-LEADTIME-WEEKS-PLUS1 > ZERO                                  
489200         COMPUTE WS-NONEED-DAYS = 7 - WS-REST-DAYS-ADJ                    
489300         COMPUTE WS-DAY-NEED-LAST =                                       
489400                               W-BEHOV-LT-VECKA(INDX1-L) / 7              
489500         COMPUTE WS-LAST-MINUS-NEED ROUNDED = WS-NONEED-DAYS *            
489600                                     WS-DAY-NEED-LAST                     
489700       ELSE                                                               
489800         MOVE ZERO TO WS-LAST-MINUS-NEED                                  
489900       END-IF                                                             
490000     ELSE                                                                 
490100       MOVE ZERO TO WS-LAST-MINUS-NEED                                    
490200     END-IF                                                               
490300     .                                                                    
490400     EJECT                                                                
490500 S29-SEP-BEHOV-INNEV-VECKA  SECTION.                                      
490600                                                                          
490700     MOVE ZERO                        TO WS-KVPB-DESSUTOM                 
490800*                                                                         
490900     COMPUTE WS-VECKO-SEP-BEHOV ROUNDED                                   
491000                                    = CLAG-KVPB-SEP / 4.33                
491100     COMPUTE WS-DAG-SEP-BEHOV ROUNDED                                     
491200                                    = WS-VECKO-SEP-BEHOV / 5              
491300                                                                          
491400     IF   WS-DAGENS-DAGNR = 7                                             
491500     OR   WS-DAGENS-DAGNR = 6                                             
491600     OR  (WS-DAGENS-DAGNR = 5                                             
491700     AND  W-TIME (1:4)   > 1700)                                          
491800          MOVE ZERO                TO WS-KVPB-DESSUTOM                    
491900     ELSE                                                                 
492000          COMPUTE WS-KVDAGAR-KVAR   = (5 - WS-DAGENS-DAGNR)               
492100          IF W-TIME(1:4)           <= 1700                                
492200             ADD +1                TO WS-KVDAGAR-KVAR                     
492300          END-IF                                                          
492400                                                                          
492500          MOVE +1                  TO WS-FAKTOR                           
492600          SUBTRACT CLAG-REDIRLEV FROM WS-FAKTOR                           
492700                                                                          
492800          COMPUTE WS-KVPB-DESSUTOM  = WS-DAG-SEP-BEHOV *                  
492900                                      WS-KVDAGAR-KVAR  *                  
493000                                      WS-FAKTOR                           
493100     END-IF                                                               
493200     .                                                                    
493300     EJECT                                                                
493400 S30-BER-SATS-TPO-LEDTIDSBEHOV  SECTION.                                  
493500     MOVE 'S30-BER-SATS-TPO-LEDTIDSBEHOV'  TO CURRENT-SECTION             
493600                                                                          
493700     PERFORM S34-BER-IX-AKTUELLT-DC                                       
493800     IF CREF-FLFLYG = JA                                                  
493900       MOVE REF-KVDLTID-AIRETA TO DAYS-KVDAYS                             
494000     ELSE                                                                 
494100       MOVE REF-KVDLTID-TOT    TO DAYS-KVDAYS                             
494200     END-IF                                                               
494300                                                                          
494400***  CALCULATE DEMAND WEEK AT SUPPLIER OR REFILLING WAREHOUSE             
494500     MOVE SPACE                    TO DAYS-TIDATE1                        
494600     MOVE 'YYWW'                   TO DAYS-KDDATFMT1                      
494700     MOVE WS-SATS-TPO-BEHOV-AAVV   TO DAYS-TIDATE2                        
494800     MOVE 'YYWW'                   TO DAYS-KDDATFMT2                      
494900                                      DAYS-IDCALEND                       
495000                                                                          
495100     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
495200                                                                          
495300     IF DAYS-KDRC = 8                                                     
495400       STRING 'FEL VID ANROP TILL WZ20DAYS 1'                             
495500       DELIMITED BY SIZE INTO FELTEXT-STR                                 
495600       DISPLAY FELTEXT                                                    
495700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
495800     ELSE                                                                 
495900       MOVE DAYS-TIDATE1(1:4)       TO WS-BEHOVSVECKA-AAVV                
496000     END-IF                                                               
496100                                                                          
496200     .                                                                    
496300     EJECT                                                                
496400 S33-BER-IX-AKTUELLT-DC SECTION.                                          
496500     MOVE 'S33-BER-IX-AKTUELLT-DC '   TO CURRENT-SECTION                  
496600                                                                          
496700     MOVE SLAG-IDDC      TO W-IDDC-B6                                     
496800     MOVE SLAG-IDDC-REF  TO W-IDDC-B616                                   
496900     PERFORM S3X-READ-OR-TAB-B616                                         
497000     .                                                                    
497100     EJECT                                                                
497200 S34-BER-IX-AKTUELLT-DC SECTION.                                          
497300     MOVE 'S34-BER-IX-AKTUELLT-DC '   TO CURRENT-SECTION                  
497400                                                                          
497500     MOVE WC-CDC-SE      TO W-IDDC-B6                                     
497600     MOVE CLAG-IDDC-REF  TO W-IDDC-B616                                   
497700     PERFORM S3X-READ-OR-TAB-B616                                         
497800     .                                                                    
497900     EJECT                                                                
498000 S3X-READ-OR-TAB-B616 SECTION.                                            
498100     MOVE 'S3X-READ-OR-TAB-B616 '  TO CURRENT-SECTION                     
498200                                                                          
498300     IF TAB-IDDC(1) = LOW-VALUE                                           
498400*--TAB IS EMPTY (FIRST CALL)                                              
498500       MOVE W-IDDC-B6      TO TAB-IDDC(1)                                 
498600       PERFORM IMS-GU-WDB616                                              
498700       MOVE REF-WDB616 TO TAB-REF-WDB616 (1)                              
498800     ELSE                                                                 
498900       MOVE +1 TO B616-IX                                                 
499000       PERFORM UNTIL B616-IX > MAX-B616-IX                                
499100         IF TAB-REF-IDDC-REF(B616-IX) = W-IDDC-B616 AND                   
499200            TAB-IDDC(B616-IX)         = W-IDDC-B6                         
499300*--ALREADY SAVED. MOVE TAB TO DLI-IO-WDB616                               
499400           MOVE TAB-REF-WDB616 (B616-IX) TO REF-WDB616                    
499500           MOVE MAX-B616-IX TO B616-IX                                    
499600         ELSE                                                             
499700           IF TAB-IDDC(B616-IX) = LOW-VALUE                               
499800*--NO MATCH. SAVE A NEW IDDC IN TABEL                                     
499900             MOVE W-IDDC-B6      TO TAB-IDDC(B616-IX)                     
500000             PERFORM IMS-GU-WDB616                                        
500100             MOVE REF-WDB616 TO TAB-REF-WDB616 (B616-IX)                  
500200             MOVE MAX-B616-IX TO B616-IX                                  
500300           END-IF                                                         
500400         END-IF                                                           
500500         ADD +1 TO B616-IX                                                
500600       END-PERFORM                                                        
500700       IF DCS-IDDC     = W-IDDC-B6     AND                                
500800          REF-IDDC-REF = W-IDDC-B616                                      
500900*--OK. WE GOT A MATCH                                                     
501000          CONTINUE                                                        
501100       ELSE                                                               
501200*--NO MATCH. INDICATES THAT THE CHART TO SMALL.                           
501300*--THERE ARE MORE THEN 180 DC/DC-REF COMBINATIONS!!                       
501400         MOVE 'NO MATCH = TOO SMALL TABLE(180)' TO FELTEXT-STR            
501500         DISPLAY FELTEXT                                                  
501600         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
501700       END-IF                                                             
501800     END-IF                                                               
501900     .                                                                    
502000     EJECT                                                                
502100 S77-KOLLA-DAPUBL  SECTION.                                               
502200     MOVE 'S77-KOLLA-DAPUBL ' TO CURRENT-SECTION                          
502300                                                                          
502400     MOVE 1 TO PUBL-IX                                                    
502500     PERFORM UNTIL PUBL-IDLANDX2(PUBL-IX) = DCS-IDLANDX2                  
502600                OR PUBL-IDLANDX2(PUBL-IX) = SPACE                         
502700        ADD 1 TO PUBL-IX                                                  
502800     END-PERFORM                                                          
502900     .                                                                    
503000     EJECT                                                                
503100 S80-CALC-LT-ADJ-PUBWK SECTION.                                           
503200     MOVE 'S80-CALC-LT-ADJ-PUBWK'  TO CURRENT-SECTION                     
503300                                                                          
503400     MOVE ZEROS                 TO WS-LTID                                
503500                                   WS-LTID-A                              
503600                                   WS-LTID-B                              
503700                                   WS-LT-WEEKS                            
503800                                   WS-LT-WEEKS-DAYS                       
503900                                   WS-REMN-DAYS                           
504000                                   W-TIFINLV-AAVV-MINUS-LT                
504100                                                                          
504200     IF CDCBEHOV-SW = JA                                                  
504300        PERFORM S34-BER-IX-AKTUELLT-DC                                    
504400     ELSE                                                                 
504500        PERFORM S33-BER-IX-AKTUELLT-DC                                    
504600     END-IF                                                               
504700                                                                          
504800     MOVE REF-KVDLTID-AIRETA    TO WS-LTID-A                              
504900     MOVE REF-KVDLTID-TOT       TO WS-LTID-B                              
505000                                                                          
505100*    DIVIDE BY 7 TO GET NUMBER OF WEEKS                                   
505200*    DECIMAL OR REMAINDER IS IGNORED IN BELOW COMPUTE                     
505300*                                                                         
505400     IF WS-FLFLYG = JA                                                    
505500        MOVE WS-LTID-A          TO WS-LTID                                
505600        DIVIDE  WS-LTID-A  BY 7                                           
505700                       GIVING WS-LT-WEEKS                                 
505800                    REMAINDER WS-REMN-DAYS                                
505900     ELSE                                                                 
506000        MOVE WS-LTID-B          TO WS-LTID                                
506100        DIVIDE  WS-LTID-B  BY 7                                           
506200                       GIVING WS-LT-WEEKS                                 
506300                    REMAINDER WS-REMN-DAYS                                
506400     END-IF                                                               
506500*                                                                         
506600     IF WS-REMN-DAYS > ZERO                                               
506700        ADD +1             TO WS-LT-WEEKS                                 
506800     END-IF                                                               
506900     COMPUTE WS-LT-WEEKS-DAYS = (7 * WS-LT-WEEKS)                         
507000*                                                                         
507100     IF WS-LT-WEEKS < 2                                                   
507200        MOVE +2                 TO WS-LT-WEEKS                            
507300     END-IF                                                               
507400     COMPUTE WS-LT-WEEKS         = (-1 * WS-LT-WEEKS)                     
507500*                                                                         
507600     MOVE W-TIFINLV-AAVV        TO W-TIFINLV-AAVV-MINUS-LT                
507700     MOVE WS-LT-WEEKS           TO W-ANTAL-VECKOR                         
507800     CALL W009VADD USING W-TIFINLV-AAVV-MINUS-LT W-ANTAL-VECKOR           
507900     .                                                                    
508000     EJECT                                                                
508100 S99-ABEND SECTION.                                                       
508200     MOVE 'S99-ABEND   '        TO CURRENT-SECTION                        
508300                                                                          
508400     SKIP2                                                                
508500     DISPLAY 'SENASTE IMS-CALL= ' DBS-SECTION                             
508600     DISPLAY 'I PROGRAM W222BHDC'                                         
508700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
508800     .                                                                    
508900     EJECT                                                                
509000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
509100* --- IMS SEKTIONER ---                                         *         
509200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
509300 IMS-GU-WDK601 SECTION.                                                   
509400     MOVE 'IMS-GU-WDK601     '      TO DBS-SECTION                        
509500                                                                          
509600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
509700            DELIMITED BY SIZE INTO SSA1                                   
509800     MOVE '  GE' TO GODK-STATUSKODER                                      
509900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601  SSA1                   
510000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
510100     PERFORM IMS-STATUSKONTROLL                                           
510200     .                                                                    
510300     SKIP3                                                                
510400 IMS-GNP-WDK611 SECTION.                                                  
510500     MOVE 'IMS-GNP-WDK611     '      TO DBS-SECTION                       
510600                                                                          
510700     MOVE 'WDK611  '       TO SSA1                                        
510800     MOVE '  GE' TO GODK-STATUSKODER                                      
510900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611  SSA1                  
511000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
511100     PERFORM IMS-STATUSKONTROLL                                           
511200     .                                                                    
511300     SKIP3                                                                
511400 IMS-GNP-WDK629 SECTION.                                                  
511500     MOVE 'IMS-GNP-WDK629     '      TO DBS-SECTION                       
511600                                                                          
511700     MOVE 'WDK629  '       TO SSA1                                        
511800     MOVE '  GE' TO GODK-STATUSKODER                                      
511900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629  SSA1                  
512000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
512100     PERFORM IMS-STATUSKONTROLL                                           
512200     .                                                                    
512300     SKIP3                                                                
512400 IMS-GNP-WDK626 SECTION.                                                  
512500     MOVE 'IMS-GNP-WDK626     '      TO DBS-SECTION                       
512600                                                                          
512700     MOVE 'WDK611    ' TO SSA1                                            
512800     MOVE 'WDK626    ' TO SSA2                                            
512900     MOVE '  GE' TO GODK-STATUSKODER                                      
513000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK626  SSA1 SSA2             
513100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
513200     PERFORM IMS-STATUSKONTROLL                                           
513300     .                                                                    
513400     SKIP3                                                                
513500 IMS-GNP-WDK624-FIRST SECTION.                                            
513600     MOVE 'IMS-GNP-WDK624-FIRST  '      TO DBS-SECTION                    
513700                                                                          
513800     MOVE 'WDK611    ' TO SSA1                                            
513900     MOVE 'WDK624  *F' TO SSA2                                            
514000     MOVE '  GE' TO GODK-STATUSKODER                                      
514100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK624  SSA1 SSA2             
514200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
514300     PERFORM IMS-STATUSKONTROLL                                           
514400     .                                                                    
514500     SKIP3                                                                
514600 IMS-GNP-WDK624-NEXT  SECTION.                                            
514700     MOVE 'IMS-GNP-WDK624-NEXT  '      TO DBS-SECTION                     
514800                                                                          
514900     MOVE SPACE        TO ALL-SSA                                         
515000     MOVE 'WDK611    ' TO SSA1                                            
515100     MOVE 'WDK624    ' TO SSA2                                            
515200     MOVE '  GE' TO GODK-STATUSKODER                                      
515300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK624  SSA1 SSA2             
515400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
515500     PERFORM IMS-STATUSKONTROLL                                           
515600     .                                                                    
515700     SKIP3                                                                
515800 IMS-GU-WDK601-ERS SECTION.                                               
515900     MOVE 'IMS-GU-WDK601-ERS '      TO DBS-SECTION                        
516000                                                                          
516100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-ERS-X                         
516200                    '&KDERS    =' W-KDERS-0-X ')'                         
516300            DELIMITED BY SIZE INTO SSA1                                   
516400     MOVE '  GE' TO GODK-STATUSKODER                                      
516500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601-ERS  SSA1               
516600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
516700     PERFORM IMS-STATUSKONTROLL                                           
516800     .                                                                    
516900     SKIP3                                                                
517000 IMS-GNP-WDK611-ERS SECTION.                                              
517100     MOVE 'IMS-GNP-WDK611-ERS '      TO DBS-SECTION                       
517200                                                                          
517300     MOVE 'WDK611  '       TO SSA1                                        
517400     MOVE '  GE' TO GODK-STATUSKODER                                      
517500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611-ERS  SSA1              
517600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
517700     PERFORM IMS-STATUSKONTROLL                                           
517800     .                                                                    
517900     SKIP3                                                                
518000 IMS-GU-WDK701 SECTION.                                                   
518100     MOVE 'IMS-GU-WDK701    '   TO DBS-SECTION                            
518200                                                                          
518300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
518400          DELIMITED BY SIZE INTO SSA1                                     
518500     MOVE '  GE' TO GODK-STATUSKODER                                      
518600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
518700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
518800     PERFORM IMS-STATUSKONTROLL                                           
518900     .                                                                    
519000     EJECT                                                                
519100 IMS-GNP-WDK711-REF SECTION.                                              
519200     MOVE 'IMS-GNP-WDK711-REF  '      TO DBS-SECTION                      
519300                                                                          
519400     STRING 'WDK711  (IDDC1    =' W-IDDC1-X                               
519500                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
519600          DELIMITED BY SIZE INTO SSA1                                     
519700     MOVE '  GE' TO GODK-STATUSKODER                                      
519800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
519900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
520000     PERFORM IMS-STATUSKONTROLL                                           
520100     .                                                                    
520200     EJECT                                                                
520300 IMS-GNP-WDK711-REF-XDC SECTION.                                          
520400     MOVE 'IMS-GNP-WDK711-REF-XDC  '  TO DBS-SECTION                      
520500                                                                          
520600     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
520700          DELIMITED BY SIZE INTO SSA1                                     
520800     MOVE '  GE' TO GODK-STATUSKODER                                      
520900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
521000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
521100     PERFORM IMS-STATUSKONTROLL                                           
521200     .                                                                    
521300     EJECT                                                                
521400 IMS-GNP-WDK711-REF-G SECTION.                                            
521500     MOVE 'IMS-GNP-WDK711-REF-G  '      TO DBS-SECTION                    
521600                                                                          
521700     STRING 'WDK711  (IDDC1   NE' W-IDDC-NON-X                            
521800                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
521900          DELIMITED BY SIZE INTO SSA1                                     
522000     MOVE '  GE' TO GODK-STATUSKODER                                      
522100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
522200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
522300     PERFORM IMS-STATUSKONTROLL                                           
522400     .                                                                    
522500     EJECT                                                                
522600 IMS-GU-WDK711 SECTION.                                                   
522700     MOVE 'IMS-GU-WDK711  '      TO DBS-SECTION                           
522800                                                                          
522900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
523000          DELIMITED BY SIZE INTO SSA1                                     
523100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
523200          DELIMITED BY SIZE INTO SSA2                                     
523300     MOVE '  GE' TO GODK-STATUSKODER                                      
523400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
523500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
523600     PERFORM IMS-STATUSKONTROLL                                           
523700     .                                                                    
523800     EJECT                                                                
523900 IMS-GU-WDK711-ERS SECTION.                                               
524000     MOVE 'IMS-GU-WDK711-E'      TO DBS-SECTION                           
524100                                                                          
524200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-ERS-X ')'                  
524300          DELIMITED BY SIZE INTO SSA1                                     
524400     STRING 'WDK711  (IDDC     =' W-IDDC-ERS-X ')'                        
524500          DELIMITED BY SIZE INTO SSA2                                     
524600     MOVE '  GE' TO GODK-STATUSKODER                                      
524700     CALL CBLTDLI USING GU WDK7-ERS-PCB DLI-IO-WDK711-ERS                 
524800                           SSA1 SSA2                                      
524900     MOVE WDK7-ERS-STATUS-CODE TO STATUS-WS                               
525000     PERFORM IMS-STATUSKONTROLL                                           
525100     .                                                                    
525200     EJECT                                                                
525300 IMS-GNP-WDK712  SECTION.                                                 
525400     MOVE 'IMS-GNP-WDK712 ' TO DBS-SECTION                                
525500                                                                          
525600     MOVE 'WDK712  '        TO SSA1                                       
525700     MOVE '  GE'            TO GODK-STATUSKODER                           
525800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK712 SSA1                   
525900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
526000     PERFORM IMS-STATUSKONTROLL                                           
526100     .                                                                    
526200     EJECT                                                                
526300 IMS-GNP-WDK722  SECTION.                                                 
526400     MOVE 'IMS-GNP-WDK722 ' TO DBS-SECTION                                
526500                                                                          
526600     MOVE 'WDK722  '        TO SSA1                                       
526700     MOVE '  GE'            TO GODK-STATUSKODER                           
526800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
526900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
527000     PERFORM IMS-STATUSKONTROLL                                           
527100     .                                                                    
527200     EJECT                                                                
527300 IMS-GU-WDB601    SECTION.                                                
527400     MOVE 'IMS-GU-WDB601  ' TO DBS-SECTION                                
527500                                                                          
527600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
527700          DELIMITED BY SIZE INTO SSA1                                     
527800     MOVE '  ' TO GODK-STATUSKODER                                        
527900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
528000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
528100     PERFORM IMS-STATUSKONTROLL                                           
528200     .                                                                    
528300     EJECT                                                                
528400 IMS-GU-WDB616    SECTION.                                                
528500     MOVE 'IMS-GU-WDB616  '  TO DBS-SECTION                               
528600                                                                          
528700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
528800          DELIMITED BY SIZE INTO SSA1                                     
528900     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
529000          DELIMITED BY SIZE INTO SSA2                                     
529100     MOVE '  ' TO GODK-STATUSKODER                                        
529200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
529300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
529400     PERFORM IMS-STATUSKONTROLL                                           
529500     .                                                                    
529600     EJECT                                                                
529700 IMS-GU-2502 SECTION.                                                     
529800     MOVE 'IMS-GU-2502 '     TO DBS-SECTION                               
529900                                                                          
530000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2501-X ')'                    
530100          DELIMITED BY SIZE INTO SSA1                                     
530200     STRING 'WDGX2502(IDREFTAB =' W-IDREFTAB-X ')'                        
530300          DELIMITED BY SIZE INTO SSA2                                     
530400     MOVE '  GE' TO GODK-STATUSKODER                                      
530500     CALL CBLTDLI USING GU 2501-PCB DLI-IO-WDGX2502 SSA1 SSA2             
530600     MOVE 2501-STATUS-CODE TO STATUS-WS                                   
530700     PERFORM IMS-STATUSKONTROLL                                           
530800     .                                                                    
530900     SKIP3                                                                
531000                                                                          
531100 IMS-GU-WDD7A1 SECTION.                                                   
531200     MOVE 'IMS-GU-WDD7A1'    TO DBS-SECTION                               
531300                                                                          
531400     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
531500                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
531600            DELIMITED BY SIZE INTO SSA1                                   
531700     MOVE '  GE' TO GODK-STATUSKODER                                      
531800     CALL CBLTDLI USING GU WDD7A-PCB DLI-IO-WDD7A1 SSA1                   
531900     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
532000     PERFORM IMS-STATUSKONTROLL                                           
532100     .                                                                    
532200     EJECT                                                                
532300 IMS-GU-WDD704 SECTION.                                                   
532400     MOVE 'IMS-GU-WDD704   ' TO DBS-SECTION                               
532500                                                                          
532600     MOVE SPACE               TO ALL-SSA                                  
532700     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
532800          DELIMITED BY SIZE INTO SSA1                                     
532900     MOVE 'WDD704 '           TO SSA2                                     
533000     MOVE '  GE'              TO GODK-STATUSKODER                         
533100     CALL CBLTDLI USING GU  WDD7-PCB DLI-IO-WDD704 SSA1 SSA2              
533200     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
533300     PERFORM IMS-STATUSKONTROLL                                           
533400     .                                                                    
533500     EJECT                                                                
533600 IMS-GU-WDK901 SECTION.                                                   
533700     MOVE 'IMS-GU-WDK901   ' TO DBS-SECTION                               
533800                                                                          
533900     MOVE SPACE               TO ALL-SSA                                  
534000     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
534100          DELIMITED BY SIZE INTO SSA1                                     
534200     MOVE '  GE'              TO GODK-STATUSKODER                         
534300     CALL CBLTDLI USING GU  WDK9-PCB DLI-IO-WDK901 SSA1                   
534400     MOVE WDK9-STATUS-CODE    TO STATUS-WS                                
534500     PERFORM IMS-STATUSKONTROLL                                           
534600     .                                                                    
534700     EJECT                                                                
534800 IMS-GNP-WDK911 SECTION.                                                  
534900     MOVE 'IMS-GNP-WDK911   ' TO DBS-SECTION                              
535000                                                                          
535100     MOVE SPACE               TO ALL-SSA                                  
535200     MOVE 'WDK911 '           TO SSA1                                     
535300     MOVE '  GE'              TO GODK-STATUSKODER                         
535400     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-WDK911 SSA1                   
535500     MOVE WDK9-STATUS-CODE    TO STATUS-WS                                
535600     PERFORM IMS-STATUSKONTROLL                                           
535700     .                                                                    
535800     EJECT                                                                
535900 IMS-STATUSKONTROLL SECTION.                                              
536000                                                                          
536100     SET STATUS-IX TO 1                                                   
536200     SEARCH GODK-STATUS                                                   
536300       AT END                                                             
536400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
536500           DELIMITED BY SIZE INTO FELTEXT-STR                             
536600         DISPLAY FELTEXT                                                  
536700         CALL FELLOG                                                      
536800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
536900         CONTINUE                                                         
537000     END-SEARCH                                                           
537100     .                                                                    
537200     EJECT                                                                
537300*    -COPY WY2000P3                                                       
537400     EJECT                                                                
537500*    -COPY WY2000Q3                                                       
537600     EJECT                                                                
537700*    -COPY WY2000P1                                                       
537800     EJECT                                                                
537900*    -COPY WY2000P9                                                       
