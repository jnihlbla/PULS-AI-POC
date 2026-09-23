000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W2222200.                                            
000400 AUTHOR.             IDK, GÖTEBORG.                                       
000500 DATE-WRITTEN.       NOV  1978.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        PROGRAMMET ÄR ETT SUBPROGRAM FÖR BERÄKNING AV                    
001100*        ARTIKEL-BEHOV. EN BEHOVSTABELL ANGER PER VECKA                   
001200*        EN ARTIKELS FÖRVÄNTADE BEHOV. BEHOVEN BESTÅR AV                  
001300*            - PROGNOS   BASERAT PÅ PB-SEP,SÄSONGSINDEX,                  
001400*                        TRÄNDER, PB-JUSTERINGAR                          
001500*            - SATSBEHOV SATSENS LEVERANSPLAN STYR DE                     
001600*                        INGÅENDE ARTIKLARNAS SATSBEHOV                   
001700*            - DO-BEHOV  REGISTRERADE DIVERSEORDER                        
001800*        BERÄKNINGS-OMFATTNING OCH -RESULTAT FÖRMEDDLAS                   
001900*        VIA LÄNKAREAN W222L222C0                                         
002000*                                                                         
002100*    SUBPROGRAM.                                                          
002200*            W009VADD    ADD AV VECKOR TILL DATUM                         
002300*            PERADD      ADD AV PERIODER TILL DATUM                       
002400*                                                                         
002500*                                                                         
002600*    ÄNDRINGAR:                                                           
002700*        2013-06-24  E'TRACKER 10162003                                   
002800*                    DECREASE NO OF WEEKS AT SUPERSESSION.                
002900*                                                                         
003000*                                                                         
003100*        2013-02-08  E'TRACKER 10143273 CHINA  LOCAL SOURCING             
003200*                    ÄNDRING PÅ BILD 2128 - ENDAST-NDCBEHOV               
003300*                    BERÄKNAS MED (KVPB-REF + KVPBREOI) / DC.             
003400*                                                                         
003500*        2014-10-13  RÄTTA SCR 8616110                                    
003600*                                                                         
003700*        2015-03-03  RÄTTA SÄSONG KOLUMN SDC'R BILD 2128.                 
003800*                                                                         
003900*        2015-05-25  RÄTTA 6 ARBETSDAGAR FÖR KINA DC ÄNDRING,             
004000*                    SE SCR 10205876.                                     
004100*                                                                         
004200*        2016-04-07  KINA EXPORT PROJEKT.REFILL FRÅN KINA TILL CDC        
004300*                                                                         
004400*        2019-03-01  AZURE 1364012 RÄTTA TREND-VÄRDE                      
004500*                                                                         
004600*        2019-03-27  ENTER PB PLAN FROM A CERTAIN DATE                    
004700*                                                                         
004800*        2020-02-25  AZURE 1571962 BYT TILL CALL W271REFL,                
004900*                    TAG BORT ALL KOD FÖR PERFORM W271REF2                
005000*                    + ÄNDRINGAR FÖR F/C IN THE FUTURE 2.                 
005100*                                                                         
005200     EJECT                                                                
005300 ENVIRONMENT DIVISION.                                                    
005400     SKIP1                                                                
005500 INPUT-OUTPUT SECTION.                                                    
005600 FILE-CONTROL.                                                            
005700     SKIP2                                                                
005800 DATA DIVISION.                                                           
005900 FILE SECTION.                                                            
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006300*    -COPY WY2000W1                                                       
006400     SKIP3                                                                
006500*    -COPY WY2000W3                                                       
006600     SKIP3                                                                
006700*    -COPY WY2000W9                                                       
006800     SKIP3                                                                
006900 01  IDPGM                   PIC X(8)    VALUE 'W2222200'.                
007000 01  CURRENT-SECTION         PIC X(32)   VALUE SPACE.                     
007100                                                                          
007200 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
007300     SKIP3                                                                
007400                                                                          
007500 01  SW-DEMAND-ADJUST        PIC X       VALUE 'N'.                       
007600     88  DEMAND-ADJUST                   VALUE 'J'.                       
007700                                                                          
007800 01  FILLER                  PIC X(16) VALUE 'KONSTANTER      '.          
007900 01  KONSTANTER.                                                          
008000     03  JA                  PIC X       VALUE 'J'.                       
008100     03  NEJ                 PIC X       VALUE 'N'.                       
008200     03  ANTAL-SEASONINDEX   PIC S9(9)   VALUE +12   COMP SYNC.           
008300     SKIP1                                                                
008400     03  JUST-PB-FINNS       PIC X       VALUE 'J'.                       
008500     03  W-JUST-PB-FINNS     PIC X       VALUE 'J'.                       
008600     03  PERIOD-ANTAL        PIC S9(9)   VALUE +12   COMP-3.              
008700     03  PB-FAKTOR-PER5      PIC S9(3)V9(2)                               
008800                                         VALUE ZERO  COMP-3.              
008900     03  MAX-BEHOVS-VECKOR   PIC S9(9)   VALUE +156  COMP SYNC.           
009000                                                                          
009100     03  ENDAST-SEPARATBEHOV PIC  X(2)   VALUE '01'.                      
009200     03  ENDAST-SATSBEHOV    PIC  X(2)   VALUE '02'.                      
009300     03  SEP-SATS-BEHOV      PIC  X(2)   VALUE '03'.                      
009400     03  ENDAST-LEVBEHOV     PIC  X(2)   VALUE '04'.                      
009500     03  SEP-DO-BEHOV        PIC  X(2)   VALUE '05'.                      
009600     03  SATS-DO-BEHOV       PIC  X(2)   VALUE '06'.                      
009700     03  SEP-SATS-DO-BEHOV   PIC  X(2)   VALUE '07'.                      
009800     03  SATS-TPO-LEVBEHOV   PIC  X(2)   VALUE '08'.                      
009900     03  SEP-SATS-TPO-LEVBEHOV      PIC  X(2)   VALUE '09'.               
010000     03  ENDAST-SDCBEHOV            PIC  X(2)   VALUE '10'.               
010100     03  SATS-TPO-SDCBEHOV          PIC  X(2)   VALUE '11'.               
010200     03  SEP-SATS-TPO-SDCBEHOV      PIC  X(2)   VALUE '12'.               
010300     03  SATS-TPO-LEV-SDCBEHOV      PIC  X(2)   VALUE '13'.               
010400     03  SEP-SATS-TPO-LEV-SDCBEHOV  PIC  X(2)   VALUE '14'.               
010500     03  ENDAST-NDCBEHOV            PIC  X(2)   VALUE '15'.               
010600     03  SATS-TPO-SDC-NDC           PIC  X(2)   VALUE '16'.               
010700     03  SEP-SATS-TPO-SDC-NDC       PIC  X(2)   VALUE '17'.               
010800     03  SATS-TPO-LEV-SDC-NDC       PIC  X(2)   VALUE '18'.               
010900     03  SEP-SATS-TPO-LEV-SDC-NDC   PIC  X(2)   VALUE '19'.               
011000     03  PB-TOTAL                   PIC  X(2)   VALUE '20'.               
011100     03  SEP-TPO-SDC-NDC            PIC  X(2)   VALUE '21'.               
011200                                                                          
011300     03  LAES-ARTIKEL        PIC S9(3)   VALUE +301  COMP-3.              
011400     03  LAES-PROGNOS-BEHOV  PIC S9(3)   VALUE +302  COMP-3.              
011500     03  LAES-TPO-SALDO      PIC S9(3)   VALUE +303  COMP-3.              
011600     03  LAES-SATS-BEHOV     PIC S9(3)   VALUE +401  COMP-3.              
011700     03  LAES-TPO-BEHOV      PIC S9(3)   VALUE +501  COMP-3.              
011800     03  LAES-TPO-BEHOV-NEXT PIC S9(3)   VALUE +502  COMP-3.              
011900     03  LAES-SDCINFO        PIC S9(3)   VALUE +601  COMP-3.              
012000     03  LAES-NDCINFO        PIC S9(3)   VALUE +701  COMP-3.              
012100     SKIP3                                                                
012200 01  FILLER                  PIC X(16) VALUE 'ARBETSAREOR     '.          
012300 01  ARBETSAREOR.                                                         
012400     SKIP1                                                                
012500*                                                                         
012600*   PER-TABELL ÄR EN RULLANDE TABELL DÄR                                  
012700*   IX = 1  ÄR JANUARI                                                    
012800*   IX = 12 ÄR DECEMBER                                                   
012900*                                                                         
013000     03 PER-TABELL OCCURS 12.                                             
013100        05 PER-PERIOD            PIC  9(2)   VALUE ZERO.                  
013200        05 PER-START-VV          PIC  9(2)   VALUE ZERO.                  
013300        05 PER-SLUT-VV           PIC  9(2)   VALUE ZERO.                  
013400     SKIP3                                                                
013500*                                                                         
013600*   K712-TABELL                                                           
013700*   LADDAS FRÅN WDK712 FÖR GIVET LINK-IDARTNR                             
013800*                                                                         
013900     03 K712-TAB.                                                         
014000        05 K712-TABELL OCCURS 50.                                         
014100           07 K712-IDLANDX2      PIC  X(2)   VALUE SPACE.                 
014200           07 K712-DAPUBL        PIC  9(8)   VALUE ZERO.                  
014300           07 K712-PRMATRL       PIC  S9(7)V9(2)  COMP-3.                 
014400     03 K712-IX                  PIC  9(3)   VALUE ZERO.                  
014500     03 K712-IX-MAX              PIC  9(3)   VALUE 50.                    
014600     SKIP3                                                                
014700     03  IX                  PIC S9(9)               COMP SYNC.           
014800     03  IX-PER              PIC S9(9)               COMP SYNC.           
014900     03  IX-FRAN             PIC S9(9)               COMP-3               
015000                                                    VALUE ZERO.           
015100     03  IX-TILL             PIC S9(9)               COMP-3               
015200                                                    VALUE ZERO.           
015300     03  IX-VECKA            PIC S9(9)               COMP-3               
015400                                                    VALUE ZERO.           
015500     03  INDX-T              PIC S9(4)   VALUE +0    COMP SYNC.           
015600     03  INDX-L              PIC S9(3)   VALUE +0    COMP SYNC.           
015700     03  AKT-DC-IX           PIC 9(2)    VALUE ZERO.                      
015800     03  IY                  PIC S9(9)               COMP SYNC.           
015900     03  TABW200-INDEX       PIC S9(4)               COMP SYNC.           
016000     03  SW-TPOBEHOV-LAEST   PIC X       VALUE 'N'.                       
016100     03  6ARBDAG-SW          PIC X       VALUE 'N'.                       
016200     SKIP1                                                                
016300     03  W-TIAAPP-AKTUELL    PIC S9(5)               COMP-3.              
016400     03  W-DATUM-AAVV        PIC S9(5)               COMP-3.              
016500     03  W-RED-NUM-1         PIC 9             VALUE ZERO.                
016600     03  W-RED-NUM-4         PIC 9(4)          VALUE ZERO.                
016700     03  W-AAVV              PIC S9(5)               COMP-3.              
016800     03  W-AAVVD             PIC S9(5)         VALUE ZERO.                
016900     03  W-TIAAVV            PIC 9(4)          VALUE ZERO.                
017000     03  WS-DAGENS-AAVVD     PIC 9(5)          VALUE ZERO.                
017100     03  W-DATUM-FORSTA-SDC  PIC S9(5)               COMP-3.              
017200     03  W-TIFINLV-AAVV-MINUS-2                                           
017300                             PIC S9(5)               COMP-3.              
017400     03  W-TIFINLV-AAVV-MINUS-LT                                          
017500                             PIC S9(5)               COMP-3.              
017600     03  WS-LTID             PIC S9(5)               COMP-3.              
017700     03  WS-LTID-A           PIC S9(5)               COMP-3.              
017800     03  WS-LTID-B           PIC S9(5)               COMP-3.              
017900     03  WS-LT-WEEKS         PIC S9(3)               COMP-3.              
018000     03  WS-LT-WEEKS-DAYS    PIC S9(3)               COMP-3.              
018100     03  WS-REMN-DAYS        PIC  9(2) VALUE ZERO.                        
018200     03  W-DATUM             PIC 9(4).                                    
018300     03  W-DAT               REDEFINES W-DATUM.                           
018400         05  W-DATUM-AA      PIC 9(2).                                    
018500         05  W-DATUM-VV      PIC 9(2).                                    
018600     03  WS-START-AAVV       PIC 9(4).                                    
018700     03  WS-START2-AAVV       REDEFINES WS-START-AAVV.                    
018800         05  WS-START-AA     PIC 9(2).                                    
018900         05  WS-START-VV      PIC 9(2).                                   
019000     SKIP1                                                                
019100     03  FILLER              PIC X(16) VALUE 'W-ANTAL-VECKOR  '.          
019200     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
019300     03  W-PER-ANT           PIC S9(3)               COMP-3.              
019400     SKIP1                                                                
019500     03  W-BER-START-AA      PIC S9(3)               COMP-3.              
019600     03  W-BER-START-VV      PIC S9(3)               COMP-3.              
019700     03  W-BER-SLUT-DATUM    PIC S9(5)               COMP-3.              
019800     03  W-BER-START-DATUM   PIC S9(5)               COMP-3.              
019900     03  W-BER-DATUM         PIC S9(5)               COMP-3.              
020000     SKIP1                                                                
020100     03  W-FAKTOR            PIC S9(1)V9(3)          COMP-3.              
020200     03  W-KVPB-SEP          PIC S9(6)V9(3)          COMP-3.              
020300     03  W-KVPB-JUST1        PIC S9(6)V9(3)          COMP-3.              
020400     03  W-KVPB-JUST2        PIC S9(6)V9(3)          COMP-3.              
020500     03  W-CLAG-DAPBPLAN     PIC 9(06).                                   
020600     03  W-KVBEHOV-SUMMA     PIC S9(7)V9(2)          COMP-3.              
020700     03  W-KVBEHOV-30V       PIC S9(7)V9(2)          COMP-3.              
020800     03  W-KVBEHOV-52V       PIC S9(7)V9(2)          COMP-3.              
020900     03  W-TIFINLV-AAVV      PIC S9(5)               COMP-3.              
021000     03  W-PER               PIC 9(2)                COMP-3.              
021100     SKIP1                                                                
021200     03  W-TIAAP             PIC S9(3)               COMP-3.              
021300                                                                          
021400     03  W-ANTVKA-FORSKJUT   PIC S9(1)               COMP-3.              
021500     03  W-KDGK              PIC S9(1)               COMP-3.              
021600     03  W-TIBEHOV-TPO       PIC S9(5)               COMP-3.              
021700**----------------------------------------------------------------        
021800****** AREOR FÖR BERÄKNING AV REFILL BEHOV TILL SDC/NDC                   
021900**----------------------------------------------------------------        
022000     03  FILLER              PIC X(16) VALUE 'W-TIME          '.          
022100     03  W-TIME                   PIC 9(8)     VALUE ZERO.                
022200     03  W-SDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
022300     03  W-SDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
022400     03  W-KVBEHOV-INNEV-VECKA    PIC S9(6)V9  VALUE ZERO COMP-3.         
022500     03  W-SDC-TILLGANG           PIC S9(7)    VALUE ZERO COMP-3.         
022600     03  W-SDC-TILLGANG-ERS       PIC S9(7)    VALUE ZERO COMP-3.         
022700     03  W-SDC-KVBEHOV-DESSUTOM   PIC S9(7)    VALUE ZERO COMP-3.         
022800     03  W-DC-REFILL-KVANT        PIC S9(7)    VALUE ZERO COMP-3.         
022900     03  W-SDC-DIFF               PIC S9(7)    VALUE ZERO COMP-3.         
023000     03  W-SDC-KVAR-EFT-VECKA     PIC S9(7)    VALUE ZERO COMP-3.         
023100     03  W-SDC-KVAR-EFT-DAG       PIC S9(7)V9  VALUE ZERO COMP-3.         
023200     03  W-SDC-ACC-KVBEHOV-VECKA  PIC S9(7)V9  VALUE ZERO COMP-3.         
023300     03  W-SDC-KVAR-KVBEHOV-VECKA PIC S9(7)V9  VALUE ZERO COMP-3.         
023400     03  W-DAGAR-KVAR             PIC S9(1)    VALUE ZERO COMP-3.         
023500     03  W-VECKODEL             PIC S9(1)V9(2) VALUE ZERO COMP-3.         
023600     03  W-SDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
023700     03  W-NDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
023800     03  W-NDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
023900     03  W-NDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
024000     03  W-NDC-TILLGANG           PIC S9(7)V9  VALUE ZERO COMP-3.         
024100     03  W-NDC-TILLGANG-ERS       PIC S9(7)V9  VALUE ZERO COMP-3.         
024200     03  W-XDC-TILLGANG-ERS       PIC S9(7)V9  VALUE ZERO COMP-3.         
024300     03  W-NDC-KVAR-EFT-VECKA     PIC S9(7)V9  VALUE ZERO COMP-3.         
024400     03  W-NDC-KVAR-EFT-DAG       PIC S9(7)V9  VALUE ZERO COMP-3.         
024500     03  W-NDC-KVBEHOV-DESSUTOM   PIC S9(7)V9  VALUE ZERO COMP-3.         
024600     03  W-NDC-DIFF               PIC S9(7)V9  VALUE ZERO COMP-3.         
024700     03  W-NDC-ACC-KVBEHOV-VECKA  PIC S9(7)V9  VALUE ZERO COMP-3.         
024800     03  W-NDC-KVAR-KVBEHOV-VECKA PIC S9(7)V9  VALUE ZERO COMP-3.         
024900     03  SPAR-KVBEHOV-VECKA       PIC S9(7)V9(2)                          
025000                                               VALUE ZERO COMP-3.         
025100     03  SPAR-KVBEHOV-DESSUTOM    PIC S9(7)V9(2)                          
025200                                               VALUE ZERO COMP-3.         
025300     03  W-BINNDAY                PIC 9(6)     VALUE ZERO.                
025400     03  W-BINNDAY-AAVV           PIC S9(5)    VALUE ZERO COMP-3.         
025500     03  WS-QX-BRYTNING           PIC 9(2)     VALUE ZERO.                
025600     03  WS-FIXA-AAR              PIC 9(2)     VALUE ZERO.                
025700     03  WS-AR                    PIC 9(2)     VALUE ZERO.                
025800     03  WS-KVVECKOR-KVAR-IAR     PIC 9(4)     VALUE ZERO.                
025900     03  WS-KVVECKOR-ERSAR        PIC 9(4)     VALUE ZERO.                
026000     03  WS-VAEFEL                PIC X(20)    VALUE SPACE.               
026100     03  WS-FLFLYG                PIC X(1)     VALUE 'N'.                 
026200     03  WS-ABEND                 PIC X(1)     VALUE 'N'.                 
026300                                                                          
026400*--- KONTROLL PÅ JUST-PB I REFILLENS F/C I FRAMTIDEN.                     
026500                                                                          
026600     03  W-SDC-KVPB-REF         PIC S9(6)V9(1) VALUE ZERO COMP-3.         
026700     03  W-SDC-JUST-PB-FINNS    PIC X          VALUE 'N'.                 
026800     03  W-SDC-KVPB-JUST OCCURS 2                                         
026900                                PIC S9(6)V9(1) VALUE ZERO COMP-3.         
027000                                                                          
027100     03  W-SDC-TIPBJUST  OCCURS 2                                         
027200                                PIC S9(5) VALUE ZERO COMP-3.              
027300                                                                          
027400     03  WS-SDC-TIPBJUST        PIC 9(4) VALUE ZERO.                      
027500                                                                          
027600     03  W-NDC-KVPB-REF         PIC S9(6)V9(1) VALUE ZERO COMP-3.         
027700     03  W-NDC-JUST-PB-FINNS    PIC X          VALUE 'N'.                 
027800     03  W-NDC-KVPB-JUST OCCURS 2                                         
027900                                PIC S9(6)V9(1) VALUE ZERO COMP-3.         
028000                                                                          
028100     03  W-NDC-TIPBJUST  OCCURS 2                                         
028200                                PIC S9(5) VALUE ZERO COMP-3.              
028300                                                                          
028400     03  WS-NDC-TIPBJUST        PIC 9(4) VALUE ZERO.                      
028500                                                                          
028600*---                                                                      
028700                                                                          
028800     03 WS-ERS-FINNS-K611         PIC X        VALUE 'N'.                 
028900                                                                          
029000     03 WS-REF-FINNS-K629         PIC X        VALUE 'N'.                 
029100*                                                                         
029200     03  WS-TISTOREF-AA           PIC 9(2)     VALUE ZERO.                
029300     03  WS-TISTOREF-AAVV         PIC 9(4)     VALUE ZERO.                
029400     03  WS-TISTOREF-AAVVD        PIC 9(5)     VALUE ZERO.                
029500     03  WS-TISTOREF-AAVV-3       PIC S9(5)    VALUE ZERO COMP-3.         
029600     03  WS-ANTAL-VECKOR          PIC S9(3)    VALUE ZERO COMP-3.         
029700     03  WS-ANTAL-VECKOR-JUST     PIC 9(2)     VALUE ZERO.                
029800     03  WS-ANTAL-AR              PIC 9(2)     VALUE ZERO.                
029900     03  WS-KDERS                 PIC 9(2)     VALUE ZERO.                
030000*                                                                         
030100     03  WS-KVPB-TREND           PIC S9(6)V9.                             
030200     03  MAX-KVVECKOR-TREND      PIC S9(3)  VALUE ZERO.                   
030300                                                                          
030400     03  FILLER              PIC X(16) VALUE 'WS-ANT-VV       '.          
030500     03 WS-ANT-VV                PIC  9(2)   VALUE ZERO.                  
030600     03 WS-TIAAVV.                                                        
030700       05 WS-AAR                 PIC  9(2)   VALUE ZERO.                  
030800       05 WS-VV                  PIC  9(2)   VALUE ZERO.                  
030900     03 TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                              
031000     03 WS-TIAAPER.                                                       
031100       05 TIAA                   PIC  9(2)   VALUE ZERO.                  
031200       05 PER                    PIC  9(2)   VALUE ZERO.                  
031300     03 TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
031400     03 WS-KVREFBER          PIC S9(7)           COMP-3.                  
031500     03 WS-KVANT-QX          PIC  9(5)V9(2) VALUE ZERO.                   
031600     03 WS-KVANT-QX-DELAR    REDEFINES WS-KVANT-QX.                       
031700        05 WS-KVANT-QX-HELTAL PIC 9(5).                                   
031800        05 WS-KVANT-QX-DECTAL PIC 9(2).                                   
031900     03 WS-ANTAL-QX          PIC S9(7)      VALUE ZERO COMP-3.            
032000     03 WS-ANTAL-BER         PIC S9(7)      VALUE ZERO COMP-3.            
032100     03  WS-IDDC-NUM             PIC 9(2)    VALUE ZERO.                  
032200     03  DC-LEDTIX                 PIC 9(2)    VALUE ZERO.                
032300     03  WS-BER-IY           PIC S9(9)   VALUE ZERO  COMP SYNC.           
032400     03  WS-BER-IX           PIC S9(9)   VALUE ZERO  COMP SYNC.           
032500     03  WS-BER-TAB-NOLL.                                                 
032600         05  FILLER          OCCURS 4.                                    
032700             07  FILLER      OCCURS 52.                                   
032800                 09  FILLER                                               
032900                             PIC S9(6)V9(3)                               
033000                                         VALUE ZERO  COMP-3.              
033100                                                                          
033200     03  W-BER-IYIX          PIC 9(4).                                    
033300     03  W-BER               REDEFINES W-BER-IYIX.                        
033400         05  W-BER-IY        PIC 9(2).                                    
033500         05  W-BER-IX        PIC 9(2).                                    
033600*AD  03  ERS-BER-IYIX        PIC 9(4).                                    
033700*AD  03  ERS-BER             REDEFINES ERS-BER-IYIX.                      
033800*AD      05  ERS-BER-IY      PIC 9(2).                                    
033900*AD      05  ERS-BER-IX      PIC 9(2).                                    
034000     03  FILLER              PIC X(16) VALUE 'WS-CURRENT-DATE '.          
034100     03  WS-CURRENT-DATE.                                                 
034200         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
034300         05  FILLER              PIC 9(4)   VALUE ZERO.                   
034400         05  FILLER              PIC 9(6)   VALUE ZERO.                   
034500                                                                          
034600     03  FILLER REDEFINES WS-CURRENT-DATE.                                
034700*-----   INKLUSIVE SEKEL                                                  
034800         05  WS-DAGENS-DATUM     PIC 9(8).                                
034900         05  WS-DAGENS-TID.                                               
035000             07 WS-DAGENS-TIMME  PIC 9(2).                                
035100             07 WS-DAGENS-MINUT  PIC 9(2).                                
035200             07 WS-DAGENS-SEKUND PIC 9(2).                                
035300     03 WS-DAGENS-VECKA          PIC 9(2)    VALUE ZERO.                  
035400     03 WS-DAGENS-DAGNR          PIC 9       VALUE ZERO.                  
035500     03 WS-DAPUBL                PIC 9(8)  VALUE ZERO.                    
035600     03 WS-CALL-KVVECKOR-BEHOV   PIC S9(3)                                
035700                                         VALUE ZERO  COMP-3.              
035800     03 WS-KVPB-PLAN-VECKA       PIC S9(6)V9(1)                           
035900                                         VALUE ZERO  COMP-3.              
036000     03 WS-ARSTOTAL              PIC S9(9)V9(2)                           
036100                                             COMP-3  VALUE ZERO.          
036200     03 WS-KVPB                  PIC S9(9)V9(2)                           
036300                                             COMP-3  VALUE ZERO.          
036400     03 WS-KVBEHOV-PER           PIC S9(7)V9(2)                           
036500                                             COMP-3  VALUE ZERO.          
036600     03 WS-KVBEHOV-PER-TEST      OCCURS 12                                
036700                                 PIC S9(7)V9(2)                           
036800                                             COMP-3  VALUE ZERO.          
036900     03 WS-JUSTERA               PIC S9V9(2) COMP-3  VALUE ZERO.          
037000     03 WS-RESEASON-TOT          PIC S9(2)V9(2)                           
037100                                             COMP-3  VALUE ZERO.          
037200                                                                          
037300****                                                                      
037400**** PERIODERNA 1,2,3,4,6,7,8 INNEHÅLLER 6 VECKOR                         
037500**** PERIOD 5 INNEHÅLLER 10 VECKOR (25-34)                                
037600****                                                                      
037700   03 FILLER                 PIC X(16) VALUE 'WS-KVBEHOV-TAB  '.          
037800   03 WS-KVBEHOV-TABELL.                                                  
037900     05 WS-KVBEHOV-VECKA         OCCURS 52                                
038000                                 PIC S9(7)V9(2)                           
038100                                             COMP-3  VALUE ZERO.          
038200   03 FILLER                 PIC X(16) VALUE 'WS-RESEASON-TAB '.          
038300   03 WS-RESEASON-TABELL.                                                 
038400     05 WS-RESEASON              OCCURS 12                                
038500                                 PIC S9(2)V9(4)                           
038600                                             COMP-3  VALUE ZERO.          
038700   03 FILLER                 PIC X(16) VALUE 'WS-RESEASON-AVR '.          
038800   03 WS-RESEASON-AVR-TABELL.                                             
038900     05 WS-RESEASON-AVR          OCCURS 12                                
039000                                 PIC S9(2)V9(2)                           
039100                                             COMP-3  VALUE ZERO.          
039200                                                                          
039300   03 WS-NOLLA-KVBEHOV.                                                   
039400     05 FILLER                   OCCURS 52                                
039500                                 PIC S9(7)V9(2)                           
039600                                             COMP-3  VALUE ZERO.          
039700                                                                          
039800   03 WS-NOLLA-RESEASON.                                                  
039900     05 FILLER                   OCCURS 12                                
040000                                 PIC S9(2)V9(4)                           
040100                                             COMP-3  VALUE ZERO.          
040200                                                                          
040300   03 WS-NOLLA-RESEASON-AVR.                                              
040400     05 FILLER                   OCCURS 12                                
040500                                 PIC S9(2)V9(2)                           
040600                                             COMP-3  VALUE ZERO.          
040700     SKIP3                                                                
040800                                                                          
040900********************************************************                  
041000*                                                      *                  
041100*   W271REF2                                           *                  
041200*                                                      *                  
041300*   SUBPROGRAM W271REF2 INLAGD I W2222200 FÖR ATT      *                  
041400*   TJÄNA TID OCH PENGAR (PGA TIDSÖDANDE CALL ANROP)   *                  
041500*   1999-04-07 STEFAN A                                *                  
041600*   2020-04-15 SPARAR VISSA FÄLT FÖR CALL TO W271REFL  *                  
041700********************************************************                  
041800                                                                          
041900 01  FILLER                  PIC X(16) VALUE 'ARBETSFAELT     '.          
042000 01  ARBETSFAELT.                                                         
042100*                                                                         
042200     03 DAGENS-DATUM             PIC 9(8).                                
042300     03 FILLER         REDEFINES DAGENS-DATUM.                            
042400         05 DAGENS-AAR           PIC 9(4).                                
042500         05 DAGENS-MAANAD        PIC 9(2).                                
042600         05 DAGENS-DAG           PIC 9(2).                                
042700     03 DAGENS-AAR-PLUS2         PIC 9(4)        VALUE ZERO.              
042800     03 JMF-AAAA                 PIC 9(4)        VALUE ZERO.              
042900                                                                          
043000*                                                                         
043100     03  FL-PRARTBES             PIC X          VALUE 'N'.                
043200     03  WS-PRARTBES             PIC S9(7)V9(2) VALUE ZERO                
043300                                                 COMP-3.                  
043400*                                                                         
043500                                                                          
043600 01  FILLER                      PIC X(24)  VALUE 'SWITCHAR'.             
043700                                                                          
043800                                                                          
043900                                                                          
044000********************************************************                  
044100*                                                      *                  
044200*   W271REF2  ARBETSFÄLT SLUT.                         *                  
044300*                                                      *                  
044400********************************************************                  
044500 77  TREND-SW                    PIC X       VALUE 'N'.                   
044600                                                                          
044700****************************************** PARAM. W009VADD                
044800 01  W009VADDW.                                                           
044900     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
045000     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
045100     SKIP3                                                                
045200****************************************** BERAKNINGSTABELL               
045300*                                          ARTKIKELBEHOV PER VECKA        
045400*                                          UNDER MAX 4ÅR                  
045500 01  FILLER                  PIC X(16) VALUE 'BERAKNINGS-TAB  '.          
045600 01  BERAKNINGS-TABELL.                                                   
045700     03  BER-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
045800     03  BER-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
045900     03  CDC-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
046000     03  CDC-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
046100     03  BER-MAX-VV          PIC S9(9)   VALUE +52   COMP SYNC.           
046200     03  BER-MAX-AA          PIC S9(9)   VALUE +4    COMP SYNC.           
046300     03  BER-ANTAL-VV        PIC S9(9)   VALUE ZERO  COMP SYNC.           
046400     03  BER-ANTAL-AA        PIC S9(9)   VALUE ZERO  COMP SYNC.           
046500     03  FILLER              PIC X(16) VALUE 'BER-TAB         '.          
046600     03  BER-TAB.                                                         
046700         05  BER-AA          OCCURS 4.                                    
046800             07  BER-VV      OCCURS 52.                                   
046900                 09  BER-BEHOV                                            
047000                             PIC S9(6)V9(3)                               
047100                                         VALUE ZERO  COMP-3.              
047200     SKIP3                                                                
047300****************************************** RESULTAT-TABELL                
047400*                                          TOTALT ARTIKELBEHOV            
047500*                                          PER VECKA MAX 4 ÅR             
047600 01  FILLER                  PIC X(16) VALUE 'RESULTAT-TABELL '.          
047700 01  RESULTAT-TABELL.                                                     
047800     03  RES-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
047900     03  RES-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
048000     03  RES-MAX-VV          PIC S9(9)   VALUE +52   COMP SYNC.           
048100     03  RES-MAX-AA          PIC S9(9)   VALUE +4    COMP SYNC.           
048200     03  RES-ANTAL-VV        PIC S9(9)   VALUE ZERO  COMP SYNC.           
048300     03  RES-ANTAL-AA        PIC S9(9)   VALUE ZERO  COMP SYNC.           
048400     03  FILLER              PIC X(16) VALUE 'RES-TAB         '.          
048500     03  RES-TAB.                                                         
048600         05  RES-AA          OCCURS 4.                                    
048700             07  REX-VV      OCCURS 52.                                   
048800                 09  RES-BEHOV                                            
048900                             PIC S9(6)V9(3)                               
049000                                         VALUE ZERO COMP-3.               
049100                                                                          
049200 01  FILLER                  PIC X(16) VALUE 'WWDC99          '.          
049300*      --- VALID IDDC CODES                                               
049400*                                                                         
049500*01    -COPY WWDC99                                                       
049600*01    -COPY WWDCKONS                                                     
049700                                                                          
049800                                                                          
049900                                                                          
050000 01  FILLER                  PIC X(16) VALUE 'W-IDDC-X        '.          
050100 01  W-IDDC-X.                                                            
050200     03  W-IDDC              PIC  X(2).                                   
050300 01  W-IDDC-MIN-X.                                                        
050400     03  W-IDDC-MIN          PIC  X(2)   VALUE LOW-VALUE.                 
050500 01  W-IDDC-MAX-X.                                                        
050600     03  W-IDDC-MAX          PIC  X(2)   VALUE HIGH-VALUE.                
050700 01  W-IDDC-ERS-X.                                                        
050800     03  W-IDDC-ERS          PIC  X(2).                                   
050900 01  W-IDDC-B6-X.                                                         
051000     03  W-IDDC-B6           PIC X(2)    VALUE SPACE.                     
051100 01  W-IDDC-REF-X.                                                        
051200     03  W-IDDC-REF          PIC X(2)    VALUE SPACE.                     
051300                                                                          
051400 01  W-IDDC-B6-CDC-X.                                                     
051500     03  W-IDDC-B6-CDC       PIC X(2)    VALUE '11'.                      
051600                                                                          
051700 01  W-IDDC-REF-CDC-X.                                                    
051800     03  W-IDDC-REF-CDC      PIC X(2)    VALUE SPACE.                     
051900                                                                          
052000                                                                          
052100 01  W-IDARTNR-ERS-X.                                                     
052200     03  W-IDARTNR-ERS       PIC S9(9)               COMP-3.              
052300 01  W-IDARTNR-X.                                                         
052400     03  W-IDARTNR           PIC S9(9)               COMP-3.              
052500 01  W-DAPRLIST-X.                                                        
052600     03  W-DAPRLIST          PIC  9(8)  VALUE ZERO.                       
052700 01  W-KDERS-0-X.                                                         
052800     03  W-KDERS-0           PIC S9(3)   COMP-3  VALUE ZERO.              
052900                                                                          
053000 01  W-WDD7A1KY-MIN.                                                      
053100     03  W-IDARTNR-D7-MIN    PIC S9(9)  COMP-3 VALUE ZERO.                
053200     03  FILLER              PIC S9(9)  COMP-3 VALUE ZERO.                
053300     03  FILLER              PIC S9(3)  COMP-3 VALUE ZERO.                
053400                                                                          
053500 01  W-WDD7A1KY-MAX.                                                      
053600     03  W-IDARTNR-D7-MAX    PIC S9(9)  COMP-3 VALUE ZERO.                
053700     03  FILLER              PIC S9(9)  COMP-3 VALUE +999999999.          
053800     03  FILLER              PIC S9(3)  COMP-3 VALUE +999.                
053900                                                                          
054000                                                                          
054100     SKIP2                                                                
054200 01  FELTEXT.                                                             
054300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT,'.            
054400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
054500 01  FELTEXT2.                                                            
054600     03  FILLER                  PIC X(9)    VALUE 'FELTEXT2,'.           
054700     03  FELTEXT2-STR      PIC X(71)   VALUE SPACE.                       
054800 01  IMSTEXT.                                                             
054900     03  FILLER                  PIC X(8)    VALUE 'IMSTEXT,'.            
055000     03  IMSTEXT-STR             PIC X(72)   VALUE SPACE.                 
055100                                                                          
055200     EJECT                                                                
055300*01  -COPY W200W001                                                       
055400     EJECT                                                                
055500 01  DYNAMISKA-SUBPROGRAM.                                                
055600     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
055700     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
055800     03  PERADD              PIC X(8)    VALUE 'PERADD'.                  
055900     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
056000     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
056100     03  WDAGKONV            PIC X(8)    VALUE 'WDAGKONV'.                
056200     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
056300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
056400     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY'.                 
056500     03  W271LTPB            PIC X(8)    VALUE 'W271LTPB'.                
056600     03  WZ20DAYS            PIC X(8)    VALUE 'WZ20DAYS'.                
056700     03  W271REFL            PIC X(8)    VALUE 'W271REFL'.                
056800     03  W271UTIL            PIC X(8)    VALUE 'W271UTIL'.                
056900     03  W271UTUP            PIC X(8)    VALUE 'W271UTUP'.                
057000     EJECT                                                                
057100 01  FILLER                  PIC X(16) VALUE 'WDATAREA        '.          
057200*    ---PARAMETRAR TILL DATKONV                                           
057300*01  -COPY WDATAREA                                                       
057400     EJECT                                                                
057500 01  FILLER                  PIC X(16) VALUE 'WDAGAREA        '.          
057600*    --- PARAMETRAR TILL DAGKONV                                          
057700*01  -COPY WDAGAREA                                                       
057800     EJECT                                                                
057900 01  FILLER                  PIC X(16) VALUE 'W271REFL        '.          
058000*    ---PARAMETRAR TILL W271REFL                                          
058100*01  -COPY W271REFL                                                       
058200     EJECT                                                                
058300 01  FILLER                  PIC X(16) VALUE 'W271UTIL        '.          
058400*    ---PARAMETRAR TILL W271UTIL                                          
058500*01 -COPY W271UTIL                                                        
058600     EJECT                                                                
058700 01  FILLER                  PIC X(16) VALUE 'W271UTUP        '.          
058800*    --- PARAMETRAR TILL W271UTUP                                         
058900*01 -COPY W271UTUP                                                        
059000     EJECT                                                                
059100 01  FILLER                  PIC X(16) VALUE 'W0005           '.          
059200*    --- PARAMETRAR TILL POSTSUM                                          
059300*                                                                         
059400*01  -COPY W0005   -PRE  POSTSUM-                                         
059500     EJECT                                                                
059600 01  FILLER                  PIC X(16) VALUE 'WORKAREA        '.          
059700*    ---PARAMETRAR TILL WORKDAY                                           
059800*01  -COPY WORKAREA                                                       
059900     EJECT                                                                
060000 01  FILLER                  PIC X(16) VALUE 'W271LTPB        '.          
060100*    ---PARAMETRAR TILL W271LTPB                                          
060200*01  -COPY W271LTPB                                                       
060300     EJECT                                                                
060400 01  FILLER                  PIC X(16) VALUE 'WZ20DAYS        '.          
060500*    ---PARAMETRAR TILL WZ20DAYS                                          
060600*01  -COPY WZ20DAYS                                                       
060700     EJECT                                                                
060800 01  FILLER                  PIC X(16) VALUE 'ABEND           '.          
060900*    --- PARAMETRAR TILL ABEND                                            
061000                                                                          
061100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
061200     SKIP3                                                                
061300     SKIP2                                                                
061400*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
061500*                                                                         
061600 01      IMS-WS.                                                          
061700   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
061800     SKIP3                                                                
061900*                            *** STATUSKOD FRÅN IMS                       
062000   03    STATUS-WS       PIC XX.                                          
062100     88  SEGMENT-FINNS               VALUE '  '.                          
062200     88  SEGMENT-SAKNAS              VALUE 'GE'                           
062300                                           'GB'.                          
062400     SKIP3                                                                
062500   03 FILLER                 PIC X(16) VALUE 'SSA1            '.          
062600   03    SSA1            PIC X(96).                                       
062700   03 FILLER                 PIC X(16) VALUE 'SSA2            '.          
062800   03    SSA2            PIC X(50).                                       
062900   03 FILLER                 PIC X(16) VALUE 'SSA3            '.          
063000   03    SSA3            PIC X(50).                                       
063100     SKIP3                                                                
063200 01 FILLER                   PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
063300 01  NYCKLAR-TILL-DLI.                                                    
063400     03  W-WDGXKEY-X.                                                     
063500         05  W-IDHTYP           PIC X(4)    VALUE '2501'.                 
063600         05  W-IDDC-2501        PIC X(2)    VALUE ZERO.                   
063700         05  W-LOWVALUE         PIC X(24)   VALUE LOW-VALUE.              
063800*                                                                         
063900     03  W-IDREFTAB-X.                                                    
064000         05  W-IDREFTAB         PIC X(1)    VALUE SPACE.                  
064100   03 FILLER                 PIC X(16) VALUE 'GODK-STATUSKODER'.          
064200   03    GODK-STATUSKODER.                                                
064300     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
064400     SKIP3                                                                
064500*01      -COPY W0003                                                      
064600     EJECT                                                                
064700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC01'.             
064800     SKIP3                                                                
064900 01  DLI-IO-AREA-ARTC01.                                                  
065000*        05  -COPY WDK601                                                 
065100     EJECT                                                                
065200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC11'.             
065300     SKIP3                                                                
065400 01  DLI-IO-AREA-ARTC11.                                                  
065500*        05  -COPY WDK611                                                 
065600     EJECT                                                                
065700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK629'.             
065800     SKIP3                                                                
065900 01  DLI-IO-WDK629.                                                       
066000*        05  -COPY WDK629                                                 
066100     EJECT                                                                
066200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-K601-ERS'.           
066300     SKIP3                                                                
066400 01  DLI-IO-AREA-K601-ERS.                                                
066500*        05  -COPY WDK601    -PRE ERS-                                    
066600     EJECT                                                                
066700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-K611-ERS'.           
066800     SKIP3                                                                
066900 01  DLI-IO-AREA-K611-ERS.                                                
067000*        05  -COPY WDK611    -PRE ERS-                                    
067100     EJECT                                                                
067200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK621'.             
067300     SKIP3                                                                
067400 01  DLI-IO-AREA-K621.                                                    
067500*        05  -COPY WDK621                                                 
067600     EJECT                                                                
067700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC24'.             
067800     SKIP3                                                                
067900 01  DLI-IO-AREA-ARTC24.                                                  
068000*        05  -COPY WDK624                                                 
068100     EJECT                                                                
068200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC26'.             
068300     SKIP3                                                                
068400 01  DLI-IO-AREA-ARTC26.                                                  
068500*        05  -COPY WDK626                                                 
068600     EJECT                                                                
068700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTM01'.             
068800     SKIP3                                                                
068900 01  DLI-IO-AREA-ARTM01.                                                  
069000*        05  -COPY WDK901 -PRE ARTM-                                      
069100     EJECT                                                                
069200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTM11'.             
069300     SKIP3                                                                
069400 01  DLI-IO-AREA-ARTM11.                                                  
069500*        05  -COPY WDK911 -PRE ARTM-                                      
069600     EJECT                                                                
069700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
069800     SKIP3                                                                
069900 01  DLI-IO-AREA-WDK701.                                                  
070000*        05  -COPY WDK701                                                 
070100     EJECT                                                                
070200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
070300     SKIP3                                                                
070400 01  DLI-IO-AREA-WDK711.                                                  
070500*        05  -COPY WDK711                                                 
070600     EJECT                                                                
070700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK727'.             
070800     SKIP3                                                                
070900 01  DLI-IO-AREA-WDK727.                                                  
071000*        05  -COPY WDK727                                                 
071100     EJECT                                                                
071200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711-E'.           
071300     SKIP3                                                                
071400 01  DLI-IO-AREA-WDK711-ERS.                                              
071500*        05  -COPY WDK711    -PRE ERS-                                    
071600     EJECT                                                                
071700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
071800     SKIP3                                                                
071900 01  DLI-IO-AREA-WDK712.                                                  
072000*        05  -COPY WDK712                                                 
072100     EJECT                                                                
072200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA'.             
072300     SKIP3                                                                
072400 01  DLI-IO-AREA.                                                         
072500     03  IO-AREA             PIC X(1036)   VALUE SPACE.                   
072600     03  WL250101 REDEFINES IO-AREA.                                      
072700*        05  -COPY WDGX2501                                               
072800                                                                          
072900     03  WL250111 REDEFINES IO-AREA.                                      
073000*        05  -COPY WDGX2502                                               
073100     EJECT                                                                
073200                                                                          
073300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
073400 01   DLI-IO-AREA-B601.                                                   
073500*     03  -COPY WDB601                                                    
073600                                                                          
073700 01  FILLER               PIC X(16)   VALUE 'B601-TABELL'.                
073800 01  B601-IX              PIC S9(4)   COMP SYNC VALUE ZERO.               
073900 01  MAX-B601-IX          PIC S9(4)   COMP SYNC VALUE +80.                
074000 01  B601-TABELL.                                                         
074100     03  FILLER OCCURS 80.                                                
074200*      05  -COPY WDB601  -PRE TAB-                                        
074300                                                                          
074400 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
074500 01   DLI-IO-AREA-B616.                                                   
074600*     03  -COPY WDB616                                                    
074700                                                                          
074800 01  FILLER               PIC X(16)   VALUE 'B616-TABELL'.                
074900 01  B616-IX              PIC S9(4)   COMP SYNC VALUE ZERO.               
075000 01  MAX-B616-IX          PIC S9(4)   COMP SYNC VALUE +180.               
075100 01  B616-TABELL.                                                         
075200     03  FILLER OCCURS 180.                                               
075300       05  TAB-IDDC       PIC XX.                                         
075400*      05  -COPY WDB616  -PRE TAB-                                        
075500                                                                          
075600******************************************************************        
075700*-- LÄSER NER LEDTIDER FÖR KINA-REFILL DC TILL CDC I EGEN TABELL.         
075800                                                                          
075900 01  FILLER               PIC X(16)   VALUE 'WDB616-CDC-AREA'.            
076000 01   DLI-IO-AREA-B616-CDC.                                               
076100*     03  -COPY WDB616   -PRE DC11-                                       
076200                                                                          
076300 01  FILLER               PIC X(16)   VALUE 'CDC-B616-TABELL'.            
076400 01  C-B616-IX            PIC S9(4)   COMP SYNC VALUE ZERO.               
076500 01  MAX-C-B616-IX        PIC S9(4)   COMP SYNC VALUE +90.                
076600 01  CDC-B616-IDDC        PIC X(2)    VALUE '11'.                         
076700 01  SPAR-REF-IDDC-REF    PIC X(2)    VALUE SPACE.                        
076800 01  CDC-B616-TABELL.                                                     
076900     03  FILLER OCCURS 90.                                                
077000       05  CDC-TAB-IDDC-REF         PIC X(2)  VALUE SPACE.                
077100       05  CDC-TAB-KVDLTID-AIRETA   PIC S9(3) VALUE ZERO COMP-3.          
077200       05  CDC-TAB-KVDLTID-TOT      PIC S9(3) VALUE ZERO COMP-3.          
077300                                                                          
077400******************************************************************        
077500 01  FILLER               PIC X(16)   VALUE 'WDD7A1 AREA'.                
077600 01   DLI-IO-AREA-D7A1.                                                   
077700*     03  -COPY WDD7A1                                                    
077800     EJECT                                                                
077900 LINKAGE SECTION.                                                         
078000     SKIP3                                                                
078100*01  AREA  -COPY W222L222C0  -PRE LINK-.                                  
078200     EJECT                                                                
078300*01  -COPY W0008  -PRE ARTC-.                                             
078400     05  FILLER              PIC X.                                       
078500     EJECT                                                                
078600*01  -COPY W0008  -PRE WDK7-.                                             
078700     05  FILLER              PIC X.                                       
078800     EJECT                                                                
078900*01  -COPY W0008  -PRE ARTM-.                                             
079000     05  FILLER              PIC X.                                       
079100     EJECT                                                                
079200*01  -COPY W0008  -PRE REFL-2501-                                         
079300     05  FILLER                  PIC X.                                   
079400     EJECT                                                                
079500*01  -COPY W0008  -PRE REFL-WDB6-                                         
079600     05  FILLER                  PIC X.                                   
079700     EJECT                                                                
079800*01  -COPY W0008  -PRE REFL-WDK7-                                         
079900     05  FILLER                  PIC X.                                   
080000     EJECT                                                                
080100*01  -COPY W0008  -PRE WDB6-                                              
080200     05  FILLER                  PIC X.                                   
080300     EJECT                                                                
080400*01  -COPY W0008  -PRE WDD7A-                                             
080500     05  FILLER                  PIC X.                                   
080600     EJECT                                                                
080700*01  -COPY W0008  -PRE WDK7-ERS-                                          
080800     05  FILLER                  PIC X.                                   
080900     EJECT                                                                
081000 01  UTIL-WDK6-PCB               PIC X.                                   
081100 01  UTIL-WDK7-PCB               PIC X.                                   
081200 01  UTIL-WDB6-PCB               PIC X.                                   
081300 01  UTUP-WDK7-PCB               PIC X.                                   
081400 01  UTUP-WDB6-PCB               PIC X.                                   
081500 01  UTUP-UTIL-WDK6-PCB          PIC X.                                   
081600 01  UTUP-UTIL-WDK7-PCB          PIC X.                                   
081700 01  UTUP-UTIL-WDB6-PCB          PIC X.                                   
081800     EJECT                                                                
081900 PROCEDURE DIVISION USING LINK-AREA ARTC-PCB WDK7-PCB ARTM-PCB            
082000                                    REFL-2501-PCB REFL-WDB6-PCB           
082100                                    REFL-WDK7-PCB                         
082200                                    WDB6-PCB WDD7A-PCB                    
082300                                    WDK7-ERS-PCB                          
082400                                    UTIL-WDK6-PCB                         
082500                                    UTIL-WDK7-PCB                         
082600                                    UTIL-WDB6-PCB                         
082700                                    UTUP-WDK7-PCB                         
082800                                    UTUP-WDB6-PCB                         
082900                                    UTUP-UTIL-WDK6-PCB                    
083000                                    UTUP-UTIL-WDK7-PCB                    
083100                                    UTUP-UTIL-WDB6-PCB                    
083200                                    .                                     
083300     PERFORM A-INITIERA                                                   
083400                                                                          
083500     MOVE LINK-IDARTNR TO W-IDARTNR                                       
083600     PERFORM IMS-GU-WDK601                                                
083700                                                                          
083800     IF  SEGMENT-FINNS                                                    
083900       MOVE JA                TO LINK-FLJANEJ-ANROP                       
084000                                                                          
084100       PERFORM IMS-GNP-WDK611                                             
084200       IF SEGMENT-FINNS                                                   
084300                                                                          
084400         IF CLAG-IDDC-REF NOT = SPACE                                     
084500           PERFORM IMS-GNP-WDK629                                         
084600           IF SEGMENT-FINNS                                               
084700             MOVE JA  TO WS-REF-FINNS-K629                                
084800             PERFORM X-READ-OR-TAB-B616-CDC                               
084900           END-IF                                                         
085000         END-IF                                                           
085100                                                                          
085200         DIVIDE ART-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                   
085300                                                                          
085400         EVALUATE LINK-KDBEHOV                                            
085500             WHEN ENDAST-SEPARATBEHOV                                     
085600                  PERFORM B-BERAKNA-PROGNOS                               
085700                                                                          
085800             WHEN ENDAST-SATSBEHOV                                        
085900                  PERFORM C-BERAKNA-SATSBEHOV                             
086000                                                                          
086100             WHEN SEP-SATS-BEHOV                                          
086200                  PERFORM B-BERAKNA-PROGNOS                               
086300                  PERFORM C-BERAKNA-SATSBEHOV                             
086400                                                                          
086500             WHEN ENDAST-LEVBEHOV                                         
086600                  PERFORM F-BERAKNA-TPO-BEHOV                             
086700                                                                          
086800             WHEN SEP-DO-BEHOV                                            
086900                  PERFORM B-BERAKNA-PROGNOS                               
087000                  PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                    
087100                                                                          
087200             WHEN SATS-DO-BEHOV                                           
087300                  PERFORM C-BERAKNA-SATSBEHOV                             
087400                  PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                    
087500                                                                          
087600             WHEN SEP-SATS-DO-BEHOV                                       
087700                  PERFORM B-BERAKNA-PROGNOS                               
087800                  PERFORM C-BERAKNA-SATSBEHOV                             
087900                  PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                    
088000                                                                          
088100             WHEN SATS-TPO-LEVBEHOV                                       
088200                  PERFORM C-BERAKNA-SATSBEHOV                             
088300                  PERFORM F-BERAKNA-TPO-BEHOV                             
088400                                                                          
088500             WHEN SEP-SATS-TPO-LEVBEHOV                                   
088600                  PERFORM B-BERAKNA-PROGNOS                               
088700                  PERFORM C-BERAKNA-SATSBEHOV                             
088800                  PERFORM F-BERAKNA-TPO-BEHOV                             
088900                                                                          
089000             WHEN ENDAST-SDCBEHOV                                         
089100                  PERFORM G-BERAKNA-SDCBEHOV                              
089200                                                                          
089300             WHEN SATS-TPO-SDCBEHOV                                       
089400                  PERFORM C-BERAKNA-SATSBEHOV                             
089500                  PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                    
089600                  PERFORM G-BERAKNA-SDCBEHOV                              
089700                                                                          
089800             WHEN SEP-SATS-TPO-SDCBEHOV                                   
089900                  PERFORM B-BERAKNA-PROGNOS                               
090000                  PERFORM C-BERAKNA-SATSBEHOV                             
090100                  PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                    
090200                  PERFORM G-BERAKNA-SDCBEHOV                              
090300                                                                          
090400             WHEN SATS-TPO-LEV-SDCBEHOV                                   
090500                  PERFORM C-BERAKNA-SATSBEHOV                             
090600                  PERFORM F-BERAKNA-TPO-BEHOV                             
090700                  PERFORM G-BERAKNA-SDCBEHOV                              
090800                                                                          
090900             WHEN SEP-SATS-TPO-LEV-SDCBEHOV                               
091000                  PERFORM B-BERAKNA-PROGNOS                               
091100                  PERFORM C-BERAKNA-SATSBEHOV                             
091200                  PERFORM F-BERAKNA-TPO-BEHOV                             
091300                  PERFORM G-BERAKNA-SDCBEHOV                              
091400                                                                          
091500             WHEN ENDAST-NDCBEHOV                                         
091600                  PERFORM H-BERAKNA-NDCBEHOV                              
091700                                                                          
091800             WHEN SATS-TPO-SDC-NDC                                        
091900                  PERFORM C-BERAKNA-SATSBEHOV                             
092000                  PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                    
092100                  PERFORM G-BERAKNA-SDCBEHOV                              
092200                  PERFORM H-BERAKNA-NDCBEHOV                              
092300                                                                          
092400             WHEN SEP-SATS-TPO-SDC-NDC                                    
092500                  IF (CLAG-DAPBPLAN > WS-DAGENS-DATUM                     
092600                  OR  CLAG-DAPBPLAN = WS-DAGENS-DATUM)                    
092700                  OR (CLAG-TIPBPLAN-JUST1-FOM > +0)                       
092800******    PLANERAT PB ÄR SATT SOM GÄLLER ISTÄLLET FÖR                     
092900******    DET TOTALA SEPARAT- OCH REFILLBEHOVEN                           
093000                                                                          
093100                    IF CLAG-DASEASON < WS-DAGENS-DATUM                    
093200******    MASKINELLT BERÄKNADE SÄSONGSINDEX GÄLLER                        
093300******    RÄKNAS FRAM NEDAN PÅ SAMMA SÄTT SOM W222PBTO GÖR                
093400******    MEN EFTERSOM DEN ANROPAR DETTA PROGRAM KAN MAN                  
093500******    INTE GÖRA CALL PÅ W222PBTO                                      
093600******    ALL KOD HAR FÅTT KOPIERATS IN I IA-BERAKNA-PBTOTAL              
093700******    SAMT I IB-BERAKNA-SASONG                                        
093800                      MOVE 52   TO LINK-KVVECKOR-BEHOV                    
093900                      PERFORM S07-INIT-DATUM                              
094000                      PERFORM B-BERAKNA-PROGNOS                           
094100                      PERFORM G-BERAKNA-SDCBEHOV                          
094200                      PERFORM H-BERAKNA-NDCBEHOV                          
094300                      PERFORM E-FLYTTA-RESULT-TILL-LINKAREA               
094400                      PERFORM K-BER-PBTOT-SASONG                          
094500                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
094600                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
094700                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
094800                                   LINK-TIBEHOV-FIRST                     
094900                      MOVE WS-CALL-KVVECKOR-BEHOV                         
095000                                TO LINK-KVVECKOR-BEHOV                    
095100                      PERFORM S07-INIT-DATUM                              
095200                    ELSE                                                  
095300******                                                                    
095400******    DETTA GÖRS FÖR ATT BERÄKNA FRAM BEHOVET I NÄRTID                
095500******    DVS INNEVARANDE PLUS TVÅ VECKOR                                 
095600******    EFTERSOM PBPLAN INTE SKA PÅVERKA NÄRTID                         
095700******    RESULTATET FRÅN I-BERAKNA-PB-PLAN FLYTTAS                       
095800******    I S10-ADD-TILL-RESULTAT-PBPLAN                                  
095900******                                                                    
096000                      MOVE 10   TO LINK-KVVECKOR-BEHOV                    
096100                      PERFORM S07-INIT-DATUM                              
096200                      PERFORM B-BERAKNA-PROGNOS                           
096300                      PERFORM G-BERAKNA-SDCBEHOV                          
096400                      PERFORM H-BERAKNA-NDCBEHOV                          
096500                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
096600                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
096700                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
096800                                   LINK-TIBEHOV-FIRST                     
096900                      MOVE WS-CALL-KVVECKOR-BEHOV                         
097000                                TO LINK-KVVECKOR-BEHOV                    
097100                      PERFORM S07-INIT-DATUM                              
097200                    END-IF                                                
097300                                                                          
097400                    PERFORM I-BERAKNA-PB-PLAN                             
097500                    PERFORM C-BERAKNA-SATSBEHOV                           
097600                    PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                  
097700                                                                          
097800                  ELSE                                                    
097900                                                                          
098000                    IF CLAG-DASEASON > WS-DAGENS-DATUM                    
098100                    OR CLAG-DASEASON = WS-DAGENS-DATUM                    
098200******    MANUELL SÄSONG ÄR SATT (GÄLLER FÖR DET TOTALA                   
098300******    SEPARAT- SAMT REFILLBEHOVEN)                                    
098400******    RÄKNA FÖRST FRAM EN ÅRSTOTAL                                    
098500******    FÖR ATT FÅ FRAM EN MASKINELL PROGNOS                            
098600******    SOM SKA PÅVERKAS AV MANUELLT SATTA SÄSONGSINDEX                 
098700                      MOVE 52   TO LINK-KVVECKOR-BEHOV                    
098800                      PERFORM S07-INIT-DATUM                              
098900                      PERFORM B-BERAKNA-PROGNOS                           
099000                      PERFORM G-BERAKNA-SDCBEHOV                          
099100                      PERFORM H-BERAKNA-NDCBEHOV                          
099200                      PERFORM E-FLYTTA-RESULT-TILL-LINKAREA               
099300                      PERFORM K-BER-PBTOT-SASONG                          
099400                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
099500                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
099600                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
099700                                   LINK-TIBEHOV-FIRST                     
099800                      MOVE WS-CALL-KVVECKOR-BEHOV                         
099900                                TO LINK-KVVECKOR-BEHOV                    
100000                      PERFORM S07-INIT-DATUM                              
100100                      PERFORM I-BERAKNA-PB-PLAN                           
100200                      PERFORM C-BERAKNA-SATSBEHOV                         
100300                      PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                
100400                    ELSE                                                  
100500                                                                          
100600                      PERFORM B-BERAKNA-PROGNOS                           
100700                      PERFORM G-BERAKNA-SDCBEHOV                          
100800                      PERFORM H-BERAKNA-NDCBEHOV                          
100900                      PERFORM C-BERAKNA-SATSBEHOV                         
101000                      PERFORM D-BERAKNA-DIVERSEORDER-BEHOV                
101100                                                                          
101200                    END-IF                                                
101300                  END-IF                                                  
101400                                                                          
101500             WHEN SATS-TPO-LEV-SDC-NDC                                    
101600                  PERFORM C-BERAKNA-SATSBEHOV                             
101700                  PERFORM F-BERAKNA-TPO-BEHOV                             
101800                  PERFORM G-BERAKNA-SDCBEHOV                              
101900                  PERFORM H-BERAKNA-NDCBEHOV                              
102000                                                                          
102100             WHEN SEP-SATS-TPO-LEV-SDC-NDC                                
102200                  IF (CLAG-DAPBPLAN > WS-DAGENS-DATUM                     
102300                  OR  CLAG-DAPBPLAN = WS-DAGENS-DATUM)                    
102400                  OR (CLAG-TIPBPLAN-JUST1-FOM > +0)                       
102500******    PLANERAT PB ÄR SATT SOM GÄLLER ISTÄLLET FÖR                     
102600******    DET TOTALA SEPARAT- OCH REFILLBEHOVEN                           
102700                                                                          
102800                    IF CLAG-DASEASON < WS-DAGENS-DATUM                    
102900******    MASKINELLT BERÄKNADE SÄSONGSINDEX GÄLLER                        
103000******    RÄKNAS FRAM NEDAN PÅ SAMMA SÄTT SOM W222PBTO GÖR                
103100******    MEN EFTERSOM DEN ANROPAR DETTA PROGRAM KAN MAN                  
103200******    INTE GÖRA CALL PÅ W222PBTO                                      
103300******    ALL KOD HAR FÅTT KOPIERATS IN I K-BER-PBTOT-SASONG              
103400******    SAMT I IB-BERAKNA-SASONG                                        
103500                      MOVE 52   TO LINK-KVVECKOR-BEHOV                    
103600                      PERFORM S07-INIT-DATUM                              
103700                      PERFORM B-BERAKNA-PROGNOS                           
103800                      PERFORM G-BERAKNA-SDCBEHOV                          
103900                      PERFORM H-BERAKNA-NDCBEHOV                          
104000                      PERFORM E-FLYTTA-RESULT-TILL-LINKAREA               
104100                      PERFORM K-BER-PBTOT-SASONG                          
104200                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
104300                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
104400                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
104500                                   LINK-TIBEHOV-FIRST                     
104600                      MOVE WS-CALL-KVVECKOR-BEHOV                         
104700                                TO LINK-KVVECKOR-BEHOV                    
104800                      PERFORM S07-INIT-DATUM                              
104900                    ELSE                                                  
105000******                                                                    
105100******    DETTA GÖRS FÖR ATT BERÄKNA FRAM BEHOVET I NÄRTID                
105200******    DVS INNEVARANDE PLUS TVÅ VECKOR                                 
105300******    EFTERSOM PBPLAN INTE SKA PÅVERKA NÄRTID                         
105400******    RESULTATET FRÅN I-BERAKNA-PB-PLAN FLYTTAS                       
105500******    I S10-ADD-TILL-RESULTAT-PBPLAN                                  
105600******                                                                    
105700                      MOVE 10   TO LINK-KVVECKOR-BEHOV                    
105800                      PERFORM S07-INIT-DATUM                              
105900                      PERFORM B-BERAKNA-PROGNOS                           
106000                      PERFORM G-BERAKNA-SDCBEHOV                          
106100                      PERFORM H-BERAKNA-NDCBEHOV                          
106200                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
106300                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
106400                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
106500                                   LINK-TIBEHOV-FIRST                     
106600                      MOVE WS-CALL-KVVECKOR-BEHOV                         
106700                                TO LINK-KVVECKOR-BEHOV                    
106800                      PERFORM S07-INIT-DATUM                              
106900                    END-IF                                                
107000                                                                          
107100                    PERFORM I-BERAKNA-PB-PLAN                             
107200                    PERFORM C-BERAKNA-SATSBEHOV                           
107300                    PERFORM F-BERAKNA-TPO-BEHOV                           
107400                                                                          
107500                  ELSE                                                    
107600                                                                          
107700                    IF CLAG-DASEASON > WS-DAGENS-DATUM                    
107800                    OR CLAG-DASEASON = WS-DAGENS-DATUM                    
107900******    MANUELL SÄSONG ÄR SATT (GÄLLER FÖR DET TOTALA                   
108000******    SEPARAT- SAMT REFILLBEHOVEN)                                    
108100******    RÄKNA FÖRST FRAM EN ÅRSTOTAL                                    
108200******    FÖR ATT FÅ FRAM EN MASKINELL PROGNOS                            
108300******    SOM SKA PÅVERKAS AV MANUELLT SATTA SÄSONGSINDEX                 
108400                      MOVE 52   TO LINK-KVVECKOR-BEHOV                    
108500                      PERFORM S07-INIT-DATUM                              
108600                      PERFORM B-BERAKNA-PROGNOS                           
108700                      PERFORM G-BERAKNA-SDCBEHOV                          
108800                      PERFORM H-BERAKNA-NDCBEHOV                          
108900                      PERFORM E-FLYTTA-RESULT-TILL-LINKAREA               
109000                      PERFORM K-BER-PBTOT-SASONG                          
109100                      PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                
109200                      PERFORM S09-NOLLSTALL-RESULTAT-PBPLAN               
109300                      MOVE ZERO TO LINK-KVBEHOV-SUMMA                     
109400                                   LINK-TIBEHOV-FIRST                     
109500                      MOVE WS-CALL-KVVECKOR-BEHOV                         
109600                                TO LINK-KVVECKOR-BEHOV                    
109700                      PERFORM S07-INIT-DATUM                              
109800                      PERFORM I-BERAKNA-PB-PLAN                           
109900                      PERFORM C-BERAKNA-SATSBEHOV                         
110000                      PERFORM F-BERAKNA-TPO-BEHOV                         
110100                    ELSE                                                  
110200                                                                          
110300                      PERFORM B-BERAKNA-PROGNOS                           
110400                      PERFORM G-BERAKNA-SDCBEHOV                          
110500                      PERFORM H-BERAKNA-NDCBEHOV                          
110600                      PERFORM C-BERAKNA-SATSBEHOV                         
110700                      PERFORM F-BERAKNA-TPO-BEHOV                         
110800                                                                          
110900                    END-IF                                                
111000                  END-IF                                                  
111100                                                                          
111200             WHEN PB-TOTAL                                                
111300                  PERFORM B-BERAKNA-PROGNOS                               
111400                  PERFORM G-BERAKNA-SDCBEHOV                              
111500                  PERFORM H-BERAKNA-NDCBEHOV                              
111600                                                                          
111700             WHEN SEP-TPO-SDC-NDC                                         
111800                  PERFORM B-BERAKNA-PROGNOS                               
111900                  PERFORM F-BERAKNA-TPO-BEHOV                             
112000                  PERFORM G-BERAKNA-SDCBEHOV                              
112100                  PERFORM H-BERAKNA-NDCBEHOV                              
112200                                                                          
112300         END-EVALUATE                                                     
112400       ELSE                                                               
112500           MOVE NEJ TO LINK-FLJANEJ-ANROP                                 
112600       END-IF                                                             
112700     ELSE                                                                 
112800         MOVE NEJ TO LINK-FLJANEJ-ANROP                                   
112900     END-IF                                                               
113000     SKIP1                                                                
113100     MOVE JA  TO TREND-SW                                                 
113200     PERFORM E-FLYTTA-RESULT-TILL-LINKAREA                                
113300     MOVE NEJ TO TREND-SW                                                 
113400* START TEST                                                              
113500*    DIVIDE LINK-KVBEHOV-SUMMA BY LINK-TIBEHOV-FIRST                      
113600*    GIVING LINK-KVBEHOV-SUMMA                                            
113700* SLUT TEST                                                               
113800     SKIP1                                                                
113900     MOVE ZERO TO RETURN-CODE                                             
114000     GOBACK                                                               
114100     .                                                                    
114200     EJECT                                                                
114300 A-INITIERA SECTION.                                                      
114400     MOVE 'A-INITIERA  '   TO CURRENT-SECTION                             
114500******************************************************************        
114600*                                                                *        
114700*    BERÄKNING AV START- OCH SLUT-TIDPUNKTER (ÅR OCH VECKA)      *        
114800*    FÖR BERÄKNING                                               *        
114900*    NOLLSTÄLLNING AV TABELLER                                   *        
115000*                                                                *        
115100***************************************************************           
115200     SKIP1                                                                
115300     MOVE FUNCTION CURRENT-DATE                                           
115400                             TO WS-CURRENT-DATE                           
115500                                                                          
115600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
115700     MOVE WS-DAGENS-DATUM (3:6)                                           
115800                             TO DAT-I-TIDATUM                             
115900                                                                          
116000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
116100                     DAT-O-TIDATUM DAT-KDSVAR                             
116200                                                                          
116300     IF DAT-KDSVAR-OK                                                     
116400                                                                          
116500       MOVE DAT-TIVV         TO WS-DAGENS-VECKA                           
116600       MOVE DAT-TIAAVVD      TO WS-DAGENS-AAVVD                           
116700       MOVE DAT-TID          TO WS-DAGENS-DAGNR                           
116800                                                                          
116900     ELSE                                                                 
117000         STRING ' FEL FRÅN WDATKONV I W2222200'                           
117100                ' (A-INIT)'                                               
117200         DELIMITED BY SIZE INTO FELTEXT-STR                               
117300         DISPLAY FELTEXT                                                  
117400         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
117500     END-IF                                                               
117600                                                                          
117700     IF  LINK-KVVECKOR-BEHOV > +97                                        
117800         MOVE +97  TO LINK-KVVECKOR-BEHOV                                 
117900     END-IF                                                               
118000     MOVE LINK-KVVECKOR-BEHOV                                             
118100                             TO WS-CALL-KVVECKOR-BEHOV                    
118200     PERFORM S07-INIT-DATUM                                               
118300     SKIP1                                                                
118400     PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                                 
118500     PERFORM S02-NOLLSTALL-RESULTAT-TAB                                   
118600     MOVE ZERO               TO LINK-KVBEHOV-SUMMA                        
118700                                LINK-KVBEHOV-DESSUTOM                     
118800                                LINK-TIBEHOV-FIRST                        
118900                                WS-ARSTOTAL                               
119000                                WS-KVPB-PLAN-VECKA                        
119100                                                                          
119200     MOVE WS-NOLLA-KVBEHOV       TO WS-KVBEHOV-TABELL                     
119300     MOVE WS-NOLLA-RESEASON      TO WS-RESEASON-TABELL                    
119400     MOVE WS-NOLLA-RESEASON-AVR  TO WS-RESEASON-AVR-TABELL                
119500                                                                          
119600     PERFORM AA-HAMTA-VV-I-PER                                            
119700     PERFORM AB-HAMTA-K712                                                
119800                                                                          
119900     MOVE NEJ  TO WS-REF-FINNS-K629                                       
120000                                                                          
120100     .                                                                    
120200     EJECT                                                                
120300 AA-HAMTA-VV-I-PER SECTION.                                               
120400     MOVE 'AA-HAMTA-VV-I-PER '  TO CURRENT-SECTION                        
120500                                                                          
120600     MOVE +1                 TO IX                                        
120700     MOVE DAT-TIAARP         TO WS-TIAAPER                                
120800     MOVE 1                  TO PER                                       
120900                                                                          
121000     PERFORM UNTIL IX        >  12                                        
121100       MOVE 'AARP  '         TO DAT-KDDATFORM                             
121200       MOVE TIAAPER          TO DAT-I-TIDATUM                             
121300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
121400                           DAT-O-TIDATUM DAT-KDSVAR                       
121500       IF DAT-KDSVAR-OK                                                   
121600          MOVE DAT-TIVV      TO PER-START-VV (IX)                         
121700       ELSE                                                               
121800           STRING ' FEL DATUM - DATKONV3 I W2222200'                      
121900                  '(AA-HAMTA, TIAAPER)'                                   
122000           DELIMITED BY SIZE INTO FELTEXT-STR                             
122100           DISPLAY FELTEXT                                                
122200           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
122300       END-IF                                                             
122400       ADD +1                TO IX                                        
122500                               PER                                        
122600     END-PERFORM                                                          
122700                                                                          
122800     MOVE 1                  TO IX-TILL                                   
122900     MOVE 2                  TO IX-FRAN                                   
123000     PERFORM UNTIL IX-FRAN   >  12                                        
123100       COMPUTE PER-SLUT-VV (IX-TILL) =                                    
123200               PER-START-VV (IX-FRAN) - 1                                 
123300       ADD 1                 TO IX-TILL                                   
123400                                IX-FRAN                                   
123500     END-PERFORM                                                          
123600     MOVE 52                 TO PER-SLUT-VV (12)                          
123700     .                                                                    
123800     EJECT                                                                
123900 AB-HAMTA-K712   SECTION.                                                 
124000     MOVE 'AB-HAMTA-K712  '  TO CURRENT-SECTION                           
124100                                                                          
124200     INITIALIZE K712-TAB                                                  
124300     MOVE 1      TO K712-IX                                               
124400                                                                          
124500     MOVE LINK-IDARTNR TO W-IDARTNR                                       
124600     PERFORM IMS-GU-WDK701                                                
124700     IF SEGMENT-FINNS                                                     
124800                                                                          
124900        PERFORM IMS-GNP-WDK712                                            
125000        PERFORM UNTIL SEGMENT-SAKNAS                                      
125100                                                                          
125200           MOVE LART-IDLANDX2  TO K712-IDLANDX2(K712-IX)                  
125300           MOVE LART-DAPUBL    TO K712-DAPUBL  (K712-IX)                  
125400           MOVE LART-PRMATRL   TO K712-PRMATRL (K712-IX)                  
125500                                                                          
125600           ADD 1 TO K712-IX                                               
125700           PERFORM IMS-GNP-WDK712                                         
125800        END-PERFORM                                                       
125900     END-IF                                                               
126000     .                                                                    
126100     EJECT                                                                
126200 B-BERAKNA-PROGNOS SECTION.                                               
126300     MOVE 'B-BERAKNA-PROGNOS '  TO CURRENT-SECTION                        
126400******************************************************************        
126500*                                                                *        
126600*    BERÄKNING AV PROGNOSBEHOV AV ARTIKEL FÖR RESPEKTIVE         *        
126700*    C-LAGER                                                     *        
126800*                                                                *        
126900******************************************************************        
127000     SKIP1                                                                
127100     PERFORM IMS-GNP-WDK626                                               
127200                                                                          
127300     IF SEGMENT-FINNS                                                     
127400       MOVE JA               TO JUST-PB-FINNS                             
127500     ELSE                                                                 
127600       MOVE NEJ              TO JUST-PB-FINNS                             
127700       MOVE ZERO        TO JUST-REPBJUST                                  
127800                         JUST-TIPBJUST-CENTR                              
127900                         JUST-DAMANSEA                                    
128000                         JUST-DASPSEA                                     
128100                         JUST-KVPB-JUST (1)                               
128200                         JUST-KVPB-JUST (2)                               
128300                         JUST-TIPBJUST    (1)                             
128400                         JUST-TIPBJUST    (2)                             
128500       MOVE 1                 TO IX                                       
128600       PERFORM UNTIL IX >        ANTAL-SEASONINDEX                        
128700           MOVE +1            TO JUST-RESEASON (IX)                       
128800           ADD 1              TO IX                                       
128900       END-PERFORM                                                        
129000     END-IF                                                               
129100     PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                                 
129200     PERFORM BA-PROGNOSBERAKNING                                          
129300     PERFORM S03-ADD-TILL-RESULTAT-TAB                                    
129400     .                                                                    
129500     EJECT                                                                
129600 BA-PROGNOSBERAKNING SECTION.                                             
129700     MOVE 'BA-PROGNOSBERAKNING '  TO CURRENT-SECTION                      
129800******************************************************************        
129900*                                                                *        
130000*    BERÄKNING AV ARTIKELPROGNOS PER VECKA. UTGÅNGSPUNKT FÖR     *        
130100*    BERÄKNINGEN ÄR PERIODBEHOVET (KVPB-SEP).                    *        
130200*    OM PERIODBEHOVS-JUSTERINGAR FANNS  I ARTIKELREGISTRET       *        
130300*    JUSTERAS BEHOVET.                                           *        
130400*    PB-BEHOV JUSTERAS FÖR DIREKTLEVERANSANDEL.                  *        
130500*                                                                *        
130600******************************************************************        
130700                                                                          
130800     PERFORM BAA-JUSTERA-PB-FAKTORER                                      
130900     MOVE +1                 TO BER-IY                                    
131000     MOVE W-BER-START-VV     TO BER-IX                                    
131100     MOVE LINK-TIBEHOV-START TO W-BER-DATUM                               
131200                                                                          
131300     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
131400                                                                          
131500         PERFORM UNTIL                                                    
131600            (BER-IY = BER-ANTAL-AA AND                                    
131700             BER-IX > BER-ANTAL-VV)                                       
131800         OR (BER-IY < BER-ANTAL-AA AND                                    
131900             BER-IX > 52)                                                 
132000                                                                          
132100             MOVE W-BER-DATUM     TO TMP1-YYWW                            
132200             MOVE W-TIFINLV-AAVV  TO TMP2-YYWW                            
132300             MOVE '68630'         TO FELTEXT2-STR                         
132400             PERFORM WY2000P3                                             
132500             IF  TMP1-YYWW < TMP2-YYWW                                    
132600                 MOVE ZERO TO BER-BEHOV (BER-IY, BER-IX)                  
132700             ELSE                                                         
132800                 MOVE W-KVPB-SEP TO BER-BEHOV (BER-IY, BER-IX)            
132900                                                                          
133000                 IF  JUST-PB-FINNS = JA                                   
133100                     PERFORM BAB-PB-JUSTERINGAR                           
133200                 END-IF                                                   
133300                                                                          
133400                 MOVE +1 TO W-FAKTOR                                      
133500                 SUBTRACT CLAG-REDIRLEV FROM W-FAKTOR                     
133600                 MULTIPLY W-FAKTOR BY BER-BEHOV (BER-IY, BER-IX)          
133700                                                                          
133800             END-IF                                                       
133900                                                                          
134000             IF BER-IY = 1 AND BER-IX = 1                                 
134100                 PERFORM BAC-JUSTERA-VA1                                  
134200             END-IF                                                       
134300                                                                          
134400             ADD 1           TO BER-IX                                    
134500                                W-BER-DATUM                               
134600         END-PERFORM                                                      
134700         ADD 1   TO BER-IY                                                
134800         MOVE 1  TO BER-IX                                                
134900                    IX-PER                                                
135000         SUBTRACT +52 FROM  W-BER-DATUM                                   
135100         IF W-BER-DATUM > 9900                                            
135200             MOVE 0001 TO W-BER-DATUM                                     
135300         ELSE                                                             
135400             ADD +100     TO    W-BER-DATUM                               
135500         END-IF                                                           
135600     END-PERFORM                                                          
135700     .                                                                    
135800     EJECT                                                                
135900 BAA-JUSTERA-PB-FAKTORER SECTION.                                         
136000     MOVE 'BAA-JUSTERA-PB-FAKTORER '  TO CURRENT-SECTION                  
136100     SKIP3                                                                
136200     MOVE ZERO TO W-KVPB-SEP                                              
136300                  W-KVPB-JUST1                                            
136400                  W-KVPB-JUST2                                            
136500                                                                          
136600     IF  CLAG-KVPB-SEP > ZERO                                             
136700         DIVIDE CLAG-KVPB-SEP BY 4.33                                     
136800                                GIVING W-KVPB-SEP                         
136900     END-IF                                                               
137000     IF  JUST-KVPB-JUST (1) > ZERO                                        
137100         DIVIDE JUST-KVPB-JUST (1) BY 4.33                                
137200                                GIVING W-KVPB-JUST1                       
137300     END-IF                                                               
137400     IF  JUST-KVPB-JUST (2) > ZERO                                        
137500         DIVIDE JUST-KVPB-JUST (2) BY 4.33                                
137600                                GIVING W-KVPB-JUST2                       
137700     END-IF                                                               
137800     .                                                                    
137900     EJECT                                                                
138000 BAB-PB-JUSTERINGAR SECTION.                                              
138100     MOVE 'BAB-PB-JUSTERINGAR '  TO CURRENT-SECTION                       
138200******************************************************************        
138300*                                                                *        
138400*    FRAMRÄKNAT PERIODBEHOV PER VECKA JUSTERAS MED               *        
138500*    FÖLJANDE FAKTORER:                                          *        
138600*                                                                *        
138700*          - PERIODBEHOVSJUST 1 OCH 2  (KVPB-JUST)               *        
138800*          - TREND                     (KVTREND)                 *        
138900*          - CENTRAL JUSTERINGSFAKTOR  (REPBJUST)                *        
139000*          - SÄSONG-INDEX              (RESEASON)                *        
139100*                                                                *        
139200******************************************************************        
139300                                                                          
139400     MOVE JUST-TIPBJUST (1)       TO TMP1-YYWW                            
139500     MOVE W-BER-DATUM             TO TMP2-YYWW                            
139600             MOVE '85631'         TO FELTEXT2-STR                         
139700     PERFORM WY2000P3                                                     
139800                                                                          
139900     IF  TMP1-YYWW         NOT > TMP2-YYWW   AND                          
140000         TMP1-YYWW         > ZERO                                         
140100         MOVE W-KVPB-JUST1  TO BER-BEHOV (BER-IY, BER-IX)                 
140200     END-IF                                                               
140300                                                                          
140400     MOVE JUST-TIPBJUST (2)       TO TMP1-YYWW                            
140500     MOVE W-BER-DATUM             TO TMP2-YYWW                            
140600             MOVE '86031'         TO FELTEXT2-STR                         
140700     PERFORM WY2000P3                                                     
140800                                                                          
140900     IF  TMP1-YYWW         NOT > TMP2-YYWW   AND                          
141000         TMP1-YYWW         > ZERO                                         
141100         MOVE W-KVPB-JUST2  TO BER-BEHOV (BER-IY, BER-IX)                 
141200     END-IF                                                               
141300                                                                          
141400     MOVE JUST-TIPBJUST (1)       TO TMP1-YYWW                            
141500     MOVE JUST-TIPBJUST (2)       TO TMP2-YYWW                            
141600     MOVE W-BER-DATUM             TO TMP3-YYWW                            
141700     PERFORM WY2000Q3                                                     
141800                                                                          
141900     IF  TMP1-YYWW         NOT > TMP3-YYWW   AND                          
142000         TMP2-YYWW         NOT > TMP3-YYWW   AND                          
142100         TMP1-YYWW         > ZERO             AND                         
142200         TMP2-YYWW         > ZERO                                         
142300         MOVE JUST-TIPBJUST (1)   TO TMP1-YYWW                            
142400         MOVE JUST-TIPBJUST (2)   TO TMP2-YYWW                            
142500             MOVE '87210'         TO FELTEXT2-STR                         
142600         PERFORM WY2000P3                                                 
142700         IF  TMP1-YYWW < TMP2-YYWW                                        
142800             MOVE W-KVPB-JUST2  TO BER-BEHOV (BER-IY, BER-IX)             
142900         ELSE                                                             
143000             MOVE W-KVPB-JUST1  TO BER-BEHOV (BER-IY, BER-IX)             
143100         END-IF                                                           
143200     END-IF                                                               
143300                                                                          
143400     MOVE JUST-TIPBJUST-CENTR    TO TMP1-YYWW                             
143500     MOVE W-BER-DATUM            TO TMP2-YYWW                             
143600             MOVE '88710'         TO FELTEXT2-STR                         
143700     PERFORM WY2000P3                                                     
143800     IF  TMP1-YYWW <= TMP2-YYWW AND                                       
143900         JUST-TIPBJUST-CENTR      > ZERO                                  
144000         MULTIPLY JUST-REPBJUST   BY BER-BEHOV (BER-IY, BER-IX)           
144100     END-IF                                                               
144200                                                                          
144300     MOVE 1 TO IX                                                         
144400     MOVE W-BER-DATUM TO W-DATUM                                          
144500     PERFORM UNTIL IX > PERIOD-ANTAL                                      
144600         IF  W-DATUM-VV NOT < PER-START-VV (IX)                           
144700         AND W-DATUM-VV NOT > PER-SLUT-VV  (IX)                           
144800             MULTIPLY JUST-RESEASON (IX)                                  
144900                              BY BER-BEHOV (BER-IY, BER-IX)               
145000         END-IF                                                           
145100         ADD 1 TO IX                                                      
145200     END-PERFORM                                                          
145300     .                                                                    
145400     EJECT                                                                
145500 BAC-JUSTERA-VA1 SECTION.                                                 
145600     MOVE 'BAC-JUSTERA-VA1 '  TO CURRENT-SECTION                          
145700******************************************************************        
145800*    SEPARATBEHOVEN JUSTERAS I INNEVARANDE VECKA                          
145900*                                                                         
146000*--  OBS! KINA HAR 6 DAGARS ARBETSVECKA                                   
146100*    IF NDC-CN OR LDC-CN OR NDC-JP OR SDC-GB OR NDC-IN                    
146200*      MOVE JA TO 6ARBDAG-SW                                              
146300*    END-IF                                                               
146400*****************************************************************         
146500                                                                          
146600*--  OBS! CDC HAR 5 DAGARS ARBETSVECKA                                    
146700     MOVE NEJ  TO 6ARBDAG-SW                                              
146800                                                                          
146900     IF LINK-TIAAVV-AKTUELL = LINK-TIBEHOV-START                          
147000         IF LINK-TID-AKTUELL > 0 AND < 6                                  
147100             ACCEPT W-TIME FROM TIME                                      
147200             COMPUTE W-DAGAR-KVAR = 5 - LINK-TID-AKTUELL                  
147300             IF W-TIME(1:2) > 05  AND                                     
147400                W-TIME(1:2) < 17                                          
147500                 ADD +1 TO W-DAGAR-KVAR                                   
147600             END-IF                                                       
147700             IF 6ARBDAG-SW = JA                                           
147800               COMPUTE W-VECKODEL = W-DAGAR-KVAR / 6                      
147900             ELSE                                                         
148000               COMPUTE W-VECKODEL = W-DAGAR-KVAR / 5                      
148100             END-IF                                                       
148200             IF W-VECKODEL < 0                                            
148300                 MOVE +0 TO W-VECKODEL                                    
148400             END-IF                                                       
148500             COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =                 
148600             W-VECKODEL * BER-BEHOV (BER-IY, BER-IX)                      
148700         END-IF                                                           
148800     END-IF                                                               
148900     .                                                                    
149000     EJECT                                                                
149100 C-BERAKNA-SATSBEHOV SECTION.                                             
149200     MOVE 'C-BERAKNA-SATSBEHOV '  TO CURRENT-SECTION                      
149300******************************************************************        
149400*                                                                *        
149500*    SATSBEHOV PER VECKA LÄSES                                   *        
149600*        - FÖRE BEGÄRD STARTVECKA: SUMMERING TILL                *        
149700*          LINK-KVBEHOV-DESSUTOM                                 *        
149800*        - INOM BEGÄRT TIDSINTERVALL: FLYTTA TILL BER-TAB        *        
149900*                                                                *        
150000******************************************************************        
150100                                                                          
150200     PERFORM IMS-GET-SATSBEHOV-FIRST                                      
150300                                                                          
150400     PERFORM UNTIL SEGMENT-SAKNAS                                         
150500       MOVE SATS-TIBEHOV-SATS   TO TMP1-YYWW                              
150600       MOVE W-BER-SLUT-DATUM    TO TMP2-YYWW                              
150700             MOVE '94510'         TO FELTEXT2-STR                         
150800       PERFORM WY2000P3                                                   
150900       IF TMP1-YYWW <= TMP2-YYWW                                          
151000                                                                          
151100          MOVE SATS-TIBEHOV-SATS   TO TMP1-YYWW                           
151200          MOVE W-TIFINLV-AAVV      TO TMP2-YYWW                           
151300          MOVE LINK-TIBEHOV-START  TO TMP3-YYWW                           
151400          PERFORM WY2000Q3                                                
151500          IF TMP1-YYWW < TMP3-YYWW                                        
151600          OR (TMP1-YYWW < TMP2-YYWW AND TMP2-YYWW < TMP3-YYWW)            
151700                                                                          
151800              ADD SATS-KVBEHOV-TOTSATS                                    
151900                                TO LINK-KVBEHOV-DESSUTOM                  
152000          ELSE                                                            
152100              MOVE SATS-TIBEHOV-SATS TO W-DATUM                           
152200              PERFORM S04-BERAKNA-BERTAB-INDEX                            
152300              ADD SATS-KVBEHOV-TOTSATS                                    
152400                                TO RES-BEHOV (BER-IY, BER-IX)             
152500          END-IF                                                          
152600       END-IF                                                             
152700                                                                          
152800       PERFORM IMS-GET-SATSBEHOV-NEXT                                     
152900     END-PERFORM                                                          
153000     .                                                                    
153100     EJECT                                                                
153200 D-BERAKNA-DIVERSEORDER-BEHOV   SECTION.                                  
153300     MOVE 'D-BERAKNA-DIVERSEORDER-BEHOV' TO CURRENT-SECTION               
153400******************************************************************        
153500*                                                                *        
153600*    BERÄKNING AV DIVERSEORDERBEHOV AV ARTIKEL                   *        
153700*                                                                *        
153800******************************************************************        
153900                                                                          
154000     MOVE NEJ TO SW-TPOBEHOV-LAEST                                        
154100     PERFORM IMS-GU-ARTM01                                                
154200                                                                          
154300     IF  SEGMENT-FINNS                                                    
154400     AND ARTM-ART-SUTPO-TOT > ZERO                                        
154500                                                                          
154600       PERFORM IMS-GNP-ARTM11                                             
154700                                                                          
154800       IF SEGMENT-FINNS                                                   
154900           PERFORM DAA-DIVERSEORDER-BERAKNING                             
155000           MOVE JA TO SW-TPOBEHOV-LAEST                                   
155100       END-IF                                                             
155200     END-IF                                                               
155300     .                                                                    
155400     EJECT                                                                
155500 DAA-DIVERSEORDER-BERAKNING SECTION.                                      
155600     MOVE 'DAA-DIVERSEORDER-BERAKNING'  TO CURRENT-SECTION                
155700******************************************************************        
155800*                                                                *        
155900*    DIVERSEORDER SALDO PER VECKA LÄSES                          *        
156000*        - FÖRE BEGÄRD STARTVECKA: SUMMERING TILL                *        
156100*          LINK-KVBEHOV-DESSUTOM                                 *        
156200*        - INOM BEGÄRT TIDSINTERVALL: FLYTTA TILL BER-TAB        *        
156300*                                                                *        
156400******************************************************************        
156500     SKIP1                                                                
156600     PERFORM UNTIL SEGMENT-SAKNAS                                         
156700       MOVE ARTM-ANT-DABEHOV (3:4)   TO W-RED-NUM-4                       
156800       MOVE W-RED-NUM-4              TO TMP1-YYWW                         
156900       MOVE W-BER-SLUT-DATUM         TO TMP2-YYWW                         
157000             MOVE '101010'           TO FELTEXT2-STR                      
157100       PERFORM WY2000P3                                                   
157200       IF  TMP1-YYWW <= TMP2-YYWW                                         
157300                                                                          
157400         MOVE ARTM-ANT-DABEHOV (3:4) TO W-RED-NUM-4                       
157500         MOVE W-RED-NUM-4            TO TMP1-YYWW                         
157600         MOVE LINK-TIBEHOV-START     TO TMP2-YYWW                         
157700         PERFORM WY2000P3                                                 
157800         IF  TMP1-YYWW  <= TMP2-YYWW                                      
157900             ADD ARTM-ANT-SUTPO-PB                                        
158000                               TO LINK-KVBEHOV-DESSUTOM                   
158100             ADD ARTM-ANT-SUTPO-EJPB                                      
158200                               TO LINK-KVBEHOV-DESSUTOM                   
158300         ELSE                                                             
158400             MOVE ARTM-ANT-DABEHOV (3:4) TO W-RED-NUM-4                   
158500             MOVE W-RED-NUM-4            TO W-DATUM                       
158600             PERFORM S04-BERAKNA-BERTAB-INDEX                             
158700             ADD ARTM-ANT-SUTPO-PB                                        
158800                                    TO RES-BEHOV (BER-IY, BER-IX)         
158900             ADD ARTM-ANT-SUTPO-EJPB                                      
159000                                    TO RES-BEHOV (BER-IY, BER-IX)         
159100         END-IF                                                           
159200       END-IF                                                             
159300                                                                          
159400       PERFORM IMS-GNP-ARTM11                                             
159500     END-PERFORM                                                          
159600     .                                                                    
159700     EJECT                                                                
159800 E-FLYTTA-RESULT-TILL-LINKAREA SECTION.                                   
159900     MOVE 'E-FLYTTA-RESULT-TILL-LINKAREA'  TO CURRENT-SECTION             
160000     SKIP3                                                                
160100     MOVE 1              TO IX                                            
160200                                                                          
160300*--- FÖR ATT KOMMA RÄTT I TRENDVÄRDEN BEHOVSVECKORNA SÅ BÖRJAR MAN        
160400*--- MED TRENDVÄRDET PÅ INNEVARANDE VECKA / KVBEHOV-DESSUTOM.             
160500*--- RAD 2 PÅ 2128 VISAR VÄRDET FÖR TRENDVECKA 2 OSV.                     
160600     IF LINK-TIAAVV-AKTUELL = LINK-TIBEHOV-START                          
160700       MOVE 1            TO INDX-T                                        
160800     ELSE                                                                 
160900       MOVE 2            TO INDX-T                                        
161000     END-IF                                                               
161100                                                                          
161200     MOVE +1             TO RES-IY                                        
161300     MOVE W-BER-START-VV TO RES-IX                                        
161400     MOVE ZERO TO W-KVBEHOV-SUMMA                                         
161500     MOVE CLAG-KVVECKOR-TREND    TO MAX-KVVECKOR-TREND                    
161600     SKIP1                                                                
161700     PERFORM UNTIL RES-IY > RES-ANTAL-AA                                  
161800     SKIP1                                                                
161900         PERFORM UNTIL                                                    
162000            (RES-IY = RES-ANTAL-AA AND                                    
162100             RES-IX > RES-ANTAL-VV)                                       
162200         OR (RES-IY < RES-ANTAL-AA AND                                    
162300             RES-IX > 52)                                                 
162400     SKIP1                                                                
162500                  MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                    
162600                  ADD  RES-BEHOV (RES-IY, RES-IX)                         
162700                              TO LINK-KVBEHOV-VECKA (IX) ROUNDED          
162800                  IF (LINK-KDBEHOV = SEP-SATS-TPO-LEV-SDC-NDC OR          
162900                      LINK-KDBEHOV = SEP-SATS-TPO-SDC-NDC)                
163000                     AND TREND-SW = JA                                    
163100                     AND CLAG-KVPB-TREND NOT = ZERO                       
163200                     IF INDX-T = 2 AND IX = 1                             
163300                       PERFORM EB-ADDERA-TREND-INNEV-VECKA                
163400                       PERFORM EA-ADDERA-TREND                            
163500                     ELSE                                                 
163600                       PERFORM EA-ADDERA-TREND                            
163700                     END-IF                                               
163800                  END-IF                                                  
163900                  ADD  LINK-KVBEHOV-VECKA (IX)                            
164000                              TO W-KVBEHOV-SUMMA                          
164100                  ADD 1 TO IX                                             
164200                           RES-IX                                         
164300                           INDX-T                                         
164400         END-PERFORM                                                      
164500         ADD 1 TO RES-IY                                                  
164600         MOVE 1 TO RES-IX                                                 
164700     END-PERFORM                                                          
164800     SKIP1                                                                
164900     MOVE ZERO TO LINK-KVBEHOV-SUMMA                                      
165000     ADD W-KVBEHOV-SUMMA TO LINK-KVBEHOV-SUMMA                            
165100     PERFORM UNTIL IX > MAX-BEHOVS-VECKOR                                 
165200         MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                             
165300         ADD 1 TO IX                                                      
165400     END-PERFORM                                                          
165500     .                                                                    
165600     EJECT                                                                
165700 EA-ADDERA-TREND       SECTION.                                           
165800     MOVE 'EA-ADDERA-TREND   '  TO CURRENT-SECTION                        
165900******************************************************************        
166000*                                                                *        
166100*    BERÄKNING AV TRENDBEHOV                                     *        
166200*                                                                *        
166300*    OBS BEHOVET EFTER TREND EJ MINDRE ÄN HÄLFTEN FÖRE           *        
166400******************************************************************        
166500                                                                          
166600        COMPUTE SPAR-KVBEHOV-VECKA ROUNDED =                              
166700           LINK-KVBEHOV-VECKA (IX) / 2                                    
166800        IF INDX-T NOT < MAX-KVVECKOR-TREND                                
166900           MOVE MAX-KVVECKOR-TREND TO INDX-T                              
167000        END-IF                                                            
167100                                                                          
167200        IF WS-DAGENS-DAGNR > 5 OR                                         
167300          (WS-DAGENS-DAGNR = 5 AND WS-DAGENS-TIMME > 17)                  
167400*          VECKOHELG                                                      
167500           COMPUTE WS-KVPB-TREND ROUNDED =                                
167600              INDX-T * CLAG-KVPB-TREND / 4.33                             
167700           END-COMPUTE                                                    
167800           ADD WS-KVPB-TREND  TO                                          
167900                    LINK-KVBEHOV-VECKA (IX)                               
168000        ELSE                                                              
168100*          MITT I VECKAN ELLER TRENDVÄRDE-START I VECKA 2                 
168200           COMPUTE WS-KVPB-TREND ROUNDED =                                
168300              INDX-T * CLAG-KVPB-TREND / 4.33                             
168400           END-COMPUTE                                                    
168500           IF (IX = 1) AND (INDX-T = 1)                                   
168600              IF WS-DAGENS-DAGNR = 2                                      
168700                 COMPUTE WS-KVPB-TREND ROUNDED =                          
168800                         WS-KVPB-TREND * 0.80                             
168900              END-IF                                                      
169000              IF WS-DAGENS-DAGNR = 3                                      
169100                 COMPUTE WS-KVPB-TREND ROUNDED =                          
169200                         WS-KVPB-TREND * 0.60                             
169300              END-IF                                                      
169400              IF WS-DAGENS-DAGNR = 4                                      
169500                 COMPUTE WS-KVPB-TREND ROUNDED =                          
169600                         WS-KVPB-TREND * 0.40                             
169700              END-IF                                                      
169800              IF WS-DAGENS-DAGNR = 5                                      
169900                 COMPUTE WS-KVPB-TREND ROUNDED =                          
170000                         WS-KVPB-TREND * 0.20                             
170100              END-IF                                                      
170200           END-IF                                                         
170300           ADD WS-KVPB-TREND  TO                                          
170400                    LINK-KVBEHOV-VECKA (IX)                               
170500        END-IF                                                            
170600                                                                          
170700        IF LINK-KVBEHOV-VECKA (IX) < ZERO                                 
170800           MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                           
170900        END-IF                                                            
171000                                                                          
171100        IF LINK-KVBEHOV-VECKA (IX) < SPAR-KVBEHOV-VECKA                   
171200           MOVE SPAR-KVBEHOV-VECKA TO LINK-KVBEHOV-VECKA (IX)             
171300        END-IF                                                            
171400     .                                                                    
171500     EJECT                                                                
171600 EB-ADDERA-TREND-INNEV-VECKA  SECTION.                                    
171700     MOVE 'EB-ADDERA-TREND-INNEV-VECKA   '  TO CURRENT-SECTION            
171800******************************************************************        
171900*                                                                *        
172000*    BERÄKNING AV TRENDBEHOV FÖRSTA VECKAN PÅ 2128               *        
172100*                                                                *        
172200*    OBS BEHOVET EFTER TREND EJ MINDRE ÄN HÄLFTEN FÖRE           *        
172300******************************************************************        
172400                                                                          
172500     MOVE 1    TO INDX-T                                                  
172600                                                                          
172700     COMPUTE SPAR-KVBEHOV-DESSUTOM ROUNDED =                              
172800        LINK-KVBEHOV-DESSUTOM  / 2                                        
172900     IF INDX-T NOT < MAX-KVVECKOR-TREND                                   
173000        MOVE MAX-KVVECKOR-TREND TO INDX-T                                 
173100     END-IF                                                               
173200                                                                          
173300     IF WS-DAGENS-DAGNR > 5 OR                                            
173400       (WS-DAGENS-DAGNR = 5 AND WS-DAGENS-TIMME > 17)                     
173500*----   VECKOHELG                                                         
173600        COMPUTE WS-KVPB-TREND ROUNDED =                                   
173700           INDX-T * CLAG-KVPB-TREND / 4.33                                
173800        END-COMPUTE                                                       
173900        ADD WS-KVPB-TREND  TO                                             
174000                 LINK-KVBEHOV-DESSUTOM                                    
174100     ELSE                                                                 
174200*----   MITT I VECKAN                                                     
174300        COMPUTE WS-KVPB-TREND ROUNDED =                                   
174400           INDX-T * CLAG-KVPB-TREND / 4.33                                
174500        END-COMPUTE                                                       
174600        IF IX = 1                                                         
174700           IF WS-DAGENS-DAGNR = 2                                         
174800              COMPUTE WS-KVPB-TREND ROUNDED =                             
174900                      WS-KVPB-TREND * 0.80                                
175000           END-IF                                                         
175100           IF WS-DAGENS-DAGNR = 3                                         
175200              COMPUTE WS-KVPB-TREND ROUNDED =                             
175300                      WS-KVPB-TREND * 0.60                                
175400           END-IF                                                         
175500           IF WS-DAGENS-DAGNR = 4                                         
175600              COMPUTE WS-KVPB-TREND ROUNDED =                             
175700                      WS-KVPB-TREND * 0.40                                
175800           END-IF                                                         
175900           IF WS-DAGENS-DAGNR = 5                                         
176000              COMPUTE WS-KVPB-TREND ROUNDED =                             
176100                      WS-KVPB-TREND * 0.20                                
176200           END-IF                                                         
176300        END-IF                                                            
176400        ADD WS-KVPB-TREND  TO                                             
176500                 LINK-KVBEHOV-DESSUTOM                                    
176600     END-IF                                                               
176700                                                                          
176800     IF LINK-KVBEHOV-DESSUTOM < ZERO                                      
176900        MOVE ZERO TO LINK-KVBEHOV-DESSUTOM                                
177000     END-IF                                                               
177100                                                                          
177200     IF LINK-KVBEHOV-DESSUTOM  < SPAR-KVBEHOV-DESSUTOM                    
177300        MOVE SPAR-KVBEHOV-DESSUTOM   TO LINK-KVBEHOV-DESSUTOM             
177400     END-IF                                                               
177500                                                                          
177600*---- SÄTT TILLBAKA INDX-T TILL 2 IGEN FÖR ANDRA VECKANS BEHOV.           
177700     MOVE 2     TO INDX-T                                                 
177800                                                                          
177900     .                                                                    
178000     EJECT                                                                
178100 F-BERAKNA-TPO-BEHOV   SECTION.                                           
178200     MOVE 'F-BERAKNA-TPO-BEHOV ' TO CURRENT-SECTION                       
178300******************************************************************        
178400*                                                                *        
178500*    BERÄKNING AV TPO-BEHOV AV ARTIKEL                           *        
178600*                                                                *        
178700******************************************************************        
178800                                                                          
178900     MOVE CLAG-KDGK        TO W-KDGK                                      
179000                                                                          
179100     MOVE NEJ TO SW-TPOBEHOV-LAEST                                        
179200     PERFORM IMS-GU-ARTM01                                                
179300                                                                          
179400     IF  SEGMENT-FINNS                                                    
179500     AND ARTM-ART-SUTPO-TOT > ZERO                                        
179600                                                                          
179700       PERFORM IMS-GNP-ARTM11                                             
179800                                                                          
179900       IF SEGMENT-FINNS                                                   
180000           PERFORM FA-TPO-BERAKNING                                       
180100           MOVE JA               TO SW-TPOBEHOV-LAEST                     
180200       END-IF                                                             
180300     END-IF                                                               
180400     .                                                                    
180500     EJECT                                                                
180600 FA-TPO-BERAKNING SECTION.                                                
180700     MOVE 'FA-TPO-BERAKNING '  TO CURRENT-SECTION                         
180800******************************************************************        
180900*                                                                *        
181000*    TPO-SALDO PER VECKA LÄSES                                   *        
181100*        - TPO-BEHOV TIDIGARELÄGGS 1 ELLER 2 VECKOR              *        
181200*        - FÖRE BEGÄRD STARTVECKA: SUMMERING TILL                *        
181300*          LINK-KVBEHOV-DESSUTOM                                 *        
181400*        - INOM BEGÄRT TIDSINTERVALL: FLYTTA TILL BER-TAB        *        
181500*                                                                *        
181600******************************************************************        
181700                                                                          
181800     PERFORM UNTIL SEGMENT-SAKNAS                                         
181900                                                                          
182000       EVALUATE W-KDGK                                                    
182100          WHEN +1                                                         
182200           MOVE -1     TO W-ANTVKA-FORSKJUT                               
182300          WHEN +2                                                         
182400            MOVE -2    TO W-ANTVKA-FORSKJUT                               
182500          WHEN +3                                                         
182600            MOVE -1    TO W-ANTVKA-FORSKJUT                               
182700          WHEN OTHER                                                      
182800            MOVE -1    TO W-ANTVKA-FORSKJUT                               
182900       END-EVALUATE                                                       
183000                                                                          
183100       MOVE ARTM-ANT-DABEHOV (3:4) TO W-RED-NUM-4                         
183200       MOVE W-RED-NUM-4            TO W009VADDW-AAVV                      
183300       MOVE W-ANTVKA-FORSKJUT TO W009VADDW-ANTAL                          
183400       CALL W009VADD USING W009VADDW-AAVV W009VADDW-ANTAL                 
183500       MOVE W009VADDW-AAVV    TO W-TIBEHOV-TPO                            
183600       MOVE W-TIBEHOV-TPO      TO TMP1-YYWW                               
183700       MOVE W-BER-SLUT-DATUM   TO TMP2-YYWW                               
183800             MOVE '112810'        TO FELTEXT2-STR                         
183900       PERFORM WY2000P3                                                   
184000       IF TMP1-YYWW <= TMP2-YYWW                                          
184100         MOVE W-TIBEHOV-TPO      TO TMP1-YYWW                             
184200         MOVE LINK-TIBEHOV-START TO TMP2-YYWW                             
184300         PERFORM WY2000P3                                                 
184400         IF TMP1-YYWW   <= TMP2-YYWW                                      
184500             ADD ARTM-ANT-SUTPO-PB                                        
184600                               TO LINK-KVBEHOV-DESSUTOM                   
184700             ADD ARTM-ANT-SUTPO-EJPB                                      
184800                               TO LINK-KVBEHOV-DESSUTOM                   
184900         ELSE                                                             
185000             MOVE W-TIBEHOV-TPO TO W-DATUM                                
185100             PERFORM S04-BERAKNA-BERTAB-INDEX                             
185200             ADD ARTM-ANT-SUTPO-PB                                        
185300                                TO RES-BEHOV (BER-IY, BER-IX)             
185400             ADD ARTM-ANT-SUTPO-EJPB                                      
185500                                TO RES-BEHOV (BER-IY, BER-IX)             
185600         END-IF                                                           
185700                                                                          
185800         IF LINK-TIBEHOV-FIRST  =  ZERO                                   
185900           MOVE W-TIBEHOV-TPO   TO LINK-TIBEHOV-FIRST                     
186000         ELSE                                                             
186100           MOVE W-TIBEHOV-TPO        TO TMP1-YYWW                         
186200           MOVE LINK-TIBEHOV-FIRST   TO TMP2-YYWW                         
186300             MOVE '115010'        TO FELTEXT2-STR                         
186400           PERFORM WY2000P3                                               
186500           IF TMP1-YYWW < TMP2-YYWW                                       
186600             MOVE W-TIBEHOV-TPO TO LINK-TIBEHOV-FIRST                     
186700           END-IF                                                         
186800         END-IF                                                           
186900                                                                          
187000       END-IF                                                             
187100       PERFORM IMS-GNP-ARTM11                                             
187200     END-PERFORM                                                          
187300     .                                                                    
187400     EJECT                                                                
187500 G-BERAKNA-SDCBEHOV   SECTION.                                            
187600     MOVE 'G-BERAKNA-SDCBEHOV '  TO CURRENT-SECTION                       
187700******************************************************************        
187800*                                                                *        
187900*    BERÄKNING AV SDC'ERNAS BEHOV AV EN ARTIKEL                  *        
188000*    KINA SKALL EXKLUDERAS FRÅN SDC'RNA.INGÅR I NDC.             *        
188100*                                                                *        
188200******************************************************************        
188300                                                                          
188310     MOVE NEJ                    TO WS-ERS-FINNS-K611                     
188320                                                                          
188400     ACCEPT W-TIME          FROM TIME                                     
188500                                                                          
188600     IF CLAG-FLREFILL = JA   AND CLAG-REDIRLEV < 1.00                     
188700       IF ART-FLERS = JA                                                  
188800          MOVE ART-IDARTNR       TO W-IDARTNR-D7-MIN                      
188900                                    W-IDARTNR-D7-MAX                      
189000          PERFORM IMS-GU-WDD7A1                                           
189100          IF SEGMENT-FINNS                                                
189200             MOVE ERS-IDARTNR    TO W-IDARTNR-ERS                         
189300             PERFORM IMS-GU-WDK601-ERS                                    
189400             IF SEGMENT-FINNS                                             
189500                PERFORM IMS-GNP-WDK611-ERS                                
189600                MOVE JA          TO WS-ERS-FINNS-K611                     
189700             END-IF                                                       
189800                                                                          
189900*----   VI BEHÖVER ÅTERSTÄLLA POSITIONEN I BASEN                          
190000             MOVE LINK-IDARTNR   TO W-IDARTNR                             
190100             PERFORM IMS-GU-WDK601                                        
190200          END-IF                                                          
190300       END-IF                                                             
190400***                                                                       
190500***                                                                       
190600       PERFORM IMS-GU-WDK701                                              
190700       IF SEGMENT-FINNS                                                   
190800         PERFORM GC-LAES-WDK711                                           
190900                                                                          
191000         PERFORM UNTIL SEGMENT-SAKNAS                                     
191100           PERFORM S20-NOLLSTALL-W-SDC-BEHOV-REF                          
191200                                                                          
191300           PERFORM S77-HITTA-K712-LAND                                    
191400           MOVE K712-DAPUBL(K712-IX) TO WS-DAPUBL                         
191500           MOVE SLAG-FLFLYG          TO WS-FLFLYG                         
191600           PERFORM S80-CALC-LT-ADJ-PUBWK                                  
191700                                                                          
191800           IF  WS-DAPUBL > ZERO                                           
191900           AND WS-DAPUBL > WS-DAGENS-DATUM                                
192000             MOVE WS-LT-WEEKS-DAYS TO DAG-KVKALDAG                        
192100             MOVE WS-DAPUBL (3:6)                                         
192200                             TO DAG-TIAAMMDD-TOM                          
192300             MOVE 003        TO DAG-KDCALL                                
192400             CALL WDAGKONV USING DAG-KDCALL                               
192500                                 DAG-DATUM-AREA                           
192600                                 DAG-KDSVAR                               
192700             IF DAG-KDSVAR = SPACE                                        
192800               CONTINUE                                                   
192900             ELSE                                                         
193000               STRING 'FEL FRÅN WDAGKONV I W2222200 '                     
193100               'G- SECTION (K712-DAPUBL)' DELIMITED BY SIZE               
193200                         INTO         FELTEXT-STR                         
193300                DISPLAY FELTEXT                                           
193400                CALL ABEND USING RKOD-ABEND-UTAN-DUMP                     
193500             END-IF                                                       
193600                                                                          
193700             MOVE DAG-TIAAMMDD-FOM                                        
193800                              TO DAT-I-TIDATUM                            
193900             MOVE 'AAMMDD'    TO DAT-KDDATFORM                            
194000                                                                          
194100             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
194200                                 DAT-O-TIDATUM DAT-KDSVAR                 
194300                                                                          
194400             IF DAT-KDSVAR-OK                                             
194500                MOVE DAT-TIAAVV-GRP                                       
194600                              TO W-TIAAVV                                 
194700                MOVE W-TIAAVV                                             
194800                              TO W-DATUM-FORSTA-SDC                       
194900             ELSE                                                         
195000                 STRING ' FEL FRÅN WDATKONV I W2222200'                   
195100                        '(G-, K712-DAPUBL)'                               
195200                 DELIMITED BY SIZE INTO FELTEXT-STR                       
195300                 DISPLAY FELTEXT                                          
195400                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
195500             END-IF                                                       
195600           ELSE                                                           
195700             MOVE W-TIFINLV-AAVV-MINUS-LT                                 
195800                              TO W-DATUM-FORSTA-SDC                       
195900           END-IF                                                         
196000                                                                          
196100           MOVE 'AAVV  '      TO DAT-KDDATFORM                            
196200           MOVE LINK-TIAAVV-AKTUELL                                       
196300                              TO DAT-I-TIDATUM                            
196400                                 W-BINNDAY-AAVV                           
196500                                                                          
196600           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
196700                           DAT-O-TIDATUM DAT-KDSVAR                       
196800                                                                          
196900           IF DAT-KDSVAR-OK                                               
197000             MOVE DAT-TIAARP(3:2) TO W-PER                                
197100             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
197200           ELSE                                                           
197300               STRING ' FEL FRÅN WDATKONV I W2222200'                     
197400                      '(G-, LINK-TIAAVV-AKTUELL)'                         
197500               DELIMITED BY SIZE INTO FELTEXT-STR                         
197600               DISPLAY FELTEXT                                            
197700               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
197800           END-IF                                                         
197900                                                                          
198000*** FINNS F/C IN THE FUTURE, KVPB-JUST FÖR AKTUELL VECKA.                 
198100           PERFORM S23-GET-WEEK-DEMAND-XDC                                
198200           MOVE +1                TO INDX-L                               
198300                                                                          
198400           COMPUTE W-SDC-TILLGANG = SLAG-KVLS +                           
198500                                    SLAG-KVAKS-PAV +                      
198600                                    SLAG-KVAKS-SDC +                      
198700                                    SLAG-KVBEART -                        
198800                                    SLAG-KVOKS-DAG -                      
198900                                    SLAG-KVOKS-BULK -                     
199000                                    SLAG-KVROS-BULK -                     
199100                                    SLAG-KVROS-DAG                        
199200                                                                          
199300           COMPUTE W-KVBEHOV-30V ROUNDED =                                
199400           ((((SLAG-KVPB-REF + SLAG-KVPBREOI) * 12) / 52) * 30)           
199500                                                                          
199600           PERFORM S15-JUSTERA-XDC-TILLGANG                               
199700           ADD W-XDC-TILLGANG-ERS TO W-SDC-TILLGANG                       
199800                                                                          
199900           IF (LINK-TID-AKTUELL = 5  AND                                  
200000               W-TIME(1:2) >      17) OR                                  
200100               LINK-TID-AKTUELL > 5                                       
200200               CONTINUE                                                   
200300           ELSE                                                           
200400             PERFORM GA-BERAKNA-INNEV-VECKA-SDC                           
200500             ADD W-SDC-KVBEHOV-DESSUTOM                                   
200600                                      TO LINK-KVBEHOV-DESSUTOM            
200700           END-IF                                                         
200800                                                                          
200900           PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                           
201000           PERFORM GB-SDCBEHOV                                            
201100           PERFORM S03-ADD-TILL-RESULTAT-TAB                              
201200                                                                          
201300           PERFORM GC-LAES-WDK711                                         
201400                                                                          
201500         END-PERFORM                                                      
201600       END-IF                                                             
201700     END-IF                                                               
201800     .                                                                    
201900     EJECT                                                                
202000 GA-BERAKNA-INNEV-VECKA-SDC SECTION.                                      
202100     MOVE 'GA-BERAKNA-INNEV-VECKA-SDC ' TO CURRENT-SECTION                
202200******************************************************************        
202300*                                                                *        
202400*    BERÄKNING AV SDC'ERNAS BEHOV AV EN ARTIKEL I INNEVARANDE    *        
202500*    VECKA.                                                      *        
202600*                                                                *        
202700*    FÖR ARTIKLAR SOM REFILLAS TILL CDC FRÅN KINA SÅ STOPPAS     *        
202800*    INLEVERANSER 2 VECKOR FÖRE ERSÄTTNING KOD X3.               *        
202900*    FÖR KDERS=X1,X2 OCH X7 LÄGGS LT DC71->11 PÅ.EJ FÖR X3.      *        
203000*                                                                *        
203100******************************************************************        
203200                                                                          
203300     MOVE W-DATUM-FORSTA-SDC    TO TMP1-YYWW                              
203400     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
203500             MOVE '121110'      TO FELTEXT2-STR                           
203600     PERFORM WY2000P3                                                     
203700     IF TMP1-YYWW <= TMP2-YYWW                                            
203800*AD    IF CLAG-TISTOREF > 0                                               
203900*AD      MOVE CLAG-TISTOREF     TO WS-TISTOREF-AAVVD                      
204000*AD    ELSE                                                               
204100*AD      MOVE 99999             TO WS-TISTOREF-AAVVD                      
204200*AD    END-IF                                                             
204300*AD    MOVE WS-TISTOREF-AAVVD(1:4) TO WS-TISTOREF-AAVV                    
204400*AD                                   WS-TISTOREF-AAVV-3                  
204500*AD    IF CLAG-IDDC-REF NOT = SPACE                                       
204600*AD      IF CLAG-TISTOREF > 0                                             
204700*AD        PERFORM S65-KOLLA-ERS-DAT-CDC                                  
204800*AD      END-IF                                                           
204900*AD    ELSE                                                               
205000***DETTA FÖR ATT MAN SKALL STOPPA INLEVERANSER 3 VECKOR FÖRE              
205100***ERSÄTTNING                                                             
205200*AD      IF CLAG-TISTOREF > 0                                             
205300*AD        MOVE -3              TO W-ANTAL-VECKOR                         
205400*AD        CALL W009VADD USING  WS-TISTOREF-AAVV-3 W-ANTAL-VECKOR         
205500*AD      END-IF                                                           
205600*AD    END-IF                                                             
205700*AD    IF LINK-TIAAVV-AKTUELL <= WS-TISTOREF-AAVV-3                       
205800*                                                                         
205900*****   WEEKLY DEMAND ALREADY ADJUSTED FOR NUMBER OF DAYS                 
206000*****   IN CURRENT WEEK                                                   
206100*                                                                         
206200         COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED                            
206300                                  = (UTUP-KVBEHOV-V (INDX-L) * 1)         
206400                                                                          
206500         COMPUTE W-SDC-KVAR-EFT-VECKA ROUNDED =                           
206600                          W-SDC-TILLGANG - W-KVBEHOV-INNEV-VECKA          
206700                                                                          
206800         IF W-JUST-PB-FINNS = NEJ                                         
206900           IF W-SDC-KVAR-EFT-VECKA    < SLAG-KVREFPKT                     
207000              COMPUTE W-SDC-DIFF      = SLAG-KVREFPKT -                   
207100                                                  W-SDC-TILLGANG          
207200                                                                          
207300***---INNEVARANDE VECKOBEHOV SKALL EJ TAS MED I REFILLKVANTEN             
207400              COMPUTE W-DC-REFILL-KVANT =                                 
207500                                      W-SDC-DIFF + SLAG-KVREFBER          
207600                                                                          
207700              PERFORM S22-ADJUST-LOW-DEMAND                               
207800              IF DEMAND-ADJUST                                            
207900                 MOVE +1             TO W-DC-REFILL-KVANT                 
208000              ELSE                                                        
208100                 PERFORM S08SDC-JUSTERA-REFILLKVANT                       
208200                 PERFORM S06-SATT-QX-PROCENT-BRYTNING                     
208300                 PERFORM S05-BERAKNA-Q-KVANT                              
208400              END-IF                                                      
208500              MOVE W-DC-REFILL-KVANT TO W-SDC-KVBEHOV-DESSUTOM            
208600           ELSE                                                           
208700              MOVE ZERO              TO W-SDC-KVBEHOV-DESSUTOM            
208800           END-IF                                                         
208900         ELSE                                                             
209000*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
209100           INITIALIZE REFL-W271REFL                                       
209200                                                                          
209300           MOVE SLAG-IDDC     TO REFL-IDDC                                
209400           MOVE SLAG-IDDC-REF TO REFL-IDDC-REF                            
209500           MOVE SLAG-IDREFTAB TO REFL-IDREFTAB                            
209600           MOVE SLAG-FLWILSON TO REFL-FLWILSON                            
209700                                                                          
209800           PERFORM S30-W271REFL-HAEMTA-PRARTBES                           
209900           MOVE WS-PRARTBES   TO REFL-PRARTBES                            
210000                                                                          
210100           MOVE SLAG-IDLEVNR  TO REFL-IN-IDLEVNR-DC                       
210200*---- GÄLLER EJ FÖR DCS-SDC OR DCS-NDC-CN, BARA NDC-NA/PF                 
210300           MOVE ZERO          TO REFL-NDC-KVDAGAR-TBT-DC                  
210400                                                                          
210500           MOVE SLAG-FLFLYG   TO REFL-FLFLYG                              
210600           MOVE W-BINNDAY     TO REFL-BINNDAY-TIAAMMDD                    
210700           MOVE LINK-IDARTNR  TO REFL-IDARTNR                             
210800           MOVE ZERO          TO REFL-KVREFPKT                            
210900                                 REFL-KVREFBER                            
211000                                                                          
211100           MOVE SLAG-TIREFPAF TO TMP1-YYMMDD                              
211200           MOVE W-BINNDAY     TO TMP2-YYMMDD                              
211300*                                                                         
211400*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
211500*                                                                         
211600           PERFORM WY2000P1                                               
211700           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
211800*                                                                         
211900*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
212000*                                                                         
212100             MOVE SLAG-KVREFBER TO REFL-IN-KVREFBER                       
212200           ELSE                                                           
212300             MOVE +0          TO REFL-IN-KVREFBER                         
212400           END-IF                                                         
212500                                                                          
212600           MOVE SLAG-TIREFPKT TO TMP1-YYMMDD                              
212700           MOVE W-BINNDAY     TO TMP2-YYMMDD                              
212800*                                                                         
212900*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
213000*                                                                         
213100           PERFORM WY2000P1                                               
213200           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
213300*                                                                         
213400*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
213500*                                                                         
213600             MOVE SLAG-KVREFPKT TO REFL-IN-KVREFPKT                       
213700           ELSE                                                           
213800             MOVE +0          TO REFL-IN-KVREFPKT                         
213900           END-IF                                                         
214000*                                                                         
214100           MOVE UTUP-LT-BEHOV-V (INDX-L)                                  
214200                              TO REFL-IN-LEADTID-BEHOV                    
214300                                                                          
214400           CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                
214500                                             REFL-WDB6-PCB                
214600                                             REFL-WDK7-PCB                
214700                                             UTIL-WDK6-PCB                
214800                                             UTIL-WDK7-PCB                
214900                                             UTIL-WDB6-PCB                
215000                                                                          
215100***----   REFL-KDSVAR = J ? / W271REFL LÄGGER INGET HÄR.                  
215200                                                                          
215300           IF W-SDC-KVAR-EFT-VECKA   < REFL-KVREFPKT                      
215400              COMPUTE W-SDC-DIFF     = REFL-KVREFPKT -                    
215500                                                  W-SDC-TILLGANG          
215600                                                                          
215700                                                                          
215800***---INNEVARANDE VECKOBEHOV SKALL EJ TAS MED I REFILLKVANTEN             
215900              COMPUTE W-DC-REFILL-KVANT =                                 
216000                      W-SDC-DIFF + REFL-KVREFBER                          
216100                                                                          
216200              PERFORM S22-ADJUST-LOW-DEMAND                               
216300              IF DEMAND-ADJUST                                            
216400                 MOVE +1            TO W-DC-REFILL-KVANT                  
216500              ELSE                                                        
216600                 IF W-JUST-PB-FINNS  = NEJ                                
216700                    PERFORM S08SDC-JUSTERA-REFILLKVANT                    
216800                 END-IF                                                   
216900                                                                          
217000                 PERFORM S06-SATT-QX-PROCENT-BRYTNING                     
217100                 PERFORM S05-BERAKNA-Q-KVANT                              
217200              END-IF                                                      
217300                                                                          
217400              MOVE W-DC-REFILL-KVANT                                      
217500                                    TO W-SDC-KVBEHOV-DESSUTOM             
217600           ELSE                                                           
217700              MOVE ZERO             TO W-SDC-KVBEHOV-DESSUTOM             
217800           END-IF                                                         
217900         END-IF                                                           
218000*AD    ELSE                                                               
218100*AD       MOVE ZERO                 TO W-SDC-KVBEHOV-DESSUTOM             
218200*AD    END-IF                                                             
218300     ELSE                                                                 
218400        MOVE ZERO                   TO W-SDC-KVBEHOV-DESSUTOM             
218500     END-IF                                                               
218600     .                                                                    
218700     EJECT                                                                
218800 GB-SDCBEHOV            SECTION.                                          
218900     MOVE 'GB-SDCBEHOV    '  TO CURRENT-SECTION                           
219000******************************************************************        
219100*                                                                *        
219200*    BERÄKNING AV SDC'ERNAS BEHOV AV EN ARTIKEL I KOMMANDE       *        
219300*    VECKOR.                                                     *        
219400*                                                                *        
219500******************************************************************        
219600                                                                          
219700     MOVE ZERO                  TO W-SDC-ACC-KVBEHOV                      
219800     ADD W-SDC-KVBEHOV-DESSUTOM TO W-SDC-TILLGANG                         
219900     ADD W-KVBEHOV-INNEV-VECKA  TO W-SDC-ACC-KVBEHOV                      
220000                                                                          
220100     MOVE +1                    TO BER-IY                                 
220200     MOVE W-BER-START-VV        TO BER-IX                                 
220300     MOVE LINK-TIBEHOV-START    TO W-BER-DATUM                            
220400                                   W-AAVV                                 
220500     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
220600     MOVE W-AAVV                TO DAT-I-TIDATUM                          
220700                                   W-BINNDAY-AAVV                         
220800                                                                          
220900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
221000                         DAT-O-TIDATUM DAT-KDSVAR                         
221100                                                                          
221200     IF DAT-KDSVAR-OK                                                     
221300       MOVE DAT-TIAARP(3:2)     TO W-PER                                  
221400       MOVE DAT-TIAAMMDD        TO W-BINNDAY                              
221500     ELSE                                                                 
221600         STRING ' FEL FRÅN WDATKONV I W2222200 '                          
221700                '(GB-, LINK-TIBEHOV)'                                     
221800         DELIMITED BY SIZE INTO FELTEXT-STR                               
221900         DISPLAY FELTEXT                                                  
222000         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
222100     END-IF                                                               
222200                                                                          
222300*AD  IF CLAG-IDDC-REF NOT = SPACE                                         
222400*AD    MOVE ZERO                     TO WS-TISTOREF-AAVV-3                
222500*AD    IF CLAG-TISTOREF > 0                                               
222600*AD      MOVE CLAG-TISTOREF          TO WS-TISTOREF-AAVVD                 
222700*AD      MOVE WS-TISTOREF-AAVVD(1:4) TO WS-TISTOREF-AAVV                  
222800*AD      PERFORM S65-KOLLA-ERS-DAT-CDC                                    
222900*AD    END-IF                                                             
223000*AD  END-IF                                                               
223100                                                                          
223200*AD  MOVE CLAG-KDERS TO WS-KDERS                                          
223300*AD  IF WS-KDERS(2:1) = 1 OR 2 OR 3 OR 7                                  
223400*AD    PERFORM S66-KOLLA-ERS-DAT                                          
223500*AD  END-IF                                                               
223600                                                                          
223700     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
223800         PERFORM UNTIL                                                    
223900            (BER-IY = BER-ANTAL-AA AND                                    
224000             BER-IX > BER-ANTAL-VV)                                       
224100         OR (BER-IY < BER-ANTAL-AA AND                                    
224200             BER-IX > 52)                                                 
224300                                                                          
224400           ADD  +1                   TO INDX-L                            
224500                                                                          
224600           COMPUTE W-SDC-KVBEHOV-VECKA ROUNDED                            
224700                                 = (UTUP-KVBEHOV-V (INDX-L) * 1)          
224800                                                                          
224900           COMPUTE W-KVBEHOV-30V ROUNDED =                                
225000           ((((SLAG-KVPB-REF + SLAG-KVPBREOI) * 12) / 52) * 30)           
225100***                                                                       
225200           MOVE W-DATUM-FORSTA-SDC   TO TMP1-YYWW                         
225300           MOVE W-BER-DATUM          TO TMP2-YYWW                         
225400             MOVE '126410'           TO FELTEXT2-STR                      
225500           PERFORM WY2000P3                                               
225600           IF TMP1-YYWW <= TMP2-YYWW                                      
225700                                                                          
225800*AD          IF CLAG-IDDC-REF = SPACE                                     
225900               PERFORM  GBA-SDCBEHOV-PER-VECKA                            
226000*AD          ELSE                                                         
226100*AD            IF (CLAG-TISTOREF > ZERO) AND                              
226200*AD               (WS-TISTOREF-AAVV-3 > ZERO)                             
226300*AD                                                                       
226400*AD              MOVE WS-TISTOREF-AAVV-3   TO TMP1-YYWW                   
226500*AD              MOVE W-BER-DATUM          TO TMP2-YYWW                   
226600*AD              IF TMP2-YYWW <= TMP1-YYWW                                
226700*AD                PERFORM  GBA-SDCBEHOV-PER-VECKA                        
226800*AD              ELSE                                                     
226900*AD                MOVE ZERO         TO BER-BEHOV(BER-IY, BER-IX)         
227000*AD              END-IF                                                   
227100*AD            ELSE                                                       
227200*AD               PERFORM  GBA-SDCBEHOV-PER-VECKA                         
227300*AD            END-IF                                                     
227400*AD          END-IF                                                       
227500           ELSE                                                           
227600             MOVE ZERO               TO BER-BEHOV(BER-IY, BER-IX)         
227700           END-IF                                                         
227800                                                                          
227900           ADD 1                     TO BER-IX                            
228000                                        W-BER-DATUM                       
228100                                                                          
228200           MOVE +1                   TO W-ANTAL-VECKOR                    
228300           CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                      
228400                                                                          
228500           MOVE 'AAVV  '             TO DAT-KDDATFORM                     
228600           MOVE W-AAVV               TO DAT-I-TIDATUM                     
228700                                        W-BINNDAY-AAVV                    
228800                                                                          
228900           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
229000                               DAT-O-TIDATUM DAT-KDSVAR                   
229100                                                                          
229200           IF DAT-KDSVAR-OK                                               
229300             MOVE DAT-TIAARP(3:2)    TO W-PER                             
229400             MOVE DAT-TIAAMMDD       TO W-BINNDAY                         
229500           ELSE                                                           
229600               STRING ' FEL FRÅN WDATKONV I W2222200 '                    
229700                      '(GB-, W-AAVV, BER-IY)'                             
229800               DELIMITED BY SIZE INTO FELTEXT-STR                         
229900               DISPLAY FELTEXT                                            
230000               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
230100           END-IF                                                         
230200                                                                          
230300         END-PERFORM                                                      
230400         ADD 1                       TO BER-IY                            
230500         MOVE 1                      TO BER-IX                            
230600                                        IX-PER                            
230700         SUBTRACT +52              FROM W-BER-DATUM                       
230800         ADD +100                    TO W-BER-DATUM                       
230900     END-PERFORM                                                          
231000                                                                          
231100     .                                                                    
231200     EJECT                                                                
231300 GBA-SDCBEHOV-PER-VECKA  SECTION.                                         
231400     MOVE 'GBA-SDCBEHOV-PER-VECKA '  TO CURRENT-SECTION                   
231500                                                                          
231600     IF W-JUST-PB-FINNS = NEJ                                             
231700                                                                          
231800        COMPUTE W-SDC-KVBEHOV-DAG ROUNDED =                               
231900                                        W-SDC-KVBEHOV-VECKA / 5           
232000                                                                          
232100        ADD W-SDC-KVBEHOV-DAG                                             
232200                                     TO W-SDC-ACC-KVBEHOV                 
232300        MOVE W-SDC-KVBEHOV-DAG                                            
232400                                     TO W-SDC-ACC-KVBEHOV-VECKA           
232500        COMPUTE W-SDC-KVAR-EFT-DAG ROUNDED =                              
232600                             W-SDC-TILLGANG - W-SDC-ACC-KVBEHOV           
232700*                                                                         
232800        MOVE +1                      TO IX                                
232900        PERFORM UNTIL      IX > +4                                        
233000        OR W-SDC-KVAR-EFT-DAG < SLAG-KVREFPKT                             
233100          ADD +1                     TO IX                                
233200          ADD W-SDC-KVBEHOV-DAG                                           
233300                                     TO W-SDC-ACC-KVBEHOV                 
233400                                        W-SDC-ACC-KVBEHOV-VECKA           
233500          COMPUTE W-SDC-KVAR-EFT-DAG ROUNDED =                            
233600                             W-SDC-TILLGANG - W-SDC-ACC-KVBEHOV           
233700        END-PERFORM                                                       
233800*                                                                         
233900        COMPUTE W-SDC-KVAR-KVBEHOV-VECKA ROUNDED =                        
234000                             W-SDC-KVBEHOV-VECKA -                        
234100                                        W-SDC-ACC-KVBEHOV-VECKA           
234200        ADD W-SDC-KVAR-KVBEHOV-VECKA                                      
234300                                     TO W-SDC-ACC-KVBEHOV                 
234400                                                                          
234500        IF W-SDC-KVAR-EFT-DAG  < SLAG-KVREFPKT                            
234600           COMPUTE W-SDC-DIFF  = SLAG-KVREFPKT -                          
234700                                             W-SDC-KVAR-EFT-DAG           
234800           COMPUTE W-DC-REFILL-KVANT =                                    
234900                                     W-SDC-DIFF + SLAG-KVREFBER           
235000                                                                          
235100           PERFORM S22-ADJUST-LOW-DEMAND                                  
235200           IF DEMAND-ADJUST                                               
235300              MOVE +1                TO W-DC-REFILL-KVANT                 
235400           ELSE                                                           
235500              PERFORM S08SDC-JUSTERA-REFILLKVANT                          
235600              PERFORM S06-SATT-QX-PROCENT-BRYTNING                        
235700              PERFORM S05-BERAKNA-Q-KVANT                                 
235800           END-IF                                                         
235900           IF WS-DAPUBL > ZERO                                            
236000             MOVE W-BINNDAY                                               
236100                                     TO TMP1-YYMMDD                       
236200             MOVE WS-DAPUBL (3:6)                                         
236300                                     TO TMP2-YYMMDD                       
236400             PERFORM WY2000P1                                             
236500             IF TMP1-YYMMDD      >= TMP2-YYMMDD                           
236600*AD            IF CLAG-IDDC-REF   = SPACE                                 
236700*AD              MOVE CLAG-KDERS     TO WS-KDERS                          
236800*AD              IF WS-KDERS(2:1) = 1 OR 2 OR 3 OR 7                      
236900*AD                MOVE BER-IY       TO W-BER-IY                          
237000*AD                MOVE BER-IX       TO W-BER-IX                          
237100*AD                IF W-BER-IYIX  > ERS-BER-IYIX                          
237200*AD                  MOVE ZERO       TO BER-BEHOV(BER-IY, BER-IX)         
237300*AD                                     W-DC-REFILL-KVANT                 
237400*AD                ELSE                                                   
237500*AD                  MOVE W-DC-REFILL-KVANT                               
237600*AD                                  TO BER-BEHOV(BER-IY, BER-IX)         
237700*AD                END-IF                                                 
237800*AD              ELSE                                                     
237900*AD                MOVE W-DC-REFILL-KVANT                                 
238000*AD                                  TO BER-BEHOV(BER-IY, BER-IX)         
238100*AD              END-IF                                                   
238200*AD            ELSE                                                       
238300                 MOVE W-DC-REFILL-KVANT                                   
238400                                     TO BER-BEHOV(BER-IY, BER-IX)         
238500*AD            END-IF                                                     
238600             ELSE                                                         
238700               MOVE BER-IY                                                
238800                                     TO WS-BER-IY                         
238900               MOVE BER-IX                                                
239000                                     TO WS-BER-IX                         
239100               IF WS-BER-IX > 1                                           
239200                 SUBTRACT 1        FROM WS-BER-IX                         
239300               ELSE                                                       
239400                 IF WS-BER-IY > 1                                         
239500                   SUBTRACT 1      FROM WS-BER-IY                         
239600                   MOVE 52           TO WS-BER-IX                         
239700                 END-IF                                                   
239800               END-IF                                                     
239900               MOVE W-DC-REFILL-KVANT                                     
240000                               TO BER-BEHOV(WS-BER-IY, WS-BER-IX)         
240100             END-IF                                                       
240200                                                                          
240300           ELSE                                                           
240400*AD          IF CLAG-IDDC-REF = SPACE                                     
240500*AD            MOVE CLAG-KDERS       TO WS-KDERS                          
240600*AD            IF WS-KDERS(2:1) = 1 OR 2 OR 3 OR 7                        
240700*AD              MOVE BER-IY         TO W-BER-IY                          
240800*AD              MOVE BER-IX         TO W-BER-IX                          
240900*AD              IF W-BER-IYIX > ERS-BER-IYIX                             
241000*AD                MOVE ZERO         TO BER-BEHOV(BER-IY, BER-IX)         
241100*AD                                     W-DC-REFILL-KVANT                 
241200*AD              ELSE                                                     
241300*AD                MOVE W-DC-REFILL-KVANT                                 
241400*AD                                  TO BER-BEHOV(BER-IY, BER-IX)         
241500*AD              END-IF                                                   
241600*AD            ELSE                                                       
241700*AD              MOVE W-DC-REFILL-KVANT                                   
241800*AD                                  TO BER-BEHOV(BER-IY, BER-IX)         
241900*AD            END-IF                                                     
242000*AD          ELSE                                                         
242100               MOVE W-DC-REFILL-KVANT                                     
242200                                     TO BER-BEHOV(BER-IY, BER-IX)         
242300*AD          END-IF                                                       
242400           END-IF                                                         
242500           ADD W-DC-REFILL-KVANT                                          
242600                                     TO W-SDC-TILLGANG                    
242700        END-IF                                                            
242800     ELSE                                                                 
242900                                                                          
243000                                                                          
243100*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
243200       INITIALIZE REFL-W271REFL                                           
243300                                                                          
243400       MOVE SLAG-IDDC                                                     
243500                          TO REFL-IDDC                                    
243600       MOVE SLAG-IDDC-REF                                                 
243700                          TO REFL-IDDC-REF                                
243800       MOVE SLAG-IDREFTAB                                                 
243900                          TO REFL-IDREFTAB                                
244000       MOVE SLAG-FLWILSON                                                 
244100                          TO REFL-FLWILSON                                
244200                                                                          
244300       PERFORM S30-W271REFL-HAEMTA-PRARTBES                               
244400       MOVE WS-PRARTBES   TO REFL-PRARTBES                                
244500                                                                          
244600       MOVE SLAG-IDLEVNR                                                  
244700                          TO REFL-IN-IDLEVNR-DC                           
244800                                                                          
244900*---- GÄLLER EJ FÖR DCS-SDC OR DCS-NDC-CN, BARA NDC-NA/PF                 
245000       MOVE ZERO          TO REFL-NDC-KVDAGAR-TBT-DC                      
245100                                                                          
245200       MOVE SLAG-FLFLYG   TO REFL-FLFLYG                                  
245300       MOVE W-BINNDAY     TO REFL-BINNDAY-TIAAMMDD                        
245400       MOVE LINK-IDARTNR  TO REFL-IDARTNR                                 
245500       MOVE ZERO          TO REFL-KVREFPKT                                
245600                             REFL-KVREFBER                                
245700                                                                          
245800       MOVE SLAG-TIREFPAF TO TMP1-YYMMDD                                  
245900       MOVE W-BINNDAY     TO TMP2-YYMMDD                                  
246000*                                                                         
246100*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
246200*                                                                         
246300       PERFORM WY2000P1                                                   
246400       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
246500*                                                                         
246600*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
246700*                                                                         
246800         MOVE SLAG-KVREFBER                                               
246900                          TO REFL-IN-KVREFBER                             
247000       ELSE                                                               
247100         MOVE +0          TO REFL-IN-KVREFBER                             
247200       END-IF                                                             
247300                                                                          
247400       MOVE SLAG-TIREFPKT TO TMP1-YYMMDD                                  
247500       MOVE W-BINNDAY     TO TMP2-YYMMDD                                  
247600*                                                                         
247700*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
247800*                                                                         
247900       PERFORM WY2000P1                                                   
248000       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
248100*                                                                         
248200*-----   MANUELL PÅFYLLNADSPUNKT ÄR SATT                                  
248300*                                                                         
248400         MOVE SLAG-KVREFPKT                                               
248500                          TO REFL-IN-KVREFPKT                             
248600       ELSE                                                               
248700         MOVE +0          TO REFL-IN-KVREFPKT                             
248800       END-IF                                                             
248900                                                                          
249000       MOVE UTUP-LT-BEHOV-V (INDX-L)                                      
249100                          TO REFL-IN-LEADTID-BEHOV                        
249200                                                                          
249300       CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                    
249400                                         REFL-WDB6-PCB                    
249500                                         REFL-WDK7-PCB                    
249600                                         UTIL-WDK6-PCB                    
249700                                         UTIL-WDK7-PCB                    
249800                                         UTIL-WDB6-PCB                    
249900                                                                          
250000       COMPUTE W-SDC-KVBEHOV-DAG ROUNDED =                                
250100                                        W-SDC-KVBEHOV-VECKA / 5           
250200                                                                          
250300       ADD W-SDC-KVBEHOV-DAG                                              
250400                                 TO W-SDC-ACC-KVBEHOV                     
250500       MOVE W-SDC-KVBEHOV-DAG                                             
250600                                 TO W-SDC-ACC-KVBEHOV-VECKA               
250700       COMPUTE W-SDC-KVAR-EFT-DAG ROUNDED =                               
250800                             W-SDC-TILLGANG - W-SDC-ACC-KVBEHOV           
250900*                                                                         
251000       MOVE +1                   TO IX                                    
251100       PERFORM UNTIL IX      > +4                                         
251200       OR W-SDC-KVAR-EFT-DAG < REFL-KVREFPKT                              
251300         ADD +1                  TO IX                                    
251400         ADD W-SDC-KVBEHOV-DAG                                            
251500                                 TO W-SDC-ACC-KVBEHOV                     
251600                                    W-SDC-ACC-KVBEHOV-VECKA               
251700         COMPUTE W-SDC-KVAR-EFT-DAG ROUNDED =                             
251800                             W-SDC-TILLGANG - W-SDC-ACC-KVBEHOV           
251900                                                                          
252000       END-PERFORM                                                        
252100*                                                                         
252200       COMPUTE W-SDC-KVAR-KVBEHOV-VECKA ROUNDED =                         
252300                             W-SDC-KVBEHOV-VECKA -                        
252400                                        W-SDC-ACC-KVBEHOV-VECKA           
252500       ADD W-SDC-KVAR-KVBEHOV-VECKA                                       
252600                                 TO W-SDC-ACC-KVBEHOV                     
252700                                                                          
252800       IF W-SDC-KVAR-EFT-DAG  < REFL-KVREFPKT                             
252900          COMPUTE W-SDC-DIFF  = REFL-KVREFPKT -                           
253000                                             W-SDC-KVAR-EFT-DAG           
253100          COMPUTE W-DC-REFILL-KVANT =                                     
253200                                     W-SDC-DIFF + REFL-KVREFBER           
253300                                                                          
253400          PERFORM S22-ADJUST-LOW-DEMAND                                   
253500          IF DEMAND-ADJUST                                                
253600             MOVE +1             TO W-DC-REFILL-KVANT                     
253700          ELSE                                                            
253800             PERFORM S06-SATT-QX-PROCENT-BRYTNING                         
253900             PERFORM S05-BERAKNA-Q-KVANT                                  
254000          END-IF                                                          
254100          IF WS-DAPUBL > ZERO                                             
254200            MOVE W-BINNDAY                                                
254300                                 TO TMP1-YYMMDD                           
254400            MOVE WS-DAPUBL (3:6)                                          
254500                                 TO TMP2-YYMMDD                           
254600            PERFORM WY2000P1                                              
254700            IF TMP1-YYMMDD >= TMP2-YYMMDD                                 
254800*AD           IF CLAG-IDDC-REF    = SPACE                                 
254900*AD             MOVE CLAG-KDERS  TO WS-KDERS                              
255000*AD             IF WS-KDERS(2:1)  = 1 OR 2 OR 3 OR 7                      
255100*AD               MOVE BER-IY    TO W-BER-IY                              
255200*AD               MOVE BER-IX    TO W-BER-IX                              
255300*AD               IF W-BER-IYIX   > ERS-BER-IYIX                          
255400*AD                 MOVE ZERO    TO BER-BEHOV(BER-IY, BER-IX)             
255500*AD                                 W-DC-REFILL-KVANT                     
255600*AD               ELSE                                                    
255700*AD                 MOVE W-DC-REFILL-KVANT                                
255800*AD                              TO BER-BEHOV(BER-IY, BER-IX)             
255900*AD               END-IF                                                  
256000*AD             ELSE                                                      
256100*AD               MOVE W-DC-REFILL-KVANT                                  
256200*AD                              TO BER-BEHOV(BER-IY, BER-IX)             
256300*AD             END-IF                                                    
256400*AD           ELSE                                                        
256500                MOVE W-DC-REFILL-KVANT                                    
256600                                 TO BER-BEHOV(BER-IY, BER-IX)             
256700*AD           END-IF                                                      
256800            ELSE                                                          
256900              MOVE BER-IY                                                 
257000                                 TO WS-BER-IY                             
257100              MOVE BER-IX                                                 
257200                                 TO WS-BER-IX                             
257300              IF WS-BER-IX   > 1                                          
257400                SUBTRACT 1     FROM WS-BER-IX                             
257500              ELSE                                                        
257600                IF WS-BER-IY > 1                                          
257700                  SUBTRACT 1   FROM WS-BER-IY                             
257800                  MOVE 52        TO WS-BER-IX                             
257900                END-IF                                                    
258000              END-IF                                                      
258100              MOVE W-DC-REFILL-KVANT                                      
258200                              TO BER-BEHOV(WS-BER-IY, WS-BER-IX)          
258300            END-IF                                                        
258400          ELSE                                                            
258500*AD         IF CLAG-IDDC-REF   = SPACE                                    
258600*AD           MOVE CLAG-KDERS    TO WS-KDERS                              
258700*AD           IF WS-KDERS(2:1) = 1 OR 2 OR 3 OR 7                         
258800*AD             MOVE BER-IY      TO W-BER-IY                              
258900*AD             MOVE BER-IX      TO W-BER-IX                              
259000*AD             IF W-BER-IYIX  > ERS-BER-IYIX                             
259100*AD               MOVE ZERO      TO BER-BEHOV(BER-IY, BER-IX)             
259200*AD                                 W-DC-REFILL-KVANT                     
259300*AD             ELSE                                                      
259400*AD               MOVE W-DC-REFILL-KVANT                                  
259500*AD                              TO BER-BEHOV(BER-IY, BER-IX)             
259600*AD             END-IF                                                    
259700*AD           ELSE                                                        
259800*AD             MOVE W-DC-REFILL-KVANT                                    
259900*AD                              TO BER-BEHOV(BER-IY, BER-IX)             
260000*AD           END-IF                                                      
260100*AD         ELSE                                                          
260200              MOVE W-DC-REFILL-KVANT                                      
260300                                 TO BER-BEHOV(BER-IY, BER-IX)             
260400*AD         END-IF                                                        
260500          END-IF                                                          
260600          ADD W-DC-REFILL-KVANT                                           
260700                                 TO W-SDC-TILLGANG                        
260800       END-IF                                                             
260900     END-IF                                                               
261000                                                                          
261100     .                                                                    
261200     EJECT                                                                
261300 GC-LAES-WDK711         SECTION.                                          
261400     MOVE 'GC-LAES-WDK711  '  TO CURRENT-SECTION                          
261500******************************************************************        
261600*                                                                *        
261700*    LÄSER WDK711                                                *        
261800*                                                                *        
261900******************************************************************        
262000     IF LINK-IDDC = SPACE OR ZERO                                         
262100        MOVE '1A'            TO W-IDDC-MIN                                
262200        MOVE '39'            TO W-IDDC-MAX                                
262300     ELSE                                                                 
262400        MOVE LINK-IDDC       TO W-IDDC-MIN                                
262500        MOVE LINK-IDDC       TO W-IDDC-MAX                                
262600     END-IF                                                               
262700     MOVE WC-CDC-SE          TO W-IDDC-REF                                
262800     PERFORM IMS-GNP-WDK711                                               
262900     IF SEGMENT-FINNS                                                     
263000       MOVE SLAG-IDDC TO W-IDDC-B6                                        
263100       PERFORM S13-READ-OR-TAB-B601                                       
263200     END-IF                                                               
263300                                                                          
263400     PERFORM UNTIL SEGMENT-SAKNAS                                         
263500     OR  (SLAG-KDREFSTA     = 'A'                                         
263600     AND (LINK-FLINKLDIRLEV = JA                                          
263700     OR  (LINK-FLINKLDIRLEV = NEJ                                         
263800     AND  SLAG-FLCDCBEH     = JA)))                                       
263900*       LÄS FRAM TILL EN SDC-POST ELLER LDC-POST                          
264000*       DVS LÄS FÖRBI NDC-POSTER OCH KINA LDC-POSTER                      
264100        PERFORM IMS-GNP-WDK711                                            
264200        IF SEGMENT-FINNS                                                  
264300          MOVE SLAG-IDDC TO W-IDDC-B6                                     
264400          PERFORM S13-READ-OR-TAB-B601                                    
264500        END-IF                                                            
264600     END-PERFORM                                                          
264700                                                                          
264800     IF SEGMENT-FINNS                                                     
264900*** FIX FÖR NEGATIVA KVOKS-BULK                                           
265000        IF SLAG-KVOKS-BULK < 0                                            
265100          MOVE ZERO          TO SLAG-KVOKS-BULK                           
265200        END-IF                                                            
265300*** FIX FÖR NEGATIVA KVOKS-DAG                                            
265400        IF SLAG-KVOKS-DAG < 0                                             
265500          MOVE ZERO          TO SLAG-KVOKS-DAG                            
265600        END-IF                                                            
265700                                                                          
265800*** KINA OCH JAPAN HAR 6 DAGARS ARBETSVECKA                               
265900        MOVE NEJ TO 6ARBDAG-SW                                            
266000        IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA) OR DCS-JAPAN             
266100           OR DCS-ENGLAND OR DCS-INDIA OR DCS-EMIRATES                    
266200          MOVE JA TO 6ARBDAG-SW                                           
266300        END-IF                                                            
266400     END-IF                                                               
266500     .                                                                    
266600     EJECT                                                                
266700 H-BERAKNA-NDCBEHOV   SECTION.                                            
266800     MOVE 'H-BERAKNA-NDCBEHOV '  TO CURRENT-SECTION                       
266900******************************************************************        
267000*                                                                *        
267100*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL                  *        
267200*    BEHOVET VISAS DEN VECKA CDC BEHÖVER LEVERERA                *        
267300*    DVS BEHOVSDAG PÅ NDC MINUS LEDTID                           *        
267400*                                                                *        
267500*    KINA NDC OCH LDC SKALL INGÅ I NDC'ERNAS BEHOV.              *        
267600******************************************************************        
267700                                                                          
267800     MOVE NEJ TO WS-ERS-FINNS-K611                                        
267900                                                                          
268000     MOVE WS-BER-TAB-NOLL     TO BER-TAB                                  
268100                                                                          
268200     ACCEPT W-TIME          FROM TIME                                     
268300                                                                          
268400     IF CLAG-REDIRLEV < 1.00                                              
268500       IF ART-FLERS = JA                                                  
268600         MOVE ART-IDARTNR     TO W-IDARTNR-D7-MIN                         
268700                                 W-IDARTNR-D7-MAX                         
268800         PERFORM IMS-GU-WDD7A1                                            
268900         IF SEGMENT-FINNS                                                 
269000           MOVE ERS-IDARTNR   TO W-IDARTNR-ERS                            
269100           PERFORM IMS-GU-WDK601-ERS                                      
269200           IF SEGMENT-FINNS                                               
269300             PERFORM IMS-GNP-WDK611-ERS                                   
269400                                                                          
269500             MOVE JA          TO WS-ERS-FINNS-K611                        
269600           END-IF                                                         
269700                                                                          
269800*----   VI BEHÖVER ÅTERSTÄLLA POSITIONEN I BASEN                          
269900           MOVE LINK-IDARTNR  TO W-IDARTNR                                
270000           PERFORM IMS-GU-WDK601                                          
270100         END-IF                                                           
270200       END-IF                                                             
270300       PERFORM IMS-GU-WDK701                                              
270400       IF SEGMENT-FINNS                                                   
270500         PERFORM HD-LAES-WDK711                                           
270600                                                                          
270700         PERFORM UNTIL SEGMENT-SAKNAS                                     
270800           PERFORM S21-NOLLSTALL-W-NDC-BEHOV-REF                          
270900*  NDC                                                                    
271000           PERFORM S77-HITTA-K712-LAND                                    
271100           MOVE K712-DAPUBL(K712-IX) TO WS-DAPUBL                         
271200           MOVE SLAG-FLFLYG          TO WS-FLFLYG                         
271300           PERFORM S80-CALC-LT-ADJ-PUBWK                                  
271400                                                                          
271500           IF  WS-DAPUBL  > ZERO                                          
271600           AND WS-DAPUBL  > WS-DAGENS-DATUM                               
271700             MOVE WS-LT-WEEKS-DAYS  TO DAG-KVKALDAG                       
271800             MOVE WS-DAPUBL (3:6)                                         
271900                              TO DAG-TIAAMMDD-TOM                         
272000             MOVE 003         TO DAG-KDCALL                               
272100             CALL WDAGKONV USING DAG-KDCALL                               
272200                                 DAG-DATUM-AREA                           
272300                                 DAG-KDSVAR                               
272400             IF DAG-KDSVAR = SPACE                                        
272500               CONTINUE                                                   
272600             ELSE                                                         
272700               STRING 'FEL FRÅN WDAGKONV I W2222200 '                     
272800               'H- SECTION (WS-DAPUBL)' DELIMITED BY SIZE                 
272900                         INTO         FELTEXT-STR                         
273000                DISPLAY FELTEXT                                           
273100                CALL ABEND USING RKOD-ABEND-UTAN-DUMP                     
273200             END-IF                                                       
273300                                                                          
273400             MOVE DAG-TIAAMMDD-FOM                                        
273500                              TO DAT-I-TIDATUM                            
273600             MOVE 'AAMMDD'    TO DAT-KDDATFORM                            
273700                                                                          
273800             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
273900                                 DAT-O-TIDATUM DAT-KDSVAR                 
274000                                                                          
274100             IF DAT-KDSVAR-OK                                             
274200                MOVE DAT-TIAAVV-GRP                                       
274300                              TO W-TIAAVV                                 
274400                MOVE W-TIAAVV                                             
274500                              TO W-DATUM-FORSTA-SDC                       
274600             ELSE                                                         
274700                STRING ' FEL FRÅN WDATKONV I W2222200 '                   
274800                       '(H-, DAG-TIAAMMDD)'                               
274900                DELIMITED BY SIZE INTO FELTEXT-STR                        
275000                DISPLAY FELTEXT                                           
275100                CALL ABEND USING RKOD-ABEND-UTAN-DUMP                     
275200             END-IF                                                       
275300           ELSE                                                           
275400             MOVE W-TIFINLV-AAVV-MINUS-LT                                 
275500                              TO W-DATUM-FORSTA-SDC                       
275600           END-IF                                                         
275700                                                                          
275800           MOVE 'AAVV  '      TO DAT-KDDATFORM                            
275900           MOVE LINK-TIAAVV-AKTUELL                                       
276000                              TO DAT-I-TIDATUM                            
276100                                 W-BINNDAY-AAVV                           
276200                                                                          
276300           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
276400                           DAT-O-TIDATUM DAT-KDSVAR                       
276500                                                                          
276600           IF DAT-KDSVAR-OK                                               
276700             MOVE DAT-TIAARP(3:2) TO W-PER                                
276800             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
276900           ELSE                                                           
277000                 STRING ' FEL FRÅN WDATKONV I W2222200 '                  
277100                        '(H-, LINK-TIAAVVD)'                              
277200                 DELIMITED BY SIZE INTO FELTEXT-STR                       
277300                 DISPLAY FELTEXT                                          
277400                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
277500           END-IF                                                         
277600                                                                          
277700*** FINNS F/C IN THE FUTURE, KVPB-JUST FÖR AKTUELL VECKA.                 
277800           PERFORM S23-GET-WEEK-DEMAND-XDC                                
277900           MOVE +1                TO INDX-L                               
278000                                                                          
278100           COMPUTE W-NDC-TILLGANG = SLAG-KVLS +                           
278200                                    SLAG-KVAKS-PAV +                      
278300                                    SLAG-KVAKS-SDC +                      
278400                                    SLAG-KVBEART -                        
278500                                    SLAG-KVOKS-BULK -                     
278600                                    SLAG-KVOKS-DAG -                      
278700                                    SLAG-KVROS-BULK -                     
278800                                    SLAG-KVROS-DAG                        
278900                                                                          
279000           COMPUTE W-KVBEHOV-52V ROUNDED =                                
279100                          ((SLAG-KVPB-REF + SLAG-KVPBREOI) * 12)          
279200                                                                          
279300*AD        PERFORM HC-JUSTERA-NDC-TILLGANG                                
279400           PERFORM S15-JUSTERA-XDC-TILLGANG                               
279500           ADD W-XDC-TILLGANG-ERS TO W-NDC-TILLGANG                       
279600                                                                          
279700           IF LINK-TID-AKTUELL = 5                                        
279800           OR LINK-TID-AKTUELL > 5                                        
279900              CONTINUE                                                    
280000           ELSE                                                           
280100              PERFORM HA-BERAKNA-INNEV-VECKA-NDC                          
280200              ADD W-NDC-KVBEHOV-DESSUTOM                                  
280300                              TO LINK-KVBEHOV-DESSUTOM                    
280400           END-IF                                                         
280500                                                                          
280600           PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                           
280700           PERFORM HB-NDCBEHOV                                            
280800           PERFORM S03-ADD-TILL-RESULTAT-TAB                              
280900                                                                          
281000           PERFORM HD-LAES-WDK711                                         
281100         END-PERFORM                                                      
281200       END-IF                                                             
281300     END-IF                                                               
281400     .                                                                    
281500     EJECT                                                                
281600 HA-BERAKNA-INNEV-VECKA-NDC SECTION.                                      
281700     MOVE 'HA-BERAKNA-INNEV-VECKA-NDC '  TO CURRENT-SECTION               
281800******************************************************************        
281900*                                                                *        
282000*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL I INNEVARANDA    *        
282100*    VECKA.                                                      *        
282200*                                                                *        
282300******************************************************************        
282400                                                                          
282500     MOVE W-DATUM-FORSTA-SDC    TO TMP1-YYWW                              
282600     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
282700             MOVE '136810'      TO FELTEXT2-STR                           
282800     PERFORM WY2000P3                                                     
282900     IF TMP1-YYWW <= TMP2-YYWW                                            
283000                                                                          
283100*****   WEEKLY DEMAND ALREADY ADJUSTED FOR NUMBER OF DAYS                 
283200*****   IN CURRENT WEEK                                                   
283300*                                                                         
283400        COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED                             
283500                             = (UTUP-KVBEHOV-V (INDX-L) * 1)              
283600                                                                          
283700        COMPUTE W-NDC-KVAR-EFT-VECKA ROUNDED =                            
283800                         W-NDC-TILLGANG - W-KVBEHOV-INNEV-VECKA           
283900                                                                          
284000        IF W-JUST-PB-FINNS = NEJ                                          
284100                                                                          
284200          IF W-NDC-KVAR-EFT-VECKA    < SLAG-KVREFPKT                      
284300             COMPUTE W-NDC-DIFF      = SLAG-KVREFPKT -                    
284400                                       W-NDC-TILLGANG                     
284500                                                                          
284600***---INNEVARANDE VECKOBEHOV SKALL EJ TAS MED I REFILLKVANTEN             
284700             COMPUTE W-DC-REFILL-KVANT =                                  
284800                                     W-NDC-DIFF + SLAG-KVREFBER           
284900                                                                          
285000             PERFORM S22-ADJUST-LOW-DEMAND                                
285100             IF DEMAND-ADJUST                                             
285200                MOVE +1             TO W-DC-REFILL-KVANT                  
285300             ELSE                                                         
285400                PERFORM S08NDC-JUSTERA-REFILLKVANT                        
285500                PERFORM S06-SATT-QX-PROCENT-BRYTNING                      
285600                PERFORM S05-BERAKNA-Q-KVANT                               
285700             END-IF                                                       
285800             MOVE W-DC-REFILL-KVANT TO W-NDC-KVBEHOV-DESSUTOM             
285900          ELSE                                                            
286000             MOVE ZERO              TO W-NDC-KVBEHOV-DESSUTOM             
286100          END-IF                                                          
286200        ELSE                                                              
286300*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
286400                                                                          
286500          INITIALIZE REFL-W271REFL                                        
286600                                                                          
286700          MOVE SLAG-IDDC        TO REFL-IDDC                              
286800          MOVE SLAG-IDDC-REF    TO REFL-IDDC-REF                          
286900          MOVE SLAG-IDREFTAB    TO REFL-IDREFTAB                          
287000          MOVE SLAG-FLWILSON    TO REFL-FLWILSON                          
287100                                                                          
287200          PERFORM S30-W271REFL-HAEMTA-PRARTBES                            
287300          MOVE WS-PRARTBES      TO REFL-PRARTBES                          
287400                                                                          
287500          MOVE SLAG-IDLEVNR     TO REFL-IN-IDLEVNR-DC                     
287600                                                                          
287700*---- GÄLLER BARA FÖR NDC-NA OCH NDC-PF                                   
287800*---- OCH SLAG-IDLEVNR NOT = '1441 ' OR 'BP2TW'                           
287900*---- --- SLAG-IDDC-REF NOT = 11, 71, 72 OR 73                            
288000*--------------------------------------------------------                 
288100          MOVE ZERO            TO REFL-NDC-KVDAGAR-TBT-DC                 
288200                                                                          
288300          MOVE SLAG-FLFLYG     TO REFL-FLFLYG                             
288400          MOVE W-BINNDAY       TO REFL-BINNDAY-TIAAMMDD                   
288500          MOVE LINK-IDARTNR    TO REFL-IDARTNR                            
288600          MOVE ZERO            TO REFL-KVREFPKT                           
288700                                  REFL-KVREFBER                           
288800                                                                          
288900          MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                             
289000          MOVE W-BINNDAY       TO TMP2-YYMMDD                             
289100*                                                                         
289200*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
289300*                                                                         
289400          PERFORM WY2000P1                                                
289500          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
289600*                                                                         
289700*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
289800*                                                                         
289900            MOVE SLAG-KVREFBER TO REFL-IN-KVREFBER                        
290000          ELSE                                                            
290100            MOVE +0            TO REFL-IN-KVREFBER                        
290200          END-IF                                                          
290300                                                                          
290400          MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                             
290500          MOVE W-BINNDAY       TO TMP2-YYMMDD                             
290600*                                                                         
290700*-----   W-BINNDAY  = MÅNDAG I AKTUELL VECKA                              
290800*                                                                         
290900          PERFORM WY2000P1                                                
291000          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
291100*                                                                         
291200*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
291300*                                                                         
291400            MOVE SLAG-KVREFPKT TO REFL-IN-KVREFPKT                        
291500          ELSE                                                            
291600            MOVE +0            TO REFL-IN-KVREFPKT                        
291700          END-IF                                                          
291800          MOVE UTUP-LT-BEHOV-V (INDX-L)                                   
291900                               TO REFL-IN-LEADTID-BEHOV                   
292000                                                                          
292100          CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                 
292200                                            REFL-WDB6-PCB                 
292300                                            REFL-WDK7-PCB                 
292400                                            UTIL-WDK6-PCB                 
292500                                            UTIL-WDK7-PCB                 
292600                                            UTIL-WDB6-PCB                 
292700                                                                          
292800          IF W-NDC-KVAR-EFT-VECKA    < REFL-KVREFPKT                      
292900             COMPUTE W-NDC-DIFF      = REFL-KVREFPKT -                    
293000                                                  W-NDC-TILLGANG          
293100                                                                          
293200***---INNEVARANDE VECKOBEHOV SKALL EJ TAS MED I REFILLKVANTEN             
293300             COMPUTE W-DC-REFILL-KVANT =                                  
293400                     W-NDC-DIFF + REFL-KVREFBER                           
293500                                                                          
293600             PERFORM S22-ADJUST-LOW-DEMAND                                
293700             IF DEMAND-ADJUST                                             
293800                MOVE +1             TO W-DC-REFILL-KVANT                  
293900             ELSE                                                         
294000                PERFORM S06-SATT-QX-PROCENT-BRYTNING                      
294100                PERFORM S05-BERAKNA-Q-KVANT                               
294200             END-IF                                                       
294300             MOVE W-DC-REFILL-KVANT TO W-NDC-KVBEHOV-DESSUTOM             
294400          ELSE                                                            
294500             MOVE ZERO              TO W-NDC-KVBEHOV-DESSUTOM             
294600          END-IF                                                          
294700                                                                          
294800        END-IF                                                            
294900     ELSE                                                                 
295000        MOVE ZERO                   TO W-NDC-KVBEHOV-DESSUTOM             
295100     END-IF                                                               
295200     .                                                                    
295300     EJECT                                                                
295400 HB-NDCBEHOV            SECTION.                                          
295500     MOVE 'HB-NDCBEHOV  '  TO CURRENT-SECTION                             
295600******************************************************************        
295700*                                                                *        
295800*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL I KOMMANDE       *        
295900*    VECKOR.                                                     *        
296000*                                                                *        
296100******************************************************************        
296200                                                                          
296300     MOVE ZERO                  TO W-NDC-ACC-KVBEHOV                      
296400     ADD W-NDC-KVBEHOV-DESSUTOM TO W-NDC-TILLGANG                         
296500     ADD W-KVBEHOV-INNEV-VECKA  TO W-NDC-ACC-KVBEHOV                      
296600                                                                          
296700     MOVE +1                    TO BER-IY                                 
296800     MOVE W-BER-START-VV        TO BER-IX                                 
296900     MOVE LINK-TIBEHOV-START    TO W-BER-DATUM                            
297000                                   W-AAVV                                 
297100     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
297200     MOVE W-AAVV                TO DAT-I-TIDATUM                          
297300                                   W-BINNDAY-AAVV                         
297400                                                                          
297500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
297600                         DAT-O-TIDATUM DAT-KDSVAR                         
297700                                                                          
297800     IF DAT-KDSVAR-OK                                                     
297900       MOVE DAT-TIAARP(3:2)     TO W-PER                                  
298000       MOVE DAT-TIAAMMDD        TO W-BINNDAY                              
298100     ELSE                                                                 
298200           STRING ' FEL FRÅN WDATKONV I W2222200 '                        
298300                  '(HB-, LINK-TIBEHOV)'                                   
298400           DELIMITED BY SIZE INTO FELTEXT-STR                             
298500           DISPLAY FELTEXT                                                
298600           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
298700     END-IF                                                               
298800                                                                          
298900     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
299000                                                                          
299100         PERFORM UNTIL                                                    
299200            (BER-IY = BER-ANTAL-AA AND                                    
299300             BER-IX > BER-ANTAL-VV)                                       
299400         OR (BER-IY < BER-ANTAL-AA AND                                    
299500             BER-IX > 52)                                                 
299600                                                                          
299700           ADD  +1                   TO INDX-L                            
299800                                                                          
299900           COMPUTE W-NDC-KVBEHOV-VECKA ROUNDED                            
300000                                = (UTUP-KVBEHOV-V (INDX-L) * 1)           
300100                                                                          
300200           COMPUTE W-KVBEHOV-52V ROUNDED =                                
300300                          ((SLAG-KVPB-REF + SLAG-KVPBREOI) * 12)          
300400                                                                          
300500           MOVE W-DATUM-FORSTA-SDC   TO TMP1-YYWW                         
300600           MOVE W-BER-DATUM          TO TMP2-YYWW                         
300700             MOVE '144910'           TO FELTEXT2-STR                      
300800           PERFORM WY2000P3                                               
300900           IF TMP1-YYWW <= TMP2-YYWW                                      
301000                                                                          
301100             IF W-JUST-PB-FINNS = NEJ                                     
301200               COMPUTE W-NDC-KVBEHOV-DAG ROUNDED =                        
301300                                        W-NDC-KVBEHOV-VECKA / 5           
301400                                                                          
301500               ADD W-NDC-KVBEHOV-DAG                                      
301600                                     TO W-NDC-ACC-KVBEHOV                 
301700               MOVE W-NDC-KVBEHOV-DAG                                     
301800                                     TO W-NDC-ACC-KVBEHOV-VECKA           
301900               COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                       
302000                             W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV           
302100*                                                                         
302200               MOVE +1               TO IX                                
302300               PERFORM UNTIL IX       > +4                                
302400               OR W-NDC-KVAR-EFT-DAG  < SLAG-KVREFPKT                     
302500                  ADD +1             TO IX                                
302600                  ADD W-NDC-KVBEHOV-DAG                                   
302700                                     TO W-NDC-ACC-KVBEHOV                 
302800                                        W-NDC-ACC-KVBEHOV-VECKA           
302900                  COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                    
303000                             W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV           
303100                                                                          
303200               END-PERFORM                                                
303300*                                                                         
303400               COMPUTE W-NDC-KVAR-KVBEHOV-VECKA ROUNDED =                 
303500                             W-NDC-KVBEHOV-VECKA -                        
303600                                        W-NDC-ACC-KVBEHOV-VECKA           
303700               ADD W-NDC-KVAR-KVBEHOV-VECKA                               
303800                                     TO W-NDC-ACC-KVBEHOV                 
303900                                                                          
304000               IF W-NDC-KVAR-EFT-DAG  < SLAG-KVREFPKT                     
304100                  COMPUTE W-NDC-DIFF  = SLAG-KVREFPKT -                   
304200                                             W-NDC-KVAR-EFT-DAG           
304300                                                                          
304400                  COMPUTE W-DC-REFILL-KVANT =                             
304500                                     W-NDC-DIFF + SLAG-KVREFBER           
304600                                                                          
304700                  PERFORM S22-ADJUST-LOW-DEMAND                           
304800                  IF DEMAND-ADJUST                                        
304900                     MOVE +1         TO W-DC-REFILL-KVANT                 
305000                  ELSE                                                    
305100                     PERFORM S08NDC-JUSTERA-REFILLKVANT                   
305200                     PERFORM S06-SATT-QX-PROCENT-BRYTNING                 
305300                     PERFORM S05-BERAKNA-Q-KVANT                          
305400                  END-IF                                                  
305500                  IF WS-DAPUBL > ZERO                                     
305600                    MOVE W-BINNDAY                                        
305700                                     TO TMP1-YYMMDD                       
305800                    MOVE WS-DAPUBL (3:6)                                  
305900                                     TO TMP2-YYMMDD                       
306000                    PERFORM WY2000P1                                      
306100                    IF TMP1-YYMMDD >= TMP2-YYMMDD                         
306200                      MOVE W-DC-REFILL-KVANT                              
306300                                     TO BER-BEHOV(BER-IY, BER-IX)         
306400                    ELSE                                                  
306500                      MOVE BER-IY                                         
306600                                     TO WS-BER-IY                         
306700                      MOVE BER-IX                                         
306800                                     TO WS-BER-IX                         
306900                      IF WS-BER-IX > 1                                    
307000                        SUBTRACT 1 FROM WS-BER-IX                         
307100                      ELSE                                                
307200                        IF WS-BER-IY > 1                                  
307300                          SUBTRACT 1 FROM WS-BER-IY                       
307400                          MOVE 52      TO WS-BER-IX                       
307500                        END-IF                                            
307600                      END-IF                                              
307700                      MOVE W-DC-REFILL-KVANT                              
307800                              TO BER-BEHOV(WS-BER-IY, WS-BER-IX)          
307900                    END-IF                                                
308000                                                                          
308100                  ELSE                                                    
308200                    MOVE W-DC-REFILL-KVANT                                
308300                              TO BER-BEHOV(BER-IY, BER-IX)                
308400                  END-IF                                                  
308500                  ADD W-DC-REFILL-KVANT                                   
308600                              TO W-NDC-TILLGANG                           
308700               END-IF                                                     
308800             ELSE                                                         
308900*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
309000                                                                          
309100               INITIALIZE REFL-W271REFL                                   
309200                                                                          
309300               MOVE SLAG-IDDC      TO REFL-IDDC                           
309400               MOVE SLAG-IDDC-REF  TO REFL-IDDC-REF                       
309500               MOVE SLAG-IDREFTAB  TO REFL-IDREFTAB                       
309600               MOVE SLAG-FLWILSON  TO REFL-FLWILSON                       
309700                                                                          
309800               PERFORM S30-W271REFL-HAEMTA-PRARTBES                       
309900               MOVE WS-PRARTBES    TO REFL-PRARTBES                       
310000                                                                          
310100               MOVE SLAG-IDLEVNR   TO REFL-IN-IDLEVNR-DC                  
310200               MOVE ZERO           TO REFL-NDC-KVDAGAR-TBT-DC             
310300                                                                          
310400               MOVE SLAG-FLFLYG    TO REFL-FLFLYG                         
310500               MOVE W-BINNDAY      TO REFL-BINNDAY-TIAAMMDD               
310600               MOVE LINK-IDARTNR   TO REFL-IDARTNR                        
310700               MOVE ZERO           TO REFL-KVREFPKT                       
310800                                      REFL-KVREFBER                       
310900                                                                          
311000               MOVE SLAG-TIREFPAF  TO TMP1-YYMMDD                         
311100               MOVE W-BINNDAY      TO TMP2-YYMMDD                         
311200*                                                                         
311300*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
311400*                                                                         
311500               PERFORM WY2000P1                                           
311600               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
311700*                                                                         
311800*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
311900*                                                                         
312000                 MOVE SLAG-KVREFBER                                       
312100                                   TO REFL-IN-KVREFBER                    
312200               ELSE                                                       
312300                 MOVE +0           TO REFL-IN-KVREFBER                    
312400               END-IF                                                     
312500                                                                          
312600               MOVE SLAG-TIREFPKT  TO TMP1-YYMMDD                         
312700               MOVE W-BINNDAY      TO TMP2-YYMMDD                         
312800*                                                                         
312900*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
313000*                                                                         
313100               PERFORM WY2000P1                                           
313200               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
313300*                                                                         
313400*-----   MANUELL PÅFYLLNADSPUNKT ÄR SATT                                  
313500*                                                                         
313600                 MOVE SLAG-KVREFPKT                                       
313700                                   TO REFL-IN-KVREFPKT                    
313800               ELSE                                                       
313900                 MOVE +0           TO REFL-IN-KVREFPKT                    
314000               END-IF                                                     
314100                                                                          
314200               MOVE UTUP-LT-BEHOV-V (INDX-L)                              
314300                                   TO REFL-IN-LEADTID-BEHOV               
314400                                                                          
314500               CALL W271REFL USING REFL-W271REFL REFL-2501-PCB            
314600                                                 REFL-WDB6-PCB            
314700                                                 REFL-WDK7-PCB            
314800                                                 UTIL-WDK6-PCB            
314900                                                 UTIL-WDK7-PCB            
315000                                                 UTIL-WDB6-PCB            
315100                                                                          
315200               COMPUTE W-NDC-KVBEHOV-DAG ROUNDED =                        
315300                                         W-NDC-KVBEHOV-VECKA / 5          
315400                                                                          
315500               ADD W-NDC-KVBEHOV-DAG                                      
315600                                    TO W-NDC-ACC-KVBEHOV                  
315700               MOVE W-NDC-KVBEHOV-DAG                                     
315800                                    TO W-NDC-ACC-KVBEHOV-VECKA            
315900               COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                       
316000                              W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV          
316100                                                                          
316200               MOVE +1              TO IX                                 
316300               PERFORM UNTIL IX      > +4                                 
316400               OR W-NDC-KVAR-EFT-DAG < REFL-KVREFPKT                      
316500                  ADD +1            TO IX                                 
316600                  ADD W-NDC-KVBEHOV-DAG                                   
316700                                    TO W-NDC-ACC-KVBEHOV                  
316800                                       W-NDC-ACC-KVBEHOV-VECKA            
316900                  COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                    
317000                              W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV          
317100               END-PERFORM                                                
317200                                                                          
317300               COMPUTE W-NDC-KVAR-KVBEHOV-VECKA ROUNDED =                 
317400                              W-NDC-KVBEHOV-VECKA -                       
317500                                         W-NDC-ACC-KVBEHOV-VECKA          
317600               ADD W-NDC-KVAR-KVBEHOV-VECKA                               
317700                                    TO W-NDC-ACC-KVBEHOV                  
317800                                                                          
317900               IF W-NDC-KVAR-EFT-DAG < REFL-KVREFPKT                      
318000                  COMPUTE W-NDC-DIFF = REFL-KVREFPKT -                    
318100                                              W-NDC-KVAR-EFT-DAG          
318200                  COMPUTE W-DC-REFILL-KVANT =                             
318300                                      W-NDC-DIFF + REFL-KVREFBER          
318400                                                                          
318500                  PERFORM S22-ADJUST-LOW-DEMAND                           
318600                  IF DEMAND-ADJUST                                        
318700                     MOVE +1        TO W-DC-REFILL-KVANT                  
318800                  ELSE                                                    
318900                     PERFORM S06-SATT-QX-PROCENT-BRYTNING                 
319000                     PERFORM S05-BERAKNA-Q-KVANT                          
319100                  END-IF                                                  
319200                  IF WS-DAPUBL > ZERO                                     
319300                     MOVE W-BINNDAY                                       
319400                                    TO TMP1-YYMMDD                        
319500                     MOVE WS-DAPUBL (3:6)                                 
319600                                    TO TMP2-YYMMDD                        
319700                     PERFORM WY2000P1                                     
319800                     IF TMP1-YYMMDD >= TMP2-YYMMDD                        
319900                       MOVE W-DC-REFILL-KVANT                             
320000                                    TO BER-BEHOV(BER-IY, BER-IX)          
320100                     ELSE                                                 
320200                       MOVE BER-IY                                        
320300                                    TO WS-BER-IY                          
320400                       MOVE BER-IX                                        
320500                                    TO WS-BER-IX                          
320600                       IF WS-BER-IX > 1                                   
320700                         SUBTRACT 1   FROM WS-BER-IX                      
320800                       ELSE                                               
320900                         IF WS-BER-IY > 1                                 
321000                           SUBTRACT 1 FROM WS-BER-IY                      
321100                           MOVE 52      TO WS-BER-IX                      
321200                         END-IF                                           
321300                       END-IF                                             
321400                       MOVE W-DC-REFILL-KVANT                             
321500                               TO BER-BEHOV(WS-BER-IY, WS-BER-IX)         
321600                     END-IF                                               
321700                                                                          
321800                  ELSE                                                    
321900                     MOVE W-DC-REFILL-KVANT                               
322000                               TO BER-BEHOV(BER-IY, BER-IX)               
322100                  END-IF                                                  
322200                  ADD W-DC-REFILL-KVANT                                   
322300                               TO W-NDC-TILLGANG                          
322400               END-IF                                                     
322500             END-IF                                                       
322600           END-IF                                                         
322700                                                                          
322800           ADD 1               TO BER-IX                                  
322900                                  W-BER-DATUM                             
323000           MOVE +1             TO W-ANTAL-VECKOR                          
323100           CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                      
323200                                                                          
323300           MOVE 'AAVV  '       TO DAT-KDDATFORM                           
323400           MOVE W-AAVV         TO DAT-I-TIDATUM                           
323500                                  W-BINNDAY-AAVV                          
323600                                                                          
323700           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
323800                           DAT-O-TIDATUM DAT-KDSVAR                       
323900                                                                          
324000           IF DAT-KDSVAR-OK                                               
324100             MOVE DAT-TIAARP(3:2) TO W-PER                                
324200             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
324300           ELSE                                                           
324400               STRING ' FEL FRÅN WDATKONV I W2222200 '                    
324500                      '(HB-, W-AAVV, BER-IY )'                            
324600               DELIMITED BY SIZE INTO FELTEXT-STR                         
324700               DISPLAY FELTEXT                                            
324800               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
324900           END-IF                                                         
325000                                                                          
325100         END-PERFORM                                                      
325200                                                                          
325300         ADD 1                    TO BER-IY                               
325400         MOVE 1                   TO BER-IX                               
325500                                     IX-PER                               
325600         SUBTRACT +52           FROM W-BER-DATUM                          
325700         ADD +100                 TO W-BER-DATUM                          
325800     END-PERFORM                                                          
325900     .                                                                    
326000     EJECT                                                                
326100*HC-JUSTERA-NDC-TILLGANG  SECTION.                                        
326200*AD  MOVE 'HC-JUSTERA-NDC-TILLGANG '  TO CURRENT-SECTION                  
326300*AD                                                                       
326400*AD  IF WS-ERS-FINNS-K611 = JA                                            
326500*AD    IF ERS-CLAG-KDERS = 01 OR 11 OR 17 OR 21 OR 27                     
326600*AD       MOVE ERS-ART-IDARTNR  TO W-IDARTNR-ERS                          
326700*AD       MOVE SLAG-IDDC        TO W-IDDC-ERS                             
326800*AD       PERFORM IMS-GU-WDK711-ERS                                       
326900*AD       IF  SEGMENT-FINNS                                               
327000*AD           COMPUTE W-NDC-TILLGANG-ERS ROUNDED =                        
327100*AD                   ERS-SLAG-KVLS                                       
327200*AD                +  ERS-SLAG-KVAKS-PAV                                  
327300*AD                +  ERS-SLAG-KVAKS-SDC                                  
327400*AD                +  ERS-SLAG-KVBEART                                    
327500*AD                -  ERS-SLAG-KVROS-DAG                                  
327600*AD                -  ERS-SLAG-KVROS-BULK                                 
327700*AD                -  ERS-SLAG-KVRESS                                     
327800*AD                -  ERS-SLAG-KVOKS-BULK                                 
327900*AD                -  ERS-SLAG-KVOKS-DAG                                  
328000*AD           ADD W-NDC-TILLGANG-ERS TO W-NDC-TILLGANG                    
328100*AD       END-IF                                                          
328200*AD    END-IF                                                             
328300*AD  END-IF                                                               
328400*AD  .                                                                    
328500*AD  EJECT                                                                
328600 HD-LAES-WDK711         SECTION.                                          
328700     MOVE 'HD-LAES-WDK711  '  TO CURRENT-SECTION                          
328800******************************************************************        
328900*                                                                *        
329000*    LÄSER WDK711                                                *        
329100*                                                                *        
329200******************************************************************        
329300     IF LINK-IDDC = SPACE OR ZERO                                         
329400        MOVE '4A'            TO W-IDDC-MIN                                
329500        MOVE '89'            TO W-IDDC-MAX                                
329600     ELSE                                                                 
329700        MOVE LINK-IDDC       TO W-IDDC-MIN                                
329800        MOVE LINK-IDDC       TO W-IDDC-MAX                                
329900     END-IF                                                               
330000                                                                          
330100     MOVE WC-CDC-SE          TO W-IDDC-REF                                
330200     PERFORM IMS-GNP-WDK711                                               
330300     IF SEGMENT-FINNS                                                     
330400       MOVE SLAG-IDDC TO W-IDDC-B6                                        
330500       PERFORM S13-READ-OR-TAB-B601                                       
330600     END-IF                                                               
330700                                                                          
330800     PERFORM UNTIL SEGMENT-SAKNAS                                         
330900     OR  (SLAG-KDREFSTA     = 'A'                                         
331000     AND  SLAG-FLREFILL     = JA                                          
331100     AND  SLAG-IDLEVNR      = '1441 '                                     
331200     AND (LINK-FLINKLDIRLEV = JA                                          
331300     OR  (LINK-FLINKLDIRLEV = NEJ                                         
331400     AND  SLAG-FLCDCBEH     = JA)))                                       
331500*--     LÄS FRAM TILL EN GILTIG NDC-POST                                  
331600*--                                                                       
331700        PERFORM IMS-GNP-WDK711                                            
331800        IF SEGMENT-FINNS                                                  
331900          MOVE SLAG-IDDC TO W-IDDC-B6                                     
332000          PERFORM S13-READ-OR-TAB-B601                                    
332100        END-IF                                                            
332200     END-PERFORM                                                          
332300                                                                          
332400     IF SEGMENT-FINNS                                                     
332500*** FIX FÖR NEGATIVA KVOKS-BULK                                           
332600        IF SLAG-KVOKS-BULK < 0                                            
332700          MOVE ZERO          TO SLAG-KVOKS-BULK                           
332800        END-IF                                                            
332900*** FIX FÖR NEGATIVA KVOKS-DAG                                            
333000        IF SLAG-KVOKS-DAG < 0                                             
333100          MOVE ZERO          TO SLAG-KVOKS-DAG                            
333200        END-IF                                                            
333300                                                                          
333400*** KINA HAR 6 DAGARS ARBETSVECKA                                         
333500        MOVE NEJ TO 6ARBDAG-SW                                            
333600        IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA) OR DCS-JAPAN             
333700           OR DCS-ENGLAND OR DCS-INDIA OR DCS-EMIRATES                    
333800          MOVE JA TO 6ARBDAG-SW                                           
333900        END-IF                                                            
334000     END-IF                                                               
334100     .                                                                    
334200     EJECT                                                                
334300 I-BERAKNA-PB-PLAN SECTION.                                               
334400     MOVE 'I-BERAKNA-PB-PLAN  '  TO CURRENT-SECTION                       
334500                                                                          
334600     PERFORM S01-NOLLSTALL-BERAKNINGS-TAB                                 
334700                                                                          
334800     COMPUTE W-DAGAR-KVAR = 5 - LINK-TID-AKTUELL                          
334900     IF W-TIME(1:2)         > 05 AND                                      
335000        W-TIME(1:2)         < 17                                          
335100         ADD +1             TO W-DAGAR-KVAR                               
335200     END-IF                                                               
335300                                                                          
335400     MOVE W-BER-START-AA     TO W-DATUM-AA                                
335500     MOVE W-BER-START-VV     TO W-DATUM-VV                                
335600     MOVE W-DATUM            TO W-AAVV                                    
335700     MOVE -1                 TO W-ANTAL-VECKOR                            
335800     CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                            
335900                                                                          
336000     MOVE 'AAVV'             TO DAT-KDDATFORM                             
336100     MOVE W-AAVV             TO DAT-I-TIDATUM                             
336200                                                                          
336300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
336400                         DAT-O-TIDATUM DAT-KDSVAR                         
336500                                                                          
336600     IF DAT-KDSVAR-OK                                                     
336700****    HÄMTA DAT-TIRP                                                    
336800        CONTINUE                                                          
336900                                                                          
337000     ELSE                                                                 
337100         STRING ' FEL FRÅN WDATKONV I W2222200 '                          
337200                '(I-BERAKNA, W-AAVV )'                                    
337300         DELIMITED BY SIZE INTO FELTEXT-STR                               
337400         DISPLAY FELTEXT                                                  
337500         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
337600     END-IF                                                               
337700                                                                          
337800     MOVE +1                 TO BER-IY                                    
337900     MOVE W-BER-START-VV     TO BER-IX                                    
338000     MOVE LINK-TIBEHOV-START TO W-BER-DATUM                               
338100                                                                          
338200     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
338300                                                                          
338400         PERFORM UNTIL                                                    
338500            (BER-IY = BER-ANTAL-AA AND                                    
338600             BER-IX > BER-ANTAL-VV)                                       
338700         OR (BER-IY < BER-ANTAL-AA AND                                    
338800             BER-IX > 52)                                                 
338900                                                                          
339000             MOVE W-BER-DATUM     TO TMP1-YYWW                            
339100             MOVE W-TIFINLV-AAVV  TO TMP2-YYWW                            
339200             MOVE '169410'        TO FELTEXT2-STR                         
339300             PERFORM WY2000P3                                             
339400             IF  TMP1-YYWW < TMP2-YYWW                                    
339500                 MOVE ZERO TO BER-BEHOV (BER-IY, BER-IX)                  
339600             ELSE                                                         
339700                                                                          
339800                 MOVE 'AAVV' TO DAT-KDDATFORM                             
339900                 MOVE W-BER-DATUM                                         
340000                             TO DAT-I-TIDATUM                             
340100                                                                          
340200                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
340300                                     DAT-O-TIDATUM DAT-KDSVAR             
340400                                                                          
340500                 IF DAT-KDSVAR-OK                                         
340600****                HÄMTA DAT-TIRP                                        
340700                    CONTINUE                                              
340800                                                                          
340900                 ELSE                                                     
341000                   STRING ' FEL FRÅN WDATKONV I W2222200 '                
341100                          '(I-BERAKNA, W-BER-DATUM )'                     
341200                   DELIMITED BY SIZE INTO FELTEXT-STR                     
341300                   DISPLAY FELTEXT                                        
341400                   CALL ABEND USING RKOD-ABEND-UTAN-DUMP                  
341500                 END-IF                                                   
341600                                                                          
341700                 PERFORM IA-KOLLA-KVPB-PLAN-VECKA                         
341800                                                                          
341900                 IF CLAG-DASEASON > WS-DAGENS-DATUM                       
342000                 OR CLAG-DASEASON = WS-DAGENS-DATUM                       
342100******                                                                    
342200******    MANUELL SÄSONG ÄR SATT (GÄLLER FÖR DET TOTALA                   
342300******    SEPARAT- SAMT REFILLBEHOVEN)                                    
342400                                                                          
342500                     COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =         
342600                               WS-KVPB-PLAN-VECKA *                       
342700                                   CLAG-RESEASON-PLAN (DAT-TIRP)          
342800                 ELSE                                                     
342900******                                                                    
343000******    INGEN MANUELL SÄSONG ÄR SATT UTAN BERÄKNAS UTIFRÅN              
343100******    DET TOTALA SEPARAT- SAMT REFILLBEHOVEN                          
343200                                                                          
343300                     COMPUTE BER-BEHOV (BER-IY, BER-IX) ROUNDED =         
343400                               WS-KVPB-PLAN-VECKA * 1.00                  
343500******                             WS-RESEASON-AVR (DAT-TIRP)             
343600                 END-IF                                                   
343700                                                                          
343800             END-IF                                                       
343900                                                                          
344000             ADD 1           TO BER-IX                                    
344100                                W-BER-DATUM                               
344200         END-PERFORM                                                      
344300         ADD 1   TO BER-IY                                                
344400         MOVE 1  TO BER-IX                                                
344500                    IX-PER                                                
344600         SUBTRACT +52 FROM  W-BER-DATUM                                   
344700         ADD +100     TO    W-BER-DATUM                                   
344800     END-PERFORM                                                          
344900     PERFORM S10-ADD-TILL-RESULTAT-PBPLAN                                 
345000     .                                                                    
345100     EJECT                                                                
345200 IA-KOLLA-KVPB-PLAN-VECKA SECTION.                                        
345300     MOVE 'IA-KOLLA-KVPB-PLAN-VECKA' TO CURRENT-SECTION                   
345400******                                                                    
345500******    MANUELL PB-PLAN ÄR SATT (GÄLLER ISTÄLLET FÖR DET                
345600******    TOTALA SEPARAT- SAMT REFILLBEHOVEN)                             
345700******                                                                    
345800     IF  DAT-TIAAMMDD >= CLAG-TIPBPLAN-JUST1-FOM                          
345900     AND DAT-TIAAMMDD <= CLAG-TIPBPLAN-JUST1-TOM                          
346000       IF (CLAG-TIPBPLAN-JUST2-FOM > +0)                                  
346100      AND (CLAG-TIPBPLAN-JUST2-FOM < CLAG-TIPBPLAN-JUST1-TOM)             
346200      AND (DAT-TIAAMMDD           >= CLAG-TIPBPLAN-JUST2-FOM)             
346300          COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                            
346400                  CLAG-KVPB-PLAN-JUST2 / 4.33                             
346500       ELSE                                                               
346600          COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                            
346700                  CLAG-KVPB-PLAN-JUST1 / 4.33                             
346800       END-IF                                                             
346900     ELSE                                                                 
347000       IF  DAT-TIAAMMDD >= CLAG-TIPBPLAN-JUST2-FOM                        
347100       AND DAT-TIAAMMDD <= CLAG-TIPBPLAN-JUST2-TOM                        
347200            COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                          
347300                    CLAG-KVPB-PLAN-JUST2 / 4.33                           
347400       ELSE                                                               
347500         IF CLAG-DAPBPLAN >= WS-DAGENS-DATUM                              
347600            COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                          
347700                    CLAG-KVPB-PLAN / 4.33                                 
347800         ELSE                                                             
347900            IF CLAG-DASEASON < WS-DAGENS-DATUM                            
348000               COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                       
348100                       WS-KVPB / 4.33                                     
348200            ELSE                                                          
348300               COMPUTE WS-KVPB-PLAN-VECKA ROUNDED =                       
348400                       CLAG-KVPB-PLAN / 4.33                              
348500            END-IF                                                        
348600         END-IF                                                           
348700       END-IF                                                             
348800     END-IF                                                               
348900     .                                                                    
349000     EJECT                                                                
349100 K-BER-PBTOT-SASONG SECTION.                                              
349200     MOVE 'K-BER-PBTOT-SASONG  '  TO CURRENT-SECTION                      
349300                                                                          
349400****                                                                      
349500**** FLYTTA IN RESULTATET TILL EN                                         
349600**** ARBETSAREA DÄR VECKOBEHOVEN LIGGER FROM VECKA 1 TOM 52               
349700****                                                                      
349800                                                                          
349900     COMPUTE IX-TILL = WS-DAGENS-VECKA + 1                                
350000                                                                          
350100     MOVE +1                 TO IX-FRAN                                   
350200     PERFORM UNTIL IX-TILL   >  52                                        
350300        ADD  LINK-KVBEHOV-VECKA(IX-FRAN)                                  
350400                             TO WS-KVBEHOV-VECKA (IX-TILL)                
350500                                WS-ARSTOTAL                               
350600        ADD +1               TO IX-FRAN                                   
350700                                IX-TILL                                   
350800     END-PERFORM                                                          
350900                                                                          
351000     MOVE +1                 TO IX-TILL                                   
351100     PERFORM UNTIL IX-FRAN   >  52                                        
351200        ADD  LINK-KVBEHOV-VECKA(IX-FRAN)                                  
351300                             TO WS-KVBEHOV-VECKA (IX-TILL)                
351400                                WS-ARSTOTAL                               
351500        ADD +1               TO IX-FRAN                                   
351600                                IX-TILL                                   
351700     END-PERFORM                                                          
351800                                                                          
351900****                                                                      
352000**** PROGNOSBEHOVET SPEGLAR BEHOVET FÖR EN 6-VECKORSPERIOD                
352100****                                                                      
352200                                                                          
352300     COMPUTE WS-KVPB ROUNDED = WS-ARSTOTAL / 12                           
352400                                                                          
352500     MOVE ZERO               TO WS-RESEASON-TOT                           
352600                                WS-JUSTERA                                
352700                                                                          
352800     MOVE +1                 TO IX-VECKA                                  
352900                                IX-PER                                    
353000     PERFORM UNTIL IX-PER > +12                                           
353100                                                                          
353200       MOVE ZERO             TO WS-KVBEHOV-PER                            
353300                                                                          
353400       PERFORM UNTIL IX-VECKA > PER-SLUT-VV (IX-PER)                      
353500                                                                          
353600         ADD WS-KVBEHOV-VECKA (IX-VECKA)                                  
353700                             TO WS-KVBEHOV-PER                            
353800         ADD +1              TO IX-VECKA                                  
353900       END-PERFORM                                                        
354000                                                                          
354100****                                                                      
354200****   RÄKNA UT SÄSONGSINDEX                                              
354300****                                                                      
354400                                                                          
354500       COMPUTE WS-RESEASON (IX-PER) ROUNDED =                             
354600               WS-KVBEHOV-PER / WS-KVPB                                   
354700                ON SIZE ERROR                                             
354800                    MOVE ZERO     TO WS-RESEASON (IX-PER)                 
354900       END-COMPUTE                                                        
355000       COMPUTE WS-RESEASON-AVR (IX-PER) ROUNDED =                         
355100               WS-RESEASON (IX-PER) * +1                                  
355200       ADD WS-RESEASON-AVR (IX-PER)                                       
355300                             TO WS-RESEASON-TOT                           
355400****   WS-KVBEHOV-PER-TEST ANVÄNDS ENBART I TESTSYFTE                     
355500       MOVE WS-KVBEHOV-PER   TO WS-KVBEHOV-PER-TEST (IX-PER)              
355600       ADD +1                TO IX-PER                                    
355700                                                                          
355800     END-PERFORM                                                          
355900                                                                          
356000****   WS-KVBEHOV-PER-TEST ANVÄNDS ENBART I TESTSYFTE                     
356100*????MOVE WS-KVBEHOV-PER     TO WS-KVBEHOV-PER-TEST (IX-PER)?IX=13        
356200                                                                          
356300     IF WS-RESEASON-TOT < +10.00                                          
356400     OR WS-RESEASON-TOT > +14.00                                          
356500       MOVE +12.00           TO WS-RESEASON-TOT                           
356600       MOVE +1               TO IX-PER                                    
356700       PERFORM UNTIL IX-PER > +12                                         
356800         MOVE +1             TO WS-RESEASON-AVR (IX-PER)                  
356900         ADD +1              TO IX-PER                                    
357000       END-PERFORM                                                        
357100     ELSE                                                                 
357200                                                                          
357300       IF WS-RESEASON-TOT > +12.00                                        
357400         MOVE -0.01          TO WS-JUSTERA                                
357500       ELSE                                                               
357600         MOVE +0.01          TO WS-JUSTERA                                
357700       END-IF                                                             
357800                                                                          
357900       PERFORM UNTIL WS-RESEASON-TOT = +12.00                             
358000       OR          WS-RESEASON-TOT = ZERO                                 
358100                                                                          
358200         MOVE +1             TO IX-PER                                    
358300         PERFORM UNTIL WS-RESEASON-TOT = +12.00                           
358400         OR IX-PER > +12                                                  
358500           IF WS-RESEASON-AVR (IX-PER) > +0.01                            
358600             ADD WS-JUSTERA  TO WS-RESEASON-AVR (IX-PER)                  
358700                                  WS-RESEASON-TOT                         
358800           END-IF                                                         
358900           ADD +1            TO IX-PER                                    
359000         END-PERFORM                                                      
359100       END-PERFORM                                                        
359200     END-IF                                                               
359300     .                                                                    
359400     EJECT                                                                
359500 X-READ-OR-TAB-B616-CDC  SECTION.                                         
359600     MOVE 'X-READ-OR-TAB-B616-CDC '  TO CURRENT-SECTION                   
359700                                                                          
359800     MOVE CLAG-IDDC-REF  TO W-IDDC-REF-CDC                                
359900     MOVE SPACE          TO SPAR-REF-IDDC-REF                             
360000                                                                          
360100     IF CDC-TAB-IDDC-REF(1) = SPACE                                       
360200*--TAB IS EMPTY (FIRST CALL)                                              
360300       PERFORM IMS-GU-WDB616-CDC                                          
360400       MOVE DC11-REF-IDDC-REF        TO CDC-TAB-IDDC-REF(1)               
360500                                        SPAR-REF-IDDC-REF                 
360600       MOVE DC11-REF-KVDLTID-AIRETA  TO CDC-TAB-KVDLTID-AIRETA(1)         
360700       MOVE DC11-REF-KVDLTID-TOT     TO CDC-TAB-KVDLTID-TOT(1)            
360800     ELSE                                                                 
360900       MOVE +1 TO C-B616-IX                                               
361000       PERFORM UNTIL C-B616-IX > MAX-C-B616-IX                            
361100         IF CDC-TAB-IDDC-REF(C-B616-IX) = CLAG-IDDC-REF                   
361200            MOVE CDC-TAB-IDDC-REF(C-B616-IX)  TO SPAR-REF-IDDC-REF        
361300*--ALREADY SAVED.                                                         
361400           MOVE MAX-C-B616-IX TO C-B616-IX                                
361500         ELSE                                                             
361600           IF CDC-TAB-IDDC-REF(C-B616-IX) = SPACE                         
361700*--NO MATCH. SAVE A NEW IDDC-REF IN TABEL                                 
361800             PERFORM IMS-GU-WDB616-CDC                                    
361900             MOVE DC11-REF-IDDC-REF    TO SPAR-REF-IDDC-REF               
362000             MOVE DC11-REF-IDDC-REF    TO                                 
362100                                 CDC-TAB-IDDC-REF(C-B616-IX)              
362200             MOVE DC11-REF-KVDLTID-AIRETA  TO                             
362300                                 CDC-TAB-KVDLTID-AIRETA(C-B616-IX)        
362400             MOVE DC11-REF-KVDLTID-TOT     TO                             
362500                                 CDC-TAB-KVDLTID-TOT(C-B616-IX)           
362600             MOVE MAX-C-B616-IX TO C-B616-IX                              
362700           END-IF                                                         
362800         END-IF                                                           
362900         ADD +1 TO C-B616-IX                                              
363000       END-PERFORM                                                        
363100       IF SPAR-REF-IDDC-REF = CLAG-IDDC-REF                               
363200*--OK. WE GOT A MATCH                                                     
363300          CONTINUE                                                        
363400       ELSE                                                               
363500*--NO MATCH. INDICATES THAT THE CHART TO SMALL.                           
363600*--THERE ARE MORE THEN 90 CDC/DC-REF COMBINATIONS!!                       
363700         MOVE 'NO MATCH = TOO SMALL CDC-TABLE(90)' TO FELTEXT-STR         
363800         DISPLAY FELTEXT                                                  
363900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
364000       END-IF                                                             
364100     END-IF                                                               
364200     .                                                                    
364300     EJECT                                                                
364400 S01-NOLLSTALL-BERAKNINGS-TAB SECTION.                                    
364500     MOVE 'S01-NOLLSTALL-BERAKNINGS-TAB'  TO CURRENT-SECTION              
364600                                                                          
364700     MOVE 1               TO IY                                           
364800     PERFORM UNTIL IY     >  BER-MAX-AA                                   
364900     MOVE 1               TO IX                                           
365000         PERFORM UNTIL IX > BER-MAX-VV                                    
365100             MOVE ZERO    TO BER-BEHOV (IY, IX)                           
365200             ADD 1        TO IX                                           
365300         END-PERFORM                                                      
365400         ADD 1            TO IY                                           
365500     END-PERFORM                                                          
365600     .                                                                    
365700     EJECT                                                                
365800 S02-NOLLSTALL-RESULTAT-TAB SECTION.                                      
365900     MOVE 'S02-NOLLSTALL-RESULTAT-TAB'  TO CURRENT-SECTION                
366000                                                                          
366100     MOVE 1               TO IY                                           
366200     PERFORM UNTIL IY     >  RES-MAX-AA                                   
366300     MOVE 1 TO IX                                                         
366400         PERFORM UNTIL IX >  RES-MAX-VV                                   
366500             MOVE ZERO    TO RES-BEHOV (IY, IX)                           
366600             ADD 1        TO IX                                           
366700         END-PERFORM                                                      
366800         ADD 1            TO IY                                           
366900     END-PERFORM                                                          
367000     .                                                                    
367100     EJECT                                                                
367200 S03-ADD-TILL-RESULTAT-TAB SECTION.                                       
367300     MOVE 'S03-ADD-TILL-RESULTAT-TAB'  TO CURRENT-SECTION                 
367400                                                                          
367500     MOVE 1                TO IY                                          
367600     PERFORM UNTIL IY      >  BER-MAX-AA                                  
367700         MOVE 1            TO IX                                          
367800         PERFORM UNTIL IX  > BER-MAX-VV                                   
367900             ADD BER-BEHOV (IY, IX) TO RES-BEHOV (IY, IX)                 
368000             ADD 1         TO IX                                          
368100         END-PERFORM                                                      
368200         ADD 1             TO IY                                          
368300     END-PERFORM                                                          
368400     .                                                                    
368500     EJECT                                                                
368600 S04-BERAKNA-BERTAB-INDEX SECTION.                                        
368700     MOVE 'S04-BERAKNA-BERTAB-INDEX '  TO CURRENT-SECTION                 
368800******************************************************************        
368900*                                                                *        
369000*    MED UTGÅNGSPUNKT FRÅN DATUM I W-DATUM BERÄKNAS              *        
369100*    ÅRS- OCH VECKO-INDEX FÖR BERÄKNINGSTABELLEN                 *        
369200*                                                                *        
369300******************************************************************        
369400                                                                          
369500     MOVE W-BER-START-AA  TO TMP1-YY                                      
369600     MOVE W-DATUM-AA      TO TMP2-YY                                      
369700     PERFORM WY2000P9                                                     
369800     COMPUTE W-ANTAL-VECKOR = W-DATUM-VV                                  
369900                            + (TMP2-YY - TMP1-YY) * 52                    
370000                                                                          
370100     MOVE +100         TO W-DATUM-AAVV                                    
370200     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
370300     MOVE W-DATUM-AAVV TO W-DATUM                                         
370400     MOVE W-DATUM-AA   TO BER-IY                                          
370500     MOVE W-DATUM-VV   TO BER-IX                                          
370600     .                                                                    
370700     EJECT                                                                
370800 S05-BERAKNA-Q-KVANT    SECTION.                                          
370900     MOVE 'S05-BERAKNA-Q-KVANT '  TO CURRENT-SECTION                      
371000                                                                          
371100     MOVE ZERO               TO WS-KVANT-QX                               
371200     MOVE W-DC-REFILL-KVANT  TO WS-ANTAL-QX                               
371300                                                                          
371400     IF CLAG-KVQPACK-4 > 1                                                
371500     OR CLAG-KVQPACK-3 > 1                                                
371600     OR CLAG-KVQPACK-2 > 1                                                
371700     OR CLAG-KVQPACK-1 > 1                                                
371800     OR CLAG-KVQPACK-0 > 1                                                
371900                                                                          
372000*KVQPACK-4                                                                
372100       IF CLAG-KVQPACK-4 > 1                                              
372200         IF (DCS-SDC AND NOT DCS-CHINA)                                   
372300           MOVE ZERO               TO WS-KVANT-QX-HELTAL                  
372400         ELSE                                                             
372500           COMPUTE WS-KVANT-QX =                                          
372600                   W-DC-REFILL-KVANT / CLAG-KVQPACK-4                     
372700           IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                         
372800             IF WS-KVANT-QX-HELTAL > ZERO                                 
372900               COMPUTE WS-ANTAL-QX =                                      
373000                       WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                
373100             END-IF                                                       
373200           ELSE                                                           
373300             COMPUTE WS-KVANT-QX-HELTAL =                                 
373400                     WS-KVANT-QX-HELTAL + 1                               
373500             COMPUTE WS-ANTAL-QX =                                        
373600                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                  
373700           END-IF                                                         
373800         END-IF                                                           
373900       END-IF                                                             
374000                                                                          
374100*KVQPACK-3                                                                
374200       IF CLAG-KVQPACK-3 > 1  AND                                         
374300          WS-KVANT-QX-HELTAL = ZERO                                       
374400         COMPUTE WS-KVANT-QX =                                            
374500                 W-DC-REFILL-KVANT / CLAG-KVQPACK-3                       
374600         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
374700           IF WS-KVANT-QX-HELTAL > ZERO                                   
374800             COMPUTE WS-ANTAL-QX =                                        
374900                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-3                  
375000           END-IF                                                         
375100         ELSE                                                             
375200           COMPUTE WS-KVANT-QX-HELTAL =                                   
375300                   WS-KVANT-QX-HELTAL + 1                                 
375400           COMPUTE WS-ANTAL-QX =                                          
375500                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-3                    
375600         END-IF                                                           
375700       END-IF                                                             
375800                                                                          
375900*KVQPACK-2                                                                
376000       IF CLAG-KVQPACK-2 > 1 AND                                          
376100          WS-KVANT-QX-HELTAL = ZERO                                       
376200         COMPUTE WS-KVANT-QX =                                            
376300                 W-DC-REFILL-KVANT / CLAG-KVQPACK-2                       
376400         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
376500           IF WS-KVANT-QX-HELTAL > ZERO                                   
376600             COMPUTE WS-ANTAL-QX =                                        
376700                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                  
376800           END-IF                                                         
376900         ELSE                                                             
377000           COMPUTE WS-KVANT-QX-HELTAL =                                   
377100                   WS-KVANT-QX-HELTAL + 1                                 
377200           COMPUTE WS-ANTAL-QX =                                          
377300                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                    
377400         END-IF                                                           
377500       END-IF                                                             
377600                                                                          
377700*KVQPACK-0                                                                
377800       IF CLAG-KVQPACK-0 > 1   AND                                        
377900          WS-KVANT-QX-HELTAL = ZERO                                       
378000         COMPUTE WS-KVANT-QX =                                            
378100                 W-DC-REFILL-KVANT / CLAG-KVQPACK-0                       
378200         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
378300           IF WS-KVANT-QX-HELTAL < 1                                      
378400             IF CLAG-KVQPACK-1 > 0                                        
378500               PERFORM S05B-KVQPACK-1                                     
378600             ELSE                                                         
378700               MOVE W-DC-REFILL-KVANT TO WS-ANTAL-QX                      
378800             END-IF                                                       
378900           ELSE                                                           
379000             COMPUTE WS-ANTAL-QX =                                        
379100                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                  
379200           END-IF                                                         
379300         ELSE                                                             
379400           COMPUTE WS-KVANT-QX-HELTAL =                                   
379500                   WS-KVANT-QX-HELTAL + 1                                 
379600           COMPUTE WS-ANTAL-QX =                                          
379700                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                    
379800         END-IF                                                           
379900       END-IF                                                             
380000                                                                          
380100*KVQPACK-1                                                                
380200       IF CLAG-KVQPACK-1 > 1                                              
380300          IF WS-KVANT-QX-HELTAL > ZERO                                    
380400             MOVE WS-ANTAL-QX TO W-DC-REFILL-KVANT                        
380500          END-IF                                                          
380600          PERFORM S05B-KVQPACK-1                                          
380700       END-IF                                                             
380800     END-IF                                                               
380900     MOVE WS-ANTAL-QX TO W-DC-REFILL-KVANT                                
381000                                                                          
381100     .                                                                    
381200     EJECT                                                                
381300                                                                          
381400 S05B-KVQPACK-1 SECTION.                                                  
381500     MOVE 'S05B-KVQPACK-1  '   TO CURRENT-SECTION                         
381600                                                                          
381700     COMPUTE WS-KVANT-QX =                                                
381800             W-DC-REFILL-KVANT / CLAG-KVQPACK-1                           
381900     IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                               
382000       IF WS-KVANT-QX-HELTAL < 1                                          
382100         COMPUTE WS-ANTAL-QX = 1 * CLAG-KVQPACK-1                         
382200       ELSE                                                               
382300         COMPUTE WS-ANTAL-QX =                                            
382400                 WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                      
382500       END-IF                                                             
382600     ELSE                                                                 
382700       COMPUTE WS-KVANT-QX-HELTAL =                                       
382800               WS-KVANT-QX-HELTAL + 1                                     
382900       COMPUTE WS-ANTAL-QX =                                              
383000               WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                        
383100     END-IF                                                               
383200     .                                                                    
383300     EJECT                                                                
383400                                                                          
383500 S06-SATT-QX-PROCENT-BRYTNING SECTION.                                    
383600     MOVE 'S06-SATT-QX-PROC-BRYTNING '  TO CURRENT-SECTION                
383700                                                                          
383800     MOVE DCS-REQXBRYT       TO WS-QX-BRYTNING                            
383900     .                                                                    
384000     EJECT                                                                
384100                                                                          
384200                                                                          
384300                                                                          
384400 S07-INIT-DATUM SECTION.                                                  
384500     MOVE 'S07-INIT-DATUM   '  TO CURRENT-SECTION                         
384600                                                                          
384700                                                                          
384800     MOVE JA TO LINK-FLJANEJ-ANROP                                        
384900     SKIP1                                                                
385000     MOVE LINK-TIBEHOV-START TO W-DATUM                                   
385100                                W-BER-START-DATUM                         
385200     MOVE W-DATUM-AA TO W-BER-START-AA                                    
385300     MOVE W-DATUM-VV TO W-BER-START-VV                                    
385400     MOVE LINK-TIBEHOV-START TO W-BER-SLUT-DATUM                          
385500     MOVE LINK-KVVECKOR-BEHOV TO W-ANTAL-VECKOR                           
385600     SUBTRACT 1 FROM W-ANTAL-VECKOR                                       
385700     CALL W009VADD USING W-BER-SLUT-DATUM W-ANTAL-VECKOR                  
385800     SKIP1                                                                
385900     MOVE +100 TO W-DATUM-AAVV                                            
386000     ADD  W-BER-START-VV TO W-DATUM-AAVV                                  
386100     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
386200     MOVE W-DATUM-AAVV TO W-DATUM                                         
386300     MOVE W-DATUM-AA TO RES-ANTAL-AA                                      
386400     MOVE W-DATUM-VV TO RES-ANTAL-VV                                      
386500******************************************************************        
386600*                                                                *        
386700*    YTTERLIGARE +7 VECKOR HAR LAGTS TILL PGA AV LEDTIDEN        *        
386800*    TILL NORDAMERIKA / STEFAN A, FRONTEC                        *        
386900*                                                                *        
387000******************************************************************        
387100*    ADD +7                  TO LINK-KVVECKOR-BEHOV                       
387200     IF  LINK-KVVECKOR-BEHOV > +156                                       
387300         MOVE +156 TO LINK-KVVECKOR-BEHOV                                 
387400     END-IF                                                               
387500     MOVE JA TO LINK-FLJANEJ-ANROP                                        
387600     SKIP1                                                                
387700     MOVE LINK-TIBEHOV-START TO W-DATUM                                   
387800                                W-BER-START-DATUM                         
387900     MOVE W-DATUM-AA TO W-BER-START-AA                                    
388000     MOVE W-DATUM-VV TO W-BER-START-VV                                    
388100     MOVE LINK-TIBEHOV-START TO W-BER-SLUT-DATUM                          
388200     MOVE LINK-KVVECKOR-BEHOV TO W-ANTAL-VECKOR                           
388300     SUBTRACT 1 FROM W-ANTAL-VECKOR                                       
388400     CALL W009VADD USING W-BER-SLUT-DATUM W-ANTAL-VECKOR                  
388500     SKIP1                                                                
388600     MOVE +100 TO W-DATUM-AAVV                                            
388700     ADD  W-BER-START-VV TO W-DATUM-AAVV                                  
388800     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
388900     MOVE W-DATUM-AAVV TO W-DATUM                                         
389000     MOVE W-DATUM-AA TO BER-ANTAL-AA                                      
389100     MOVE W-DATUM-VV TO BER-ANTAL-VV                                      
389200     .                                                                    
389300     EJECT                                                                
389400                                                                          
389500                                                                          
389600 S08SDC-JUSTERA-REFILLKVANT   SECTION.                                    
389700     MOVE 'S08SDC-JUSTERA-REFILLKVANT'  TO CURRENT-SECTION                
389800                                                                          
389900     IF SLAG-KVREFPKT - W-SDC-TILLGANG > W-KVBEHOV-30V                    
390000        CONTINUE                                                          
390100     ELSE                                                                 
390200        IF W-DC-REFILL-KVANT < W-KVBEHOV-30V                              
390300           CONTINUE                                                       
390400        ELSE                                                              
390500           MOVE W-KVBEHOV-30V TO W-DC-REFILL-KVANT                        
390600        END-IF                                                            
390700     END-IF                                                               
390800     .                                                                    
390900     EJECT                                                                
391000                                                                          
391100                                                                          
391200 S08NDC-JUSTERA-REFILLKVANT   SECTION.                                    
391300     MOVE 'S08NDC-JUSTERA-REFILLKVANT'  TO CURRENT-SECTION                
391400                                                                          
391500     IF SLAG-KVREFPKT - W-NDC-TILLGANG > W-KVBEHOV-52V                    
391600        CONTINUE                                                          
391700     ELSE                                                                 
391800        IF W-DC-REFILL-KVANT < W-KVBEHOV-52V                              
391900           CONTINUE                                                       
392000        ELSE                                                              
392100           MOVE W-KVBEHOV-52V TO W-DC-REFILL-KVANT                        
392200        END-IF                                                            
392300     END-IF                                                               
392400     .                                                                    
392500     EJECT                                                                
392600                                                                          
392700                                                                          
392800 S09-NOLLSTALL-RESULTAT-PBPLAN SECTION.                                   
392900     MOVE 'S09-NOLLSTALL-RESULTAT-PBPLAN'  TO CURRENT-SECTION             
393000*------------------------------------------------------------             
393100*--- DET BLIR FEL IX OM MAN HAR VECKA 52 + 2V = 54.                       
393200*--- DET BLIR FEL IX OM MAN HAR VECKA 53 + 2V = 55.                       
393300*--- STARTVECKA 53 BLIR ALLTID AA +1 OCH VV=01, SE SDC-BEHOV.             
393400*------------------------------------------------------------             
393500                                                                          
393600     IF W-BER-START-VV = +53                                              
393700       MOVE 2             TO IY                                           
393800       MOVE 1             TO IX                                           
393900     ELSE                                                                 
394000       MOVE 1               TO IY                                         
394100       MOVE W-BER-START-VV  TO IX                                         
394200     END-IF                                                               
394300     IF LINK-KDBEHOV = SEP-SATS-TPO-LEV-SDC-NDC                           
394400        CONTINUE                                                          
394500     ELSE                                                                 
394600        ADD  2              TO IX                                         
394700     END-IF                                                               
394800     PERFORM UNTIL IY     >  RES-MAX-AA                                   
394900         PERFORM UNTIL IX >  RES-MAX-VV                                   
395000             MOVE ZERO    TO RES-BEHOV (IY, IX)                           
395100             ADD 1        TO IX                                           
395200         END-PERFORM                                                      
395300         ADD 1            TO IY                                           
395400         IF IX = 54                                                       
395500           MOVE 2         TO IX                                           
395600         ELSE                                                             
395700           MOVE 1         TO IX                                           
395800         END-IF                                                           
395900     END-PERFORM                                                          
396000     .                                                                    
396100     EJECT                                                                
396200 S10-ADD-TILL-RESULTAT-PBPLAN SECTION.                                    
396300     MOVE 'S10-ADD-TILL-RESULTAT-PBPLAN'  TO CURRENT-SECTION              
396400                                                                          
396500     IF W-BER-START-VV = +53                                              
396600       MOVE 2              TO IY                                          
396700       MOVE 1              TO IX                                          
396800     ELSE                                                                 
396900       MOVE 1               TO IY                                         
397000       MOVE W-BER-START-VV  TO IX                                         
397100     END-IF                                                               
397200     IF LINK-KDBEHOV = SEP-SATS-TPO-LEV-SDC-NDC                           
397300        CONTINUE                                                          
397400     ELSE                                                                 
397500        ADD  2              TO IX                                         
397600     END-IF                                                               
397700     PERFORM UNTIL IY      >  BER-MAX-AA                                  
397800         PERFORM UNTIL IX  > BER-MAX-VV                                   
397900             ADD BER-BEHOV (IY, IX) TO RES-BEHOV (IY, IX)                 
398000             ADD 1         TO IX                                          
398100         END-PERFORM                                                      
398200         ADD 1             TO IY                                          
398300         IF IX = 54                                                       
398400           MOVE 2          TO IX                                          
398500         ELSE                                                             
398600           MOVE 1          TO IX                                          
398700         END-IF                                                           
398800     END-PERFORM                                                          
398900     .                                                                    
399000     EJECT                                                                
399100 S13-READ-OR-TAB-B601 SECTION.                                            
399200     MOVE 'S13-READ-OR-TAB-B601 '  TO CURRENT-SECTION                     
399300                                                                          
399400     IF TAB-DCS-IDDC(1) = LOW-VALUE                                       
399500*--TAB IS EMPTY (FIRST CALL)                                              
399600       MOVE SLAG-IDDC TO W-IDDC-B6                                        
399700                         WS-IDDC                                          
399800       PERFORM IMS-GU-WDB601                                              
399900       MOVE DCS-WDB601 TO TAB-DCS-WDB601 (1)                              
400000     ELSE                                                                 
400100       MOVE +1 TO B601-IX                                                 
400200       PERFORM UNTIL B601-IX > MAX-B601-IX                                
400300         IF TAB-DCS-IDDC(B601-IX) = SLAG-IDDC                             
400400*--ALREADY SAVED. MOVE TAB TO DLI-IO-WDB601                               
400500           MOVE TAB-DCS-WDB601 (B601-IX) TO DCS-WDB601                    
400600           MOVE MAX-B601-IX TO B601-IX                                    
400700         ELSE                                                             
400800           IF TAB-DCS-IDDC(B601-IX) = LOW-VALUE                           
400900*--NO MATCH. SAVE A NEW IDDC IN TABEL                                     
401000             MOVE SLAG-IDDC TO W-IDDC-B6                                  
401100                               WS-IDDC                                    
401200             PERFORM IMS-GU-WDB601                                        
401300             MOVE DCS-WDB601 TO TAB-DCS-WDB601 (B601-IX)                  
401400             MOVE MAX-B601-IX TO B601-IX                                  
401500           END-IF                                                         
401600         END-IF                                                           
401700         ADD +1 TO B601-IX                                                
401800       END-PERFORM                                                        
401900       IF DCS-IDDC NOT = SLAG-IDDC                                        
402000*--NO MATCH. INDICATES THAT THE TABEL TO SMALL.                           
402100*--THERE ARE MORE THEN 80 XDC:S IN WDB601!!                               
402200         MOVE 'NO MATCH = TOO SMALL TABLE(80)' TO FELTEXT-STR             
402300         DISPLAY FELTEXT                                                  
402400         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
402500       END-IF                                                             
402600     END-IF                                                               
402700     .                                                                    
402800     EJECT                                                                
402900 S15-JUSTERA-XDC-TILLGANG  SECTION.                                       
403000     MOVE 'S15-JUSTERA-XDC-TILLGANG '  TO CURRENT-SECTION                 
403100                                                                          
403200     MOVE ZERO                  TO W-XDC-TILLGANG-ERS                     
403300                                                                          
403400     IF WS-ERS-FINNS-K611 = JA                                            
403500       IF ERS-CLAG-KDERS = 01 OR 11 OR 17 OR 21 OR 27                     
403600          MOVE ERS-ART-IDARTNR  TO W-IDARTNR-ERS                          
403700          MOVE SLAG-IDDC        TO W-IDDC-ERS                             
403800          PERFORM IMS-GU-WDK711-ERS                                       
403900          IF  SEGMENT-FINNS                                               
404000              COMPUTE W-XDC-TILLGANG-ERS ROUNDED =                        
404100                      ERS-SLAG-KVLS                                       
404200                   +  ERS-SLAG-KVAKS-PAV                                  
404300                   +  ERS-SLAG-KVAKS-SDC                                  
404400                   +  ERS-SLAG-KVBEART                                    
404500                   -  ERS-SLAG-KVROS-DAG                                  
404600                   -  ERS-SLAG-KVROS-BULK                                 
404700                   -  ERS-SLAG-KVRESS                                     
404800                   -  ERS-SLAG-KVOKS-BULK                                 
404900                   -  ERS-SLAG-KVOKS-DAG                                  
405000*AD           ADD W-NDC-TILLGANG-ERS TO W-NDC-TILLGANG                    
405100          END-IF                                                          
405200       END-IF                                                             
405300     END-IF                                                               
405400     .                                                                    
405500     EJECT                                                                
405600 S20-NOLLSTALL-W-SDC-BEHOV-REF SECTION.                                   
405700     MOVE 'S20-NOLLSTALL-W-SDC-BEHOV-REF'  TO CURRENT-SECTION             
405800                                                                          
405900     MOVE ZERO     TO  W-SDC-KVBEHOV-VECKA                                
406000                       W-SDC-TILLGANG                                     
406100                       W-SDC-TILLGANG-ERS                                 
406200                       W-XDC-TILLGANG-ERS                                 
406300                       W-SDC-KVBEHOV-DESSUTOM                             
406400                       W-SDC-DIFF                                         
406500                       W-SDC-KVAR-EFT-VECKA                               
406600                       W-SDC-KVAR-EFT-DAG                                 
406700                       W-SDC-KVAR-KVBEHOV-VECKA                           
406800                       W-SDC-ACC-KVBEHOV                                  
406900                       W-SDC-ACC-KVBEHOV-VECKA                            
407000                       W-KVBEHOV-INNEV-VECKA                              
407100                       W-DC-REFILL-KVANT                                  
407200                       W-SDC-KVBEHOV-DAG                                  
407300     .                                                                    
407400     EJECT                                                                
407500 S21-NOLLSTALL-W-NDC-BEHOV-REF SECTION.                                   
407600     MOVE 'S21-NOLLSTALL-W-NDC-BEHOV-REF'  TO CURRENT-SECTION             
407700                                                                          
407800     MOVE ZERO     TO W-NDC-ACC-KVBEHOV                                   
407900                      W-NDC-KVBEHOV-VECKA                                 
408000                      W-NDC-KVBEHOV-DAG                                   
408100                      W-NDC-TILLGANG                                      
408200                      W-NDC-TILLGANG-ERS                                  
408300                      W-XDC-TILLGANG-ERS                                  
408400                      W-NDC-KVAR-EFT-VECKA                                
408500                      W-NDC-KVAR-EFT-DAG                                  
408600                      W-NDC-KVBEHOV-DESSUTOM                              
408700                      W-NDC-DIFF                                          
408800                      W-NDC-ACC-KVBEHOV-VECKA                             
408900                      W-NDC-KVAR-KVBEHOV-VECKA                            
409000                      W-KVBEHOV-INNEV-VECKA                               
409100                      W-DC-REFILL-KVANT                                   
409200                                                                          
409300     .                                                                    
409400     EJECT                                                                
409500 S22-ADJUST-LOW-DEMAND  SECTION.                                          
409600                                                                          
409700     MOVE NEJ                       TO SW-DEMAND-ADJUST                   
409800     IF W-JUST-PB-FINNS = NEJ                                             
409900        IF  SLAG-KVREFPKT      = +1                                       
410000        AND SLAG-KVREFBER      = +1                                       
410100        AND CLAG-KVQPACK-1     < 2                                        
410200        AND W-DC-REFILL-KVANT  >= +1                                      
410300            SET DEMAND-ADJUST       TO TRUE                               
410400        END-IF                                                            
410500     ELSE                                                                 
410600        IF  REFL-KVREFPKT      = +1                                       
410700        AND REFL-KVREFBER      = +1                                       
410800        AND CLAG-KVQPACK-1     < 2                                        
410900        AND W-DC-REFILL-KVANT  >= +1                                      
411000            SET DEMAND-ADJUST       TO TRUE                               
411100        END-IF                                                            
411200     END-IF                                                               
411300     .                                                                    
411400     EJECT                                                                
411500 S23-GET-WEEK-DEMAND-XDC SECTION.                                         
411600     MOVE 'S23-GET-WEEK-DEMAND-XDC' TO CURRENT-SECTION                    
411700                                                                          
411800     INITIALIZE UTUP-W271UTUP                                             
411900     MOVE NEJ                       TO W-JUST-PB-FINNS                    
412000     MOVE LINK-IDARTNR              TO UTUP-IDARTNR                       
412100     MOVE SLAG-IDDC                 TO UTUP-IDDC                          
412200     MOVE SLAG-IDDC-REF             TO UTUP-IDDC-REF                      
412300     MOVE W-BINNDAY                 TO UTUP-TIAAMMDD                      
412400     MOVE 003                       TO UTUP-KDCALL                        
412500*                                                                         
412600     CALL W271UTUP USING UTUP-W271UTUP                                    
412700                         UTUP-WDK7-PCB                                    
412800                         UTUP-WDB6-PCB                                    
412900                         UTUP-UTIL-WDK6-PCB                               
413000                         UTUP-UTIL-WDK7-PCB                               
413100                         UTUP-UTIL-WDB6-PCB                               
413200     IF UTUP-KDSVAR-OK                                                    
413300        IF UTUP-FLPB-JUST = JA                                            
413400           MOVE JA                  TO W-JUST-PB-FINNS                    
413500        END-IF                                                            
413600     ELSE                                                                 
413700        MOVE 'FEL FRÅN W22222 S23- '                                      
413800                                    TO FELTEXT-STR                        
413900        DISPLAY FELTEXT                                                   
414000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
414100     END-IF                                                               
414200     .                                                                    
414300     EJECT                                                                
414400 S30-W271REFL-HAEMTA-PRARTBES SECTION.                                    
414500     MOVE 'S30-W271REFL-HAEMTA-PRARTBES'  TO CURRENT-SECTION              
414600                                                                          
414700     MOVE ZERO TO WS-PRARTBES                                             
414800                                                                          
414900     IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA) OR                          
415000        DCS-NDC-NA                                                        
415100*-- FÖR KINA XDC:ER GÄLLER MATERIALPRIS FRÅN K712 SOM BESTPRIS            
415200*** FÖR USA/CA NDC  GÄLLER MATERIALPRIS FRÅN K712 SOM BESTPRIS            
415300       MOVE 1 TO K712-IX                                                  
415400       PERFORM UNTIL K712-IX > K712-IX-MAX                                
415500         IF K712-IDLANDX2(K712-IX) = DCS-IDLANDX2                         
415600                                                                          
415700           MOVE K712-PRMATRL(K712-IX) TO WS-PRARTBES                      
415800           COMPUTE K712-IX = K712-IX-MAX + 1                              
415900         ELSE                                                             
416000           ADD 1 TO K712-IX                                               
416100           IF K712-IX > K712-IX-MAX                                       
416200            STRING 'S30 K712-IX SAKNAS FÖR NDC-XX: ' REFL-IDDC            
416300             DELIMITED BY SIZE INTO FELTEXT-STR                           
416400             CALL ABEND USING RKOD-ABEND-UTAN-DUMP                        
416500           END-IF                                                         
416600         END-IF                                                           
416700       END-PERFORM                                                        
416800     ELSE                                                                 
416900       MOVE CLAG-PRARTSTD         TO WS-PRARTBES                          
417000     END-IF                                                               
417100     .                                                                    
417200     EJECT                                                                
417300 S34-GET-B616-LEDTID-CDC   SECTION.                                       
417400     MOVE 'S34-GET-B616-LEDTID-CDC  '  TO CURRENT-SECTION                 
417500                                                                          
417600     MOVE +1 TO C-B616-IX                                                 
417700     PERFORM UNTIL CDC-TAB-IDDC-REF(C-B616-IX) = CLAG-IDDC-REF            
417800                OR CDC-TAB-IDDC-REF(C-B616-IX) = SPACE                    
417900        ADD +1 TO C-B616-IX                                               
418000     END-PERFORM                                                          
418100                                                                          
418200     .                                                                    
418300     EJECT                                                                
418400 S33-READ-OR-TAB-B616 SECTION.                                            
418500     MOVE 'S33-READ-OR-TAB-B616 '  TO CURRENT-SECTION                     
418600                                                                          
418700     IF TAB-IDDC(1) = LOW-VALUE                                           
418800*--TAB IS EMPTY (FIRST CALL)                                              
418900       MOVE SLAG-IDDC      TO TAB-IDDC(1)                                 
419000       MOVE SLAG-IDDC-REF  TO W-IDDC-REF                                  
419100       PERFORM IMS-GU-WDB616                                              
419200       MOVE REF-WDB616 TO TAB-REF-WDB616 (1)                              
419300     ELSE                                                                 
419400       MOVE +1 TO B616-IX                                                 
419500       PERFORM UNTIL B616-IX > MAX-B616-IX                                
419600         IF TAB-REF-IDDC-REF(B616-IX) = SLAG-IDDC-REF AND                 
419700            TAB-IDDC(B616-IX) = SLAG-IDDC                                 
419800*--ALREADY SAVED. MOVE TAB TO DLI-IO-WDB616                               
419900           MOVE TAB-REF-WDB616 (B616-IX) TO REF-WDB616                    
420000           MOVE MAX-B616-IX TO B616-IX                                    
420100         ELSE                                                             
420200           IF TAB-IDDC(B616-IX) = LOW-VALUE                               
420300*--NO MATCH. SAVE A NEW IDDC IN TABEL                                     
420400             MOVE SLAG-IDDC      TO TAB-IDDC(B616-IX)                     
420500             MOVE SLAG-IDDC-REF  TO W-IDDC-REF                            
420600             PERFORM IMS-GU-WDB616                                        
420700             MOVE REF-WDB616 TO TAB-REF-WDB616 (B616-IX)                  
420800             MOVE MAX-B616-IX TO B616-IX                                  
420900           END-IF                                                         
421000         END-IF                                                           
421100         ADD +1 TO B616-IX                                                
421200       END-PERFORM                                                        
421300       IF DCS-IDDC     = SLAG-IDDC     AND                                
421400          REF-IDDC-REF = SLAG-IDDC-REF                                    
421500*--OK. WE GOT A MATCH                                                     
421600          CONTINUE                                                        
421700       ELSE                                                               
421800*--NO MATCH. INDICATES THAT THE CHART TO SMALL.                           
421900*--THERE ARE MORE THEN 180 DC/DC-REF COMBINATIONS!!                       
422000         MOVE 'NO MATCH = TOO SMALL TABLE(180)' TO FELTEXT-STR            
422100         DISPLAY FELTEXT                                                  
422200         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
422300       END-IF                                                             
422400     END-IF                                                               
422500     .                                                                    
422600     EJECT                                                                
422700*S65-KOLLA-ERS-DAT-CDC  SECTION.                                          
422800*AD  MOVE 'S65-KOLLA-ERS-DAT-CDC '  TO CURRENT-SECTION                    
422900*** -----------------------------------------------------------           
423000*** -- FÖR ERS X1,X2 OCH X7 SKALL MAN LÄGGA TILL LEDTIDEN FÖR             
423100*** -- REFILL FRÅN KINA TILL CDC (BÅT).EJ FÖR ERS X3.                     
423200*** --                                                                    
423300*** -- OM WDK629 REFILLSEGMENT SAKNAS (=FEL, SKALL ALLTID FINNAS)         
423400*** -- FLYTTA ERS.DAT UTAN HÄNSYN TILL LT.VILL EJ ABENDA I RTN            
423500*** -- W200V1.PRATA MED PATRIK.                                           
423600*** -----------------------------------------------------------           
423700                                                                          
423800*AD  MOVE CLAG-KDERS  TO WS-KDERS                                         
423900*AD                                                                       
424000*AD  IF WS-KDERS(2:1)  = 3                                                
424100*AD    MOVE WS-TISTOREF-AAVV    TO WS-TISTOREF-AAVV-3                     
424200*AD    MOVE -3                  TO W-ANTAL-VECKOR                         
424300*AD    CALL W009VADD USING  WS-TISTOREF-AAVV-3 W-ANTAL-VECKOR             
424400*AD  END-IF                                                               
424500*AD                                                                       
424600*AD  IF WS-KDERS(2:1) = 1 OR 2 OR 7                                       
424700*AD    IF WS-REF-FINNS-K629 = JA                                          
424800*AD      PERFORM S34-GET-B616-LEDTID-CDC                                  
424900*AD                                                                       
425000*AD      IF CREF-FLFLYG = JA                                              
425100*AD        MOVE CDC-TAB-KVDLTID-AIRETA(C-B616-IX)  TO DAYS-KVDAYS         
425200*AD      ELSE                                                             
425300*AD        MOVE CDC-TAB-KVDLTID-TOT(C-B616-IX)     TO DAYS-KVDAYS         
425400*AD      END-IF                                                           
425500*AD                                                                       
425600*AD      MOVE WS-TISTOREF-AAVVD        TO DAYS-TIDATE1                    
425700*AD      MOVE 'YYWWD'                  TO DAYS-KDDATFMT1                  
425800*AD      MOVE 'YYWWD'                  TO DAYS-KDDATFMT2                  
425900*AD      MOVE SPACE                    TO DAYS-TIDATE2                    
426000*AD                                       DAYS-IDCALEND                   
426100*AD                                                                       
426200*AD      CALL WZ20DAYS USING DAYS-WZ20DAYS                                
426300*AD                                                                       
426400*AD      IF DAYS-KDRC = 8                                                 
426500*AD        STRING 'FEL VID ANROP TILL WZ20DAYS '                          
426600*AD        DELIMITED BY SIZE INTO FELTEXT-STR                             
426700*AD        DISPLAY FELTEXT                                                
426800*AD        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
426900*AD      ELSE                                                             
427000*AD        MOVE DAYS-TIDATE2(1:5)       TO WS-TISTOREF-AAVVD              
427100*AD        MOVE WS-TISTOREF-AAVVD(1:4)  TO WS-TISTOREF-AAVV               
427200*AD                                        WS-TISTOREF-AAVV-3             
427300*AD        MOVE -3                      TO W-ANTAL-VECKOR                 
427400*AD        CALL W009VADD USING  WS-TISTOREF-AAVV-3 W-ANTAL-VECKOR         
427500*AD      END-IF                                                           
427600*AD    ELSE                                                               
427700*AD      MOVE WS-TISTOREF-AAVV    TO WS-TISTOREF-AAVV-3                   
427800*AD      MOVE -3                  TO W-ANTAL-VECKOR                       
427900*AD      CALL W009VADD USING  WS-TISTOREF-AAVV-3 W-ANTAL-VECKOR           
428000*AD    END-IF                                                             
428100*AD  END-IF                                                               
428200*AD  .                                                                    
428300*AD  EJECT                                                                
428400*S66-KOLLA-ERS-DAT SECTION.                                               
428500*AD  MOVE 'S66-KOLLA-ERS-DAT  '  TO CURRENT-SECTION                       
428600*AD                                                                       
428700*AD  MOVE ZERO               TO WS-START-AAVV                             
428800*AD                             WS-TISTOREF-AAVV                          
428900*AD  MOVE WS-DAGENS-TIAAAA(3:2)    TO WS-START-AA                         
429000*AD  MOVE W-BER-START-VV     TO WS-START-VV                               
429100*AD  IF CLAG-TISTOREF > 50000                                             
429200*AD    MOVE 01011            TO WS-TISTOREF-AAVVD                         
429300*AD  ELSE                                                                 
429400*AD    MOVE CLAG-TISTOREF    TO WS-TISTOREF-AAVVD                         
429500*AD  END-IF                                                               
429600*AD  MOVE WS-TISTOREF-AAVVD(1:4)  TO WS-TISTOREF-AAVV                     
429700*AD  MOVE WS-TISTOREF-AAVVD(1:2)  TO WS-TISTOREF-AA                       
429800*AD  COMPUTE WS-AR = WS-TISTOREF-AA - WS-START-AA                         
429900*AD  IF CLAG-TISTOREF > 0                                                 
430000*AD  AND WS-TISTOREF-AAVV > WS-START-AAVV                                 
430100*AD    IF WS-AR = 0                                                       
430200*AD     COMPUTE WS-ANTAL-VECKOR = WS-TISTOREF-AAVV - WS-START-AAVV        
430300*AD    ELSE                                                               
430400*AD     COMPUTE WS-KVVECKOR-KVAR-IAR = 52 - WS-START-VV                   
430500*AD     MOVE WS-TISTOREF-AAVVD(3:2) TO WS-KVVECKOR-ERSAR                  
430600*AD     IF WS-AR = 1                                                      
430700*AD       COMPUTE WS-ANTAL-VECKOR = WS-KVVECKOR-ERSAR                     
430800*AD                               + WS-KVVECKOR-KVAR-IAR                  
430900*AD     ELSE                                                              
431000*AD       MOVE +200 TO WS-ANTAL-VECKOR                                    
431100*AD     END-IF                                                            
431200*AD    END-IF                                                             
431300*AD    COMPUTE WS-ANTAL-VECKOR = WS-ANTAL-VECKOR + WS-START-VV            
431400*AD    IF WS-ANTAL-VECKOR > 0                                             
431500***DETTA FÖR ATT MAN SKALL STOPPA INLEVERANSER 2 VECKOR FÖRE              
431600***ERSÄTTNING  WS-ANTAL-VECKOR = WS-ANTAL-VECKOR - 3                      
431700*AD      COMPUTE WS-ANTAL-VECKOR = WS-ANTAL-VECKOR - 3                    
431800*AD      IF WS-ANTAL-VECKOR < 0                                           
431900*AD        MOVE 0 TO WS-ANTAL-VECKOR                                      
432000*AD      END-IF                                                           
432100*AD      COMPUTE WS-ANTAL-AR = WS-ANTAL-VECKOR / 52                       
432200*AD      COMPUTE WS-ANTAL-VECKOR-JUST = WS-ANTAL-VECKOR -                 
432300*AD              (WS-ANTAL-AR * 52)                                       
432400*AD    ELSE                                                               
432500*AD      MOVE ZERO          TO WS-ANTAL-AR                                
432600*AD                            WS-ANTAL-VECKOR-JUST                       
432700*AD    END-IF                                                             
432800*AD  ELSE                                                                 
432900*AD    IF CLAG-TISTOREF = 0                                               
433000*AD      MOVE  5            TO WS-ANTAL-AR                                
433100*AD      MOVE 52            TO WS-ANTAL-VECKOR-JUST                       
433200*AD    ELSE                                                               
433300*AD      MOVE ZERO          TO WS-ANTAL-AR                                
433400*AD                            WS-ANTAL-VECKOR-JUST                       
433500*AD    END-IF                                                             
433600*AD  END-IF                                                               
433700                                                                          
433800**** HÄR LÄGGER VI INDEX TILL ETT JOBB FÄLT SÅ ATT DET SKALL              
433900**** BLI LÄTTARE ATT JÄMFÖRA                                              
434000**** ADDERAR ETT ÅR EFTERSOM INDEX IY BÖRJAR PÅ +1                        
434100*AD  ADD +1         TO WS-ANTAL-AR                                        
434200*AD  MOVE WS-ANTAL-AR           TO ERS-BER-IY                             
434300*AD  MOVE WS-ANTAL-VECKOR-JUST  TO ERS-BER-IX                             
434400*AD  .                                                                    
434500*AD  EJECT                                                                
434600 S77-HITTA-K712-LAND SECTION.                                             
434700     MOVE 'S77-HITTA-K712-LAND '  TO CURRENT-SECTION                      
434800                                                                          
434900     MOVE 1 TO K712-IX                                                    
435000     PERFORM UNTIL K712-IDLANDX2(K712-IX) = DCS-IDLANDX2                  
435100                OR K712-IDLANDX2(K712-IX) = SPACE                         
435200        ADD 1 TO K712-IX                                                  
435300     END-PERFORM                                                          
435400     .                                                                    
435500     EJECT                                                                
435600 S80-CALC-LT-ADJ-PUBWK SECTION.                                           
435700     MOVE 'S80-CALC-LT-ADJ-PUBWK'  TO CURRENT-SECTION                     
435800                                                                          
435900     MOVE ZEROS                 TO WS-LTID                                
436000                                   WS-LTID-A                              
436100                                   WS-LTID-B                              
436200                                   WS-LT-WEEKS                            
436300                                   WS-LT-WEEKS-DAYS                       
436400                                   WS-REMN-DAYS                           
436500                                   W-TIFINLV-AAVV-MINUS-LT                
436600                                                                          
436700     PERFORM S33-READ-OR-TAB-B616                                         
436800                                                                          
436900     MOVE REF-KVDLTID-AIRETA    TO WS-LTID-A                              
437000     MOVE REF-KVDLTID-TOT       TO WS-LTID-B                              
437100                                                                          
437200*    DIVIDE BY 7 TO GET NUMBER OF WEEKS                                   
437300*    DECIMAL OR REMAINDER IS IGNORED IN BELOW COMPUTE                     
437400*                                                                         
437500     IF WS-FLFLYG = JA                                                    
437600        MOVE WS-LTID-A          TO WS-LTID                                
437700        DIVIDE  WS-LTID-A  BY 7                                           
437800                       GIVING WS-LT-WEEKS                                 
437900                    REMAINDER WS-REMN-DAYS                                
438000     ELSE                                                                 
438100        MOVE WS-LTID-B          TO WS-LTID                                
438200        DIVIDE  WS-LTID-B  BY 7                                           
438300                       GIVING WS-LT-WEEKS                                 
438400                    REMAINDER WS-REMN-DAYS                                
438500     END-IF                                                               
438600*                                                                         
438700     IF WS-REMN-DAYS > ZERO                                               
438800        ADD +1             TO WS-LT-WEEKS                                 
438900     END-IF                                                               
439000     COMPUTE WS-LT-WEEKS-DAYS = (7 * WS-LT-WEEKS)                         
439100*                                                                         
439200     IF WS-LT-WEEKS < 2                                                   
439300        MOVE +2                 TO WS-LT-WEEKS                            
439400     END-IF                                                               
439500*                                                                         
439600     COMPUTE WS-LT-WEEKS         = (-1 * WS-LT-WEEKS)                     
439700                                                                          
439800     MOVE W-TIFINLV-AAVV        TO W-TIFINLV-AAVV-MINUS-LT                
439900     MOVE WS-LT-WEEKS           TO W-ANTAL-VECKOR                         
440000     CALL W009VADD USING W-TIFINLV-AAVV-MINUS-LT W-ANTAL-VECKOR           
440100     .                                                                    
440200     EJECT                                                                
440300                                                                          
440400* --- IMS SEKTIONER ---                                                   
440500     SKIP3                                                                
440600                                                                          
440700 IMS-GU-WDK601 SECTION.                                                   
440800     MOVE 'IMS-GU-WDK601 ' TO IMSTEXT-STR                                 
440900     IF ARTC-DBD-NAME = 'WDK6'                                            
441000       STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                           
441100                      '&KDERS    =' W-KDERS-0-X ')'                       
441200              DELIMITED BY SIZE INTO SSA1                                 
441300     ELSE                                                                 
441400       STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X                           
441500                      '&KDERS    =' W-KDERS-0-X ')'                       
441600              DELIMITED BY SIZE INTO SSA1                                 
441700     END-IF                                                               
441800     MOVE '  GE' TO GODK-STATUSKODER                                      
441900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC01 SSA1               
442000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
442100     PERFORM IMS-STATUSKONTROLL                                           
442200     .                                                                    
442300     SKIP3                                                                
442400 IMS-GNP-WDK611  SECTION.                                                 
442500     MOVE 'IMS-GNP-WDK611 ' TO IMSTEXT-STR                                
442600     IF ARTC-DBD-NAME = 'WDK6'                                            
442700       MOVE  'WDK611 '    TO SSA1                                         
442800     ELSE                                                                 
442900       MOVE  'WLARTC11 '  TO SSA1                                         
443000     END-IF                                                               
443100     MOVE '  ' TO GODK-STATUSKODER                                        
443200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC11 SSA1              
443300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
443400     PERFORM IMS-STATUSKONTROLL                                           
443500     .                                                                    
443600     SKIP3                                                                
443700 IMS-GNP-WDK629  SECTION.                                                 
443800     MOVE 'IMS-GNP-WDK629 ' TO IMSTEXT-STR                                
443900                                                                          
444000     MOVE  'WDK629   '  TO SSA1                                           
444100     MOVE '  GE' TO GODK-STATUSKODER                                      
444200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK629 SSA1                   
444300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
444400     PERFORM IMS-STATUSKONTROLL                                           
444500     .                                                                    
444600     SKIP3                                                                
444700 IMS-GU-WDK601-ERS SECTION.                                               
444800     MOVE 'IMS-GU-WDK601-ERS ' TO IMSTEXT-STR                             
444900     IF ARTC-DBD-NAME = 'WDK6'                                            
445000       STRING 'WDK601  (IDARTNR  =' W-IDARTNR-ERS-X                       
445100                      '&KDERS    =' W-KDERS-0-X ')'                       
445200              DELIMITED BY SIZE INTO SSA1                                 
445300     ELSE                                                                 
445400       STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ERS-X                       
445500                      '&KDERS    =' W-KDERS-0-X ')'                       
445600              DELIMITED BY SIZE INTO SSA1                                 
445700     END-IF                                                               
445800     MOVE '  GE' TO GODK-STATUSKODER                                      
445900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-K601-ERS SSA1             
446000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
446100     PERFORM IMS-STATUSKONTROLL                                           
446200     .                                                                    
446300     SKIP3                                                                
446400 IMS-GNP-WDK611-ERS  SECTION.                                             
446500     MOVE 'IMS-GNP-WDK611-ERS ' TO IMSTEXT-STR                            
446600     IF ARTC-DBD-NAME = 'WDK6'                                            
446700       MOVE  'WDK611 '    TO SSA1                                         
446800     ELSE                                                                 
446900       MOVE  'WLARTC11 '  TO SSA1                                         
447000     END-IF                                                               
447100     MOVE '  ' TO GODK-STATUSKODER                                        
447200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-K611-ERS SSA1            
447300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
447400     PERFORM IMS-STATUSKONTROLL                                           
447500     .                                                                    
447600     SKIP3                                                                
447700 IMS-GNP-WDK621-FIRST SECTION.                                            
447800     IF ARTC-DBD-NAME = 'WDK6'                                            
447900       STRING 'WDK621  *F(DAPRLIST=>' W-DAPRLIST-X ')'                    
448000              DELIMITED BY SIZE INTO SSA1                                 
448100     ELSE                                                                 
448200       STRING 'WLARTC21*F(DAPRLIST=>' W-DAPRLIST-X ')'                    
448300              DELIMITED BY SIZE INTO SSA1                                 
448400     END-IF                                                               
448500     MOVE '  GE' TO GODK-STATUSKODER                                      
448600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-K621 SSA1                
448700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
448800     PERFORM IMS-STATUSKONTROLL                                           
448900     .                                                                    
449000 IMS-GNP-WDK621     SECTION.                                              
449100     IF ARTC-DBD-NAME = 'WDK6'                                            
449200       STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                      
449300              DELIMITED BY SIZE INTO SSA1                                 
449400     ELSE                                                                 
449500       STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                      
449600              DELIMITED BY SIZE INTO SSA1                                 
449700     END-IF                                                               
449800     MOVE '  GE' TO GODK-STATUSKODER                                      
449900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-K621 SSA1                
450000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
450100     PERFORM IMS-STATUSKONTROLL                                           
450200     .                                                                    
450300 IMS-GNP-WDK626     SECTION.                                              
450400     MOVE 'IMS-GNP-WDK626 ' TO IMSTEXT-STR                                
450500     IF ARTC-DBD-NAME = 'WDK6'                                            
450600       MOVE 'WDK611  *F' TO SSA1                                          
450700       MOVE 'WDK626 '    TO SSA2                                          
450800     ELSE                                                                 
450900       MOVE 'WLARTC11*F' TO SSA1                                          
451000       MOVE 'WLARTC26  ' TO SSA2                                          
451100     END-IF                                                               
451200     MOVE '  GE' TO GODK-STATUSKODER                                      
451300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC26 SSA1 SSA2         
451400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
451500     PERFORM IMS-STATUSKONTROLL                                           
451600     .                                                                    
451700     EJECT                                                                
451800 IMS-GET-SATSBEHOV-FIRST SECTION.                                         
451900     MOVE 'IMS-GET-SATSBEHOV-FIRST ' TO IMSTEXT-STR                       
452000     IF ARTC-DBD-NAME = 'WDK6'                                            
452100       MOVE 'WDK611  *F' TO SSA1                                          
452200       MOVE 'WDK624    ' TO SSA2                                          
452300     ELSE                                                                 
452400       MOVE 'WLARTC11*F' TO SSA1                                          
452500       MOVE 'WLARTC24  ' TO SSA2                                          
452600     END-IF                                                               
452700     MOVE '  GE' TO GODK-STATUSKODER                                      
452800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC24 SSA1 SSA2         
452900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
453000     PERFORM IMS-STATUSKONTROLL                                           
453100     .                                                                    
453200     SKIP3                                                                
453300 IMS-GET-SATSBEHOV-NEXT SECTION.                                          
453400     MOVE 'IMS-GET-SATSBEHOV-NEXT ' TO IMSTEXT-STR                        
453500     IF ARTC-DBD-NAME = 'WDK6'                                            
453600       MOVE 'WDK611    ' TO SSA1                                          
453700       MOVE 'WDK624    ' TO SSA2                                          
453800     ELSE                                                                 
453900       MOVE 'WLARTC11 ' TO SSA1                                           
454000       MOVE 'WLARTC24 ' TO SSA2                                           
454100     END-IF                                                               
454200     MOVE '  GE' TO GODK-STATUSKODER                                      
454300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC24 SSA1 SSA2         
454400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
454500     PERFORM IMS-STATUSKONTROLL                                           
454600     .                                                                    
454700     EJECT                                                                
454800 IMS-GU-ARTM01 SECTION.                                                   
454900     MOVE 'IMS-GU-ARTM01 ' TO IMSTEXT-STR                                 
455000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
455100            DELIMITED BY SIZE INTO SSA1                                   
455200     MOVE '  GE' TO GODK-STATUSKODER                                      
455300     CALL CBLTDLI USING GU  ARTM-PCB DLI-IO-AREA-ARTM01 SSA1              
455400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
455500     PERFORM IMS-STATUSKONTROLL                                           
455600     .                                                                    
455700     SKIP3                                                                
455800 IMS-GNP-ARTM11 SECTION.                                                  
455900     MOVE 'IMS-GNP-ARTM11 ' TO IMSTEXT-STR                                
456000     MOVE 'WLARTM11 ' TO SSA1                                             
456100     MOVE '  GE' TO GODK-STATUSKODER                                      
456200     CALL CBLTDLI USING GNP ARTM-PCB DLI-IO-AREA-ARTM11 SSA1              
456300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
456400     PERFORM IMS-STATUSKONTROLL                                           
456500     .                                                                    
456600     EJECT                                                                
456700 IMS-GU-WDK701      SECTION.                                              
456800     MOVE 'IMS-GU-WDK701 ' TO IMSTEXT-STR                                 
456900                                                                          
457000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
457100            DELIMITED BY SIZE INTO SSA1                                   
457200     MOVE '  GE' TO GODK-STATUSKODER                                      
457300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
457400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
457500     PERFORM IMS-STATUSKONTROLL                                           
457600     .                                                                    
457700     SKIP3                                                                
457800 IMS-GNP-WDK711  SECTION.                                                 
457900     MOVE 'IMS-GNP-WDK711 ' TO IMSTEXT-STR                                
458000     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
458100                    '&IDDC    <=' W-IDDC-MAX-X                            
458200                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
458300          DELIMITED BY SIZE INTO SSA1                                     
458400     MOVE '  GE'       TO GODK-STATUSKODER                                
458500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
458600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
458700     PERFORM IMS-STATUSKONTROLL                                           
458800     .                                                                    
458900     SKIP3                                                                
459000 IMS-GNP-WDK727 SECTION.                                                  
459100     MOVE 'IMS-GNP-WDK727 '  TO IMSTEXT-STR                               
459200                                                                          
459300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
459400          DELIMITED BY SIZE INTO SSA1                                     
459500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
459600          DELIMITED BY SIZE INTO SSA2                                     
459700     MOVE 'WDK727  '        TO SSA3                                       
459800     MOVE '  GE'            TO GODK-STATUSKODER                           
459900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK727 SSA1 SSA2         
460000                                                        SSA3              
460100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
460200     PERFORM IMS-STATUSKONTROLL                                           
460300     .                                                                    
460400     EJECT                                                                
460500 IMS-GU-WDK711-ERS SECTION.                                               
460600     MOVE 'IMS-GU-WDK711-ERS ' TO IMSTEXT-STR                             
460700                                                                          
460800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-ERS-X ')'                     
460900          DELIMITED BY SIZE INTO SSA1                                     
461000     STRING 'WDK711  (IDDC     =' W-IDDC-ERS-X ')'                        
461100          DELIMITED BY SIZE INTO SSA2                                     
461200     MOVE '  GE' TO GODK-STATUSKODER                                      
461300     CALL CBLTDLI USING GU WDK7-ERS-PCB DLI-IO-AREA-WDK711-ERS            
461400                           SSA1 SSA2                                      
461500     MOVE WDK7-ERS-STATUS-CODE TO STATUS-WS                               
461600     PERFORM IMS-STATUSKONTROLL                                           
461700     .                                                                    
461800     SKIP3                                                                
461900                                                                          
462000 IMS-GNP-WDK712  SECTION.                                                 
462100     MOVE 'IMS-GNP-WDK712 ' TO IMSTEXT-STR                                
462200                                                                          
462300     MOVE 'WDK712  '        TO SSA1                                       
462400     MOVE '  GE'            TO GODK-STATUSKODER                           
462500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK712 SSA1              
462600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
462700     PERFORM IMS-STATUSKONTROLL                                           
462800     .                                                                    
462900     SKIP3                                                                
463000                                                                          
463100 IMS-GU-WDB601    SECTION.                                                
463200     MOVE 'IMS-GU-WDB601  ' TO IMSTEXT-STR                                
463300                                                                          
463400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
463500          DELIMITED BY SIZE INTO SSA1                                     
463600     MOVE '  ' TO GODK-STATUSKODER                                        
463700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
463800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
463900     PERFORM IMS-STATUSKONTROLL                                           
464000     .                                                                    
464100                                                                          
464200 IMS-GU-WDB616    SECTION.                                                
464300     MOVE 'IMS-GU-WDB616  ' TO IMSTEXT-STR                                
464400                                                                          
464500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
464600          DELIMITED BY SIZE INTO SSA1                                     
464700     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
464800          DELIMITED BY SIZE INTO SSA2                                     
464900     MOVE '  ' TO GODK-STATUSKODER                                        
465000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
465100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
465200     PERFORM IMS-STATUSKONTROLL                                           
465300     .                                                                    
465400     EJECT                                                                
465500 IMS-GU-WDB616-CDC   SECTION.                                             
465600     MOVE 'IMS-GU-WDB616-CDC  ' TO IMSTEXT-STR                            
465700                                                                          
465800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-CDC-X ')'                     
465900          DELIMITED BY SIZE INTO SSA1                                     
466000     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-CDC-X ')'                    
466100          DELIMITED BY SIZE INTO SSA2                                     
466200     MOVE '  ' TO GODK-STATUSKODER                                        
466300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616-CDC SSA1 SSA2        
466400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
466500     PERFORM IMS-STATUSKONTROLL                                           
466600     .                                                                    
466700     EJECT                                                                
466800 IMS-GU-WDD7A1 SECTION.                                                   
466900     MOVE 'IMS-GU-WDD7A1  ' TO IMSTEXT-STR                                
467000                                                                          
467100     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
467200                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
467300            DELIMITED BY SIZE INTO SSA1                                   
467400     MOVE '  GE' TO GODK-STATUSKODER                                      
467500     CALL CBLTDLI USING GU WDD7A-PCB DLI-IO-AREA-D7A1 SSA1                
467600     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
467700     PERFORM IMS-STATUSKONTROLL                                           
467800     .                                                                    
467900     EJECT                                                                
468000 IMS-STATUSKONTROLL SECTION.                                              
468100     SET STATUS-IX TO 1                                                   
468200     SEARCH GODK-STATUS                                                   
468300         AT END CALL FELLOG DISPLAY 'CALL FELLOG FRÅN '                   
468400                                    IMSTEXT-STR ' I PGM W2222200'         
468500                            DISPLAY 'IMS STATUS=' STATUS-WS               
468600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
468700          CONTINUE                                                        
468800     END-SEARCH                                                           
468900     .                                                                    
469000     EJECT                                                                
469100*    -COPY WY2000P1                                                       
469200     EJECT                                                                
469300*    -COPY WY2000P9                                                       
469400     EJECT                                                                
469500*    -COPY WY2000P3                                                       
469600     EJECT                                                                
469700*    -COPY WY2000Q3                                                       
