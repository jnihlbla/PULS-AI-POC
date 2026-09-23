000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W6115900.                                            
000400 AUTHOR.             STEFAN ÅSGÅRDEN.                                     
000500 DATE-WRITTEN.       SEP  2002.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        PROGRAMMET ÄR EN KOPIA AV W2222200                               
001100*        PROGRAMMET ÄR ETT SUBPROGRAM FÖR BERÄKNING AV                    
001200*        ARTIKEL-BEHOV. EN BEHOVSTABELL ANGER PER VECKA                   
001300*        EN ARTIKELS FÖRVÄNTADE BEHOV. BEHOVEN BESTÅR AV                  
001400*            - PROGNOS   BASERAT PÅ PB-SEP,SÄSONGSINDEX,                  
001500*                        TRÄNDER, PB-JUSTERINGAR                          
001600*            - SATSBEHOV SATSENS LEVERANSPLAN STYR DE                     
001700*                        INGÅENDE ARTIKLARNAS SATSBEHOV                   
001800*            - DO-BEHOV  REGISTRERADE DIVERSEORDER                        
001900*        BERÄKNINGS-OMFATTNING OCH -RESULTAT FÖRMEDDLAS                   
002000*        VIA LÄNKAREAN W61159                                             
002100*    SUBPROGRAM.                                                          
002200*            W009VADD    ADD AV VECKOR TILL DATUM                         
002300*            PERADD      ADD AV PERIODER TILL DATUM                       
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP1                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003500 77  IDPGM                       PIC X(08)   VALUE 'W6115900'.            
003600     SKIP2                                                                
003700*    -COPY WY2000W1                                                       
003800     SKIP2                                                                
003900*    -COPY WY2000W6                                                       
004000     SKIP3                                                                
004100*    -COPY WY2000W3                                                       
004200     SKIP3                                                                
004300*    -COPY WY2000W9                                                       
004400     SKIP3                                                                
004500 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
004600     SKIP3                                                                
004700 01  KONSTANTER.                                                          
004800     03  JA                  PIC X       VALUE 'J'.                       
004900     03  NEJ                 PIC X       VALUE 'N'.                       
005000     03  ANTAL-SEASONINDEX   PIC S9(9)   VALUE +12   COMP SYNC.           
005100     SKIP1                                                                
005200     03  JUST-PB-FINNS       PIC X       VALUE 'J'.                       
005300     03  PERIOD-ANTAL        PIC S9(9)   VALUE +12   COMP-3.              
005400     03  PB-FAKTOR-PER5      PIC S9(3)V9(2)                               
005500                                         VALUE ZERO  COMP-3.              
005600     03  MAX-BEHOVS-VECKOR   PIC S9(9)   VALUE +70  COMP SYNC.            
005700                                                                          
005800     03  ENDAST-SEPARATBEHOV PIC  X(2)   VALUE '01'.                      
005900     03  ENDAST-SATSBEHOV    PIC  X(2)   VALUE '02'.                      
006000     03  SEP-SATS-BEHOV      PIC  X(2)   VALUE '03'.                      
006100     03  ENDAST-LEVBEHOV     PIC  X(2)   VALUE '04'.                      
006200     03  SEP-DO-BEHOV        PIC  X(2)   VALUE '05'.                      
006300     03  SATS-DO-BEHOV       PIC  X(2)   VALUE '06'.                      
006400     03  SEP-SATS-DO-BEHOV   PIC  X(2)   VALUE '07'.                      
006500     03  SATS-TPO-LEVBEHOV   PIC  X(2)   VALUE '08'.                      
006600     03  SEP-SATS-TPO-LEVBEHOV      PIC  X(2)   VALUE '09'.               
006700     03  ENDAST-SDCBEHOV            PIC  X(2)   VALUE '10'.               
006800     03  SATS-TPO-SDCBEHOV          PIC  X(2)   VALUE '11'.               
006900     03  SEP-SATS-TPO-SDCBEHOV      PIC  X(2)   VALUE '12'.               
007000     03  SATS-TPO-LEV-SDCBEHOV      PIC  X(2)   VALUE '13'.               
007100     03  SEP-SATS-TPO-LEV-SDCBEHOV  PIC  X(2)   VALUE '14'.               
007200     03  ENDAST-NDCBEHOV            PIC  X(2)   VALUE '15'.               
007300     03  SATS-TPO-SDC-NDC           PIC  X(2)   VALUE '16'.               
007400     03  SEP-SATS-TPO-SDC-NDC       PIC  X(2)   VALUE '17'.               
007500     03  SATS-TPO-LEV-SDC-NDC       PIC  X(2)   VALUE '18'.               
007600     03  SEP-SATS-TPO-LEV-SDC-NDC   PIC  X(2)   VALUE '19'.               
007700     03  PB-TOTAL                   PIC  X(2)   VALUE '20'.               
007800                                                                          
007900     03  LAES-ARTIKEL        PIC S9(3)   VALUE +301  COMP-3.              
008000     03  LAES-PROGNOS-BEHOV  PIC S9(3)   VALUE +302  COMP-3.              
008100     03  LAES-TPO-SALDO      PIC S9(3)   VALUE +303  COMP-3.              
008200     03  LAES-SATS-BEHOV     PIC S9(3)   VALUE +401  COMP-3.              
008300     03  LAES-TPO-BEHOV      PIC S9(3)   VALUE +501  COMP-3.              
008400     03  LAES-TPO-BEHOV-NEXT PIC S9(3)   VALUE +502  COMP-3.              
008500     03  LAES-SDCINFO        PIC S9(3)   VALUE +601  COMP-3.              
008600     03  LAES-NDCINFO        PIC S9(3)   VALUE +701  COMP-3.              
008700     SKIP3                                                                
008800     SKIP3                                                                
008900 01  FILLER                    PIC X(16)  VALUE 'ARBETSAREOR'.            
009000 01  ARBETSAREOR.                                                         
009100     SKIP1                                                                
009200*                                                                         
009300*   PER-TABELL ÄR EN RULLANDE TABELL DÄR                                  
009400*   IX = 1  ÄR JANUARI                                                    
009500*   IX = 12 ÄR DECEMBER                                                   
009600*                                                                         
009700     03 PER-TABELL OCCURS 12.                                             
009800        05 PER-PERIOD            PIC  9(2)   VALUE ZERO.                  
009900        05 PER-START-VV          PIC  9(2)   VALUE ZERO.                  
010000        05 PER-SLUT-VV           PIC  9(2)   VALUE ZERO.                  
010100     SKIP3                                                                
010200     03  FILLER              PIC X(16)  VALUE 'IX        '.               
010300     03  IX                  PIC S9(9)               COMP SYNC.           
010400     03  IX-PER              PIC S9(9)               COMP SYNC.           
010500     03  IX-FRAN             PIC S9(9)               COMP-3               
010600                                                    VALUE ZERO.           
010700     03  IX-TILL             PIC S9(9)               COMP-3               
010800                                                    VALUE ZERO.           
010900     03  IX-VECKA            PIC S9(9)               COMP-3               
011000                                                    VALUE ZERO.           
011100     03  LINK-IX             PIC S9(9)               COMP-3               
011200                                                    VALUE ZERO.           
011300     03  IY                  PIC S9(9)               COMP SYNC.           
011400     03  TABW200-INDEX       PIC S9(4)               COMP SYNC.           
011500     03  SW-TPOBEHOV-LAEST   PIC X       VALUE 'N'.                       
011600     SKIP1                                                                
011700     03  FILLER              PIC X(16)  VALUE 'W-TIAAP-AKTUELL '.         
011800     03  W-TIAAP-AKTUELL     PIC S9(3)               COMP-3.              
011900     03  W-DATUM-AAVV        PIC S9(5)               COMP-3.              
012000     03  W-RED-NUM-1         PIC 9             VALUE ZERO.                
012100     03  W-RED-NUM-4         PIC 9(4)          VALUE ZERO.                
012200     03  W-AAVV              PIC S9(5)               COMP-3.              
012300     03  W-AAVVD             PIC S9(5)         VALUE ZERO.                
012400     03  W-TIAAVV            PIC 9(4)          VALUE ZERO.                
012500     03  W-DATUM-FORSTA-SDC  PIC S9(5)               COMP-3.              
012600     03  W-TIFINLV-AAVV-MINUS-2                                           
012700                             PIC S9(5)               COMP-3.              
012800     03  W-DATUM             PIC 9(4).                                    
012900     03  W-DAT               REDEFINES W-DATUM.                           
013000         05  W-DATUM-AA      PIC 9(2).                                    
013100         05  W-DATUM-VV      PIC 9(2).                                    
013200     SKIP1                                                                
013300     03  FILLER              PIC X(16)  VALUE 'W-ANTAL-VECKOR  '.         
013400     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
013500     03  W-PER-ANT           PIC S9(3)               COMP-3.              
013600     03  W-ANT-TREND-PER     PIC S9(3)               COMP-3.              
013700     SKIP1                                                                
013800     03  W-BER-START-AA      PIC S9(3)               COMP-3.              
013900     03  W-BER-START-VV      PIC S9(3)               COMP-3.              
014000     03  W-BER-SLUT-DATUM    PIC S9(5)               COMP-3.              
014100     03  W-BER-START-DATUM   PIC S9(5)               COMP-3.              
014200     03  W-BER-DATUM         PIC S9(5)               COMP-3.              
014300     03  W-BER-DATUM-MINUS-1 PIC S9(5)               COMP-3.              
014400     SKIP1                                                                
014500     03  FILLER              PIC X(16)  VALUE 'W-FAKTOR        '.         
014600     03  W-FAKTOR            PIC S9(1)V9(3)          COMP-3.              
014700     03  W-KVPB-SEP          PIC S9(6)V9(3)          COMP-3.              
014800     03  W-KVPB-JUST1        PIC S9(6)V9(3)          COMP-3.              
014900     03  W-KVPB-JUST2        PIC S9(6)V9(3)          COMP-3.              
015000     03  W-KVTREND           PIC S9(6)V9(3)          COMP-3.              
015100     03  W-KVTREND-ACC       PIC S9(6)V9(3)          COMP-3.              
015200     03  W-KVBEHOV-SUMMA     PIC S9(7)V9(2)          COMP-3.              
015300     03  W-TIFINLV-AAVV      PIC S9(5)               COMP-3.              
015400     03  W-PER               PIC 9(2)                COMP-3.              
015500     SKIP1                                                                
015600     03  FILLER              PIC X(16)  VALUE 'W-TITREND       '.         
015700     03  W-TITREND           PIC 9(3).                                    
015800     03  W-TITR  REDEFINES W-TITREND.                                     
015900         05  W-TITREND-AA    PIC 9(2).                                    
016000         05  W-TITREND-P     PIC 9.                                       
016100     03  W-TIAAP             PIC S9(3)               COMP-3.              
016200     03  W-TREND-START-AAVV  PIC S9(5)               COMP-3.              
016300     03  W-TREND-SLUT-AAVV   PIC S9(5)               COMP-3.              
016400                                                                          
016500     03  W-ANTVKA-FORSKJUT   PIC S9(1)               COMP-3.              
016600     03  W-KDGK              PIC S9(1)               COMP-3.              
016700     03  W-TIBEHOV-TPO       PIC S9(5)               COMP-3.              
016900****** AREOR FÖR BERÄKNING AV REFILL BEHOV TILL SDC                       
017000     03  FILLER              PIC X(16)  VALUE 'W-TIME          '.         
017100     03  W-TIME                   PIC 9(8)     VALUE ZERO.                
017200     03  W-SDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
017300     03  W-SDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
017400     03  W-KVBEHOV-INNEV-VECKA    PIC S9(6)V9  VALUE ZERO COMP-3.         
017500     03  W-SDC-TILLGANG           PIC S9(7)    VALUE ZERO COMP-3.         
017600     03  W-SDC-KVAR-EFTER-VECKA   PIC S9(7)    VALUE ZERO COMP-3.         
017700     03  W-SDC-KVAR-EFT-DAG       PIC S9(7)    VALUE ZERO COMP-3.         
017800     03  W-SDC-KVBEHOV-DESSUTOM   PIC S9(7)    VALUE ZERO COMP-3.         
017900     03  W-DC-REFILL-KVANT        PIC S9(7)    VALUE ZERO COMP-3.         
018000     03  W-SDC-KVANT              PIC S9(7)    VALUE ZERO COMP-3.         
018100     03  W-SDC-REST               PIC S9(7)    VALUE ZERO COMP-3.         
018200     03  W-SDC-ANT-REFILL         PIC S9(7)    VALUE ZERO COMP-3.         
018300     03  W-SDC-ROUND              PIC S9V9(2)  VALUE ZERO COMP-3.         
018400     03  W-SDC-DIFF               PIC S9(7)    VALUE ZERO COMP-3.         
018500     03  W-SDC-KVAR-EFT-VECKA     PIC S9(7)    VALUE ZERO COMP-3.         
018600     03  W-DAGAR-KVAR             PIC S9(1)    VALUE ZERO COMP-3.         
018700     03  W-VECKODEL            PIC S9(1)V9(2) VALUE ZERO COMP-3.          
018800     03  W-SDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
018900     03  FILLER              PIC X(16)  VALUE 'W-NDC-ACC-KVBEHO'.         
019000     03  W-NDC-ACC-KVBEHOV        PIC S9(8)V9  VALUE ZERO COMP-3.         
019100     03  W-NDC-KVBEHOV-VECKA      PIC S9(6)V9  VALUE ZERO COMP-3.         
019200     03  W-NDC-KVBEHOV-DAG        PIC S9(6)V9  VALUE ZERO COMP-3.         
019300     03  W-NDC-TILLGANG           PIC S9(7)V9  VALUE ZERO COMP-3.         
019400     03  W-NDC-KVAR-EFT-VECKA     PIC S9(7)V9  VALUE ZERO COMP-3.         
019500     03  W-NDC-KVAR-EFT-DAG       PIC S9(7)V9  VALUE ZERO COMP-3.         
019600     03  W-NDC-KVBEHOV-DESSUTOM   PIC S9(7)V9  VALUE ZERO COMP-3.         
019700*    03  W-NDC-REFILL-KVANT       PIC S9(7)V9  VALUE ZERO COMP-3.         
019800     03  W-NDC-DIFF               PIC S9(7)V9  VALUE ZERO COMP-3.         
019900     03  W-NDC-ACC-KVBEHOV-VECKA  PIC S9(7)V9  VALUE ZERO COMP-3.         
020000     03  W-NDC-KVAR-KVBEHOV-VECKA PIC S9(7)V9  VALUE ZERO COMP-3.         
020100     03  W-BINNDAY                PIC 9(6)     VALUE ZERO.                
020200     03  WS-QX-BRYTNING           PIC 9(2)     VALUE ZERO.                
020300     03  WS-NOLL                  PIC 9(2)     VALUE ZERO.                
020400     03  WS-BALANCE-ERSATT       PIC S9(7)   VALUE ZERO COMP-3.           
020500     03  WS-KVKUNDRETUR          PIC S9(7)   VALUE ZERO COMP-3.           
020600     03  WS-KVREFPKT              PIC S9(7)  VALUE ZERO COMP-3.           
020700     03  WS-KVREFBER              PIC S9(7)  VALUE ZERO COMP-3.           
020800     03  WS-DAPUBL                PIC 9(8)  VALUE ZERO.                   
020900                                                                          
021000     03 WS-ANT-VV                PIC  9(2)   VALUE ZERO.                  
021100     03 WS-TIAAVV.                                                        
021200       05 WS-AAR                 PIC  9(2)   VALUE ZERO.                  
021300       05 WS-VV                  PIC  9(2)   VALUE ZERO.                  
021400     03 TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                              
021500     03 WS-TIAAPER.                                                       
021600       05 TIAA                   PIC  9(2)   VALUE ZERO.                  
021700       05 PER                    PIC  9(2)   VALUE ZERO.                  
021800     03 TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
021900     03 WS-KVANT-QX          PIC  9(5)V9(2) VALUE ZERO.                   
022000     03 WS-KVANT-QX-DELAR    REDEFINES WS-KVANT-QX.                       
022100        05 WS-KVANT-QX-HELTAL PIC 9(5).                                   
022200        05 WS-KVANT-QX-DECTAL PIC 9(2).                                   
022300     03 WS-ANTAL-QX          PIC S9(7)      VALUE ZERO COMP-3.            
022400     03  FILLER              PIC X(16)  VALUE 'WS-ANTAL-BER    '.         
022500     03 WS-ANTAL-BER         PIC S9(7)      VALUE ZERO COMP-3.            
022600     03  WS-IDDC-NUM             PIC 9(2)    VALUE ZERO.                  
022700     03  WS-BER-IY           PIC S9(9)   VALUE ZERO  COMP SYNC.           
022800     03  WS-BER-IX           PIC S9(9)   VALUE ZERO  COMP SYNC.           
022900     03  FILLER              PIC X(16)  VALUE 'WS-BER-TAB-NOLL '.         
023000     03  WS-BER-TAB-NOLL.                                                 
023100         05  FILLER          OCCURS 4.                                    
023200             07  FILLER      OCCURS 52.                                   
023300                 09  FILLER                                               
023400                             PIC S9(6)V9(3)                               
023500                                         VALUE ZERO  COMP-3.              
023600                 09  FILLER                                               
023700                             PIC S9(5)   VALUE ZERO  COMP-3.              
023800                                                                          
023900     03  FILLER              PIC X(16)  VALUE 'WS-CURRENT-DATE '.         
024000     03  WS-CURRENT-DATE.                                                 
024100         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
024200         05  FILLER              PIC 9(4)   VALUE ZERO.                   
024300         05  FILLER              PIC 9(6)   VALUE ZERO.                   
024400                                                                          
024500     03  FILLER REDEFINES WS-CURRENT-DATE.                                
024600*-----   INKLUSIVE SEKEL                                                  
024700         05  WS-DAGENS-DATUM     PIC 9(8).                                
024800         05  WS-DAGENS-TID.                                               
024900             07 WS-DAGENS-TIMME  PIC 9(2).                                
025000             07 WS-DAGENS-MINUT  PIC 9(2).                                
025100             07 WS-DAGENS-SEKUND PIC 9(2).                                
025200     03 WS-DAGENS-VECKA          PIC 9(2)    VALUE ZERO.                  
025300     03 WS-CALL-KVVECKOR-BEHOV   PIC S9(3)                                
025400                                         VALUE ZERO  COMP-3.              
025500     03 WS-KVPB-PLAN-VECKA       PIC S9(6)V9(1)                           
025600                                         VALUE ZERO  COMP-3.              
025700     03 WS-ARSTOTAL              PIC S9(9)V9(2)                           
025800                                             COMP-3  VALUE ZERO.          
025900     03 WS-KVPB                  PIC S9(9)V9(2)                           
026000                                             COMP-3  VALUE ZERO.          
026100     03 WS-KVBEHOV-PER           PIC S9(7)V9(2)                           
026200                                             COMP-3  VALUE ZERO.          
026300     03  FILLER              PIC X(16)  VALUE 'WS-KVBEHOV-PER-T'.         
026400     03 WS-KVBEHOV-PER-TEST      OCCURS 12                                
026500                                 PIC S9(7)V9(2)                           
026600                                             COMP-3  VALUE ZERO.          
026700     03 WS-JUSTERA               PIC S9V9(2) COMP-3  VALUE ZERO.          
026800     03 WS-RESEASON-TOT          PIC S9(2)V9(2)                           
026900                                             COMP-3  VALUE ZERO.          
027000                                                                          
027100****                                                                      
027200**** PERIODERNA 1,2,3,4,6,7,8 INNEHÅLLER 6 VECKOR                         
027300**** PERIOD 5 INNEHÅLLER 10 VECKOR (25-34)                                
027400****                                                                      
027500   03    FILLER              PIC X(16)  VALUE 'WS-KVBEHOV-TABEL'.         
027600   03 WS-KVBEHOV-TABELL.                                                  
027700     05 WS-KVBEHOV-VECKA         OCCURS 52                                
027800                                 PIC S9(7)V9(2)                           
027900                                             COMP-3  VALUE ZERO.          
028000   03    FILLER              PIC X(16)  VALUE 'WS-RESEASON-TABE'.         
028100   03 WS-RESEASON-TABELL.                                                 
028200     05 WS-RESEASON              OCCURS 12                                
028300                                 PIC S9(2)V9(4)                           
028400                                             COMP-3  VALUE ZERO.          
028500   03    FILLER              PIC X(16)  VALUE 'WS-RESEASON-AVR-'.         
028600   03 WS-RESEASON-AVR-TABELL.                                             
028700     05 WS-RESEASON-AVR          OCCURS 12                                
028800                                 PIC S9(2)V9(2)                           
028900                                             COMP-3  VALUE ZERO.          
029000   03 WS-NOLLA-KVBEHOV.                                                   
029100     05 FILLER                   OCCURS 52                                
029200                                 PIC S9(7)V9(2)                           
029300                                             COMP-3  VALUE ZERO.          
029400   03 WS-NOLLA-RESEASON.                                                  
029500     05 FILLER                   OCCURS 12                                
029600                                 PIC S9(2)V9(4)                           
029700                                             COMP-3  VALUE ZERO.          
029800     SKIP3                                                                
029900                                                                          
030000********************************************************                  
030100*                                                      *                  
030200*   W271REF2                                           *                  
030300*                                                      *                  
030400*   SUBPROGRAM W271REF2 INLAGD I W2222200 FÖR ATT      *                  
030500*   TJÄNA TID OCH PENGAR (PGA TIDSÖDANDE CALL ANROP)   *                  
030600*   1999-04-07 STEFAN A                                *                  
030700*                                                      *                  
030800********************************************************                  
030900                                                                          
031000 01      FILLER              PIC X(16)  VALUE 'ARBETSFAELT     '.         
031100 01  ARBETSFAELT.                                                         
031200*                                                                         
031300     03 DAGENS-DATUM             PIC 9(8).                                
031400     03 FILLER REDEFINES     DAGENS-DATUM.                                
031500         05 DAGENS-AAR           PIC 9(4).                                
031600         05 DAGENS-MAANAD        PIC 9(2).                                
031700         05 DAGENS-DAG           PIC 9(2).                                
031800     03 DAGENS-AAR-PLUS2         PIC 9(4)        VALUE ZERO.              
031900     03 JMF-AAAA                 PIC 9(4)        VALUE ZERO.              
032000                                                                          
032100*                                                                         
032200     03  WS-SNITT-VECKOR         PIC 9(1)V9(2)  VALUE 4.33.               
032300     03  WS-DAGAR-LO2530         PIC 9(1)V9(1)  VALUE 9.5.                
032400*                                                                         
032500     03  WS-N                 PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
032600     03  WS-NP                PIC S9(10)V9(5) VALUE ZERO COMP-3.          
032700     03  WS-UB                PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
032800     03  WS-NP-UB             PIC S9(10)V9(5) VALUE ZERO COMP-3.          
032900     03  WS-TOT-DAGAR-PLATSBR PIC S9(2)V9(1)  VALUE ZERO COMP-3.          
033000     03  WS-LEDTIDSBEHOV      PIC S9(6)V9(3)  VALUE ZERO COMP-3.          
033100     03  WS-TABELLBEHOV       PIC S9(6)V9(3)  VALUE ZERO COMP-3.          
033200     03  WS-PAFYLLNPKT        PIC S9(7)       VALUE ZERO COMP-3.          
033300     03  WS-PAFYLLNKVANT      PIC S9(7)       VALUE ZERO COMP-3.          
033400     03  WS-OVERLAGERPKT      PIC S9(7)       VALUE ZERO COMP-3.          
033500     03  WS-KVPB-REF-DAY-BINN PIC S9(6)V9(5)  VALUE ZERO COMP-3.          
033600                                                                          
033700     03  WS-SP-IDDC              PIC X(2)    VALUE SPACE.                 
033800     03  WS-SP-IDREFTAB          PIC X(1)    VALUE SPACE.                 
033900     03  WS-SP-IDLEVNR         PIC S9(5)   VALUE ZERO COMP-3.             
034000     03  FILLER              PIC X(16)  VALUE 'WS-KLASS        '.         
034100*                                                                         
034200     03  WS-KLASS.                                                        
034300         05  WS-KLASS-RAD        PIC 9(2)    VALUE ZERO.                  
034400         05  WS-KLASS-RAD-X REDEFINES WS-KLASS-RAD.                       
034500             07  FILLER          PIC X(2).                                
034600         05  WS-KLASS-KOL        PIC X(1)    VALUE SPACE.                 
034700*                                                                         
034800     03  FL-PRARTBES               PIC X          VALUE 'N'.              
034900     03  WS-PRARTBES               PIC S9(7)V9(2) VALUE ZERO              
035000                                                   COMP-3.                
035100     03  WS-KVPB-REF               PIC S9(6)V9(3) VALUE ZERO              
035200                                                   COMP-3.                
035300     03  WS-KVPB-REF-DAY-PER-I     PIC S9(6)V9(5) VALUE ZERO              
035400                                                   COMP-3.                
035500     03  WS-KVPB-REF-DAY-PER-II    PIC S9(6)V9(5) VALUE ZERO              
035600                                                   COMP-3.                
035700     03  WS-KVPB-REF-DAY-PER-III   PIC S9(6)V9(5) VALUE ZERO              
035800                                                   COMP-3.                
035900     03  WS-KVPB-REF-DAY-PER-IV    PIC S9(6)V9(5) VALUE ZERO              
036000                                                   COMP-3.                
036100     03  WS-KVPB-REF-DAY-PER-V     PIC S9(6)V9(5) VALUE ZERO              
036200                                                   COMP-3.                
036300     03  WS-KVPB-REF-DAY-PER-VI    PIC S9(6)V9(5) VALUE ZERO              
036400                                                   COMP-3.                
036500     03  WS-KVREFLIM               PIC S9(5)      VALUE ZERO              
036600                                                   COMP-3.                
036700     03  WS-KVREFKVA               PIC S9(5)      VALUE ZERO              
036800                                                   COMP-3.                
036900     03  FILLER              PIC X(16)  VALUE 'WS-KVARBDAG     '.         
037000     03  WS-KVARBDAG               PIC 9(3)    VALUE ZERO.                
037100     03  WS-BINNDAY-TIAAMMDD       PIC 9(6)    VALUE ZERO.                
037200     03  WS-TIAAMMDD-FOM           PIC 9(6)    VALUE ZERO.                
037300     03  FILLER REDEFINES WS-TIAAMMDD-FOM.                                
037400         05 WS-TIAA            PIC 9(2).                                  
037500         05 WS-TIMM            PIC 9(2).                                  
037600         05 WS-TIDD            PIC 9(2).                                  
037700     03  WS-BINNDAY-TIAARP     PIC 9(4)    VALUE ZERO.                    
037800     03  FILLER REDEFINES WS-BINNDAY-TIAARP.                              
037900         05 WS-BINNDAY-TIAA    PIC 9(2).                                  
038000         05 WS-BINNDAY-TIRP    PIC 9(2).                                  
038100                                                                          
038200     03  FILLER              PIC X(16)  VALUE 'WS-RADIX        '.         
038300     03  WS-RADIX                  PIC 9(2)    VALUE ZERO.                
038400     03  WS-PR-KOLIX               PIC 9(2)    VALUE ZERO.                
038500*    03  IX                        PIC 9(2)    VALUE ZERO.                
038600     03  PER-IX                    PIC 9(2)    VALUE ZERO.                
038700     03  FILLER              PIC X(16)  VALUE 'TAB-KVARBDAG    '.         
038800     03  TAB-KVARBDAG              PIC 9(3)    VALUE ZERO                 
038900                                   OCCURS 12.                             
039000                                                                          
039100     03  FILLER              PIC X(16)  VALUE 'WS-SP-GRUNDTABEL'.         
039200     03  WS-SP-GRUNDTABELL-DC    PIC X(2).                                
039300                                                                          
039400     03  WS-SW-TAB-0             PIC X(1).                                
039500                                                                          
039600     03  FILLER              PIC X(16)  VALUE 'SPARAREOR       '.         
039700     03  SPARAREOR.                                                       
039800                                                                          
039900         05  SPARAREA-AKTUELL-TABELL.                                     
040000*            07 -COPY WDGX2502 -PRE AKTTAB-                               
040100                                                                          
040200         05  SPARAREA-GRUNDTABELL.                                        
040300*            07 -COPY WDGX2502 -PRE GRUNDTAB-                             
040400                                                                          
040500     03  TE-TABELL-SAKNAS.                                                
040600         05 TABELL-SAKNAS-TEXT    PIC X(22)                               
040700                           VALUE 'TABELL SAKNAS FÖR DC '.                 
040800         05 TABELL-SAKNAS-DC PIC X(2).                                    
040900                                                                          
041000                                                                          
041100 01  FILLER                    PIC X(24)  VALUE 'SWITCHAR'.               
041200                                                                          
041300 77  GRUNDTABELL-SW              PIC X       VALUE 'N'.                   
041400     88  GRUNDTABELL                         VALUE 'J'.                   
041500                                                                          
041600 77  PLATSBRIST-SW               PIC X       VALUE 'N'.                   
041700     88  PLATSBRIST                          VALUE 'J'.                   
041800                                                                          
041900                                                                          
042000****************************************** PARAM. W009VADD                
042100 01  W009VADDW.                                                           
042200     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
042300     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
042400     SKIP3                                                                
042500****************************************** BERAKNINGSTABELL               
042600*                                          ARTKIKELBEHOV PER VECKA        
042700*                                          UNDER MAX 4ÅR                  
042800 01  FILLER                  PIC X(16)  VALUE 'BERAKNINGS-TABEL'.         
042900 01  BERAKNINGS-TABELL.                                                   
043000     03  BER-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
043100     03  BER-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
043200     03  CDC-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
043300     03  CDC-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
043400     03  BER-MAX-VV          PIC S9(9)   VALUE +52   COMP SYNC.           
043500     03  BER-MAX-AA          PIC S9(9)   VALUE +4    COMP SYNC.           
043600     03  BER-ANTAL-VV        PIC S9(9)   VALUE ZERO  COMP SYNC.           
043700     03  BER-ANTAL-AA        PIC S9(9)   VALUE ZERO  COMP SYNC.           
043800     03  FILLER              PIC X(16)   VALUE 'BER-TAB         '.        
043900     03  BER-TAB.                                                         
044000         05  BER-AA          OCCURS 4.                                    
044100             07  BER-VV      OCCURS 52.                                   
044200                 09  BER-BEHOV                                            
044300                             PIC S9(6)V9(3)                               
044400                                         VALUE ZERO  COMP-3.              
044500                 09  BER-TIAAVVD-BEHOV                                    
044600                             PIC S9(5)               COMP-3.              
044700     SKIP3                                                                
044800****************************************** RESULTAT-TABELL                
044900*                                          TOTALT ARTIKELBEHOV            
045000*                                          PER VECKA MAX 4 ÅR             
045100 01      FILLER              PIC X(16)   VALUE 'RESULTAT-TABELL '.        
045200 01  RESULTAT-TABELL.                                                     
045300     03  RES-IX              PIC S9(9)   VALUE ZERO  COMP SYNC.           
045400     03  RES-IY              PIC S9(9)   VALUE ZERO  COMP SYNC.           
045500     03  RES-MAX-VV          PIC S9(9)   VALUE +52   COMP SYNC.           
045600     03  RES-MAX-AA          PIC S9(9)   VALUE +4    COMP SYNC.           
045700     03  RES-ANTAL-VV        PIC S9(9)   VALUE ZERO  COMP SYNC.           
045800     03  RES-ANTAL-AA        PIC S9(9)   VALUE ZERO  COMP SYNC.           
045900     03  FILLER              PIC X(16)   VALUE 'RES-TAB         '.        
046000     03  RES-TAB.                                                         
046100         05  RES-AA          OCCURS 4.                                    
046200             07  REX-VV      OCCURS 52.                                   
046300                 09  RES-BEHOV                                            
046400                             PIC S9(6)V9(3)          COMP-3.              
046500                 09  RES-TIAAVVD-BEHOV                                    
046600                             PIC S9(5)               COMP-3.              
046700                                                                          
046800                                                                          
046900*      --- VALID IDDC CODES                                               
047000*                                                                         
047100*01    -COPY WWDC99                                                       
047200                                                                          
047300 01      FILLER              PIC X(16)   VALUE 'W-IDDC-X        '.        
047400 01  W-IDDC-X.                                                            
047500     03  W-IDDC              PIC  X(2).                                   
047600 01  W-IDLAND-X.                                                          
047700     03  W-IDLAND            PIC  X(2) VALUE SPACE.                       
047800 01  W-IDDC-B6-X.                                                         
047900     03  W-IDDC-B6           PIC X(2)   VALUE SPACE.                      
048000 01  W-IDDC-B616-X.                                                       
048100     03  W-IDDC-B616         PIC X(2)   VALUE SPACE.                      
048200 01  W-IDARTNR-X.                                                         
048300     03  W-IDARTNR           PIC S9(9)               COMP-3.              
048400 01  W-DAPRLIST-X.                                                        
048500     03  W-DAPRLIST          PIC  9(8)  VALUE ZERO.                       
048600 01  W-KDERS-0-X.                                                         
048700     03  W-KDERS-0           PIC S9(3)   COMP-3  VALUE ZERO.              
048800 01  W-IDARTNR-ERS-X.                                                     
048900     03  W-IDARTNR-ERS       PIC S9(9)   VALUE ZERO COMP-3.               
049000 01  W-KDSEGKEY-X.                                                        
049100     03  W-KDSEGKEY          PIC X(1)    VALUE '1'.                       
049200 01  W-IDDC-ERS-X.                                                        
049300     03  W-IDDC-ERS          PIC X(2)    VALUE SPACE.                     
049400 01  W-WDD7A1KY-MIN-X.                                                    
049500     03  W-IDARTNR-TILLK-MIN     PIC S9(9)   COMP-3  VALUE ZERO.          
049600     03  W-IDARTNR-ERS-MIN       PIC S9(9)   COMP-3  VALUE ZERO.          
049700     03  W-IDKORTNR-MIN          PIC S9(3)   COMP-3  VALUE ZERO.          
049800 01  W-WDD7A1KY-MAX-X.                                                    
049900     03  W-IDARTNR-TILLK-MAX     PIC S9(9)   COMP-3                       
050000                                             VALUE +999999999.            
050100     03  W-IDARTNR-ERS-MAX       PIC S9(9)   COMP-3                       
050200                                             VALUE +999999999.            
050300     03  W-IDKORTNR-MAX          PIC S9(3)   COMP-3  VALUE +999.          
050400                                                                          
050500                                                                          
050600     SKIP2                                                                
050700 01  FELTEXT.                                                             
050800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
050900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
051000 01  FELTEXT2.                                                            
051100     03  FILLER                  PIC X(9)    VALUE 'FELTEXT2'.            
051200     03  FELTEXT2-STR      PIC X(71)   VALUE SPACE.                       
051300                                                                          
051400     EJECT                                                                
051500*01  -COPY W200W001                                                       
051600     EJECT                                                                
051700 01  DYNAMISKA-SUBPROGRAM.                                                
051800     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
051900     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
052000     03  PERADD              PIC X(8)    VALUE 'PERADD'.                  
052100     03  W2222210            PIC X(8)    VALUE 'W2222210'.                
052200     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
052300     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
052400     03  WDAGKONV            PIC X(8)    VALUE 'WDAGKONV'.                
052500     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
052600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
052700     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY'.                 
052800     03  W271LTPB            PIC X(8)    VALUE 'W271LTPB'.                
052900     EJECT                                                                
053000*    ---PARAMETRAR TILL DATKONV                                           
053100*01  -COPY WDATAREA                                                       
053200     EJECT                                                                
053300*    --- PARAMETRAR TILL DAGKONV                                          
053400*01  -COPY WDAGAREA                                                       
053500     EJECT                                                                
053600*    ---PARAMETRAR TILL W271REF2                                          
053700*01  -COPY W271REF2                                                       
053800     EJECT                                                                
053900*    --- PARAMETRAR TILL POSTSUM                                          
054000*                                                                         
054100*01  -COPY W0005   -PRE  POSTSUM-                                         
054200     EJECT                                                                
054300*    ---PARAMETRAR TILL WORKDAY                                           
054400*01  -COPY WORKAREA                                                       
054500     EJECT                                                                
054600*    ---PARAMETRAR TILL W271LTPB                                          
054700*01  -COPY W271LTPB                                                       
054800     EJECT                                                                
054900*    --- PARAMETRAR TILL ABEND                                            
055000                                                                          
055100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
055200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
055300     SKIP3                                                                
055400     SKIP2                                                                
055500*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
055600*                                                                         
055700 01      IMS-WS.                                                          
055800   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
055900     SKIP3                                                                
056000*                            *** STATUSKOD FRÅN IMS                       
056100   03    STATUS-WS       PIC XX.                                          
056200     88  SEGMENT-FINNS               VALUE '  '.                          
056300     88  SEGMENT-SAKNAS              VALUE 'GE'                           
056400                                           'GB'.                          
056500     SKIP3                                                                
056600   03    SSA1            PIC X(128).                                      
056700   03    SSA2            PIC X(128).                                      
056800   03    SSA3            PIC X(128).                                      
056900     SKIP3                                                                
057000 01  NYCKLAR-TILL-DLI.                                                    
057100     03  W-WDGXKEY-X.                                                     
057200         05  W-IDHTYP           PIC X(4)    VALUE '2501'.                 
057300         05  W-IDDC-2501        PIC X(2)    VALUE ZERO.                   
057400         05  W-LOWVALUE         PIC X(24)   VALUE LOW-VALUE.              
057500*                                                                         
057600     03  W-IDREFTAB-X.                                                    
057700         05  W-IDREFTAB         PIC X(1)    VALUE SPACE.                  
057800   03    GODK-STATUSKODER.                                                
057900     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
058000     SKIP3                                                                
058100*01      -COPY W0003                                                      
058200     EJECT                                                                
058300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
058400     SKIP3                                                                
058500 01  DLI-IO-AREA-WDK601.                                                  
058600*        05  -COPY WDK601                                                 
058700     EJECT                                                                
058800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
058900     SKIP3                                                                
059000 01  DLI-IO-AREA-WDK611.                                                  
059100*        05  -COPY WDK611                                                 
059200     EJECT                                                                
059300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK621'.             
059400     SKIP3                                                                
059500 01  DLI-IO-AREA-WDK621.                                                  
059600*        05  -COPY WDK621                                                 
059700     EJECT                                                                
059800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
059900     SKIP3                                                                
060000 01  DLI-IO-AREA-WDK701.                                                  
060100*        05  -COPY WDK701                                                 
060200     EJECT                                                                
060300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
060400     SKIP3                                                                
060500 01  DLI-IO-AREA-WDK711.                                                  
060600*        05  -COPY WDK711                                                 
060700     EJECT                                                                
060800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
060900     SKIP3                                                                
061000 01  DLI-IO-AREA-WDK712.                                                  
061100*        05  -COPY WDK712                                                 
061200     EJECT                                                                
061300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
061400     SKIP3                                                                
061500 01  DLI-IO-AREA.                                                         
061600     03  IO-AREA                 PIC X(1036)   VALUE SPACE.               
061700     03  WL250101 REDEFINES IO-AREA.                                      
061800*        05  -COPY WDGX2501                                               
061900                                                                          
062000     03  WL250111 REDEFINES IO-AREA.                                      
062100*        05  -COPY WDGX2502                                               
062200                                                                          
062300 01  FILLER                   PIC X(24)                                   
062400                              VALUE 'DLI-IO-WDK601-ERS'.                  
062500 01  DLI-IO-AREA-WDK601-ERS.                                              
062600*     03  -COPY WDK601       -PRE ERS-                                    
062700                                                                          
062800     EJECT                                                                
062900                                                                          
063000 01  FILLER                   PIC X(24)                                   
063100                              VALUE 'DLI-IO-WDK611-ERS'.                  
063200 01  DLI-IO-AREA-WDK611-ERS.                                              
063300*     03  -COPY WDK611       -PRE ERS-                                    
063400                                                                          
063500     EJECT                                                                
063600                                                                          
063700 01  FILLER                   PIC X(24)                                   
063800                              VALUE 'DLI-IO-WDK711-ERS'.                  
063900 01  DLI-IO-AREA-WDK711-ERS.                                              
064000*    03  -COPY WDK711        -PRE  ERS-                                   
064100     EJECT                                                                
064200                                                                          
064300 01  FILLER                   PIC X(24) VALUE 'DLI-IO-WDL601'.            
064400 01  DLI-IO-AREA-WDL601.                                                  
064500*    03  -COPY WDL601                                                     
064600     EJECT                                                                
064700                                                                          
064800 01  FILLER                   PIC X(24) VALUE 'DLI-IO-WDL611'.            
064900 01  DLI-IO-AREA-WDL611.                                                  
065000*     03  -COPY WDL611                                                    
065100                                                                          
065200     EJECT                                                                
065300 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDD7A1'.            
065400     SKIP3                                                                
065500 01  DLI-IO-AREA-WDD7A1.                                                  
065600*        05  -COPY WDD7A1    -PRE ERSB-                                   
065700     EJECT                                                                
065800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
065900 01   DLI-IO-AREA-B601.                                                   
066000*     03  -COPY WDB601                                                    
066100                                                                          
066200 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
066300 01   DLI-IO-AREA-B616.                                                   
066400*     03  -COPY WDB616                                                    
066500                                                                          
066600     EJECT                                                                
066700 LINKAGE SECTION.                                                         
066800     SKIP3                                                                
066900*01  AREA  -COPY W61159      -PRE LINK-.                                  
067000     EJECT                                                                
067100*01  -COPY W0008  -PRE WDK6-.                                             
067200     05  FILLER              PIC X.                                       
067300     EJECT                                                                
067400*01  -COPY W0008  -PRE WDK7-.                                             
067500     05  FILLER              PIC X.                                       
067600     EJECT                                                                
067700*01  -COPY W0008  -PRE 2501-                                              
067800     05  FILLER                  PIC X.                                   
067900     EJECT                                                                
068000*01  -COPY W0008  -PRE WDK6-2-.                                           
068100     05  FILLER              PIC X.                                       
068200     EJECT                                                                
068300*01  -COPY W0008  -PRE WDK7-2-.                                           
068400     05  FILLER              PIC X.                                       
068500     EJECT                                                                
068600*01  -COPY W0008  -PRE WDL6-                                              
068700     05  FILLER                  PIC X.                                   
068800     EJECT                                                                
068900*01  -COPY W0008  -PRE WDD7A-                                             
069000     05  FILLER                  PIC X.                                   
069100     EJECT                                                                
069200*01  -COPY W0008      -PRE WDB6-                                          
069300     05  FILLER                  PIC X.                                   
069400                                                                          
069500     EJECT                                                                
069600 PROCEDURE DIVISION USING LINK-AREA WDK6-PCB WDK7-PCB                     
069700                                    2501-PCB                              
069800                                    WDK6-2-PCB WDK7-2-PCB                 
069900                                    WDL6-PCB WDD7A-PCB                    
070000                                    WDB6-PCB.                             
070100     PERFORM A-INITIERA                                                   
070200                                                                          
070300     MOVE LINK-IDARTNR TO W-IDARTNR                                       
070400     PERFORM IMS-GU-K601                                                  
070500                                                                          
070600     IF  SEGMENT-FINNS                                                    
070700       MOVE JA                TO LINK-FLJANEJ-ANROP                       
070800                                                                          
070900       PERFORM IMS-GNP-K611                                               
071000       IF SEGMENT-FINNS                                                   
071100                                                                          
071200         DIVIDE ART-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                   
071300                                                                          
071400         MOVE LINK-IDDC TO WS-IDDC                                        
071500                           W-IDDC-B6                                      
071600         PERFORM IMS-GU-WDB601                                            
071700         IF DCS-SDC                                                       
071800            PERFORM G-BERAKNA-SDCBEHOV                                    
071900         END-IF                                                           
072000         IF DCS-NDC-NA                                                    
072100         OR DCS-NDC-PF                                                    
072110         OR DCS-NDC-OTHERS                                                
072200         OR DCS-NDC-CN                                                    
072300            PERFORM H-BERAKNA-NDCBEHOV                                    
072400         END-IF                                                           
072500       ELSE                                                               
072600           MOVE NEJ TO LINK-FLJANEJ-ANROP                                 
072700       END-IF                                                             
072800     ELSE                                                                 
072900         MOVE NEJ TO LINK-FLJANEJ-ANROP                                   
073000     END-IF                                                               
073100     SKIP1                                                                
073200     SKIP1                                                                
073300******************************************************************        
073400*                                                                *        
073500*    DUMPA PROGRAMMET                                            *        
073600*                                                                *        
073700******************************************************************        
073800*                                                                         
073900*    IF  LINK-IDARTNR = 181799                                            
074000*    AND LINK-IDDC = '42'                                                 
074100*        STRING 'FRAMTVINGAD DUMP I W61159     '                          
074200*        DELIMITED BY SIZE INTO FELTEXT-STR                               
074300*        CALL FELLOG                                                      
074400*    END-IF                                                               
074500     MOVE ZERO TO RETURN-CODE                                             
074600     GOBACK                                                               
074700     .                                                                    
074800     EJECT                                                                
074900 A-INITIERA SECTION.                                                      
075000******************************************************************        
075100*                                                                *        
075200*    BERÄKNING AV START- OCH SLUT-TIDPUNKTER (ÅR OCH VECKA)      *        
075300*    FÖR BERÄKNING                                               *        
075400*    NOLLSTÄLLNING AV TABELLER                                   *        
075500*                                                                *        
075600******************************************************************        
075700     SKIP1                                                                
075800     MOVE FUNCTION CURRENT-DATE                                           
075900                             TO WS-CURRENT-DATE                           
076000                                                                          
076100     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
076200     MOVE WS-DAGENS-DATUM (3:6)                                           
076300                             TO DAT-I-TIDATUM                             
076400                                                                          
076500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
076600                     DAT-O-TIDATUM DAT-KDSVAR                             
076700                                                                          
076800     IF DAT-KDSVAR-OK                                                     
076900                                                                          
077000       MOVE DAT-TIVV         TO WS-DAGENS-VECKA                           
077100                                                                          
077200     ELSE                                                                 
077300         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
077400         DELIMITED BY SIZE INTO FELTEXT                                   
077500         CALL FELLOG                                                      
077600     END-IF                                                               
077700                                                                          
077800     IF  LINK-KVVECKOR-BEHOV > +70                                        
077900         MOVE +70  TO LINK-KVVECKOR-BEHOV                                 
078000     END-IF                                                               
078100     MOVE LINK-KVVECKOR-BEHOV                                             
078200                             TO WS-CALL-KVVECKOR-BEHOV                    
078300     PERFORM S07-INIT-DATUM                                               
078400     SKIP1                                                                
078500     MOVE ZERO               TO LINK-KVBEHOV-DESSUTOM                     
078600     MOVE 1                  TO LINK-IX                                   
078700     PERFORM UNTIL LINK-IX > 70                                           
078800       MOVE ZERO             TO LINK-KVBEHOV-VECKA (LINK-IX)              
078900                                LINK-TIAAVVD-BEHOV (LINK-IX)              
079000       ADD 1                 TO LINK-IX                                   
079100     END-PERFORM                                                          
079200     MOVE ZERO               TO LINK-IX                                   
079300     MOVE WS-NOLLA-KVBEHOV   TO WS-KVBEHOV-TABELL                         
079400     MOVE WS-NOLLA-RESEASON  TO WS-RESEASON-TABELL                        
079500                                WS-RESEASON-AVR-TABELL                    
079600     PERFORM AA-HAMTA-VV-I-PER                                            
079700     .                                                                    
079800 AA-HAMTA-VV-I-PER SECTION.                                               
079900                                                                          
080000     MOVE +1                 TO IX                                        
080100     MOVE DAT-TIAARP         TO WS-TIAAPER                                
080200     MOVE 1                  TO PER                                       
080300                                                                          
080400     PERFORM UNTIL IX        >  12                                        
080500       MOVE 'AARP  '         TO DAT-KDDATFORM                             
080600       MOVE TIAAPER          TO DAT-I-TIDATUM                             
080700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
080800                           DAT-O-TIDATUM DAT-KDSVAR                       
080900       IF DAT-KDSVAR-OK                                                   
081000          MOVE DAT-TIVV      TO PER-START-VV (IX)                         
081100       ELSE                                                               
081200         MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                     
081300         CALL FELLOG                                                      
081400       END-IF                                                             
081500       ADD +1                TO IX                                        
081600                               PER                                        
081700     END-PERFORM                                                          
081800                                                                          
081900     MOVE 1                  TO IX-TILL                                   
082000     MOVE 2                  TO IX-FRAN                                   
082100     PERFORM UNTIL IX-FRAN   >  12                                        
082200       COMPUTE PER-SLUT-VV (IX-TILL) =                                    
082300               PER-START-VV (IX-FRAN) - 1                                 
082400       ADD 1                 TO IX-TILL                                   
082500                                IX-FRAN                                   
082600     END-PERFORM                                                          
082700     MOVE 52                 TO PER-SLUT-VV (12)                          
082800     .                                                                    
082900     EJECT                                                                
083000 G-BERAKNA-SDCBEHOV   SECTION.                                            
083100******************************************************************        
083200*                                                                *        
083300*    BERÄKNING AV SDC'ERNAS BEHOV AV EN ARTIKEL                  *        
083400*                                                                *        
083500******************************************************************        
083600                                                                          
083700     MOVE ZERO                TO W-SDC-ACC-KVBEHOV                        
083800     MOVE W-TIFINLV-AAVV      TO W-TIFINLV-AAVV-MINUS-2                   
083900     MOVE -2                  TO W-ANTAL-VECKOR                           
084000     CALL W009VADD USING W-TIFINLV-AAVV-MINUS-2 W-ANTAL-VECKOR            
084100                                                                          
084200     ACCEPT W-TIME         FROM TIME                                      
084300                                                                          
084400     IF CLAG-FLREFILL = JA AND CLAG-REDIRLEV < 1.00                       
084500       PERFORM IMS-GU-WDK701                                              
084600       IF SEGMENT-FINNS                                                   
084700         PERFORM GC-LAES-WDK711                                           
084800         IF SEGMENT-FINNS                                                 
084900           MOVE W-TIFINLV-AAVV-MINUS-2                                    
085000                             TO W-DATUM-FORSTA-SDC                        
085100                                                                          
085200           MOVE 'AAVV  '     TO DAT-KDDATFORM                             
085300           MOVE LINK-TIAAVV-AKTUELL                                       
085400                             TO DAT-I-TIDATUM                             
085500                                                                          
085600           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
085700                             DAT-O-TIDATUM DAT-KDSVAR                     
085800                                                                          
085900           IF DAT-KDSVAR-OK                                               
086000             MOVE DAT-TIAARP(3:2) TO W-PER                                
086100             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
086200           ELSE                                                           
086300               STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                    
086400               DELIMITED BY SIZE INTO FELTEXT                             
086500               CALL FELLOG                                                
086600           END-IF                                                         
086700                                                                          
086800           COMPUTE W-SDC-KVBEHOV-VECKA ROUNDED =                          
086900                               ((SLAG-KVPB-REF * 12) / 52) *              
087000                                 SLAG-RESEASON (W-PER)                    
087100           COMPUTE W-SDC-TILLGANG = SLAG-KVLS +                           
087200                                      SLAG-KVAKS-PAV +                    
087300                                      SLAG-KVAKS-SDC +                    
087400                                      SLAG-KVBEART -                      
087500                                      SLAG-KVOKS-DAG -                    
087600                                      SLAG-KVOKS-BULK -                   
087700                                      SLAG-KVROS-BULK -                   
087800                                      SLAG-KVROS-DAG                      
087900           PERFORM S08-BER-BALANCE-ERSATT                                 
088000           PERFORM S09-KUNDRETURER                                        
088100           COMPUTE W-SDC-TILLGANG = W-SDC-TILLGANG                        
088200                                    - WS-BALANCE-ERSATT                   
088300                                    + WS-KVKUNDRETUR                      
088400           IF  LINK-TID-AKTUELL > 5                                       
088500               CONTINUE                                                   
088600           ELSE                                                           
088700             PERFORM GA-BERAKNA-INNEV-VECKA-SDC                           
088800           END-IF                                                         
088900           PERFORM GB-SDCBEHOV                                            
089000         END-IF                                                           
089100       END-IF                                                             
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500 GA-BERAKNA-INNEV-VECKA-SDC SECTION.                                      
089600******************************************************************        
089700*                                                                *        
089800*    BERÄKNING AV SDC'ERNAS BEHOV AV EN ARTIKEL I INNEVARANDA    *        
089900*    VECKA.                                                      *        
090000*                                                                *        
090100******************************************************************        
090200                                                                          
090300     MOVE W-DATUM-FORSTA-SDC    TO TMP1-YYWW                              
090400     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
090500             MOVE '121110'        TO FELTEXT2-STR                         
090600     PERFORM WY2000P3                                                     
090700     IF TMP1-YYWW <= TMP2-YYWW                                            
090800        COMPUTE W-DAGAR-KVAR = 6 - LINK-TID-AKTUELL                       
090900                                                                          
091000        COMPUTE W-VECKODEL  = W-DAGAR-KVAR / 5                            
091100        COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED =                           
091200                  W-SDC-KVBEHOV-VECKA * W-VECKODEL                        
091300                                                                          
091400        IF  SLAG-RESEASON (1) = 1.00                                      
091500        AND SLAG-RESEASON (2) = 1.00                                      
091600        AND SLAG-RESEASON (3) = 1.00                                      
091700        AND SLAG-RESEASON (4) = 1.00                                      
091800        AND SLAG-RESEASON (5) = 1.00                                      
091900        AND SLAG-RESEASON (6) = 1.00                                      
092000        AND SLAG-RESEASON (7) = 1.00                                      
092100        AND SLAG-RESEASON (8) = 1.00                                      
092200        AND SLAG-RESEASON (9) = 1.00                                      
092300        AND SLAG-RESEASON (10) = 1.00                                     
092400        AND SLAG-RESEASON (11) = 1.00                                     
092500        AND SLAG-RESEASON (12) = 1.00                                     
092600           MOVE SLAG-KVREFPKT                                             
092700                             TO WS-KVREFPKT                               
092800           MOVE SLAG-KVREFBER                                             
092900                             TO WS-KVREFBER                               
093000        ELSE                                                              
093100                                                                          
093200*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
093300          MOVE SLAG-IDDC     TO REF2-IDDC                                 
093400          MOVE SLAG-IDREFTAB TO REF2-IDREFTAB                             
093500          MOVE SLAG-FLWILSON TO REF2-FLWILSON                             
093600          MOVE ZERO          TO REF2-PRARTBES                             
093700          MOVE SLAG-KVPB-REF TO REF2-KVPB-REF                             
093800          MOVE SLAG-RESEASON (1)                                          
093900                             TO REF2-RESEASON (1)                         
094000          MOVE SLAG-RESEASON (2)                                          
094100                             TO REF2-RESEASON (2)                         
094200          MOVE SLAG-RESEASON (3)                                          
094300                             TO REF2-RESEASON (3)                         
094400          MOVE SLAG-RESEASON (4)                                          
094500                             TO REF2-RESEASON (4)                         
094600          MOVE SLAG-RESEASON (5)                                          
094700                             TO REF2-RESEASON (5)                         
094800          MOVE SLAG-RESEASON (6)                                          
094900                             TO REF2-RESEASON (6)                         
095000          MOVE SLAG-RESEASON (7)                                          
095100                             TO REF2-RESEASON (7)                         
095200          MOVE SLAG-RESEASON (8)                                          
095300                             TO REF2-RESEASON (8)                         
095400          MOVE SLAG-RESEASON (9)                                          
095500                             TO REF2-RESEASON (9)                         
095600          MOVE SLAG-RESEASON (10)                                         
095700                             TO REF2-RESEASON (10)                        
095800          MOVE SLAG-RESEASON (11)                                         
095900                             TO REF2-RESEASON (11)                        
096000          MOVE SLAG-RESEASON (12)                                         
096100                             TO REF2-RESEASON (12)                        
096200          MOVE SLAG-FLFLYG   TO REF2-FLFLYG                               
096300          MOVE W-BINNDAY     TO REF2-BINNDAY-TIAAMMDD                     
096400          MOVE ZERO          TO REF2-KVREFPKT                             
096500                                REF2-KVREFBER                             
096600          MOVE SLAG-TIREFPAF TO TMP1-YYMMDD                               
096700          MOVE W-BINNDAY     TO TMP2-YYMMDD                               
096800*                                                                         
096900*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
097000*                                                                         
097100          PERFORM WY2000P1                                                
097200          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
097300*                                                                         
097400*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
097500*                                                                         
097600            MOVE SLAG-KVREFBER TO REF2-IN-KVREFBER                        
097700          ELSE                                                            
097800            MOVE +0          TO REF2-IN-KVREFBER                          
097900          END-IF                                                          
098000                                                                          
098100          MOVE SLAG-TIREFPKT TO TMP1-YYMMDD                               
098200          MOVE W-BINNDAY     TO TMP2-YYMMDD                               
098300*                                                                         
098400*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
098500*                                                                         
098600          PERFORM WY2000P1                                                
098700          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
098800*                                                                         
098900*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
099000*                                                                         
099100            MOVE SLAG-KVREFPKT TO REF2-IN-KVREFPKT                        
099200          ELSE                                                            
099300            MOVE +0          TO REF2-IN-KVREFPKT                          
099400          END-IF                                                          
099500                                                                          
099600          PERFORM W271REF2                                                
099700          MOVE REF2-KVREFPKT TO WS-KVREFPKT                               
099800          MOVE REF2-KVREFBER TO WS-KVREFBER                               
099900        END-IF                                                            
100000        COMPUTE W-SDC-KVBEHOV-DAG ROUNDED =                               
100100                W-SDC-KVBEHOV-VECKA / 5                                   
100200                                                                          
100300        MOVE LINK-TID-AKTUELL                                             
100400                             TO IX                                        
100500        PERFORM UNTIL IX > +5                                             
100600                                                                          
100700           ADD W-SDC-KVBEHOV-DAG                                          
100800                             TO W-SDC-ACC-KVBEHOV                         
100900           COMPUTE W-SDC-KVAR-EFT-DAG ROUNDED =                           
101000                   W-SDC-TILLGANG - W-SDC-ACC-KVBEHOV                     
101100                                                                          
101200                                                                          
101300          IF W-SDC-KVAR-EFT-DAG    < WS-KVREFPKT                          
101400             COMPUTE W-SDC-DIFF    = WS-KVREFPKT -                        
101500                                     W-SDC-KVAR-EFT-DAG                   
101600             COMPUTE W-DC-REFILL-KVANT =                                  
101700                     W-SDC-DIFF + WS-KVREFBER                             
101800             PERFORM S06-SATT-QX-PROCENT-BRYTNING                         
101900             PERFORM S05-BERAKNA-Q-KVANT                                  
102000             ADD 1           TO LINK-IX                                   
102100             MOVE W-DC-REFILL-KVANT                                       
102200                             TO LINK-KVBEHOV-VECKA (LINK-IX)              
102300             ADD W-DC-REFILL-KVANT                                        
102400                             TO W-SDC-TILLGANG                            
102500             COMPUTE LINK-TIAAVVD-BEHOV (LINK-IX) =                       
102600                    (LINK-TIAAVV-AKTUELL * 10) + IX                       
102700          END-IF                                                          
102800          ADD 1              TO IX                                        
102900        END-PERFORM                                                       
103000     END-IF                                                               
103100     .                                                                    
103200     EJECT                                                                
103300 GB-SDCBEHOV            SECTION.                                          
103400******************************************************************        
103500*                                                                *        
103600*    BERÄKNING AV SDC'ERNAS BEHOV AV EN ARTIKEL I KOMMANDE       *        
103700*    VECKOR.                                                     *        
103800*                                                                *        
103900******************************************************************        
104000                                                                          
104100     MOVE +1                 TO BER-IY                                    
104200     MOVE W-BER-START-VV     TO BER-IX                                    
104300     MOVE LINK-TIBEHOV-START TO W-BER-DATUM                               
104400                                W-AAVV                                    
104500     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
104600     MOVE W-AAVV             TO DAT-I-TIDATUM                             
104700                                                                          
104800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
104900                         DAT-O-TIDATUM DAT-KDSVAR                         
105000                                                                          
105100     IF DAT-KDSVAR-OK                                                     
105200       MOVE DAT-TIAARP(3:2) TO W-PER                                      
105300       MOVE DAT-TIAAMMDD    TO W-BINNDAY                                  
105400     ELSE                                                                 
105500         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
105600         DELIMITED BY SIZE INTO FELTEXT                                   
105700         CALL FELLOG                                                      
105800     END-IF                                                               
105900                                                                          
106000     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
106100     OR            LINK-IX   =  70                                        
106200                                                                          
106300         PERFORM UNTIL                                                    
106400            (BER-IY = BER-ANTAL-AA AND                                    
106500             BER-IX > BER-ANTAL-VV)                                       
106600         OR (BER-IY < BER-ANTAL-AA AND                                    
106700             BER-IX > 52)                                                 
106800         OR  LINK-IX   =  70                                              
106900                                                                          
107000           COMPUTE W-SDC-KVBEHOV-VECKA ROUNDED =                          
107100                         ((SLAG-KVPB-REF * 12) / 52) *                    
107200                         SLAG-RESEASON (W-PER)                            
107300           MOVE W-DATUM-FORSTA-SDC   TO TMP1-YYWW                         
107400           MOVE W-BER-DATUM          TO TMP2-YYWW                         
107500             MOVE '126410'        TO FELTEXT2-STR                         
107600           PERFORM WY2000P3                                               
107700           IF TMP1-YYWW <= TMP2-YYWW                                      
107800                                                                          
107900             IF SLAG-RESEASON (1) = 1.00                                  
108000             AND SLAG-RESEASON (2) = 1.00                                 
108100             AND SLAG-RESEASON (3) = 1.00                                 
108200             AND SLAG-RESEASON (4) = 1.00                                 
108300             AND SLAG-RESEASON (5) = 1.00                                 
108400             AND SLAG-RESEASON (6) = 1.00                                 
108500             AND SLAG-RESEASON (7) = 1.00                                 
108600             AND SLAG-RESEASON (8) = 1.00                                 
108700             AND SLAG-RESEASON (9) = 1.00                                 
108800             AND SLAG-RESEASON (10) = 1.00                                
108900             AND SLAG-RESEASON (11) = 1.00                                
109000             AND SLAG-RESEASON (12) = 1.00                                
109100               MOVE SLAG-KVREFPKT                                         
109200                             TO WS-KVREFPKT                               
109300               MOVE SLAG-KVREFBER                                         
109400                             TO WS-KVREFBER                               
109500             ELSE                                                         
109600                                                                          
109700*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
109800               MOVE SLAG-IDDC                                             
109900                             TO REF2-IDDC                                 
110000               MOVE SLAG-IDREFTAB                                         
110100                             TO REF2-IDREFTAB                             
110200               MOVE SLAG-FLWILSON                                         
110300                             TO REF2-FLWILSON                             
110400               MOVE ZERO     TO REF2-PRARTBES                             
110500               MOVE SLAG-KVPB-REF                                         
110600                             TO REF2-KVPB-REF                             
110700               MOVE SLAG-RESEASON (1)                                     
110800                             TO REF2-RESEASON (1)                         
110900               MOVE SLAG-RESEASON (2)                                     
111000                             TO REF2-RESEASON (2)                         
111100               MOVE SLAG-RESEASON (3)                                     
111200                             TO REF2-RESEASON (3)                         
111300               MOVE SLAG-RESEASON (4)                                     
111400                             TO REF2-RESEASON (4)                         
111500               MOVE SLAG-RESEASON (5)                                     
111600                             TO REF2-RESEASON (5)                         
111700               MOVE SLAG-RESEASON (6)                                     
111800                             TO REF2-RESEASON (6)                         
111900               MOVE SLAG-RESEASON (7)                                     
112000                             TO REF2-RESEASON (7)                         
112100               MOVE SLAG-RESEASON (8)                                     
112200                             TO REF2-RESEASON (8)                         
112300               MOVE SLAG-RESEASON (9)                                     
112400                             TO REF2-RESEASON (9)                         
112500               MOVE SLAG-RESEASON (10)                                    
112600                             TO REF2-RESEASON (10)                        
112700               MOVE SLAG-RESEASON (11)                                    
112800                             TO REF2-RESEASON (11)                        
112900               MOVE SLAG-RESEASON (12)                                    
113000                             TO REF2-RESEASON (12)                        
113100               MOVE SLAG-FLFLYG TO REF2-FLFLYG                            
113200               MOVE W-BINNDAY TO REF2-BINNDAY-TIAAMMDD                    
113300               MOVE ZERO     TO REF2-KVREFPKT                             
113400                                REF2-KVREFBER                             
113500               MOVE SLAG-TIREFPAF                                         
113600                             TO TMP1-YYMMDD                               
113700               MOVE W-BINNDAY TO TMP2-YYMMDD                              
113800*                                                                         
113900*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
114000*                                                                         
114100               PERFORM WY2000P1                                           
114200               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
114300*                                                                         
114400*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
114500*                                                                         
114600                 MOVE SLAG-KVREFBER                                       
114700                             TO REF2-IN-KVREFBER                          
114800               ELSE                                                       
114900                 MOVE +0     TO REF2-IN-KVREFBER                          
115000               END-IF                                                     
115100                                                                          
115200               MOVE SLAG-TIREFPKT                                         
115300                             TO TMP1-YYMMDD                               
115400               MOVE W-BINNDAY TO TMP2-YYMMDD                              
115500*                                                                         
115600*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
115700*                                                                         
115800               PERFORM WY2000P1                                           
115900               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
116000*                                                                         
116100*-----   MANUELL PÅFYLLNADSPUNKT ÄR SATT                                  
116200*                                                                         
116300                 MOVE SLAG-KVREFPKT                                       
116400                             TO REF2-IN-KVREFPKT                          
116500               ELSE                                                       
116600                 MOVE +0     TO REF2-IN-KVREFPKT                          
116700               END-IF                                                     
116800                                                                          
116900               PERFORM W271REF2                                           
117000               MOVE REF2-KVREFPKT                                         
117100                             TO WS-KVREFPKT                               
117200               MOVE REF2-KVREFBER                                         
117300                             TO WS-KVREFBER                               
117400                                                                          
117500             END-IF                                                       
117600                                                                          
117700             COMPUTE W-SDC-KVBEHOV-DAG ROUNDED =                          
117800                     W-SDC-KVBEHOV-VECKA / 5                              
117900                                                                          
118000             MOVE +1         TO IX                                        
118100             PERFORM UNTIL IX > +5                                        
118200             OR LINK-IX = 70                                              
118300                                                                          
118400               ADD W-SDC-KVBEHOV-DAG                                      
118500                         TO W-SDC-ACC-KVBEHOV                             
118600               COMPUTE W-SDC-KVAR-EFT-DAG ROUNDED =                       
118700                       W-SDC-TILLGANG - W-SDC-ACC-KVBEHOV                 
118800                                                                          
118900               IF W-SDC-KVAR-EFT-DAG   < WS-KVREFPKT                      
119000                  ADD 1      TO LINK-IX                                   
119100                  COMPUTE W-SDC-DIFF   = WS-KVREFPKT -                    
119200                                        W-SDC-KVAR-EFT-DAG                
119300                  COMPUTE W-DC-REFILL-KVANT =                             
119400                          W-SDC-DIFF + WS-KVREFBER                        
119500                  PERFORM S06-SATT-QX-PROCENT-BRYTNING                    
119600                  PERFORM S05-BERAKNA-Q-KVANT                             
119700                  MOVE W-DC-REFILL-KVANT                                  
119800                             TO LINK-KVBEHOV-VECKA (LINK-IX)              
119900                  COMPUTE LINK-TIAAVVD-BEHOV (LINK-IX) =                  
120000                              (W-BER-DATUM * 10) + IX                     
120100                  END-COMPUTE                                             
120200                  ADD W-DC-REFILL-KVANT                                   
120300                            TO W-SDC-TILLGANG                             
120400               END-IF                                                     
120500                                                                          
120600               ADD +1        TO IX                                        
120700             END-PERFORM                                                  
120800           END-IF                                                         
120900                                                                          
121000           ADD 1                   TO BER-IX                              
121100                                      W-BER-DATUM                         
121200         END-PERFORM                                                      
121300         ADD 1                     TO BER-IY                              
121400         MOVE 1                    TO BER-IX                              
121500                                      IX-PER                              
121600         SUBTRACT +52              FROM  W-BER-DATUM                      
121700         ADD +100                  TO    W-BER-DATUM                      
121800     END-PERFORM                                                          
121900                                                                          
122000     .                                                                    
122100     EJECT                                                                
122200 GC-LAES-WDK711         SECTION.                                          
122300******************************************************************        
122400*                                                                *        
122500*    LÄSER WDK711                                                *        
122600*                                                                *        
122700******************************************************************        
122800                                                                          
122900     PERFORM IMS-GNP-WDK711                                               
123000     IF SEGMENT-FINNS                                                     
123100       MOVE SLAG-IDDC TO WS-IDDC                                          
123200                         W-IDDC-B6                                        
123300       PERFORM IMS-GU-WDB601                                              
123400     END-IF                                                               
123500                                                                          
123600     PERFORM UNTIL SEGMENT-SAKNAS                                         
123700     OR (DCS-SDC                                                          
123800     AND SLAG-KDREFSTA = 'A'                                              
123900     AND SLAG-FLCDCBEH = JA                                               
124000     AND (LINK-IDDC = '11'                                                
124100     OR  LINK-IDDC = SLAG-IDDC))                                          
124200*       LÄS FRAM TILL EN SDC-POST                                         
124300*       DVS LÄS FÖRBI NDC-POSTER                                          
124400        PERFORM IMS-GNP-WDK711                                            
124500        IF SEGMENT-FINNS                                                  
124600          MOVE SLAG-IDDC TO WS-IDDC                                       
124700                            W-IDDC-B6                                     
124800          PERFORM IMS-GU-WDB601                                           
124900        END-IF                                                            
125000     END-PERFORM                                                          
125100                                                                          
125200     IF SEGMENT-FINNS                                                     
125300*** FIX FÖR NEGATIVA KVOKS-BULK                                           
125400        IF SLAG-KVOKS-BULK < 0                                            
125500          MOVE ZERO          TO SLAG-KVOKS-BULK                           
125600        END-IF                                                            
125700*** FIX FÖR NEGATIVA KVOKS-DAG                                            
125800        IF SLAG-KVOKS-DAG < 0                                             
125900          MOVE ZERO          TO SLAG-KVOKS-DAG                            
126000        END-IF                                                            
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400 H-BERAKNA-NDCBEHOV   SECTION.                                            
126500******************************************************************        
126600*                                                                *        
126700*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL                  *        
126800*    BEHOVET VISAS DEN VECKA CDC BEHÖVER LEVERERA                *        
126900*    DVS BEHOVSDAG PÅ NDC MINUS LEDTID                           *        
127000*                                                                *        
127100******************************************************************        
127200                                                                          
127300     MOVE ZERO                TO W-NDC-ACC-KVBEHOV                        
127400     MOVE WS-BER-TAB-NOLL     TO BER-TAB                                  
127500     MOVE W-TIFINLV-AAVV      TO W-TIFINLV-AAVV-MINUS-2                   
127600     MOVE -2                  TO W-ANTAL-VECKOR                           
127700     CALL W009VADD USING W-TIFINLV-AAVV-MINUS-2 W-ANTAL-VECKOR            
127800                                                                          
127900     ACCEPT W-TIME         FROM TIME                                      
128000                                                                          
128100     IF CLAG-FLREFILL = JA AND CLAG-REDIRLEV < 1.00                       
128200       PERFORM IMS-GU-WDK701                                              
128300       IF SEGMENT-FINNS                                                   
128400         PERFORM HD-LAES-WDK711                                           
128500         IF SEGMENT-FINNS                                                 
128600*  NDC                                                                    
128700*                                                                         
128800*          NDC-US HÄMTAS FRÅN WWDC99                                      
128900*          MHA WS-IDDC                                                    
129000*                                                                         
129100                                                                          
129200           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
129300           PERFORM IMS-GNP-WDK712                                         
129400           IF SEGMENT-FINNS                                               
129500             MOVE LART-DAPUBL TO WS-DAPUBL                                
129600           END-IF                                                         
129700           IF WS-DAPUBL > ZERO                                            
129800             PERFORM S33-BER-IX-AKTUELLT-DC                               
129900             MOVE REF-KVDLTID-TOT                                         
130000                         TO DAG-KVKALDAG                                  
130100             MOVE LART-DAPUBL (3:6)                                       
130200                         TO DAG-TIAAMMDD-TOM                              
130300             MOVE 003        TO DAG-KDCALL                                
130400             CALL WDAGKONV USING DAG-KDCALL                               
130500                                 DAG-DATUM-AREA                           
130600                                 DAG-KDSVAR                               
130700             IF DAG-KDSVAR = SPACE                                        
130800               CONTINUE                                                   
130900             ELSE                                                         
131000               MOVE 'FEL FRÅN WDAGKONV, I H-BERAKNA'                      
131100                           TO         FELTEXT-STR                         
131200                DISPLAY FELTEXT                                           
131300                CALL FELLOG                                               
131400             END-IF                                                       
131500                                                                          
131600             MOVE DAG-TIAAMMDD-FOM                                        
131700                             TO DAT-I-TIDATUM                             
131800             MOVE 'AAMMDD'   TO DAT-KDDATFORM                             
131900                                                                          
132000             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
132100                                   DAT-O-TIDATUM DAT-KDSVAR               
132200                                                                          
132300             IF DAT-KDSVAR-OK                                             
132400               MOVE DAT-TIAAVV-GRP                                        
132500                             TO W-TIAAVV                                  
132600               MOVE W-TIAAVV TO TMP1-YYWW                                 
132700               MOVE W-TIFINLV-AAVV-MINUS-2                                
132800                             TO TMP2-YYWW                                 
132900               PERFORM WY2000P3                                           
133000               IF TMP1-YYWW <= TMP2-YYWW                                  
133100                 MOVE W-TIFINLV-AAVV-MINUS-2                              
133200                             TO W-DATUM-FORSTA-SDC                        
133300               ELSE                                                       
133400                 MOVE DAT-TIAAVV-GRP                                      
133500                             TO W-TIAAVV                                  
133600                 MOVE W-TIAAVV                                            
133700                             TO W-DATUM-FORSTA-SDC                        
133800               END-IF                                                     
133900             ELSE                                                         
134000                 STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                  
134100                 DELIMITED BY SIZE INTO FELTEXT                           
134200                 CALL FELLOG                                              
134300             END-IF                                                       
134400           ELSE                                                           
134500             MOVE W-TIFINLV-AAVV-MINUS-2                                  
134600                               TO W-DATUM-FORSTA-SDC                      
134700           END-IF                                                         
134800                                                                          
134900           MOVE 'AAVV  '     TO DAT-KDDATFORM                             
135000           MOVE LINK-TIAAVV-AKTUELL                                       
135100                                   TO DAT-I-TIDATUM                       
135200                                                                          
135300           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
135400                             DAT-O-TIDATUM DAT-KDSVAR                     
135500                                                                          
135600           IF DAT-KDSVAR-OK                                               
135700             MOVE DAT-TIAARP(3:2) TO W-PER                                
135800             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
135900           ELSE                                                           
136000               STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                    
136100               DELIMITED BY SIZE INTO FELTEXT                             
136200               CALL FELLOG                                                
136300           END-IF                                                         
136400                                                                          
136500           COMPUTE W-NDC-KVBEHOV-VECKA ROUNDED =                          
136600                               ((SLAG-KVPB-REF * 12) / 52) *              
136700                               SLAG-RESEASON (W-PER)                      
136800           COMPUTE W-NDC-TILLGANG = SLAG-KVLS +                           
136900                                    SLAG-KVAKS-PAV +                      
137000                                    SLAG-KVAKS-SDC +                      
137100                                    SLAG-KVBEART -                        
137200                                    SLAG-KVOKS-BULK -                     
137300                                    SLAG-KVOKS-DAG -                      
137400                                    SLAG-KVROS-BULK -                     
137500                                    SLAG-KVROS-DAG                        
137600           PERFORM S08-BER-BALANCE-ERSATT                                 
137700           PERFORM S09-KUNDRETURER                                        
137800           COMPUTE W-NDC-TILLGANG = W-NDC-TILLGANG                        
137900                                  - WS-BALANCE-ERSATT                     
138000                                  + WS-KVKUNDRETUR                        
138100           MOVE ZERO       TO W-NDC-KVBEHOV-DESSUTOM                      
138200           IF LINK-TID-AKTUELL > 5                                        
138300               CONTINUE                                                   
138400           ELSE                                                           
138500               PERFORM HA-BERAKNA-INNEV-VECKA-NDC                         
138600           END-IF                                                         
138700           PERFORM HB-NDCBEHOV                                            
138800         END-IF                                                           
138900       END-IF                                                             
139000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300 HA-BERAKNA-INNEV-VECKA-NDC SECTION.                                      
139400******************************************************************        
139500*                                                                *        
139600*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL I INNEVARANDA    *        
139700*    VECKA.                                                      *        
139800*                                                                *        
139900******************************************************************        
140000                                                                          
140100     MOVE W-DATUM-FORSTA-SDC    TO TMP1-YYWW                              
140200     MOVE LINK-TIAAVV-AKTUELL   TO TMP2-YYWW                              
140300             MOVE '136810'        TO FELTEXT2-STR                         
140400     PERFORM WY2000P3                                                     
140500     IF TMP1-YYWW <= TMP2-YYWW                                            
140600        COMPUTE W-DAGAR-KVAR = 6 - LINK-TID-AKTUELL                       
140700                                                                          
140800        COMPUTE W-VECKODEL  = W-DAGAR-KVAR / 5                            
140900        COMPUTE W-KVBEHOV-INNEV-VECKA ROUNDED =                           
141000                  W-NDC-KVBEHOV-VECKA * W-VECKODEL                        
141100                                                                          
141200        IF  SLAG-RESEASON (1) = 1.00                                      
141300        AND SLAG-RESEASON (2) = 1.00                                      
141400        AND SLAG-RESEASON (3) = 1.00                                      
141500        AND SLAG-RESEASON (4) = 1.00                                      
141600        AND SLAG-RESEASON (5) = 1.00                                      
141700        AND SLAG-RESEASON (6) = 1.00                                      
141800        AND SLAG-RESEASON (7) = 1.00                                      
141900        AND SLAG-RESEASON (8) = 1.00                                      
142000        AND SLAG-RESEASON (9) = 1.00                                      
142100        AND SLAG-RESEASON (10) = 1.00                                     
142200        AND SLAG-RESEASON (11) = 1.00                                     
142300        AND SLAG-RESEASON (12) = 1.00                                     
142400           MOVE SLAG-KVREFPKT                                             
142500                             TO WS-KVREFPKT                               
142600           MOVE SLAG-KVREFBER                                             
142700                             TO WS-KVREFBER                               
142800        ELSE                                                              
142900*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
143000          MOVE SLAG-IDDC     TO REF2-IDDC                                 
143100          MOVE SLAG-IDREFTAB TO REF2-IDREFTAB                             
143200          MOVE SLAG-FLWILSON TO REF2-FLWILSON                             
143300          MOVE ZERO          TO REF2-PRARTBES                             
143400          MOVE SLAG-KVPB-REF TO REF2-KVPB-REF                             
143500          MOVE SLAG-RESEASON (1)                                          
143600                             TO REF2-RESEASON (1)                         
143700          MOVE SLAG-RESEASON (2)                                          
143800                             TO REF2-RESEASON (2)                         
143900          MOVE SLAG-RESEASON (3)                                          
144000                             TO REF2-RESEASON (3)                         
144100          MOVE SLAG-RESEASON (4)                                          
144200                             TO REF2-RESEASON (4)                         
144300          MOVE SLAG-RESEASON (5)                                          
144400                             TO REF2-RESEASON (5)                         
144500          MOVE SLAG-RESEASON (6)                                          
144600                             TO REF2-RESEASON (6)                         
144700          MOVE SLAG-RESEASON (7)                                          
144800                             TO REF2-RESEASON (7)                         
144900          MOVE SLAG-RESEASON (8)                                          
145000                             TO REF2-RESEASON (8)                         
145100          MOVE SLAG-RESEASON (9)                                          
145200                             TO REF2-RESEASON (9)                         
145300          MOVE SLAG-RESEASON (10)                                         
145400                             TO REF2-RESEASON (10)                        
145500          MOVE SLAG-RESEASON (11)                                         
145600                             TO REF2-RESEASON (11)                        
145700          MOVE SLAG-RESEASON (12)                                         
145800                             TO REF2-RESEASON (12)                        
145900          MOVE SLAG-FLFLYG   TO REF2-FLFLYG                               
146000          MOVE W-BINNDAY     TO REF2-BINNDAY-TIAAMMDD                     
146100          MOVE ZERO          TO REF2-KVREFPKT                             
146200                                REF2-KVREFBER                             
146300          MOVE SLAG-TIREFPAF TO TMP1-YYMMDD                               
146400          MOVE W-BINNDAY     TO TMP2-YYMMDD                               
146500*                                                                         
146600*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
146700*                                                                         
146800          PERFORM WY2000P1                                                
146900          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
147000*                                                                         
147100*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
147200*                                                                         
147300            MOVE SLAG-KVREFBER TO REF2-IN-KVREFBER                        
147400          ELSE                                                            
147500            MOVE +0          TO REF2-IN-KVREFBER                          
147600          END-IF                                                          
147700                                                                          
147800          MOVE SLAG-TIREFPKT TO TMP1-YYMMDD                               
147900          MOVE W-BINNDAY     TO TMP2-YYMMDD                               
148000*                                                                         
148100*-----     W-BINNDAY  = MÅNDAG I AKTUELL VECKA                            
148200*                                                                         
148300          PERFORM WY2000P1                                                
148400          IF TMP1-YYMMDD >= TMP2-YYMMDD                                   
148500*                                                                         
148600*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
148700*                                                                         
148800            MOVE SLAG-KVREFPKT TO REF2-IN-KVREFPKT                        
148900          ELSE                                                            
149000            MOVE +0          TO REF2-IN-KVREFPKT                          
149100          END-IF                                                          
149200                                                                          
149300          PERFORM W271REF2                                                
149400          MOVE REF2-KVREFPKT TO WS-KVREFPKT                               
149500          MOVE REF2-KVREFBER TO WS-KVREFBER                               
149600        END-IF                                                            
149700        COMPUTE W-NDC-KVBEHOV-DAG ROUNDED =                               
149800                W-NDC-KVBEHOV-VECKA / 5                                   
149900                                                                          
150000        MOVE LINK-TID-AKTUELL                                             
150100                             TO IX                                        
150200        PERFORM UNTIL IX > +5                                             
150300                                                                          
150400           ADD W-NDC-KVBEHOV-DAG                                          
150500                             TO W-NDC-ACC-KVBEHOV                         
150600           COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                           
150700                   W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV                     
150800                                                                          
150900           IF W-NDC-KVAR-EFT-DAG   < WS-KVREFPKT                          
151000              COMPUTE W-NDC-DIFF   = WS-KVREFPKT -                        
151100                                     W-NDC-KVAR-EFT-DAG                   
151200              COMPUTE W-DC-REFILL-KVANT =                                 
151300                      W-NDC-DIFF + WS-KVREFBER                            
151400              PERFORM S06-SATT-QX-PROCENT-BRYTNING                        
151500              PERFORM S05-BERAKNA-Q-KVANT                                 
151600              ADD 1          TO LINK-IX                                   
151700              MOVE W-DC-REFILL-KVANT                                      
151800                             TO LINK-KVBEHOV-VECKA (LINK-IX)              
151900              ADD W-DC-REFILL-KVANT                                       
152000                             TO W-NDC-TILLGANG                            
152100              COMPUTE LINK-TIAAVVD-BEHOV (LINK-IX) =                      
152200                     (LINK-TIAAVV-AKTUELL * 10) + IX                      
152300           END-IF                                                         
152400                                                                          
152500           ADD +1            TO IX                                        
152600        END-PERFORM                                                       
152700     END-IF                                                               
152800     .                                                                    
152900     EJECT                                                                
153000 HB-NDCBEHOV            SECTION.                                          
153100******************************************************************        
153200*                                                                *        
153300*    BERÄKNING AV NDC'ERNAS BEHOV AV EN ARTIKEL I KOMMANDE       *        
153400*    VECKOR.                                                     *        
153500*                                                                *        
153600******************************************************************        
153700                                                                          
153800     MOVE +1                 TO BER-IY                                    
153900     MOVE W-BER-START-VV     TO BER-IX                                    
154000     MOVE LINK-TIBEHOV-START TO W-BER-DATUM                               
154100                                W-AAVV                                    
154200     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
154300     MOVE W-AAVV             TO DAT-I-TIDATUM                             
154400                                                                          
154500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
154600                         DAT-O-TIDATUM DAT-KDSVAR                         
154700                                                                          
154800     IF DAT-KDSVAR-OK                                                     
154900       MOVE DAT-TIAARP(3:2) TO W-PER                                      
155000       MOVE DAT-TIAAMMDD    TO W-BINNDAY                                  
155100     ELSE                                                                 
155200         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
155300         DELIMITED BY SIZE INTO FELTEXT                                   
155400         CALL FELLOG                                                      
155500     END-IF                                                               
155600                                                                          
155700     PERFORM UNTIL BER-IY    >  BER-ANTAL-AA                              
155800     OR            LINK-IX   =  70                                        
155900                                                                          
156000         PERFORM UNTIL                                                    
156100            (BER-IY = BER-ANTAL-AA AND                                    
156200             BER-IX > BER-ANTAL-VV)                                       
156300         OR (BER-IY < BER-ANTAL-AA AND                                    
156400             BER-IX > 52)                                                 
156500         OR  LINK-IX   =  70                                              
156600                                                                          
156700           COMPUTE W-NDC-KVBEHOV-VECKA ROUNDED =                          
156800                         ((SLAG-KVPB-REF * 12) / 52) *                    
156900                         SLAG-RESEASON (W-PER)                            
157000           MOVE W-DATUM-FORSTA-SDC   TO TMP1-YYWW                         
157100           MOVE W-BER-DATUM          TO TMP2-YYWW                         
157200             MOVE '144910'        TO FELTEXT2-STR                         
157300           PERFORM WY2000P3                                               
157400           IF TMP1-YYWW <= TMP2-YYWW                                      
157500                                                                          
157600             IF SLAG-RESEASON (1) = 1.00                                  
157700             AND SLAG-RESEASON (2) = 1.00                                 
157800             AND SLAG-RESEASON (3) = 1.00                                 
157900             AND SLAG-RESEASON (4) = 1.00                                 
158000             AND SLAG-RESEASON (5) = 1.00                                 
158100             AND SLAG-RESEASON (6) = 1.00                                 
158200             AND SLAG-RESEASON (7) = 1.00                                 
158300             AND SLAG-RESEASON (8) = 1.00                                 
158400             AND SLAG-RESEASON (9) = 1.00                                 
158500             AND SLAG-RESEASON (10) = 1.00                                
158600             AND SLAG-RESEASON (11) = 1.00                                
158700             AND SLAG-RESEASON (12) = 1.00                                
158800               MOVE SLAG-KVREFPKT                                         
158900                             TO WS-KVREFPKT                               
159000               MOVE SLAG-KVREFBER                                         
159100                             TO WS-KVREFBER                               
159200             ELSE                                                         
159300                                                                          
159400*******  HÄMTA REFILLPUNKT OCH REFILLKVANT                                
159500               MOVE SLAG-IDDC                                             
159600                             TO REF2-IDDC                                 
159700               MOVE SLAG-IDREFTAB                                         
159800                             TO REF2-IDREFTAB                             
159900               MOVE SLAG-FLWILSON                                         
160000                             TO REF2-FLWILSON                             
160100               MOVE ZERO     TO REF2-PRARTBES                             
160200               MOVE SLAG-KVPB-REF                                         
160300                             TO REF2-KVPB-REF                             
160400               MOVE SLAG-RESEASON (1)                                     
160500                             TO REF2-RESEASON (1)                         
160600               MOVE SLAG-RESEASON (2)                                     
160700                             TO REF2-RESEASON (2)                         
160800               MOVE SLAG-RESEASON (3)                                     
160900                             TO REF2-RESEASON (3)                         
161000               MOVE SLAG-RESEASON (4)                                     
161100                             TO REF2-RESEASON (4)                         
161200               MOVE SLAG-RESEASON (5)                                     
161300                             TO REF2-RESEASON (5)                         
161400               MOVE SLAG-RESEASON (6)                                     
161500                             TO REF2-RESEASON (6)                         
161600               MOVE SLAG-RESEASON (7)                                     
161700                             TO REF2-RESEASON (7)                         
161800               MOVE SLAG-RESEASON (8)                                     
161900                             TO REF2-RESEASON (8)                         
162000               MOVE SLAG-RESEASON (9)                                     
162100                             TO REF2-RESEASON (9)                         
162200               MOVE SLAG-RESEASON (10)                                    
162300                             TO REF2-RESEASON (10)                        
162400               MOVE SLAG-RESEASON (11)                                    
162500                             TO REF2-RESEASON (11)                        
162600               MOVE SLAG-RESEASON (12)                                    
162700                             TO REF2-RESEASON (12)                        
162800               MOVE SLAG-FLFLYG TO REF2-FLFLYG                            
162900               MOVE W-BINNDAY TO REF2-BINNDAY-TIAAMMDD                    
163000               MOVE ZERO     TO REF2-KVREFPKT                             
163100                                REF2-KVREFBER                             
163200               MOVE SLAG-TIREFPAF                                         
163300                             TO TMP1-YYMMDD                               
163400               MOVE W-BINNDAY TO TMP2-YYMMDD                              
163500*                                                                         
163600*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
163700*                                                                         
163800               PERFORM WY2000P1                                           
163900               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
164000*                                                                         
164100*-----   MANUELL PÅFYLLNADSKVANT ÄR SATT                                  
164200*                                                                         
164300                 MOVE SLAG-KVREFBER                                       
164400                             TO REF2-IN-KVREFBER                          
164500               ELSE                                                       
164600                 MOVE +0     TO REF2-IN-KVREFBER                          
164700               END-IF                                                     
164800                                                                          
164900               MOVE SLAG-TIREFPKT                                         
165000                             TO TMP1-YYMMDD                               
165100               MOVE W-BINNDAY TO TMP2-YYMMDD                              
165200*                                                                         
165300*-----   W-BINNDAY    = MÅNDAG I AKTUELL VECKA                            
165400*                                                                         
165500               PERFORM WY2000P1                                           
165600               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
165700*                                                                         
165800*-----   MANUELL PÅFYLLNADSPUNKT ÄR SATT                                  
165900*                                                                         
166000                 MOVE SLAG-KVREFPKT                                       
166100                             TO REF2-IN-KVREFPKT                          
166200               ELSE                                                       
166300                 MOVE +0     TO REF2-IN-KVREFPKT                          
166400               END-IF                                                     
166500                                                                          
166600               PERFORM W271REF2                                           
166700               MOVE REF2-KVREFPKT                                         
166800                             TO WS-KVREFPKT                               
166900               MOVE REF2-KVREFBER                                         
167000                             TO WS-KVREFBER                               
167100                                                                          
167200             END-IF                                                       
167300                                                                          
167400             COMPUTE W-NDC-KVBEHOV-DAG ROUNDED =                          
167500                     W-NDC-KVBEHOV-VECKA / 5                              
167600                                                                          
167700             MOVE +1         TO IX                                        
167800             PERFORM UNTIL IX > +5                                        
167900             OR LINK-IX = 70                                              
168000                                                                          
168100               ADD W-NDC-KVBEHOV-DAG                                      
168200                         TO W-NDC-ACC-KVBEHOV                             
168300               COMPUTE W-NDC-KVAR-EFT-DAG ROUNDED =                       
168400                       W-NDC-TILLGANG - W-NDC-ACC-KVBEHOV                 
168500               IF W-NDC-KVAR-EFT-DAG < WS-KVREFPKT                        
168600                  ADD 1      TO LINK-IX                                   
168700                  COMPUTE W-NDC-DIFF = WS-KVREFPKT -                      
168800                                            W-NDC-KVAR-EFT-DAG            
168900                  COMPUTE W-DC-REFILL-KVANT =                             
169000                          W-NDC-DIFF + WS-KVREFBER                        
169100                  PERFORM S06-SATT-QX-PROCENT-BRYTNING                    
169200                  PERFORM S05-BERAKNA-Q-KVANT                             
169300                  IF WS-DAPUBL > ZERO                                     
169400                     MOVE W-BINNDAY                                       
169500                             TO TMP1-YYMMDD                               
169600                     MOVE WS-DAPUBL (3:6)                                 
169700                             TO TMP2-YYMMDD                               
169800                     PERFORM WY2000P1                                     
169900                     IF TMP1-YYMMDD >= TMP2-YYMMDD                        
170000                       MOVE W-DC-REFILL-KVANT                             
170100                             TO LINK-KVBEHOV-VECKA (LINK-IX)              
170200                       COMPUTE LINK-TIAAVVD-BEHOV (LINK-IX)               
170300                            = (W-BER-DATUM * 10) + IX                     
170400                       END-COMPUTE                                        
170500                     ELSE                                                 
170600                       MOVE BER-IY                                        
170700                             TO WS-BER-IY                                 
170800                       MOVE BER-IX                                        
170900                             TO WS-BER-IX                                 
171000                       IF WS-BER-IX > 1                                   
171100                         SUBTRACT 1 FROM WS-BER-IX                        
171200                       ELSE                                               
171300                         IF WS-BER-IY > ZERO                              
171400                           SUBTRACT 1 FROM WS-BER-IY                      
171500                           MOVE 52    TO WS-BER-IX                        
171600                         END-IF                                           
171700                       END-IF                                             
171800                       MOVE W-DC-REFILL-KVANT                             
171900                             TO LINK-KVBEHOV-VECKA (LINK-IX)              
172000                       CALL W009VADD USING                                
172100                              W-BER-DATUM-MINUS-1 W-ANTAL-VECKOR          
172200                       COMPUTE LINK-TIAAVVD-BEHOV (LINK-IX)               
172300                            = (W-BER-DATUM-MINUS-1 * 10) + IX             
172400                       END-COMPUTE                                        
172500                     END-IF                                               
172600                                                                          
172700                  ELSE                                                    
172800                    MOVE W-DC-REFILL-KVANT                                
172900                             TO LINK-KVBEHOV-VECKA (LINK-IX)              
173000                    COMPUTE LINK-TIAAVVD-BEHOV (LINK-IX) =                
173100                              (W-BER-DATUM * 10) + IX                     
173200                    END-COMPUTE                                           
173300                  END-IF                                                  
173400                  ADD W-DC-REFILL-KVANT                                   
173500                             TO W-NDC-TILLGANG                            
173600               END-IF                                                     
173700                                                                          
173800               ADD +1        TO IX                                        
173900             END-PERFORM                                                  
174000           END-IF                                                         
174100                                                                          
174200           ADD 1                   TO BER-IX                              
174300                                      W-BER-DATUM                         
174400           MOVE +1            TO W-ANTAL-VECKOR                           
174500           CALL W009VADD USING W-AAVV W-ANTAL-VECKOR                      
174600                                                                          
174700           MOVE 'AAVV  '     TO DAT-KDDATFORM                             
174800           MOVE W-AAVV       TO DAT-I-TIDATUM                             
174900                                                                          
175000           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
175100                           DAT-O-TIDATUM DAT-KDSVAR                       
175200                                                                          
175300           IF DAT-KDSVAR-OK                                               
175400             MOVE DAT-TIAARP(3:2) TO W-PER                                
175500             MOVE DAT-TIAAMMDD    TO W-BINNDAY                            
175600           ELSE                                                           
175700               STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                    
175800               DELIMITED BY SIZE INTO FELTEXT                             
175900               CALL FELLOG                                                
176000           END-IF                                                         
176100                                                                          
176200         END-PERFORM                                                      
176300         ADD 1                     TO BER-IY                              
176400         MOVE 1                    TO BER-IX                              
176500                                      IX-PER                              
176600         SUBTRACT +52              FROM  W-BER-DATUM                      
176700         ADD +100                  TO    W-BER-DATUM                      
176800     END-PERFORM                                                          
176900     .                                                                    
177000     EJECT                                                                
177100 HD-LAES-WDK711         SECTION.                                          
177200******************************************************************        
177300*                                                                *        
177400*    LÄSER WDK711 TILLS TRÄFF PÅ LINK-IDDC                       *        
177500*                                                                *        
177600******************************************************************        
177700                                                                          
177800     PERFORM IMS-GNP-WDK711                                               
177900     IF SEGMENT-FINNS                                                     
178000       MOVE SLAG-IDDC TO W-IDDC-B6                                        
178100                         WS-IDDC                                          
178200       PERFORM IMS-GU-WDB601                                              
178300     END-IF                                                               
178400                                                                          
178500     PERFORM UNTIL SEGMENT-SAKNAS                                         
178600     OR ((DCS-NDC-NA                                                      
178700     OR   DCS-NDC-PF                                                      
178710     OR   DCS-NDC-OTHERS                                                  
178800     OR   DCS-NDC-CN)                                                     
178900     AND SLAG-KDREFSTA = 'A'                                              
179000     AND SLAG-FLCDCBEH = JA                                               
179100     AND SLAG-IDLEVNR = '1441 '                                           
179200     AND (LINK-IDDC = '11'                                                
179300     OR  LINK-IDDC = SLAG-IDDC))                                          
179400*--     LÄS FRAM TILL EN GILTIG NDC-POST                                  
179500*--                                                                       
179600        PERFORM IMS-GNP-WDK711                                            
179700        IF SEGMENT-FINNS                                                  
179800          MOVE SLAG-IDDC TO W-IDDC-B6                                     
179900                            WS-IDDC                                       
180000          PERFORM IMS-GU-WDB601                                           
180100        END-IF                                                            
180200     END-PERFORM                                                          
180300                                                                          
180400     IF SEGMENT-FINNS                                                     
180500*** FIX FÖR NEGATIVA KVOKS-BULK                                           
180600        IF SLAG-KVOKS-BULK < 0                                            
180700          MOVE ZERO          TO SLAG-KVOKS-BULK                           
180800        END-IF                                                            
180900*** FIX FÖR NEGATIVA KVOKS-DAG                                            
181000        IF SLAG-KVOKS-DAG < 0                                             
181100          MOVE ZERO          TO SLAG-KVOKS-DAG                            
181200        END-IF                                                            
181300     END-IF                                                               
181400     .                                                                    
181500     EJECT                                                                
181600                                                                          
181700 S05-BERAKNA-Q-KVANT    SECTION.                                          
181800                                                                          
181900     MOVE ZERO               TO WS-KVANT-QX                               
182000     MOVE W-DC-REFILL-KVANT  TO WS-ANTAL-QX                               
182100                                                                          
182200     IF CLAG-KVQPACK-4 > 1                                                
182300     OR CLAG-KVQPACK-3 > 1                                                
182400     OR CLAG-KVQPACK-2 > 1                                                
182500     OR CLAG-KVQPACK-1 > 1                                                
182600     OR CLAG-KVQPACK-0 > 1                                                
182700                                                                          
182800*KVQPACK-4                                                                
182900       IF CLAG-KVQPACK-4 > 1                                              
183000         IF DCS-SDC                                                       
183100           MOVE ZERO               TO WS-KVANT-QX-HELTAL                  
183200         ELSE                                                             
183300           COMPUTE WS-KVANT-QX =                                          
183400                   W-DC-REFILL-KVANT / CLAG-KVQPACK-4                     
183500           IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                         
183600             IF WS-KVANT-QX-HELTAL > ZERO                                 
183700               COMPUTE WS-ANTAL-QX =                                      
183800                       WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                
183900             END-IF                                                       
184000           ELSE                                                           
184100             COMPUTE WS-KVANT-QX-HELTAL =                                 
184200                     WS-KVANT-QX-HELTAL + 1                               
184300             COMPUTE WS-ANTAL-QX =                                        
184400                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                  
184500           END-IF                                                         
184600         END-IF                                                           
184700       END-IF                                                             
184800                                                                          
184900*KVQPACK-3                                                                
185000       IF CLAG-KVQPACK-3 > 1  AND                                         
185100          WS-KVANT-QX-HELTAL = ZERO                                       
185200         COMPUTE WS-KVANT-QX =                                            
185300                 W-DC-REFILL-KVANT / CLAG-KVQPACK-3                       
185400         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
185500           IF WS-KVANT-QX-HELTAL > ZERO                                   
185600             COMPUTE WS-ANTAL-QX =                                        
185700                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-3                  
185800           END-IF                                                         
185900         ELSE                                                             
186000           COMPUTE WS-KVANT-QX-HELTAL =                                   
186100                   WS-KVANT-QX-HELTAL + 1                                 
186200           COMPUTE WS-ANTAL-QX =                                          
186300                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-3                    
186400         END-IF                                                           
186500       END-IF                                                             
186600                                                                          
186700*KVQPACK-2                                                                
186800       IF CLAG-KVQPACK-2 > 1 AND                                          
186900          WS-KVANT-QX-HELTAL = ZERO                                       
187000         COMPUTE WS-KVANT-QX =                                            
187100                 W-DC-REFILL-KVANT / CLAG-KVQPACK-2                       
187200         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
187300           IF WS-KVANT-QX-HELTAL > ZERO                                   
187400             COMPUTE WS-ANTAL-QX =                                        
187500                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                  
187600           END-IF                                                         
187700         ELSE                                                             
187800           COMPUTE WS-KVANT-QX-HELTAL =                                   
187900                   WS-KVANT-QX-HELTAL + 1                                 
188000           COMPUTE WS-ANTAL-QX =                                          
188100                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                    
188200         END-IF                                                           
188300       END-IF                                                             
188400                                                                          
188500*KVQPACK-0                                                                
188600       IF CLAG-KVQPACK-0 > 1   AND                                        
188700          WS-KVANT-QX-HELTAL = ZERO                                       
188800         COMPUTE WS-KVANT-QX =                                            
188900                 W-DC-REFILL-KVANT / CLAG-KVQPACK-0                       
189000         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
189100           IF WS-KVANT-QX-HELTAL < 1                                      
189200             IF CLAG-KVQPACK-1 > 0                                        
189300               PERFORM S05B-KVQPACK-1                                     
189400             ELSE                                                         
189500               MOVE W-DC-REFILL-KVANT TO WS-ANTAL-QX                      
189600             END-IF                                                       
189700           ELSE                                                           
189800             COMPUTE WS-ANTAL-QX =                                        
189900                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                  
190000           END-IF                                                         
190100         ELSE                                                             
190200           COMPUTE WS-KVANT-QX-HELTAL =                                   
190300                   WS-KVANT-QX-HELTAL + 1                                 
190400           COMPUTE WS-ANTAL-QX =                                          
190500                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                    
190600         END-IF                                                           
190700       END-IF                                                             
190800                                                                          
190900*KVQPACK-1                                                                
191000       IF CLAG-KVQPACK-1 > 1                                              
191100          IF WS-KVANT-QX-HELTAL > ZERO                                    
191200             MOVE WS-ANTAL-QX TO W-DC-REFILL-KVANT                        
191300          END-IF                                                          
191400          PERFORM S05B-KVQPACK-1                                          
191500       END-IF                                                             
191600     END-IF                                                               
191700     MOVE WS-ANTAL-QX TO W-DC-REFILL-KVANT                                
191800                                                                          
191900     .                                                                    
192000     EJECT                                                                
192100                                                                          
192200 S05B-KVQPACK-1 SECTION.                                                  
192300                                                                          
192400     COMPUTE WS-KVANT-QX =                                                
192500             W-DC-REFILL-KVANT / CLAG-KVQPACK-1                           
192600     IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                               
192700       IF WS-KVANT-QX-HELTAL < 1                                          
192800         COMPUTE WS-ANTAL-QX = 1 * CLAG-KVQPACK-1                         
192900       ELSE                                                               
193000         COMPUTE WS-ANTAL-QX =                                            
193100                 WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                      
193200       END-IF                                                             
193300     ELSE                                                                 
193400       COMPUTE WS-KVANT-QX-HELTAL =                                       
193500               WS-KVANT-QX-HELTAL + 1                                     
193600       COMPUTE WS-ANTAL-QX =                                              
193700               WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                        
193800     END-IF                                                               
193900     .                                                                    
194000     EJECT                                                                
194100                                                                          
194200 S06-SATT-QX-PROCENT-BRYTNING.                                            
194300                                                                          
194400     MOVE DCS-REQXBRYT       TO WS-QX-BRYTNING                            
194500     .                                                                    
194600     EJECT                                                                
194700                                                                          
194800                                                                          
194900                                                                          
195000 S07-INIT-DATUM SECTION.                                                  
195100                                                                          
195200                                                                          
195300     MOVE JA TO LINK-FLJANEJ-ANROP                                        
195400     SKIP1                                                                
195500     MOVE LINK-TIBEHOV-START TO W-DATUM                                   
195600                                W-BER-START-DATUM                         
195700     MOVE W-DATUM-AA TO W-BER-START-AA                                    
195800     MOVE W-DATUM-VV TO W-BER-START-VV                                    
195900     MOVE LINK-TIBEHOV-START TO W-BER-SLUT-DATUM                          
196000     MOVE LINK-KVVECKOR-BEHOV TO W-ANTAL-VECKOR                           
196100     SUBTRACT 1 FROM W-ANTAL-VECKOR                                       
196200     CALL W009VADD USING W-BER-SLUT-DATUM W-ANTAL-VECKOR                  
196300     SKIP1                                                                
196400     MOVE +100 TO W-DATUM-AAVV                                            
196500     ADD  W-BER-START-VV TO W-DATUM-AAVV                                  
196600     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
196700     MOVE W-DATUM-AAVV TO W-DATUM                                         
196800     MOVE W-DATUM-AA TO RES-ANTAL-AA                                      
196900     MOVE W-DATUM-VV TO RES-ANTAL-VV                                      
197000******************************************************************        
197100*                                                                *        
197200*    YTTERLIGARE +7 VECKOR HAR LAGTS TILL PGA AV LEDTIDEN        *        
197300*    TILL NORDAMERIKA / STEFAN A, FRONTEC                        *        
197400*                                                                *        
197500******************************************************************        
197600     ADD +7                  TO LINK-KVVECKOR-BEHOV                       
197700     IF  LINK-KVVECKOR-BEHOV > +156                                       
197800         MOVE +156 TO LINK-KVVECKOR-BEHOV                                 
197900     END-IF                                                               
198000     MOVE JA TO LINK-FLJANEJ-ANROP                                        
198100     SKIP1                                                                
198200     MOVE LINK-TIBEHOV-START TO W-DATUM                                   
198300                                W-BER-START-DATUM                         
198400     MOVE W-DATUM-AA TO W-BER-START-AA                                    
198500     MOVE W-DATUM-VV TO W-BER-START-VV                                    
198600     MOVE LINK-TIBEHOV-START TO W-BER-SLUT-DATUM                          
198700     MOVE LINK-KVVECKOR-BEHOV TO W-ANTAL-VECKOR                           
198800     SUBTRACT 1 FROM W-ANTAL-VECKOR                                       
198900     CALL W009VADD USING W-BER-SLUT-DATUM W-ANTAL-VECKOR                  
199000     SKIP1                                                                
199100     MOVE +100 TO W-DATUM-AAVV                                            
199200     ADD  W-BER-START-VV TO W-DATUM-AAVV                                  
199300     CALL W009VADD USING W-DATUM-AAVV W-ANTAL-VECKOR                      
199400     MOVE W-DATUM-AAVV TO W-DATUM                                         
199500     MOVE W-DATUM-AA TO BER-ANTAL-AA                                      
199600     MOVE W-DATUM-VV TO BER-ANTAL-VV                                      
199700     .                                                                    
199800     EJECT                                                                
199900 S08-BER-BALANCE-ERSATT SECTION.                                          
200000                                                                          
200100     MOVE ZERO TO WS-BALANCE-ERSATT                                       
200200     IF ART-FLERS = JA                                                    
200300        MOVE LINK-IDARTNR    TO W-IDARTNR-TILLK-MIN                       
200400                                W-IDARTNR-TILLK-MAX                       
200500        PERFORM IMS-GU-WDD7A1                                             
200600        IF SEGMENT-FINNS                                                  
200700           MOVE ERSB-ERS-IDARTNR TO W-IDARTNR-ERS                         
200800           PERFORM IMS-GU-K601-ERS                                        
200900           IF SEGMENT-FINNS                                               
201000              IF ERS-ART-KDERS-UTG = ZERO                                 
201100                 PERFORM IMS-GNP-K611-ERS                                 
201200                 IF SEGMENT-FINNS                                         
201300                   IF ERS-CLAG-KDERS = 11 OR 21 OR 27 OR 01 OR 17         
201400                      MOVE LINK-IDDC                                      
201500                             TO W-IDDC-ERS                                
201600                      PERFORM IMS-GU-K711-ERS                             
201700                      IF SEGMENT-FINNS                                    
201800                         COMPUTE WS-BALANCE-ERSATT =                      
201900                                    ERS-SLAG-KVLS         +               
202000                                    ERS-SLAG-KVBEART      +               
202100                                    ERS-SLAG-KVAKS-PAV    +               
202200                                    ERS-SLAG-KVAKS-SDC    -               
202300                                    ERS-SLAG-KVRESS       -               
202400                                    ERS-SLAG-KVROS-DAG    -               
202500                                    ERS-SLAG-KVROS-BULK   -               
202600                                    ERS-SLAG-KVOKS-DAG    -               
202700                                    ERS-SLAG-KVOKS-BULK                   
202800                      END-IF                                              
202900                   END-IF                                                 
203000                 END-IF                                                   
203100              END-IF                                                      
203200           END-IF                                                         
203300        END-IF                                                            
203400     END-IF                                                               
203500     .                                                                    
203600     EJECT                                                                
203700                                                                          
203800                                                                          
203900 S09-KUNDRETURER SECTION.                                                 
204000                                                                          
204100     MOVE LINK-IDDC         TO W-IDDC                                     
204200     MOVE ZERO              TO WS-KVKUNDRETUR                             
204300     PERFORM IMS-GU-L601                                                  
204400     IF SEGMENT-FINNS                                                     
204500*       SAMLA R30(AK PÅ VÄG) OCH 310(AK-NDC)                              
204600        PERFORM IMS-GNP-L611                                              
204700        PERFORM UNTIL SEGMENT-SAKNAS                                      
204800           IF INL-IDPTYP = 'R30'                                          
204900           OR INL-IDPTYP = 'R31'                                          
205000           OR INL-IDPTYP = '310'                                          
205100              IF INL-IDDC = LINK-IDDC                                     
205200                IF INL-KDRT = 7                                           
205300*KDRT = 7 ÄR KUNDRETUR                                                    
205400                  ADD INL-KVAVIS TO WS-KVKUNDRETUR                        
205500                                                                          
205600                END-IF                                                    
205700              END-IF                                                      
205800           END-IF                                                         
205900           PERFORM IMS-GNP-L611                                           
206000        END-PERFORM                                                       
206100     END-IF                                                               
206200     .                                                                    
206300     EJECT                                                                
206400 S33-BER-IX-AKTUELLT-DC SECTION.                                          
206500                                                                          
206600     MOVE SLAG-IDDC-REF  TO W-IDDC-B616                                   
206700     PERFORM IMS-GU-WDB616                                                
206800     .                                                                    
206900     EJECT                                                                
207000 W271REF2 SECTION.                                                        
207100                                                                          
207200********************************************************                  
207300*                                                      *                  
207400*   W271REF2                                           *                  
207500*                                                      *                  
207600*   SUBPROGRAM W271REF2 INLAGD I W2222200 FÖR ATT      *                  
207700*   TJÄNA TID OCH PENGAR (PGA TIDSÖDANDE CALL ANROP)   *                  
207800*                                                      *                  
207900********************************************************                  
208000                                                                          
208100     PERFORM REF2-A-INIT                                                  
208200     PERFORM REF2-D-NOLLSTALL-ARBETSFALT                                  
208300                                                                          
208400                                                                          
208500     PERFORM REF2-C-HAEMTA-LEDTID                                         
208600     MOVE REF2-IDDC          TO WS-SP-IDDC                                
208700                                W-IDDC-2501                               
208800                                                                          
208900     MOVE REF2-IDREFTAB TO WS-SP-IDREFTAB                                 
209000     PERFORM REF2-B-LAS-2501                                              
209100                                                                          
209200     IF REF2-KVPB-REF = ZERO                                              
209300        PERFORM REF2-E-PUNKTER-ARTIKEL-UTAN-PB                            
209400     ELSE                                                                 
209500        PERFORM REF2-F-BERAKNA-PUNKTER                                    
209600     END-IF                                                               
209700     .                                                                    
209800     EJECT                                                                
209900                                                                          
210000                                                                          
210100 REF2-A-INIT SECTION.                                                     
210200                                                                          
210300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
210400     COMPUTE DAGENS-AAR-PLUS2 = DAGENS-AAR + 2                            
210500     .                                                                    
210600     EJECT                                                                
210700                                                                          
210800                                                                          
210900 REF2-B-LAS-2501 SECTION.                                                 
211000***                                                                       
211100*   LÄS HÄNDELSEBASEN WL2501 MED OLIKA NYCKLAR TILLS MAN FÅR TRÄFF        
211200***                                                                       
211300                                                                          
211400                                                                          
211500     MOVE NEJ TO GRUNDTABELL-SW                                           
211600                                                                          
211700     IF REF2-IDREFTAB NOT = '0'                                           
211800        MOVE REF2-IDREFTAB   TO W-IDREFTAB                                
211900        PERFORM IMS-GU-2502                                               
212000        IF SEGMENT-SAKNAS                                                 
212100           PERFORM REF2-BA-HAMTA-GRUNDTABELL                              
212200        ELSE                                                              
212300           MOVE 2502-WDGX2502 TO SPARAREA-AKTUELL-TABELL                  
212400        END-IF                                                            
212500     ELSE                                                                 
212600        MOVE REF2-IDREFTAB TO W-IDREFTAB                                  
212700                                                                          
212800        PERFORM IMS-GU-2502                                               
212900                                                                          
213000        IF SEGMENT-SAKNAS                                                 
213100           MOVE REF2-IDDC TO TABELL-SAKNAS-DC                             
213200           MOVE TE-TABELL-SAKNAS TO FELTEXT-STR                           
213300           DISPLAY FELTEXT                                                
213400           PERFORM REF2-S99-ABEND                                         
213500        ELSE                                                              
213600           MOVE 2502-WDGX2502 TO SPARAREA-AKTUELL-TABELL                  
213700        END-IF                                                            
213800     END-IF                                                               
213900     .                                                                    
214000     EJECT                                                                
214100                                                                          
214200                                                                          
214300 REF2-BA-HAMTA-GRUNDTABELL SECTION.                                       
214400***                                                                       
214500*   LÄSER HÄNDELSE-BASEN MED TABELL 0 OM DEN INTE ÄR                      
214600*   INLÄST VID TIDIGARE ANROP, SEGMENTET SKA ALLTID FINNAS                
214700***                                                                       
214800     MOVE JA TO GRUNDTABELL-SW                                            
214900                                                                          
215000     IF (REF2-IDDC = WS-SP-GRUNDTABELL-DC)                                
215100*                                                                         
215200*----  DET FINNS REDAN ETT INLÄST SEGMENT FRÅN BASEN                      
215300*                                                                         
215400       CONTINUE                                                           
215500     ELSE                                                                 
215600       MOVE '0'  TO W-IDREFTAB                                            
215700       PERFORM IMS-GU-2502                                                
215800       IF SEGMENT-SAKNAS                                                  
215900           MOVE REF2-IDDC TO TABELL-SAKNAS-DC                             
216000           MOVE TE-TABELL-SAKNAS TO FELTEXT-STR                           
216100           DISPLAY FELTEXT                                                
216200           PERFORM REF2-S99-ABEND                                         
216300       ELSE                                                               
216400          MOVE 2502-WDGX2502 TO SPARAREA-GRUNDTABELL                      
216500          MOVE REF2-IDDC TO WS-SP-GRUNDTABELL-DC                          
216600       END-IF                                                             
216700     END-IF                                                               
216800     .                                                                    
216900     EJECT                                                                
217000                                                                          
217100 REF2-C-HAEMTA-LEDTID SECTION.                                            
217200***                                                                       
217300*   HÄMTA LEDTID FÖR ETT DC FRÅN TABELLEN W271LEDT SOM ÄNVÄNDS            
217400*   VID BERÄKNINGEN AV PÅFYLLNADSPUNKT                                    
217500***                                                                       
217600                                                                          
217700     MOVE SLAG-IDDC-REF  TO W-IDDC-B616                                   
217800     PERFORM IMS-GU-WDB616                                                
217900                                                                          
218000     IF (DCS-NDC-NA                                                       
218100     OR  DCS-NDC-PF                                                       
218110     OR  DCS-NDC-OTHERS                                                   
218200     OR  DCS-NDC-CN)                                                      
218300        INITIALIZE W271LTPB-W271LTPB                                      
218400        IF REF2-IDDC = '62'                                               
218500        AND REF2-FLFLYG = JA                                              
218600*                                                                         
218700*      ARTIKLAR SOM ENBART FLYGS (EJ BÅT)                                 
218800*                                                                         
218900                                                                          
219000                                                                          
219100          MOVE REF-KVDLTID-AIRETA                                         
219200                             TO DAG-KVKALDAG                              
219300          ADD 1              TO DAG-KVKALDAG                              
219400                                                                          
219500*      REF2-BINNDAY-TIAAMMDD ÄR DET DATUM VILKET BEHOVET                  
219600*      BERÄKNAS I CDC DVS EJ BINNDAY                                      
219700          MOVE REF2-BINNDAY-TIAAMMDD                                      
219800                             TO DAG-TIAAMMDD-FOM                          
219900          MOVE 002           TO DAG-KDCALL                                
220000          CALL WDAGKONV USING DAG-KDCALL                                  
220100                              DAG-DATUM-AREA                              
220200                              DAG-KDSVAR                                  
220300                                                                          
220400          IF DAG-KDSVAR = SPACE                                           
220500             MOVE DAG-TIAAMMDD-TOM                                        
220600                             TO W271LTPB-BINNDAY-TIAAMMDD                 
220700          ELSE                                                            
220800             MOVE 'FEL FRÅN WDAGKONV 1, I REF2-C- SECT I W6115900'        
220900                                      TO FELTEXT-STR                      
221000             DISPLAY FELTEXT                                              
221100             PERFORM REF2-S99-ABEND                                       
221200          END-IF                                                          
221300                                                                          
221400        END-IF                                                            
221500        MOVE REF2-IDDC        TO W271LTPB-IDDC                            
221600        MOVE SLAG-IDDC-REF    TO W271LTPB-IDDC-REF                        
221700        MOVE REF-KVDLTID-TOT           TO                                 
221800                             W271LTPB-KVDLTID-TOT                         
221900        MOVE REF-KVDLTID-BOATPAC       TO                                 
222000                             W271LTPB-KVDLTID-BOATPAC                     
222100        MOVE REF-KVDLTID-BOATTRP       TO                                 
222200                             W271LTPB-KVDLTID-BOATTRP                     
222300        MOVE REF-KVDLTID-BOAT2DC       TO                                 
222400                             W271LTPB-KVDLTID-BOAT2DC                     
222500        MOVE REF-KVDLTID-BOATINS       TO                                 
222600                             W271LTPB-KVDLTID-BOATINS                     
222700        MOVE REF-KVDLTID-AIRREQ        TO                                 
222800                             W271LTPB-KVDLTID-AIRREQ                      
222900        MOVE REF-KVDLTID-AIRETA        TO                                 
223000                             W271LTPB-KVDLTID-AIRETA                      
223100        MOVE REF2-BINNDAY-TIAAMMDD                                        
223200                            TO W271LTPB-START-DATUM                       
223300        CALL W271LTPB USING                                               
223400                     W271LTPB-W271LTPB                                    
223500        MOVE W271LTPB-BINNDAY-TIAAMMDD                                    
223600                           TO WS-BINNDAY-TIAAMMDD                         
223700     ELSE                                                                 
223800                                                                          
223900       MOVE REF-KVDLTID-BOATTRP                                           
224000                           TO DAG-KVKALDAG                                
224100       ADD 1                 TO DAG-KVKALDAG                              
224200                                                                          
224300*   REF2-BINNDAY-TIAAMMDD ÄR DET DATUM VILKET BEHOVET                     
224400*   BERÄKNAS I CDC DVS EJ BINNDAY                                         
224500       MOVE REF2-BINNDAY-TIAAMMDD                                         
224600                           TO DAG-TIAAMMDD-FOM                            
224700       MOVE 002              TO DAG-KDCALL                                
224800       CALL WDAGKONV USING DAG-KDCALL                                     
224900                           DAG-DATUM-AREA                                 
225000                           DAG-KDSVAR                                     
225100                                                                          
225200       IF DAG-KDSVAR = SPACE                                              
225300          MOVE DAG-TIAAMMDD-TOM                                           
225400                           TO WS-BINNDAY-TIAAMMDD                         
225500       ELSE                                                               
225600          MOVE 'FEL FRÅN WDAGKONV 2, I REF2-C SECT W6115900'              
225700                                   TO FELTEXT-STR                         
225800          DISPLAY FELTEXT                                                 
225900          PERFORM REF2-S99-ABEND                                          
226000       END-IF                                                             
226100                                                                          
226200     END-IF                                                               
226300     .                                                                    
226400     EJECT                                                                
226500                                                                          
226600                                                                          
226700 REF2-D-NOLLSTALL-ARBETSFALT SECTION.                                     
226800                                                                          
226900     MOVE ZERO TO        WS-N                                             
227000                         WS-NP                                            
227100                         WS-UB                                            
227200                         WS-NP-UB                                         
227300                         WS-TOT-DAGAR-PLATSBR                             
227400                         WS-LEDTIDSBEHOV                                  
227500                         WS-TABELLBEHOV                                   
227600                         WS-PAFYLLNPKT                                    
227700                         WS-PAFYLLNKVANT                                  
227800                         WS-OVERLAGERPKT                                  
227900                         WS-KVPB-REF                                      
228000                         WS-KVPB-REF-DAY-PER-I                            
228100                         WS-KVPB-REF-DAY-PER-II                           
228200                         WS-KVPB-REF-DAY-PER-III                          
228300                         WS-KVPB-REF-DAY-PER-VI                           
228400                         WS-KVPB-REF-DAY-PER-V                            
228500                         WS-KVPB-REF-DAY-PER-IV                           
228600                         WS-KVPB-REF-DAY-BINN                             
228700     .                                                                    
228800     EJECT                                                                
228900                                                                          
229000                                                                          
229100 REF2-E-PUNKTER-ARTIKEL-UTAN-PB SECTION.                                  
229200                                                                          
229300                                                                          
229400     PERFORM REF2-S07-PLATS-I-TABELL-NU                                   
229500                                                                          
229600     MOVE +0 TO REF2-KVREFPKT                                             
229700                REF2-KVREFBER                                             
229800     .                                                                    
229900     EJECT                                                                
230000                                                                          
230100                                                                          
230200 REF2-F-BERAKNA-PUNKTER SECTION.                                          
230300                                                                          
230400                                                                          
230500     PERFORM REF2-S07-PLATS-I-TABELL-NU                                   
230600     PERFORM REF2-S05-LEDTIDSBEHOV                                        
230700     PERFORM REF2-S01-PAFYLLNPUNKT                                        
230800     PERFORM REF2-S02-PAFYLLNKVANT                                        
230900     .                                                                    
231000     EJECT                                                                
231100                                                                          
231200                                                                          
231300 REF2-S01-PAFYLLNPUNKT SECTION.                                           
231400                                                                          
231500     IF REF2-IN-KVREFPKT > 0                                              
231600*MANUELL PÅFYLLNADSPUNKT GÄLLER                                           
231700        MOVE REF2-IN-KVREFPKT TO WS-PAFYLLNPKT                            
231800                                 REF2-KVREFPKT                            
231900     ELSE                                                                 
232000        PERFORM REF2-S01B-PAFYLLNPKT-NORMAL                               
232100     END-IF                                                               
232200     .                                                                    
232300     EJECT                                                                
232400                                                                          
232500                                                                          
232600 REF2-S01B-PAFYLLNPKT-NORMAL SECTION.                                     
232700                                                                          
232800     IF GRUNDTABELL                                                       
232900        IF GRUNDTAB-2502-KDREFPKT-LIM(WS-RADIX, WS-PR-KOLIX) = 'D'        
233000                                                                          
233100          MOVE GRUNDTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX)             
233200                             TO WS-KVREFLIM                               
233300          IF  REF2-RESEASON (1) = 1.00                                    
233400          AND REF2-RESEASON (2) = 1.00                                    
233500          AND REF2-RESEASON (3) = 1.00                                    
233600          AND REF2-RESEASON (4) = 1.00                                    
233700          AND REF2-RESEASON (5) = 1.00                                    
233800          AND REF2-RESEASON (6) = 1.00                                    
233900          AND REF2-RESEASON (7) = 1.00                                    
234000          AND REF2-RESEASON (8) = 1.00                                    
234100          AND REF2-RESEASON (9) = 1.00                                    
234200          AND REF2-RESEASON (10) = 1.00                                   
234300          AND REF2-RESEASON (11) = 1.00                                   
234400          AND REF2-RESEASON (12) = 1.00                                   
234500              COMPUTE WS-TABELLBEHOV ROUNDED =                            
234600                      WS-KVREFLIM * WS-KVPB-REF-DAY-BINN                  
234700          ELSE                                                            
234800              PERFORM REF2-S11-TAB-BEHOV-PKT                              
234900          END-IF                                                          
235000          COMPUTE WS-PAFYLLNPKT ROUNDED =                                 
235100                          ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)             
235200                                                                          
235300          MOVE WS-PAFYLLNPKT TO REF2-KVREFPKT                             
235400                                                                          
235500        ELSE                                                              
235600          COMPUTE WS-TABELLBEHOV =                                        
235700              (GRUNDTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX))            
235800                                                                          
235900          COMPUTE WS-PAFYLLNPKT ROUNDED =                                 
236000                          ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)             
236100          MOVE WS-PAFYLLNPKT TO REF2-KVREFPKT                             
236200        END-IF                                                            
236300     ELSE                                                                 
236400        IF AKTTAB-2502-KDREFPKT-LIM (WS-RADIX, WS-PR-KOLIX) = 'D'         
236500                                                                          
236600          MOVE AKTTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX)               
236700                             TO WS-KVREFLIM                               
236800          IF  REF2-RESEASON (1) = 1.00                                    
236900          AND REF2-RESEASON (2) = 1.00                                    
237000          AND REF2-RESEASON (3) = 1.00                                    
237100          AND REF2-RESEASON (4) = 1.00                                    
237200          AND REF2-RESEASON (5) = 1.00                                    
237300          AND REF2-RESEASON (6) = 1.00                                    
237400          AND REF2-RESEASON (7) = 1.00                                    
237500          AND REF2-RESEASON (8) = 1.00                                    
237600          AND REF2-RESEASON (9) = 1.00                                    
237700          AND REF2-RESEASON (10) = 1.00                                   
237800          AND REF2-RESEASON (11) = 1.00                                   
237900          AND REF2-RESEASON (12) = 1.00                                   
238000              COMPUTE WS-TABELLBEHOV ROUNDED =                            
238100                      WS-KVREFLIM * WS-KVPB-REF-DAY-BINN                  
238200          ELSE                                                            
238300              PERFORM REF2-S11-TAB-BEHOV-PKT                              
238400          END-IF                                                          
238500                                                                          
238600          COMPUTE WS-PAFYLLNPKT ROUNDED =                                 
238700                          (WS-TABELLBEHOV + WS-LEDTIDSBEHOV)              
238800                                                                          
238900          MOVE WS-PAFYLLNPKT TO REF2-KVREFPKT                             
239000                                                                          
239100        ELSE                                                              
239200          COMPUTE WS-TABELLBEHOV =                                        
239300                (AKTTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX))            
239400                                                                          
239500          COMPUTE WS-PAFYLLNPKT ROUNDED =                                 
239600                          (WS-TABELLBEHOV + WS-LEDTIDSBEHOV)              
239700                                                                          
239800          MOVE WS-PAFYLLNPKT TO REF2-KVREFPKT                             
239900        END-IF                                                            
240000     END-IF                                                               
240100     .                                                                    
240200     EJECT                                                                
240300                                                                          
240400                                                                          
240500 REF2-S02-PAFYLLNKVANT SECTION.                                           
240600                                                                          
240700                                                                          
240800     IF REF2-IN-KVREFBER > 0                                              
240900*                                                                         
241000*----  MANUELL PÅFYLLNADSKVANTITET ÄR SATT                                
241100*                                                                         
241200        MOVE REF2-IN-KVREFBER TO WS-PAFYLLNKVANT                          
241300                                REF2-KVREFBER                             
241400     ELSE                                                                 
241500        IF REF2-FLWILSON = JA                                             
241600           PERFORM REF2-S02B-KVANT-M-WILSON                               
241700        ELSE                                                              
241800           PERFORM REF2-S02C-KVANT-M-TABELL                               
241900        END-IF                                                            
242000                                                                          
242100        IF WS-PAFYLLNKVANT = ZERO                                         
242200          MOVE +1 TO WS-PAFYLLNKVANT                                      
242300        END-IF                                                            
242400        MOVE WS-PAFYLLNKVANT TO REF2-KVREFBER                             
242500     END-IF                                                               
242600     .                                                                    
242700     EJECT                                                                
242800                                                                          
242900                                                                          
243000 REF2-S02B-KVANT-M-WILSON SECTION.                                        
243100                                                                          
243200*WILSONFORMEL FÖR BERÄKNING AV PÅFYLLNADSKVANT                            
243300*KVANT = ROTEN UR ((2N * P)/(U * B)                                       
243400*                                                                         
243500*     N = ÅRSBEHOV, P = ORDERSÄRKOSTNAD, U = LAGERSÄRKOSTNAD              
243600*     B = BESTÄLLNINGSPRIS                                                
243700                                                                          
243800     COMPUTE WS-N = (REF2-KVPB-REF * 12)                                  
243900                                                                          
244000     COMPUTE WS-NP = ((2 * WS-N) * 9.25)                                  
244100                                                                          
244200*                    PRARTBES * LAGERRÄNTA                                
244300                                                                          
244400     COMPUTE WS-UB = (WS-PRARTBES * 0.30)                                 
244500                                                                          
244600     IF WS-UB = ZERO                                                      
244700        MOVE 1.0 TO WS-UB                                                 
244800     END-IF                                                               
244900                                                                          
245000     COMPUTE WS-NP-UB = (WS-NP / WS-UB)                                   
245100                                                                          
245200     COMPUTE WS-PAFYLLNKVANT ROUNDED = (WS-NP-UB ** 0.5)                  
245300                                                                          
245400     .                                                                    
245500     EJECT                                                                
245600                                                                          
245700                                                                          
245800 REF2-S02C-KVANT-M-TABELL SECTION.                                        
245900                                                                          
246000     IF GRUNDTABELL                                                       
246100        IF GRUNDTAB-2502-KDREFPKT-KVA(WS-RADIX, WS-PR-KOLIX)              
246200                                               = 'D'                      
246300                                                                          
246400          MOVE GRUNDTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)             
246500                               TO WS-KVREFKVA                             
246600          IF  REF2-RESEASON (1) = 1.00                                    
246700          AND REF2-RESEASON (2) = 1.00                                    
246800          AND REF2-RESEASON (3) = 1.00                                    
246900          AND REF2-RESEASON (4) = 1.00                                    
247000          AND REF2-RESEASON (5) = 1.00                                    
247100          AND REF2-RESEASON (6) = 1.00                                    
247200          AND REF2-RESEASON (7) = 1.00                                    
247300          AND REF2-RESEASON (8) = 1.00                                    
247400          AND REF2-RESEASON (9) = 1.00                                    
247500          AND REF2-RESEASON (10) = 1.00                                   
247600          AND REF2-RESEASON (11) = 1.00                                   
247700          AND REF2-RESEASON (12) = 1.00                                   
247800              COMPUTE WS-PAFYLLNKVANT ROUNDED =                           
247900                      WS-KVREFKVA * WS-KVPB-REF-DAY-BINN                  
248000          ELSE                                                            
248100              PERFORM REF2-S12-TAB-BEHOV-KVANT                            
248200          END-IF                                                          
248300                                                                          
248400          MOVE WS-PAFYLLNKVANT TO REF2-KVREFBER                           
248500                                                                          
248600        ELSE                                                              
248700          MOVE  GRUNDTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)            
248800                               TO WS-PAFYLLNKVANT                         
248900                                                                          
249000          MOVE WS-PAFYLLNKVANT TO REF2-KVREFBER                           
249100        END-IF                                                            
249200     ELSE                                                                 
249300        IF AKTTAB-2502-KDREFPKT-KVA (WS-RADIX, WS-PR-KOLIX) = 'D'         
249400                                                                          
249500          MOVE AKTTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)               
249600                               TO WS-KVREFKVA                             
249700          IF  REF2-RESEASON (1) = 1.00                                    
249800          AND REF2-RESEASON (2) = 1.00                                    
249900          AND REF2-RESEASON (3) = 1.00                                    
250000          AND REF2-RESEASON (4) = 1.00                                    
250100          AND REF2-RESEASON (5) = 1.00                                    
250200          AND REF2-RESEASON (6) = 1.00                                    
250300          AND REF2-RESEASON (7) = 1.00                                    
250400          AND REF2-RESEASON (8) = 1.00                                    
250500          AND REF2-RESEASON (9) = 1.00                                    
250600          AND REF2-RESEASON (10) = 1.00                                   
250700          AND REF2-RESEASON (11) = 1.00                                   
250800          AND REF2-RESEASON (12) = 1.00                                   
250900              COMPUTE WS-PAFYLLNKVANT ROUNDED =                           
251000                      WS-KVREFKVA * WS-KVPB-REF-DAY-BINN                  
251100          ELSE                                                            
251200              PERFORM REF2-S12-TAB-BEHOV-KVANT                            
251300          END-IF                                                          
251400                                                                          
251500          MOVE WS-PAFYLLNKVANT TO REF2-KVREFBER                           
251600                                                                          
251700        ELSE                                                              
251800          MOVE AKTTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)               
251900                               TO WS-PAFYLLNKVANT                         
252000                                                                          
252100          MOVE WS-PAFYLLNKVANT TO REF2-KVREFBER                           
252200        END-IF                                                            
252300     END-IF                                                               
252400     .                                                                    
252500     EJECT                                                                
252600                                                                          
252700                                                                          
252800                                                                          
252900 REF2-S05-LEDTIDSBEHOV SECTION.                                           
253000***                                                                       
253100*   BERÄKNAR DAGSBEHOVET I STYCK FÖR EN ARTIKEL SOM FINNS MED             
253200*   ANTAL DAGSBEHOV I WDR211                                              
253300*   ANTAL VECKOR I PERIODEN ÄR ETT SNITT FÖR ETT HELT ÅR (52/12)          
253400***                                                                       
253500                                                                          
253600                                                                          
253700     IF DCS-SDC                                                           
253800        COMPUTE WS-KVPB-REF-DAY-BINN =                                    
253900                         (REF2-KVPB-REF / (WS-SNITT-VECKOR * 5))          
254000        COMPUTE WS-LEDTIDSBEHOV =                                         
254100                (WS-KVPB-REF-DAY-BINN *                                   
254200                (REF-KVDLTID-TOT + 1))                                    
254300     END-IF                                                               
254400                                                                          
254500     IF DCS-NDC-NA                                                        
254600     OR DCS-NDC-PF                                                        
254610     OR DCS-NDC-OTHERS                                                    
254700     OR DCS-NDC-CN                                                        
254800*       SUMMERA BEHOV FÖR DE ARBETSDAGAR SOM LIGGER INOM                  
254900*       LEDTIDEN                                                          
255000                                                                          
255100        COMPUTE WS-KVPB-REF-DAY-PER-I =                                   
255200        (((REF2-KVPB-REF *                                                
255300        REF2-RESEASON(W271LTPB-PER-I-TIRP)) / 4.33) / 5)                  
255400                                                                          
255500        COMPUTE WS-KVPB-REF-DAY-PER-II =                                  
255600        (((REF2-KVPB-REF *                                                
255700        REF2-RESEASON(W271LTPB-PER-II-TIRP)) / 4.33) / 5)                 
255800                                                                          
255900        COMPUTE WS-KVPB-REF-DAY-PER-III =                                 
256000        (((REF2-KVPB-REF *                                                
256100        REF2-RESEASON(W271LTPB-PER-III-TIRP)) / 4.33) / 5)                
256200                                                                          
256300        COMPUTE WS-KVPB-REF-DAY-PER-IV =                                  
256400        (((REF2-KVPB-REF *                                                
256500        REF2-RESEASON(W271LTPB-PER-IV-TIRP)) / 4.33) / 5)                 
256600                                                                          
256700        COMPUTE WS-KVPB-REF-DAY-PER-V =                                   
256800        (((REF2-KVPB-REF *                                                
256900        REF2-RESEASON(W271LTPB-PER-V-TIRP)) / 4.33) / 5)                  
257000                                                                          
257100        COMPUTE WS-KVPB-REF-DAY-PER-VI =                                  
257200        (((REF2-KVPB-REF *                                                
257300        REF2-RESEASON(W271LTPB-PER-VI-TIRP)) / 4.33) / 5)                 
257400                                                                          
257500        COMPUTE WS-KVPB-REF-DAY-BINN =                                    
257600        (((REF2-KVPB-REF *                                                
257700        REF2-RESEASON(W271LTPB-BINNDAY-TIRP)) / 4.33) / 5)                
257800                                                                          
257900        COMPUTE WS-LEDTIDSBEHOV ROUNDED =                                 
258000        ((W271LTPB-KVDAGAR-PER-I   *                                      
258100                          WS-KVPB-REF-DAY-PER-I)   +                      
258200        (W271LTPB-KVDAGAR-PER-II  *                                       
258300                          WS-KVPB-REF-DAY-PER-II)  +                      
258400        (W271LTPB-KVDAGAR-PER-III *                                       
258500                          WS-KVPB-REF-DAY-PER-III) +                      
258600        (W271LTPB-KVDAGAR-PER-IV  *                                       
258700                          WS-KVPB-REF-DAY-PER-IV)  +                      
258800        (W271LTPB-KVDAGAR-PER-V   *                                       
258900                          WS-KVPB-REF-DAY-PER-V)   +                      
259000        (W271LTPB-KVDAGAR-PER-VI  *                                       
259100                          WS-KVPB-REF-DAY-PER-VI))                        
259200                                                                          
259300     END-IF                                                               
259400     .                                                                    
259500     EJECT                                                                
259600                                                                          
259700                                                                          
259800 REF2-S07-PLATS-I-TABELL-NU SECTION.                                      
259900                                                                          
260000                                                                          
260100     MOVE REF2-KVPB-REF TO WS-KVPB-REF                                    
260200                                                                          
260300     IF W-IDLAND = 'CN'                                                   
260400     OR W-IDLAND = 'US'                                                   
260500       MOVE LART-PRMATRL        TO WS-PRARTBES                            
260600     ELSE                                                                 
260700       COMPUTE W-DAPRLIST = 99999999 - DAGENS-DATUM                       
260800       PERFORM IMS-GNP-WDK621                                             
260900       IF SEGMENT-SAKNAS                                                  
261000         MOVE CLAG-PRARTSTD       TO WS-PRARTBES                          
261100       ELSE                                                               
261200         MOVE NEJ                 TO FL-PRARTBES                          
261300         PERFORM UNTIL  SEGMENT-SAKNAS                                    
261400           IF PRL-SUINLEV-PR > ZERO                                       
261500             MOVE PRL-PRARTBES-PR  TO WS-PRARTBES                         
261600             SET SEGMENT-SAKNAS TO TRUE                                   
261700           ELSE                                                           
261800             IF FL-PRARTBES = NEJ                                         
261900               MOVE PRL-PRARTBES-PR TO WS-PRARTBES                        
262000               MOVE JA              TO FL-PRARTBES                        
262100             END-IF                                                       
262200             PERFORM IMS-GNP-WDK621                                       
262300           END-IF                                                         
262400         END-PERFORM                                                      
262500       END-IF                                                             
262600     END-IF                                                               
262700                                                                          
262800     IF REF2-KVPB-REF > 99999.9                                           
262900        MOVE +99999.9 TO WS-KVPB-REF                                      
263000     END-IF                                                               
263100     IF WS-PRARTBES > 9999999.99                                          
263200        MOVE +9999999.99 TO WS-PRARTBES                                   
263300     END-IF                                                               
263400     PERFORM REF2-S09-LETA-I-TABELL                                       
263500     .                                                                    
263600     EJECT                                                                
263700                                                                          
263800                                                                          
263900 REF2-S09-LETA-I-TABELL SECTION.                                          
264000                                                                          
264100     IF GRUNDTABELL                                                       
264200                                                                          
264300*****   TABELLEN FINNS I DEN SPARADE AREAN FÖR TABELL = 0                 
264400                                                                          
264500        MOVE 1 TO WS-RADIX                                                
264600        PERFORM UNTIL ( GRUNDTAB-2502-PRARTBES (WS-RADIX)                 
264700                                        = WS-PRARTBES                     
264800                       OR GRUNDTAB-2502-PRARTBES (WS-RADIX)               
264900                                        > WS-PRARTBES)                    
265000          ADD 1 TO WS-RADIX                                               
265100        END-PERFORM                                                       
265200                                                                          
265300        MOVE 1 TO WS-PR-KOLIX                                             
265400        PERFORM UNTIL                                                     
265500                (GRUNDTAB-2502-KVPB-REF( WS-PR-KOLIX)                     
265600                                        = WS-KVPB-REF                     
265700              OR GRUNDTAB-2502-KVPB-REF(WS-PR-KOLIX)                      
265800                                        > WS-KVPB-REF )                   
265900                                                                          
266000          ADD 1 TO WS-PR-KOLIX                                            
266100        END-PERFORM                                                       
266200                                                                          
266300     ELSE                                                                 
266400                                                                          
266500*****   TABELLEN FINNS I DEN SPARADE AREAN FÖR SAMMA SOM FÖREG.           
266600                                                                          
266700        MOVE 1 TO WS-RADIX                                                
266800        PERFORM UNTIL (AKTTAB-2502-PRARTBES (WS-RADIX)                    
266900                                        = WS-PRARTBES                     
267000                       OR AKTTAB-2502-PRARTBES (WS-RADIX)                 
267100                                        > WS-PRARTBES )                   
267200          ADD 1 TO WS-RADIX                                               
267300        END-PERFORM                                                       
267400                                                                          
267500        MOVE 1 TO WS-PR-KOLIX                                             
267600        PERFORM UNTIL                                                     
267700                (AKTTAB-2502-KVPB-REF( WS-PR-KOLIX)                       
267800                                        = WS-KVPB-REF                     
267900              OR AKTTAB-2502-KVPB-REF( WS-PR-KOLIX)                       
268000                                        > WS-KVPB-REF )                   
268100                                                                          
268200          ADD 1 TO WS-PR-KOLIX                                            
268300        END-PERFORM                                                       
268400     END-IF                                                               
268500     .                                                                    
268600     EJECT                                                                
268700                                                                          
268800                                                                          
268900 REF2-S11-TAB-BEHOV-PKT SECTION.                                          
269000                                                                          
269100*                                                                         
269200*    BERÄKNING AV SÄKERHETSLAGRET (WS-TABELLBEHOV) GÖRS                   
269300*    GENOM ATT RÄKNA ANTALET ARBETSDAGAR FROM BINDNINGSDATUM              
269400*    OCH DÄRIGENOM TA HÄNSYN TILL SÄSONG                                  
269500*                                                                         
269600                                                                          
269700     MOVE ZERO               TO TAB-KVARBDAG (1)                          
269800                                TAB-KVARBDAG (2)                          
269900                                TAB-KVARBDAG (3)                          
270000                                TAB-KVARBDAG (4)                          
270100                                TAB-KVARBDAG (5)                          
270200                                TAB-KVARBDAG (6)                          
270300                                TAB-KVARBDAG (7)                          
270400                                TAB-KVARBDAG (8)                          
270500                                TAB-KVARBDAG (9)                          
270600                                TAB-KVARBDAG (10)                         
270700                                TAB-KVARBDAG (11)                         
270800                                TAB-KVARBDAG (12)                         
270900                                                                          
271000     MOVE WS-BINNDAY-TIAAMMDD                                             
271100                             TO WORK-TIAAMMDD-FOM                         
271200     MOVE 002                TO WORK-KDCALL                               
271300     MOVE REF2-IDDC          TO WORK-IDDC                                 
271400     MOVE WS-KVREFLIM        TO WORK-KVWORKD                              
271500     CALL WORKDAY            USING WORK-KDCALL                            
271600                                   WORK-DATE-AREA                         
271700                                   WORK-KDSVAR                            
271800     IF WORK-KDSVAR-OK                                                    
271900        CONTINUE                                                          
272000     ELSE                                                                 
272100        MOVE 'FEL FRÅN WORKDAY I REF2-S11-1 SECTION I W6115900'           
272200                                 TO          FELTEXT-STR                  
272300        DISPLAY FELTEXT                                                   
272400        PERFORM REF2-S99-ABEND                                            
272500     END-IF                                                               
272600                                                                          
272700     IF WS-BINNDAY-TIAAMMDD (1:4) = WORK-TIAAMMDD-TOM (1:4)               
272800*                                                                         
272900*    ALLA ARBETSDAGAR I SAMMA PERIOD                                      
273000*                                                                         
273100       COMPUTE WS-TABELLBEHOV ROUNDED =                                   
273200               WS-KVREFLIM * WS-KVPB-REF-DAY-BINN                         
273300     ELSE                                                                 
273400*                                                                         
273500*    ARBETSDAGAR I OLIKA PERIODER                                         
273600*                                                                         
273700       MOVE WORK-TIAAMMDD-TOM TO WS-TIAAMMDD-FOM                          
273800       MOVE 01               TO WS-TIAAMMDD-FOM (5:2)                     
273900                                                                          
274000       PERFORM UNTIL (WS-BINNDAY-TIAAMMDD (1:4)                           
274100                                       = WS-TIAAMMDD-FOM (1:4))           
274200         MOVE WS-TIAAMMDD-FOM                                             
274300                             TO WORK-TIAAMMDD-FOM                         
274400         MOVE REF2-IDDC      TO WORK-IDDC                                 
274500         MOVE 001            TO WORK-KDCALL                               
274600         CALL WORKDAY        USING WORK-KDCALL                            
274700                                   WORK-DATE-AREA                         
274800                                   WORK-KDSVAR                            
274900         IF WORK-KDSVAR-OK                                                
275000           MOVE WS-TIAAMMDD-FOM (3:2)                                     
275100                             TO PER-IX                                    
275200           MOVE WORK-KVWORKD  TO WS-KVARBDAG                              
275300           IF WORK-TIAAMMDD-TOM (5:2) = 01                                
275400*                                                                         
275500*    ATT RÄKNA UT ANTALET ARBETSDAGAR I EN MÅNAD GÖRS GENOM               
275600*    ATT INITIERA WORK-TIAAMMDD-TOM MED DEN FÖRSTA I NÄSTA MÅNAD          
275700*    DÄRFÖR SKA ANTALET ARBETSDAGAR MINSKAS MED ETT OM DENNA              
275800*    DAG ÄR EN ARBETSDAG                                                  
275900*                                                                         
276000             MOVE WORK-TIAAMMDD-TOM                                       
276100                             TO WORK-TIAAMMDD-FOM                         
276200             MOVE 001        TO WORK-KDCALL                               
276300             MOVE REF2-IDDC  TO WORK-IDDC                                 
276400             CALL WORKDAY    USING WORK-KDCALL                            
276500                                   WORK-DATE-AREA                         
276600                                   WORK-KDSVAR                            
276700             IF WORK-KDSVAR-OK                                            
276800               SUBTRACT WORK-KVWORKD                                      
276900                             FROM WS-KVARBDAG                             
277000             ELSE                                                         
277100               MOVE 'FEL FRÅN WORKDAY, REF2-S11-3 SECT I W6115900'        
277200                                         TO  FELTEXT-STR                  
277300               DISPLAY FELTEXT                                            
277400               PERFORM REF2-S99-ABEND                                     
277500             END-IF                                                       
277600           END-IF                                                         
277700           MOVE WS-KVARBDAG  TO TAB-KVARBDAG (PER-IX)                     
277800           SUBTRACT WS-KVARBDAG                                           
277900                             FROM WS-KVREFLIM                             
278000           MOVE WS-TIAAMMDD-FOM                                           
278100                             TO WORK-TIAAMMDD-TOM                         
278200           IF WS-TIAAMMDD-FOM = 000101                                    
278300             MOVE 991201     TO WS-TIAAMMDD-FOM                           
278400           ELSE                                                           
278500             IF WS-TIMM = 01                                              
278600               SUBTRACT 1    FROM WS-TIAA                                 
278700               MOVE 12       TO WS-TIMM                                   
278800             ELSE                                                         
278900               SUBTRACT 1    FROM WS-TIMM                                 
279000             END-IF                                                       
279100           END-IF                                                         
279200         ELSE                                                             
279300           MOVE 'FEL FRÅN WORKDAY, REF2-S11-2 SECTION I W6115900'         
279400                                    TO       FELTEXT-STR                  
279500           DISPLAY FELTEXT                                                
279600           PERFORM REF2-S99-ABEND                                         
279700         END-IF                                                           
279800       END-PERFORM                                                        
279900       MOVE WS-TIAAMMDD-FOM (3:2)                                         
280000                             TO PER-IX                                    
280100       MOVE WS-KVREFLIM      TO TAB-KVARBDAG (PER-IX)                     
280200                                                                          
280300       MOVE ZERO             TO WS-TABELLBEHOV                            
280400       MOVE 1                TO PER-IX                                    
280500                                                                          
280600       PERFORM UNTIL (PER-IX > 12)                                        
280700                                                                          
280800         COMPUTE WS-TABELLBEHOV ROUNDED =                                 
280900                 WS-TABELLBEHOV + (TAB-KVARBDAG (PER-IX)                  
281000                 * (((REF2-KVPB-REF * REF2-RESEASON (PER-IX))             
281100                      / 4.33 ) / 5))                                      
281200         ADD 1               TO PER-IX                                    
281300       END-PERFORM                                                        
281400     END-IF                                                               
281500     .                                                                    
281600     EJECT                                                                
281700                                                                          
281800                                                                          
281900 REF2-S12-TAB-BEHOV-KVANT SECTION.                                        
282000                                                                          
282100*                                                                         
282200*    BERÄKNING AV PÅFYLLNADSKVANTEN (WS-PAFYLLNKVANT) GÖRS                
282300*    GENOM ATT RÄKNA ANTALET ARBETSDAGAR FROM BINDNINGSDATUM              
282400*    OCH DÄRIGENOM TA HÄNSYN TILL SÄSONG                                  
282500*                                                                         
282600                                                                          
282700     MOVE ZERO               TO TAB-KVARBDAG (1)                          
282800                                TAB-KVARBDAG (2)                          
282900                                TAB-KVARBDAG (3)                          
283000                                TAB-KVARBDAG (4)                          
283100                                TAB-KVARBDAG (5)                          
283200                                TAB-KVARBDAG (6)                          
283300                                TAB-KVARBDAG (7)                          
283400                                TAB-KVARBDAG (8)                          
283500                                TAB-KVARBDAG (9)                          
283600                                TAB-KVARBDAG (10)                         
283700                                TAB-KVARBDAG (11)                         
283800                                TAB-KVARBDAG (12)                         
283900                                                                          
284000     MOVE WS-BINNDAY-TIAAMMDD                                             
284100                             TO WORK-TIAAMMDD-FOM                         
284200     MOVE 002                TO WORK-KDCALL                               
284300     MOVE REF2-IDDC          TO WORK-IDDC                                 
284400     MOVE WS-KVREFKVA        TO WORK-KVWORKD                              
284500     CALL WORKDAY            USING WORK-KDCALL                            
284600                                   WORK-DATE-AREA                         
284700                                   WORK-KDSVAR                            
284800     IF WORK-KDSVAR-OK                                                    
284900        IF WORK-TIAAMMDD-TOM (1:2) > 50                                   
285000          MOVE 19            TO JMF-AAAA (1:2)                            
285100          MOVE WORK-TIAAMMDD-TOM (1:2)                                    
285200                             TO JMF-AAAA (3:2)                            
285300        ELSE                                                              
285400          MOVE 20            TO JMF-AAAA (1:2)                            
285500          MOVE WORK-TIAAMMDD-TOM (1:2)                                    
285600                             TO JMF-AAAA (3:2)                            
285700        END-IF                                                            
285800****                                                                      
285900**** KONTROLL OM TIDPUNKTEN LIGGER UTANFÖR ARBETSTIDSKALENDERN            
286000****                                                                      
286100        IF JMF-AAAA > DAGENS-AAR-PLUS2                                    
286200          MOVE DAGENS-AAR-PLUS2 (3:2)                                     
286300                             TO WORK-TIAAMMDD-TOM (1:2)                   
286400          MOVE 1231          TO WORK-TIAAMMDD-TOM (3:2)                   
286500        END-IF                                                            
286600     ELSE                                                                 
286700        MOVE 'FEL FRÅN WORKDAY, REF2-S12-1 SECTION I W6115900'            
286800                                 TO          FELTEXT-STR                  
286900        DISPLAY FELTEXT                                                   
287000        PERFORM REF2-S99-ABEND                                            
287100     END-IF                                                               
287200                                                                          
287300     IF WS-BINNDAY-TIAAMMDD (1:4) = WORK-TIAAMMDD-TOM (1:4)               
287400*                                                                         
287500*    ALLA ARBETSDAGAR I SAMMA PERIOD                                      
287600*                                                                         
287700       COMPUTE WS-PAFYLLNKVANT ROUNDED =                                  
287800               WS-KVREFKVA * WS-KVPB-REF-DAY-BINN                         
287900     ELSE                                                                 
288000*                                                                         
288100*    ARBETSDAGAR I OLIKA PERIODER                                         
288200*                                                                         
288300       MOVE WORK-TIAAMMDD-TOM TO WS-TIAAMMDD-FOM                          
288400       MOVE 01               TO WS-TIAAMMDD-FOM (5:2)                     
288500                                                                          
288600       PERFORM UNTIL (WS-BINNDAY-TIAAMMDD (1:4)                           
288700                                       = WS-TIAAMMDD-FOM (1:4))           
288800         MOVE WS-TIAAMMDD-FOM                                             
288900                             TO WORK-TIAAMMDD-FOM                         
289000         MOVE REF2-IDDC      TO WORK-IDDC                                 
289100         MOVE 001            TO WORK-KDCALL                               
289200         CALL WORKDAY        USING WORK-KDCALL                            
289300                                   WORK-DATE-AREA                         
289400                                   WORK-KDSVAR                            
289500         IF WORK-KDSVAR-OK                                                
289600           MOVE WS-TIAAMMDD-FOM (3:2)                                     
289700                             TO PER-IX                                    
289800           MOVE WORK-KVWORKD  TO WS-KVARBDAG                              
289900           IF WORK-TIAAMMDD-TOM (5:2) = 01                                
290000*                                                                         
290100*    ATT RÄKNA UT ANTALET ARBETSDAGAR I EN MÅNAD GÖRS GENOM               
290200*    ATT INITIERA WORK-TIAAMMDD-TOM MED DEN FÖRSTA I NÄSTA MÅNAD          
290300*    DÄRFÖR SKA ANTALET ARBETSDAGAR MINSKAS MED ETT OM DENNA              
290400*    DAG ÄR EN ARBETSDAG                                                  
290500*                                                                         
290600             MOVE WORK-TIAAMMDD-TOM                                       
290700                             TO WORK-TIAAMMDD-FOM                         
290800             MOVE 001        TO WORK-KDCALL                               
290900             MOVE REF2-IDDC  TO WORK-IDDC                                 
291000             CALL WORKDAY    USING WORK-KDCALL                            
291100                                   WORK-DATE-AREA                         
291200                                   WORK-KDSVAR                            
291300             IF WORK-KDSVAR-OK                                            
291400               SUBTRACT WORK-KVWORKD                                      
291500                             FROM WS-KVARBDAG                             
291600             ELSE                                                         
291700               MOVE 'FEL FRÅN WORKDAY, REF2-S12-3 SECT I W6115900'        
291800                                         TO  FELTEXT-STR                  
291900               DISPLAY FELTEXT                                            
292000               PERFORM REF2-S99-ABEND                                     
292100             END-IF                                                       
292200           END-IF                                                         
292300           MOVE WS-KVARBDAG  TO TAB-KVARBDAG (PER-IX)                     
292400           SUBTRACT WS-KVARBDAG                                           
292500                             FROM WS-KVREFKVA                             
292600           MOVE WS-TIAAMMDD-FOM                                           
292700                             TO WORK-TIAAMMDD-TOM                         
292800           IF WS-TIAAMMDD-FOM = 000101                                    
292900             MOVE 991201     TO WS-TIAAMMDD-FOM                           
293000           ELSE                                                           
293100             IF WS-TIMM = 01                                              
293200               SUBTRACT 1    FROM WS-TIAA                                 
293300               MOVE 12       TO WS-TIMM                                   
293400             ELSE                                                         
293500               SUBTRACT 1    FROM WS-TIMM                                 
293600             END-IF                                                       
293700           END-IF                                                         
293800         ELSE                                                             
293900           MOVE 'FEL FRÅN WORKDAY, REF2-S12-2 SECTION I W6115900'         
294000                                    TO       FELTEXT-STR                  
294100           DISPLAY FELTEXT                                                
294200           PERFORM REF2-S99-ABEND                                         
294300         END-IF                                                           
294400       END-PERFORM                                                        
294500       MOVE WS-TIAAMMDD-FOM (3:2)                                         
294600                             TO PER-IX                                    
294700       MOVE WS-KVREFKVA      TO TAB-KVARBDAG (PER-IX)                     
294800                                                                          
294900       MOVE ZERO             TO WS-PAFYLLNKVANT                           
295000       MOVE 1                TO PER-IX                                    
295100                                                                          
295200       PERFORM UNTIL (PER-IX > 12)                                        
295300                                                                          
295400         COMPUTE WS-PAFYLLNKVANT ROUNDED =                                
295500                 WS-PAFYLLNKVANT + (TAB-KVARBDAG (PER-IX)                 
295600                 * (((REF2-KVPB-REF * REF2-RESEASON (PER-IX))             
295700                      / 4.33 ) / 5))                                      
295800         ADD 1               TO PER-IX                                    
295900       END-PERFORM                                                        
296000     END-IF                                                               
296100     .                                                                    
296200     EJECT                                                                
296300                                                                          
296400                                                                          
296500 REF2-S99-ABEND SECTION.                                                  
296600                                                                          
296700     SKIP2                                                                
296800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
296900     .                                                                    
297000     EJECT                                                                
297100                                                                          
297200                                                                          
297300                                                                          
297400* --- IMS SEKTIONER ---                                                   
297500     SKIP3                                                                
297600                                                                          
297700 IMS-GU-2502 SECTION.                                                     
297800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
297900          DELIMITED BY SIZE INTO SSA1                                     
298000     STRING 'WDGX2502(IDREFTAB =' W-IDREFTAB-X ')'                        
298100          DELIMITED BY SIZE INTO SSA2                                     
298200     MOVE '  GE' TO GODK-STATUSKODER                                      
298300     CALL CBLTDLI USING GU 2501-PCB DLI-IO-AREA SSA1 SSA2                 
298400     MOVE 2501-STATUS-CODE TO STATUS-WS                                   
298500     PERFORM IMS-STATUSKONTROLL                                           
298600     .                                                                    
298700     EJECT                                                                
298800                                                                          
298900 IMS-GU-K601 SECTION.                                                     
299000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                             
299100                    '&KDERS    =' W-KDERS-0-X ')'                         
299200            DELIMITED BY SIZE INTO SSA1                                   
299300     MOVE '  GE' TO GODK-STATUSKODER                                      
299400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
299500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
299600     PERFORM IMS-STATUSKONTROLL                                           
299700     .                                                                    
299800     SKIP3                                                                
299900 IMS-GNP-K611    SECTION.                                                 
300000     MOVE 'WDK611   '  TO SSA1                                            
300100     MOVE '  ' TO GODK-STATUSKODER                                        
300200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
300300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
300400     PERFORM IMS-STATUSKONTROLL                                           
300500     .                                                                    
300600     SKIP3                                                                
300700 IMS-GNP-WDK621    SECTION.                                               
300800     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                        
300900            DELIMITED BY SIZE INTO SSA1                                   
301000     MOVE '  GE' TO GODK-STATUSKODER                                      
301100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
301200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
301300     PERFORM IMS-STATUSKONTROLL                                           
301400     .                                                                    
301500     SKIP3                                                                
301600 IMS-GU-WDK701    SECTION.                                                
301700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
301800            DELIMITED BY SIZE INTO SSA1                                   
301900     MOVE '  GE' TO GODK-STATUSKODER                                      
302000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
302100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
302200     PERFORM IMS-STATUSKONTROLL                                           
302300     .                                                                    
302400     SKIP3                                                                
302500 IMS-GNP-WDK711    SECTION.                                               
302600     MOVE 'WDK711   ' TO SSA1                                             
302700     MOVE '  GE'        TO GODK-STATUSKODER                               
302800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
302900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
303000     PERFORM IMS-STATUSKONTROLL                                           
303100     .                                                                    
303200     SKIP3                                                                
303300     EJECT                                                                
303400 IMS-GNP-WDK712    SECTION.                                               
303500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
303600          DELIMITED BY SIZE INTO SSA1                                     
303700     MOVE '  GE'        TO GODK-STATUSKODER                               
303800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK712 SSA1              
303900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
304000     PERFORM IMS-STATUSKONTROLL                                           
304100     .                                                                    
304200     SKIP3                                                                
304300     EJECT                                                                
304400 IMS-GU-K601-ERS SECTION.                                                 
304500                                                                          
304600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-ERS-X ')'                     
304700          DELIMITED BY SIZE INTO SSA1                                     
304800     MOVE '  GE' TO GODK-STATUSKODER                                      
304900     CALL CBLTDLI USING GU WDK6-2-PCB DLI-IO-AREA-WDK601-ERS SSA1         
305000     MOVE WDK6-2-STATUS-CODE TO STATUS-WS                                 
305100     PERFORM IMS-STATUSKONTROLL                                           
305200     .                                                                    
305300     EJECT                                                                
305400 IMS-GNP-K611-ERS SECTION.                                                
305500                                                                          
305600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
305700          DELIMITED BY SIZE INTO SSA1                                     
305800     MOVE '  GE' TO GODK-STATUSKODER                                      
305900     CALL CBLTDLI USING GNP WDK6-2-PCB DLI-IO-AREA-WDK611-ERS SSA1        
306000     MOVE WDK6-2-STATUS-CODE TO STATUS-WS                                 
306100     PERFORM IMS-STATUSKONTROLL                                           
306200     .                                                                    
306300     EJECT                                                                
306400 IMS-GU-K711-ERS SECTION.                                                 
306500                                                                          
306600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-ERS-X ')'                     
306700          DELIMITED BY SIZE INTO SSA1                                     
306800     STRING 'WDK711  (IDDC     =' W-IDDC-ERS-X ')'                        
306900          DELIMITED BY SIZE INTO SSA2                                     
307000     MOVE '  GE' TO GODK-STATUSKODER                                      
307100     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-AREA-WDK711-ERS              
307200          SSA1 SSA2                                                       
307300     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
307400     PERFORM IMS-STATUSKONTROLL                                           
307500     .                                                                    
307600     EJECT                                                                
307700 IMS-GU-L601 SECTION.                                                     
307800                                                                          
307900     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
308000          DELIMITED BY SIZE INTO SSA1                                     
308100     MOVE '  GE' TO GODK-STATUSKODER                                      
308200     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
308300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
308400     PERFORM IMS-STATUSKONTROLL                                           
308500     .                                                                    
308600     EJECT                                                                
308700 IMS-GNP-L611 SECTION.                                                    
308800                                                                          
308900     STRING 'WDL611  (IDDC     =' W-IDDC-X ')'                            
309000          DELIMITED BY SIZE INTO SSA1                                     
309100     MOVE '  GE' TO GODK-STATUSKODER                                      
309200     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
309300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
309400     PERFORM IMS-STATUSKONTROLL                                           
309500     .                                                                    
309600     EJECT                                                                
309700 IMS-GU-WDD7A1 SECTION.                                                   
309800                                                                          
309900     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN-X                        
310000                    '&WDD7A1KY<=' W-WDD7A1KY-MAX-X ')'                    
310100            DELIMITED BY SIZE INTO SSA1                                   
310200     MOVE '  GE' TO GODK-STATUSKODER                                      
310300     CALL CBLTDLI USING GU WDD7A-PCB DLI-IO-AREA-WDD7A1 SSA1              
310400     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
310500     PERFORM IMS-STATUSKONTROLL                                           
310600     .                                                                    
310700     EJECT                                                                
310800 IMS-GU-WDB601    SECTION.                                                
310900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
311000          DELIMITED BY SIZE INTO SSA1                                     
311100     MOVE '  ' TO GODK-STATUSKODER                                        
311200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
311300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
311400     PERFORM IMS-STATUSKONTROLL                                           
311500     .                                                                    
311600                                                                          
311700 IMS-GU-WDB616    SECTION.                                                
311800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
311900          DELIMITED BY SIZE INTO SSA1                                     
312000     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
312100          DELIMITED BY SIZE INTO SSA2                                     
312200     MOVE '  ' TO GODK-STATUSKODER                                        
312300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
312400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
312500     PERFORM IMS-STATUSKONTROLL                                           
312600     .                                                                    
312700     EJECT                                                                
312800 IMS-STATUSKONTROLL SECTION.                                              
312900     SET STATUS-IX TO 1                                                   
313000     SEARCH GODK-STATUS  AT END CALL FELLOG                               
313100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
313200          CONTINUE                                                        
313300     END-SEARCH                                                           
313400     .                                                                    
313500     EJECT                                                                
313600*    -COPY WY2000P1                                                       
313700     EJECT                                                                
313800*    -COPY WY2000P3                                                       
