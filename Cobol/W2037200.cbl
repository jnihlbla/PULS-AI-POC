000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2037200.                                                
000300 AUTHOR.         TOMMIE JIVARP/ STEFAN ANDREASSON                         
000400 DATE-WRITTEN.   98/05/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MANUELLA KÖP FRÅN CDC TILL NDC I JAPAN OCH AUSTRALIEN            
000900*                                                                         
001000*        ORDERFÖRSLAG FRÅN REFILLSYSTEMET KOMMER UPP PÅ                   
001100*        DENNA BILD VILKET MAN KAN ACCEPTERA, ÄNDRA ELLER                 
001200*        FÖRKASTA                                                         
001300*        DESSUTOM KAN MAN LÄGGA MANUELLA ORDERBESTÄLLNINGAR HÄR           
001400*                                                                         
001500*        ENTER :    ANVÄNDS VID SIMULERING                                
001600*        PF 11 :    UPPDATERING                                           
001700*                   1) ETT ORDERFÖRSLAG MED KVANTITET IFYLLD              
001800*                      KOMMER ATT GENERERA EN ORDER                       
001900*                   2) ETT ORDERFÖRSLAG MED NOLL I KVANTITET              
002000*                      KOMMER ATT TAS BORT FRÅN WDE3 (FYSISKT)            
002100*                   3) VID EN NY BESTÄLLNING KOMMER EN POST PÅ            
002200*                      WDE3 ATT LÄGGAS UPP VILKET KOMMER ATT              
002300*                      GENERERA EN ORDER                                  
002400*                   4) DATAELEMENTEN KVPB-REF, FLREFBEO                   
002500*                      SAMT TEARTNOT KOMMER ATT UPPDATERAS                
002600*                      SÄTTS MANUELL PROGNOS PÅ                           
002700*                      PASSIV ARTIKEL AKTIVERAS DEN                       
002800*                                                                         
002900*        PF 7          BLÄDDRING BAKÅT                                    
003000*        PF 8          BLÄDDRING FRAMÅT (ETT ORDERFÖRSLAG LIGGER          
003100*                      KVAR PÅ WDE3 SOM ICKE BEHANDLAT OM MAN EJ          
003200*                      UPPDATERAR MED PF11 FÖRST, MED KVANT = 0)          
003300*                                                                         
003400*        PROGRAMMET UPPDATERAR WDK7                                       
003500*                              WLUSEA (WDP7)                              
003600*                              WDE3                                       
003700*        PROGRAMMET LÄSER      WDD3                                       
003800*                              WDA5                                       
003900*                              WDK6                                       
004000*                              WDN6                                       
004100*                              WDD7                                       
004200*                              WDK9                                       
004300*                              WDL7 + WDL4                                
004400*                              WDL6                                       
004500*                                                                         
004600*    INDATA.                                                              
004700*        TRANSAKTION: W2T372                                              
004800*                     W2T372U                                             
004900*        MID:         W2I37201                                            
005000*                                                                         
005100*    UTDATA.                                                              
005200*        MOD:         W2O37201                                            
005300*                                                                         
005400*   ÄNDRINGAR:                                                            
005500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
005600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
005700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
005800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
005900*        09-SEP-2021:STORY 2224095 REMOVED WS-TIERSDAT-VIPS CTRL.         
006000*                   ADDED KVDISP-SEND-DC CHECK FOR ALL KDERS PARTS        
006100*                   EXCEPT KDERS = 52.                                    
006104*                                                                         
006200                                                                          
006300     SKIP3                                                                
006400 ENVIRONMENT DIVISION.                                                    
006500     EJECT                                                                
006600 DATA DIVISION.                                                           
006700 WORKING-STORAGE SECTION.                                                 
006800*    -COPY WY2000W1                                                       
006900     SKIP3                                                                
007000 77  IDPGM                       PIC X(08)   VALUE 'W2037200'.            
007100                                                                          
007200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007400                                                                          
007500 77  YES                         PIC X       VALUE 'Y'.                   
007600 77  JA                          PIC X       VALUE 'J'.                   
007700 77  NEJ                         PIC X       VALUE 'N'.                   
007800 77  AKTIV                       PIC X       VALUE 'A'.                   
007900 77  PASSIV                      PIC X       VALUE 'P'.                   
008000 77  DEFINITIV                   PIC S9      VALUE +1  COMP-3.            
008100 77  MOD-IX                      PIC 9(3)    VALUE ZERO.                  
008200 77  IX                          PIC 9(3)    VALUE ZERO.                  
008300 77  IX2                         PIC 9(3)    VALUE ZERO.                  
008400 77  INDX                        PIC S9(3)   VALUE ZERO.                  
008500 77  IX-VV                       PIC 9(2)    VALUE ZERO.                  
008600 77  IX-CD                       PIC 9(3)    VALUE ZERO.                  
008700 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
008800 77  DAGENS-DATUM-K724           PIC  9(8)   VALUE ZERO.                  
008900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 77  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
009100 77  WS-FLSIM                    PIC X       VALUE SPACES.                
009200 77  SW-KVPB-SEP                 PIC X       VALUE ' '.                   
009300     88  SW-KVPB-SEP-JA                      VALUE 'J'.                   
009400     88  SW-KVPB-SEP-NEJ                     VALUE 'N'.                   
009500 77  SW-KVPB-PLAN                PIC X       VALUE ' '.                   
009600     88  KVPB-PLAN-UPD-JA                    VALUE 'J'.                   
009700     88  KVPB-PLAN-UPD-NEJ                   VALUE 'N'.                   
009800*                                                                         
009900*01  -COPY WWDC99                                                         
010000*01  -COPY WWDC99      -PRE REFILL-                                       
010100*01  -COPY WWDCLAND                                                       
010200                                                                          
010300 01  DAGENS-PER                  PIC 9(4)   VALUE ZERO.                   
010400 01  DAG-PER REDEFINES DAGENS-PER.                                        
010500         05 DAGENS-AA            PIC 9(2).                                
010600         05 DAGENS-PP            PIC 9(2).                                
010700 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
010800 77  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
010900       EJECT                                                              
011000 01  WS.                                                                  
011100  05 WS-TEST-2.                                                           
011200     10  WS-A2                   PIC X     VALUE SPACE.                   
011300     10  FILLER                  PIC X     VALUE '/'.                     
011400                                                                          
011500  05 WS-TEMFSINF.                                                         
011600    10 WS-TEMFSINF-SOURCE        PIC X(07)   VALUE SPACE.                 
011700    10 FILLER                    PIC X       VALUE SPACE.                 
011800    10 FILLER.                                                            
011900      15 WS-TEMFSINF-NDC         PIC X(10)   VALUE SPACE.                 
012000      15 FILLER                  PIC X       VALUE SPACE.                 
012100    10 WS-TEMFSINF-TEXT          PIC X(13)   VALUE SPACE.                 
012200                                                                          
012300  05 WS-TEMF-RED-OS.                                                      
012400    10 WS-TEMF-TEXT-OS           PIC X(3)    VALUE SPACE.                 
012500    10 WS-TEMF-OS-NDC            PIC X(3)    VALUE SPACE.                 
012600                                                                          
012700  05 WS-TEMF-RED-INVBAL.                                                  
012800    10 FILLER                    PIC X(5)    VALUE 'INVB '.               
012900    10 WS-TEMF-UTRSALDO          PIC -(6)9.                               
013000    10 FILLER                    PIC X       VALUE SPACE.                 
013100                                                                          
013200  05 WS-TEMF-UTRSALDO-NUM        PIC S9(7).                               
013300                                                                          
013400  05 WS-ANT-VV                   PIC  9(2)   VALUE ZERO.                  
013500  05 WS-TIAAVV.                                                           
013600    10 WS-AAR                    PIC  9(2)   VALUE ZERO.                  
013700    10 WS-VV                     PIC  9(2)   VALUE ZERO.                  
013800  05 TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                                 
013900  05 WS-TIAAPER.                                                          
014000    10 TIAA                      PIC  9(2)   VALUE ZERO.                  
014100    10 PER                       PIC  9(2)   VALUE ZERO.                  
014200  05    TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
014300  05 WS-FOM-TOM.                                                          
014400    10 WS-FOM                    PIC  X(2)   VALUE ZERO.                  
014500    10 WS-STRECK                 PIC  X(1)   VALUE '-'.                   
014600    10 WS-TOM                    PIC  X(2)   VALUE ZERO.                  
014700*                                                                         
014800*   WS-TABELL ÄR EN RULLANDE TABELL DÄR                                   
014900*   IX = 1 ÄR DAGENS PERIOD ETT ÅR TILLBAKA                               
015000*   IX = 12 ÄR FÖRRA PERIODEN                                             
015100*                                                                         
015200  05 WS-TABELL    OCCURS 12.                                              
015300    10 WS-PER                    PIC  9(2)   VALUE ZERO.                  
015400    10 WS-FORSTA-V               PIC  9(2)   VALUE ZERO.                  
015500    10 WS-SISTA-V                PIC  9(2)   VALUE ZERO.                  
015600    10 WS-KVOI                   PIC S9(7)   VALUE ZERO.                  
015700  05 WS-BALANCE                  PIC S9(7)   VALUE ZERO.                  
015800  05 WS-REST                     PIC S9(7)   VALUE ZERO.                  
015900  05 WS-SLASK                    PIC S9(7)   VALUE ZERO.                  
016000  05 WS-KDERS                    PIC 9(3)    VALUE ZERO.                  
016100  05 WS-FOREG-AAR                PIC  9(4)   VALUE ZERO.                  
016200  05 WS-KVROS                    PIC S9(7)   VALUE ZERO.                  
016300  05 WS-KVQPACK-3                PIC S9(5)  VALUE ZERO COMP-3.            
016400                                                                          
016500  05 WS-ANTAL-POSTER             PIC S9(7)   VALUE ZERO.                  
016600  05 WS-ANTAL-VECKOR             PIC S9(3)   VALUE ZERO.                  
016700  05 WS-KVOI-SUM                 PIC S9(7)   VALUE ZERO.                  
016800  05 WS-KVAVIS                   PIC S9(9)   VALUE ZERO.                  
016900  05 WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
017000  05 WS-DAPRLIST                 PIC 9(8)    VALUE ZERO.                  
017100  05 WS-SPARA-IDARTNR            PIC 9(9)    VALUE ZERO.                  
017200  05 WS-SPARA-IDDC               PIC 9(2)    VALUE ZERO.                  
017300  05 WS-AVER-COST-NUM            PIC 9(7)V9(2)                            
017400                                             VALUE ZERO.                  
017500  05 WS-AVER-COST-RED            PIC Z(4)9.9(2).                          
017600  05 WS-KR-VIKT                  PIC S9(9)V9(2)                           
017700                                             VALUE ZERO.                  
017800  05 WS-KR-VIKT-RED              PIC  9(9)   VALUE ZERO.                  
017900  05 WS-KR-VOLYM                 PIC S9(9)V9(2)                           
018000                                             VALUE ZERO.                  
018100  05 WS-KR-VOLYM-RED             PIC  9(9)   VALUE ZERO.                  
018200  05 WS-KVAKS                    PIC S9(9)V9(2)                           
018300                                             VALUE ZERO.                  
018400  05 WS-KVROS-CDC                PIC S9(6)   VALUE ZERO.                  
018500  05 WS-KVPB                     PIC S9(9)V9(2)                           
018600                                             VALUE ZERO.                  
018700  05 WS-TOTAL-ANTAL              PIC 9(9)    VALUE ZERO.                  
018800  05 WS-INDEX                    PIC S9V9(2) VALUE ZERO.                  
018900  05 WS-DASPSEA                  PIC 9(7)    VALUE ZERO.                  
019000  05 WS-OSAKERHET                PIC 9(2)V9  VALUE ZERO.                  
019100  05 WS-SIMIX-SUM                PIC S9(2)V9(2)                           
019200                                             VALUE ZERO.                  
019300  05 WS-SIMIX                    PIC S9(2)V9(2)                           
019400                                             VALUE ZERO.                  
019500  05 WS-SIMIX-X                  PIC X(4).                                
019600  05 WS-SIMIX-N                  REDEFINES WS-SIMIX-X                     
019700                                 PIC 9.9(2).                              
019800  05 WS-SUPERWEEK                PIC S9(9)V9(2)                           
019900                                             VALUE ZERO.                  
020000                                                                          
020100  05 WS-AVAILABLE                PIC S9(7)   VALUE ZERO.                  
020200  05 WS-ORDERED                  PIC  9(7)   VALUE ZERO.                  
020300  05 WS-KVAKS-SDC                PIC  9(7)   VALUE ZERO.                  
020400  05 WS-PURCHQTY                 PIC  9(7)   VALUE ZERO.                  
020500  05 WS-IDDC-SAVED               PIC  X(2)   VALUE SPACE.                 
020600  05 WS-IDDC-REF-K7              PIC X(2)    VALUE SPACES.                
020700                                                                          
020800  05 WS-PURCHQTY-SIM             PIC  9(7)   VALUE ZERO.                  
020900                                                                          
021000  05 WS-RED-PURCHQTY             PIC Z(6)9   VALUE ZERO.                  
021100  05 WS-KVPB-REF                 PIC  9(6)V9 VALUE ZERO.                  
021200  05 WS-KVPBREOI                 PIC  9(6)V9 VALUE ZERO.                  
021300  05 WS-KVPB-TOT                 PIC  9(6)V9 VALUE ZERO.                  
021400  05 WS-RED-KVPB-REF             PIC Z(5)9.9 VALUE ZERO.                  
021500  05 WS-RED-KVPBREOI             PIC Z(5)9.9 VALUE ZERO.                  
021600  05 WS-OVERLAGER                PIC X       VALUE 'N'.                   
021700  05 WS-EOP                      PIC 9(5)    VALUE ZERO.                  
021800                                                                          
021900  05 WS-ANT-REVIEW               PIC S9(5)   VALUE ZERO.                  
022000  05 WS-KTRL-PRIO                PIC 9(2)    VALUE ZERO.                  
022100  05 WS-COUNT                    PIC 9       VALUE ZERO.                  
022200  05 WS-RESTKVANT                PIC S9(7)   VALUE ZERO.                  
022300  05 WS-IDREFTYP                 PIC X       VALUE SPACE.                 
022400  05 WS-IDLAND-HEAD              PIC X(2)    VALUE SPACE.                 
022500  05 WS-IDLAND-SEND              PIC X(2)    VALUE SPACE.                 
022600  05 WS-KVOKS-TOT                PIC S9(7)   VALUE ZERO COMP-3.           
022700  05 WS-IDDISTR                  PIC 9(4).                                
022800  05 WS-REF-KDREFTXT             PIC 9(2)    VALUE ZERO.                  
022910  05 WS-FIRST-ETA                PIC 9(6)    VALUE 999999.                
023000  05 WS-FIRST-ETA-QTY            PIC 9(7)    VALUE ZERO.                  
023100  05 WS-SECOND-ETA               PIC 9(6)    VALUE 999999.                
023200  05 WS-SECOND-ETA-QTY           PIC 9(7)    VALUE ZERO.                  
023300  05 WS-KVBR-TOT                 PIC S9(7)   VALUE ZERO COMP-3.           
023400  05 WS-KDFRAKT                  PIC S9(3)   VALUE ZERO COMP-3.           
023500  05 WS-FLAGGA-FCD               PIC X       VALUE SPACE.                 
023600  05 WS-DAPUBL                   PIC 9(6)    VALUE ZERO.                  
023700  05 WS-DAPUBL-AAVVD             PIC 9(6)    VALUE ZERO.                  
023800  05 WS-SPARA-IDDC-FD-HAEMTA     PIC X(2)    VALUE SPACE.                 
023900  05 WS-IDDC-SPAR                PIC X(2)    VALUE SPACE.                 
024000  05 WS-HELTAL-BEST              PIC S9(7)   VALUE ZERO COMP-3.           
024100  05 WS-HELTAL-SALDO             PIC S9(7)   VALUE ZERO COMP-3.           
024200  05 WS-SALDO                    PIC S9(7)   VALUE ZERO COMP-3.           
024300  05 WS-VKART                    PIC S9(7)   VALUE ZERO COMP-3.           
024400  05 WS-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO COMP-3.        
024500  05 WS-PREADV-QTY               PIC S9(9)   VALUE ZERO.                  
024600  05 WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
024700  05 WS-TIINLINL                 PIC 9(6)    VALUE ZERO.                  
024800  05 FL-PRARTBES                 PIC X       VALUE 'N'.                   
024900  05 WS-PRARTBES                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
025000  05 WS-FLREFERAL                PIC X       VALUE 'N'.                   
025100  05 WS-KVDISP-SEND-DC           PIC S9(7)   VALUE ZERO COMP-3.           
025200  05 WS-REAIRCO                  PIC S9(6)V9(1) VALUE ZERO COMP-3.        
025210  05 WS-PRFRAKT                  PIC S9(7)      VALUE ZERO COMP-3.        
025220  05 WS-AIR-COST-SEK             PIC 9(7) VALUE ZERO.                     
025300                                                                          
025400*********************************************************                 
025500*    WS-MSGI-AREA-2372                                                    
025600*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
025700*           (I MSGI-SPAR-AREA)                                            
025800*********************************************************                 
025900  05 WS-MSGI-AREA-2372.                                                   
026000    10 WS-MSGI-IDTRANS-2372      PIC X(4)    VALUE '2372'.                
026100    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
026200    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
026300    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
026400    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
026500    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
026600                                                                          
026700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
026800                                                                          
026900  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
027000  05 WS-IDARTNR-NUM              REDEFINES WS-IDARTNR                     
027100                                 PIC 9(9).                                
027200  05 IDDC-WS                     PIC X(2)    VALUE SPACE.                 
027300  05 IDDC-WS-NUM                 REDEFINES IDDC-WS                        
027400                                 PIC 9(2).                                
027500  05 WS-IDPERSON-BUY             PIC X(3)    VALUE SPACE.                 
027600  05 WS-IDPERSON-BUY-NUM         REDEFINES WS-IDPERSON-BUY                
027700                                 PIC 9(3).                                
027800  05 WS-IDPERSON-BUY-RED         PIC Z(2)9.                               
027900  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
028000  05 WS-STATUS                   PIC X       VALUE SPACE.                 
028100                                                                          
028200 01   WS-IDDC-REF                PIC X(2)    VALUE SPACES.                
028300 01   WS-KDERS-INOM              PIC 9(3)    VALUE ZEROES.                
028400                                                                          
028500 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
028600     88  SW-TRAEFF-JA                        VALUE 'J'.                   
028700     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
028800                                                                          
028900 77  SW-SEASON                   PIC X       VALUE ' '.                   
029000     88  SW-SEASON-JA                        VALUE 'J'.                   
029100     88  SW-SEASON-NEJ                       VALUE 'N'.                   
029200                                                                          
029300 77  SW-KTRL-ERS                 PIC X       VALUE ' '.                   
029400     88  SW-KTRL-ERS-JA                      VALUE 'J'.                   
029500     88  SW-KTRL-ERS-NEJ                     VALUE 'N'.                   
029600                                                                          
029700 77  SW-HAEMTA-INPUT-FAELT       PIC X       VALUE 'J'.                   
029800     88  SW-HAEMTA-INPUT-FAELT-JA            VALUE 'J'.                   
029900     88  SW-HAEMTA-INPUT-FAELT-NEJ           VALUE 'N'.                   
030000                                                                          
030100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
030200     88  INDATA-OK                           VALUE 'J'.                   
030300     88  INDATA-FEL                          VALUE 'N'.                   
030400                                                                          
030500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
030600     88  NYCKLAR-OK                          VALUE 'J'.                   
030700     88  NYCKLAR-FEL                         VALUE 'N'.                   
030800                                                                          
030900 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
031000     88  SIM-INDEX-JA                        VALUE 'J'.                   
031100     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
031200                                                                          
031300 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
031400     88  SIM-ANTAL-JA                        VALUE 'J'.                   
031500     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
031600                                                                          
031700 77  FOERSTA-R32-SW              PIC X       VALUE 'J'.                   
031800     88  FOERSTA-R32-JA                      VALUE 'J'.                   
031900     88  FOERSTA-R32-NEJ                     VALUE 'N'.                   
032000                                                                          
032100 77  NY-WDK7-SW                  PIC X       VALUE 'N'.                   
032200     88  NY-WDK7-JA                          VALUE 'J'.                   
032300     88  NY-WDK7-NEJ                         VALUE 'N'.                   
032400                                                                          
032500 77  WAIT-FOR-RORELSE-SW         PIC X       VALUE 'N'.                   
032600     88  WAIT-FOR-RORELSE                    VALUE 'J'.                   
032700     88  NOT-WAIT-FOR-RORELSE                VALUE 'N'.                   
032800                                                                          
032900 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
033000     88  LYNK-PART                           VALUE 'J'.                   
033100                                                                          
033200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
033300     88  EGEN-MID                            VALUE '2372'.                
033400     88  GODK-MID                            VALUE '2371' '2352'          
033500                                                   '2353' '2354'          
033600                                                   '2355' '2356'          
033700                                                   '2357' '2358'          
033800                                                   '2359' '6322'.         
033900     88  HELP-MID                            VALUE '0551'.                
034000                                                                          
034100*77  TEST-IDDC-REF               PIC X(2)    VALUE SPACE.                 
034200 77  SW-TEST-IDDC-REF            PIC X       VALUE 'N'.                   
034300     88  GODK-IDDC-REF-JA                    VALUE 'J'.                   
034400                                                                          
034500 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
034600     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
034700     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
034800     SKIP3                                                                
034900 77  REFILL-PART-SW              PIC X       VALUE SPACE.                 
035000     88  REFILL-PART                         VALUE 'J'.                   
035100     88  NOT-REFILL-PART                     VALUE 'N'.                   
035200     SKIP3                                                                
035300 77  DISTRICT-FOUND-SW           PIC X       VALUE 'J'.                   
035400     88  DISTRICT-FOUND                      VALUE 'J'.                   
035500     88  DISTRICT-NOT-FOUND                  VALUE 'N'.                   
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16) VALUE 'REFILLFÖRSLAG'.         
035800     SKIP3                                                                
035900*01  -COPY W271RTXT                                                       
036000     EJECT                                                                
036100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
036200 01  GENERELLA-SUBPROGRAM.                                                
036300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
036400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
036500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
036600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
036700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
036800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
036900     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
037000     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
037100     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
037200     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
037300     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
037400     EJECT                                                                
037500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
037600*01 -COPY WMEDAREA                                                        
037700     EJECT                                                                
037800*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
037900*01  -COPY WDECAREA                                                       
038000     EJECT                                                                
038100*    --- PARAMETRAR TILL W271REFL                                         
038200*01 -COPY W271REFL       -PRE W271-                                       
038300     EJECT                                                                
038400*    --- PARAMETRAR TILL W271UTIL                                         
038500*01 -COPY W271UTIL                                                        
038600     EJECT                                                                
038700*    --- PARAMETRAR TILL W271UTUP                                         
038800*01 -COPY W271UTUP       -PRE W271-                                       
038900     EJECT                                                                
039000*    --- PARAMETRAR TILL W272UTUP                                         
039100*01 -COPY W272UTUP       -PRE W272-                                       
039200     EJECT                                                                
039300 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
039400*   -COPY W005WDK7                                                        
039500     EJECT                                                                
039600                                                                          
039700 01  MESSAGE-CODES.                                                       
039800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
039900     03  CONFLICT                PIC X(3)    VALUE '002'.                 
040000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
040100     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
040200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
040300     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
040400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
040500     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
040600     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
040700     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
040800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
040900     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
041000     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
041100     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
041200     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
041300     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
041400     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
041410     03  ERR-HIGH-AIR-COST       PIC X(3)    VALUE '369'.                 
041500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
041600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
041700     03  ERR-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
041800                                                                          
041900 01  MEDDELANDE.                                                          
042000     03  MED-1                  PIC X(30)                                 
042100         VALUE 'TYPE : A,B,C OR L             '.                          
042200     03  MED-2                  PIC X(30)                                 
042300         VALUE 'STATUS : R OR N               '.                          
042400     03  MED-3                  PIC X(30)                                 
042500         VALUE 'FORECAST WRONG                '.                          
042600     03  MED-4                  PIC X(30)                                 
042700         VALUE 'PURCHQTY WRONG                '.                          
042800     03  MED-5                  PIC X(30)                                 
042900         VALUE 'AUT REFILL ORDERING WRONG     '.                          
043000     03  MED-6                  PIC X(30)                                 
043100         VALUE 'CAN NOT UPDATE WITH NEW KEY   '.                          
043200     03  MED-7                  PIC X(30)                                 
043300         VALUE 'OT: A,B,C OR L                '.                          
043400     03  MED-8                  PIC X(30)                                 
043500         VALUE 'CONFLICT OT/VENDOR            '.                          
043600     03  MED-9                  PIC X(30)                                 
043700         VALUE 'NO VALID PRICE                '.                          
043800     03  MED-10                 PIC X(30)                                 
043900         VALUE 'UNEVEN MULTIPEL OF Q1         '.                          
044000     03  MED-11                 PIC X(30)                                 
044100         VALUE 'PART MARKED AS AIRFREIGHT ONLY'.                          
044200     03  MED-12                 PIC X(30)                                 
044300         VALUE 'DC 62 ONLY                    '.                          
044400     03  MED-13                 PIC X(30)                                 
044500         VALUE 'ENTER N, J/Y OR LEAVE BLANK   '.                          
044600     03  MED-14                 PIC X(30)                                 
044700         VALUE 'NOT ALLOWED FOR LOCAL PARTS   '.                          
044800     03  MED-15                 PIC X(30)                                 
044900         VALUE 'DISTRICT NOT FOUND            '.                          
045000     03  MED-16                  PIC X(30)                                
045100         VALUE 'LYNK & CO PART                '.                          
045110     03  MED-17                 PIC X(30)                                 
045120         VALUE 'PART LOCKED FROM AIRFREIGHT   '.                          
045200                                                                          
045300     EJECT                                                                
045400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
045500*01  -COPY WDATAREA                                                       
045600     EJECT                                                                
045700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
045800*                                                                         
045900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
046000     SKIP3                                                                
046100*01 -COPY WMSGINIT                                                        
046200     EJECT                                                                
046300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
046400*                                                                         
046500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
046600     SKIP3                                                                
046700*01  MID -COPY W2I37201                                                   
046800     EJECT                                                                
046900*    --- VID HOPP FRÅN 2371 ANVÄNDS W2I37101                              
047000*    ---                                                                  
047100*01  MID -COPY W2I37101                                                   
047200     EJECT                                                                
047300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
047400     SKIP3                                                                
047500*01  -COPY WMSGAREA                                                       
047600     EJECT                                                                
047700     03  MOD REDEFINES MSG-AREA.                                          
047800*      05  -COPY W2O37201                                                 
047900     EJECT                                                                
048000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
048100     SKIP3                                                                
048200*01  -COPY WMFSAREA                                                       
048300     EJECT                                                                
048400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
048500*                                                                         
048600     EJECT                                                                
048700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
048800     SKIP3                                                                
048900                                                                          
049000 01  NYCKLAR-TILL-DLI.                                                    
049100     03  W-IDARTNR-X.                                                     
049200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
049300     03  W-WDGXKEY-2261-X.                                                
049400          05 W-IDHTYP            PIC X(4)    VALUE '2261'.                
049500          05 W-IDDC-2261         PIC X(2)    VALUE SPACE.                 
049600          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
049700                                                                          
049800     03  W-IDDC-X.                                                        
049900         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
050000     03  W-RECEIVING-IDDC        PIC X(02)   VALUE SPACE.                 
050100     03  W-IDLAND-X.                                                      
050200         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
050300     03  W-IDDC-B6-X.                                                     
050400         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
050500     03  W-IDDC-B616-X.                                                   
050600         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
050700     03  W-IDUSER-X.                                                      
050800         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
050900     03  W-IDSKYLT-X.                                                     
051000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
051100     03  W-IDLEVNR-21-X.                                                  
051200         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
051300     03  W-DAPRLIST-21-N.                                                 
051400         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
051500     03  W-IDLEVNR-K7-X.                                                  
051600         05  W-IDLEVNR-K7        PIC X(5)    VALUE LOW-VALUE.             
051700     03  W-DAPRLIST-K7-N.                                                 
051800         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
051900                                                                          
052000     03  W-KDNOTTYP-X.                                                    
052100         05  W-KDNOTTYP          PIC  S9(01) COMP-3                       
052200                                             VALUE ZERO.                  
052300                                                                          
052400     03 W-WDE301KY-X.                                                     
052500         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
052600         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
052700         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
052800         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
052900         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
053000                                                                          
053100     03 W-WDE301KY-MIN-X.                                                 
053200         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
053300         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
053400         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
053500         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
053600         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
053700                                                                          
053800     03 W-WDE301KY-MAX-X.                                                 
053900         05  W-IDDC-MAX          PIC X(2)  VALUE HIGH-VALUE.              
054000         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE +999 COMP-3.             
054100         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
054200         05  W-IDARTNR-MAX       PIC S9(9)                                
054300                                         VALUE +999999999 COMP-3.         
054400         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
054500                                                                          
054600     03  W-WDA5A1KY-MIN.                                                  
054700         05  W-IDARTNR-N3-MIN     PIC S9(9)  VALUE ZERO COMP-3.           
054800         05  FILLER               PIC X(35)  VALUE LOW-VALUE.             
054900                                                                          
055000     03  W-WDA5A1KY-MAX.                                                  
055100         05  W-IDARTNR-N3-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
055200         05  FILLER               PIC X(35)  VALUE HIGH-VALUE.            
055300                                                                          
055400     03  W-WDA501KY.                                                      
055500         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
055600         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
055700         05  W-IDKUNDRF-N2.                                               
055800             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
055900             07  FILLER           PIC X(5)           VALUE SPACE.         
056000         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
056100         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
056200                                                                          
056300   03  W-WDN611KY-X.                                                      
056400     05  W-IDFORDON              PIC S9(2)   VALUE ZERO  COMP-3.          
056500     05  W-TIOMBRYT-1            PIC S9(7)   VALUE ZERO  COMP-3.          
056600     SKIP2                                                                
056700                                                                          
056800   03  W-WDD7A1KY-MIN.                                                    
056900     05  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
057000     05  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
057100     05  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
057200                                                                          
057300   03  W-WDD7A1KY-MAX.                                                    
057400     05  W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.                  
057500     05  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.            
057600     05  FILLER            PIC S9(3)  COMP-3 VALUE +999.                  
057700                                                                          
057800   03  W-IDARTNR-TILLK-X.                                                 
057900     05  W-IDARTNR-TILLK         PIC S9(9)   COMP-3.                      
058000   03  W-IDARTNR-ERS-LOW-X.                                               
058100     05  W-IDARTNR-ERS-LOW       PIC S9(9)   COMP-3  VALUE ZERO.          
058200   03  W-IDARTNR-ERS-HIGH-X.                                              
058300     05  W-IDARTNR-ERS-HIGH      PIC S9(9)   COMP-3                       
058400                                  VALUE +999999999.                       
058500                                                                          
058600   03  W-W6D1HSEQ-X.                                                      
058700     05  W-IDARTNR-HSEQ  PIC S9(9)   VALUE ZERO  COMP-3.                  
058800                                                                          
058900   03  W-KDSEGKEY-X.                                                      
059000         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
059100                                                                          
059200   03  W-IDLEVNR-X.                                                       
059300         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
059400                                                                          
059500     SKIP2                                                                
059600*    --- STATUS-KOD FRÅN IMS                                              
059700 01  STATUS-WS                   PIC XX.                                  
059800     88  SEGMENT-FINNS                       VALUE '  '.                  
059900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
060000     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
060100                                                   'GB'.                  
060200     SKIP2                                                                
060300 01  GODK-STATUSKODER.                                                    
060400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
060500     SKIP3                                                                
060600 01  SSA1                        PIC X(128).                              
060700 01  SSA2                        PIC X(128).                              
060800 01  SSA3                        PIC X(128).                              
060900     EJECT                                                                
061000*01  -COPY WWDCKONS                                                       
061100     EJECT                                                                
061200*    --- IMS FUNKTIONSKODER                                               
061300*01  -COPY W0003                                                          
061400     EJECT                                                                
061500*    ---  DLI INPUT-OUTPUT AREA                                           
061600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
061700     SKIP3                                                                
061800 01  DLI-IO-AREA-WDK711.                                                  
061900*        05  -COPY WDK711                                                 
062000     EJECT                                                                
062100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
062200     SKIP3                                                                
062300 01  DLI-IO-AREA-WDK712.                                                  
062400*        05  -COPY WDK712                                                 
062500     EJECT                                                                
062600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK727'.             
062700     SKIP3                                                                
062800 01  DLI-IO-AREA-WDK727.                                                  
062900*        05  -COPY WDK727                                                 
063000     EJECT                                                                
063100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
063200     SKIP3                                                                
063300 01  DLI-IO-AREA-WDL711.                                                  
063400*    03  -COPY WDL711                                                     
063500     EJECT                                                                
063600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL411'.             
063700     SKIP3                                                                
063800 01  DLI-IO-AREA-WDL411.                                                  
063900*    03  -COPY WDL411                                                     
064000     EJECT                                                                
064100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX2262'.                    
064200 01  DLI-IO-WDGX2262.                                                     
064300*    03  -COPY WDGX2262                                                   
064400     EJECT                                                                
064500                                                                          
064600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
064700     SKIP3                                                                
064800 01  DLI-IO-AREA-WDD301.                                                  
064900*        05  -COPY WDD301  -PRE WDD301-                                   
065000     EJECT                                                                
065100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
065200     SKIP3                                                                
065300 01  DLI-IO-AREA-WDD311.                                                  
065400*        05  -COPY WDD311  -PRE WDD311-                                   
065500     EJECT                                                                
065600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE301'.             
065700     SKIP3                                                                
065800 01  DLI-IO-AREA-WDE301.                                                  
065900*        05  -COPY WDE301                                                 
066000     EJECT                                                                
066100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDA501'.             
066200 01  DLI-IO-AREA-WDA501.                                                  
066300*  03    WDA501 -COPY WDA501                                              
066400     EJECT                                                                
066500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ORDQ01'.             
066600 01  DLI-IO-AREA-ORDQ01.                                                  
066700*  03    WDA5A1 -COPY WDA5A1                                              
066800     EJECT                                                                
066900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
067000     SKIP3                                                                
067100 01  DLI-IO-AREA-WDK601.                                                  
067200*        05  -COPY WDK601                                                 
067300     EJECT                                                                
067400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
067500     SKIP3                                                                
067600 01  DLI-IO-AREA-WDK611.                                                  
067700*        05  -COPY WDK611                                                 
067800     EJECT                                                                
067900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK621'.             
068000     SKIP3                                                                
068100 01  DLI-IO-AREA-WDK621.                                                  
068200*        05  -COPY WDK621                                                 
068300     EJECT                                                                
068400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK625'.             
068500     SKIP3                                                                
068600 01  DLI-IO-AREA-WDK625.                                                  
068700*        05  -COPY WDK625                                                 
068800     EJECT                                                                
068900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDN601'.            
069000     SKIP3                                                                
069100 01  DLI-IO-AREA-WDN601.                                                  
069200*  03    WLWDN601 -COPY WDN601 -PRE WDN6-                                 
069300     EJECT                                                                
069400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDN611'.            
069500     SKIP3                                                                
069600 01  DLI-IO-AREA-WDN611.                                                  
069700*  03    WLWDN611 -COPY WDN611 -PRE WDN6-                                 
069800     EJECT                                                                
069900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD701'.            
070000     SKIP3                                                                
070100 01  DLI-IO-AREA-WDD701.                                                  
070200*  05  -COPY WDD701  -PRE WDD701-                                         
070300     EJECT                                                                
070400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD702'.            
070500     SKIP3                                                                
070600 01  DLI-IO-AREA-WDD702.                                                  
070700*  05  -COPY WDD702  -PRE WDD702-                                         
070800     EJECT                                                                
070900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSB01'.            
071000     SKIP3                                                                
071100 01  DLI-IO-AREA-ERSB01.                                                  
071200*  03  WLERSB01 -COPY WDD7A1  -PRE ERSB01-                                
071300     EJECT                                                                
071400                                                                          
071500 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-INLA11'.            
071600     SKIP3                                                                
071700 01  DLI-IO-AREA-INLA11.                                                  
071800*  03  W6INLA11 -COPY W6D111 -PRE INLA-                                   
071900     EJECT                                                                
072000                                                                          
072100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL601'.            
072200 01  DLI-IO-AREA-WDL601.                                                  
072300*  05   -COPY WDL601 -PRE WDL6-                                           
072400     EJECT                                                                
072500                                                                          
072600 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL611'.            
072700 01  DLI-IO-AREA-WDL611.                                                  
072800*  05  -COPY WDL611 -PRE WDL6-                                            
072900     EJECT                                                                
073000                                                                          
073100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL612'.            
073200 01  DLI-IO-AREA-WDL612.                                                  
073300*  05  -COPY WDL612 -PRE WDL6-                                            
073400     EJECT                                                                
073500                                                                          
073600 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDK901'.            
073700     SKIP3                                                                
073800 01  DLI-IO-AREA-WDK901.                                                  
073900*  05  -COPY WDK901  -PRE WDK9-                                           
074000     EJECT                                                                
074100 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA01'.                      
074200 01  DLI-IO-LEVA01.                                                       
074300*    03  -COPY WDF101                                                     
074400     EJECT                                                                
074500                                                                          
074600 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA16'.                      
074700 01  DLI-IO-LEVA16.                                                       
074800*    03  -COPY WDF116                                                     
074900     EJECT                                                                
075000                                                                          
075100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
075200 01   DLI-IO-AREA-B601.                                                   
075300*     03  -COPY WDB601                                                    
075400 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
075500 01   DLI-IO-AREA-B616.                                                   
075600*     03  -COPY WDB616 -PRE B6-                                           
075700                                                                          
075800 01  FILLER               PIC X(16)   VALUE 'WDB601 NEXT'.                
075900 01   DLI-IO-AREA-B601-NEXT.                                              
076000*     03  -COPY WDB601 -PRE NEXT-                                         
076100     EJECT                                                                
076200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
076300 01  DLI-IO-WDK711.                                                       
076400*        05  -COPY WDK711 -PRE K7-                                        
076500     EJECT                                                                
076600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK722'.             
076700 01  DLI-IO-WDK722.                                                       
076800*        05  -COPY WDK722                                                 
076900     EJECT                                                                
077000                                                                          
077100 LINKAGE SECTION.                                                         
077200                                                                          
077300*01  -COPY W0009   -PRE MSG-                                              
077400     EJECT                                                                
077500*01  -COPY W0008  -PRE  USEA-                                             
077600     05  FILLER                          PIC X.                           
077700     EJECT                                                                
077800*01  -COPY W0008  -PRE  WDK7-                                             
077900     05  FILLER                          PIC X.                           
078000     EJECT                                                                
078100*01  -COPY W0008  -PRE  WDK7-2-                                           
078200     05  FILLER                          PIC X.                           
078300     EJECT                                                                
078400*01  -COPY W0008  -PRE  WDK7-3-                                           
078500     05  FILLER                          PIC X.                           
078600     EJECT                                                                
078700*01  -COPY W0008  -PRE  WDL7-                                             
078800     05  FILLER                          PIC X.                           
078900     EJECT                                                                
079000*01  -COPY W0008  -PRE  WDL4-                                             
079100     05  FILLER                          PIC X.                           
079200     EJECT                                                                
079300*01  -COPY W0008  -PRE  WDD3-                                             
079400     05  FILLER                          PIC X.                           
079500     EJECT                                                                
079600*01  -COPY W0008  -PRE WDE3-                                              
079700     05  FILLER                          PIC X.                           
079800     EJECT                                                                
079900*01  -COPY W0008  -PRE WDA5-                                              
080000     05  FILLER                          PIC X.                           
080100     EJECT                                                                
080200*01  -COPY W0008  -PRE ORDQ-                                              
080300     05  FILLER                          PIC X.                           
080400     EJECT                                                                
080500*01  -COPY W0008  -PRE WDK6-                                              
080600     05  FILLER                          PIC X.                           
080700     EJECT                                                                
080800*01  -COPY W0008  -PRE WDN6-                                              
080900     05  FILLER                          PIC X.                           
081000     EJECT                                                                
081100*01  -COPY W0008  -PRE WDD7-                                              
081200     05  FILLER                          PIC X.                           
081300     EJECT                                                                
081400*01  -COPY W0008  -PRE ERSB-                                              
081500     05  FILLER                          PIC X.                           
081600     EJECT                                                                
081700*01  -COPY W0008  -PRE INLA-                                              
081800     05  FILLER                          PIC X.                           
081900     EJECT                                                                
082000*01  -COPY W0008  -PRE WDK9-                                              
082100     05  FILLER                          PIC X.                           
082200     EJECT                                                                
082300*01  -COPY W0008  -PRE  LEVA-                                             
082400     05  FILLER                          PIC X.                           
082500     EJECT                                                                
082600*01  -COPY W0008  -PRE WDL6-                                              
082700     05  FILLER                          PIC X.                           
082800     EJECT                                                                
082900*01  -COPY W0008  -PRE WDB6-                                              
083000     05  FILLER                          PIC X.                           
083100     EJECT                                                                
083200*01  -COPY W0008  -PRE WDB6-NEXT-                                         
083300     05  FILLER                          PIC X.                           
083400     EJECT                                                                
083500*01  -COPY W0008  -PRE 2261-                                              
083600     05  FILLER                          PIC X.                           
083700     EJECT                                                                
083800*01  -COPY W0008  -PRE UTIL-WDK6-                                         
083900     05  FILLER                          PIC X.                           
084000     EJECT                                                                
084100*01  -COPY W0008  -PRE UTIL-WDK7-                                         
084200     05  FILLER                          PIC X.                           
084300     EJECT                                                                
084400*01  -COPY W0008  -PRE UTIL-WDB6-                                         
084500     05  FILLER                          PIC X.                           
084600*****W271REFL**********                                                   
084700 01  REFL1-2501-PCB                      PIC X.                           
084800 01  REFL1-WDB6-PCB                      PIC X.                           
084900 01  REFL1-WDK7-PCB                      PIC X.                           
085000 01  REFL1-UTIL-WDK6-PCB                 PIC X.                           
085100 01  REFL1-UTIL-WDK7-PCB                 PIC X.                           
085200 01  REFL1-UTIL-WDB6-PCB                 PIC X.                           
085300*****W271UTUP**********                                                   
085400 01  UTUP1-WDK7-PCB                      PIC X.                           
085500 01  UTUP1-WDB6-PCB                      PIC X.                           
085600 01  UTUP1-UTIL-WDK6-PCB                 PIC X.                           
085700 01  UTUP1-UTIL-WDK7-PCB                 PIC X.                           
085800 01  UTUP1-UTIL-WDB6-PCB                 PIC X.                           
085900*****W272UTUP**********                                                   
086000 01  U2-WDK6-PCB                         PIC X.                           
086100 01  U2-WDB6-PCB                         PIC X.                           
086200 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
086300 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
086400 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
086500 01  U2-PBTO-W222-REFL1-2501-PCB         PIC X.                           
086600 01  U2-PBTO-W222-REFL1-WDB6R-PCB        PIC X.                           
086700 01  U2-PBTO-W222-REFL1-WDK7R-PCB        PIC X.                           
086800 01  U2-PBTO-W222-REFL1-UTIL-K6-PCB      PIC X.                           
086900 01  U2-PBTO-W222-REFL1-UTIL-K7-PCB      PIC X.                           
087000 01  U2-PBTO-W222-REFL1-UTIL-B6-PCB      PIC X.                           
087100 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
087200 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
087300 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
087400 01  U2-PBTO-W222-UTUP1-WDK7-PCB         PIC X.                           
087500 01  U2-PBTO-W222-UTUP1-WDB6-PCB         PIC X.                           
087600 01  U2-PBTO-W222-UTUP1-UTIL-K6-PCB      PIC X.                           
087700 01  U2-PBTO-W222-UTUP1-UTIL-K7-PCB      PIC X.                           
087800 01  U2-PBTO-W222-UTUP1-UTIL-B6-PCB      PIC X.                           
087900 01  U2-REFL2-2501-PCB                   PIC X.                           
088000 01  U2-REFL2-WDB6-PCB                   PIC X.                           
088100 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
088200 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
088300 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
088400 01  U2-W222-WDK6-PCB                    PIC X.                           
088500 01  U2-W222-WDK7-PCB                    PIC X.                           
088600 01  U2-W222-ARTM-PCB                    PIC X.                           
088700 01  U2-W222-2501-PCB                    PIC X.                           
088800 01  U2-W222-WDB6R-PCB                   PIC X.                           
088900 01  U2-W222-WDK7R-PCB                   PIC X.                           
089000 01  U2-W222-WDB6-PCB                    PIC X.                           
089100 01  U2-W222-WDD7-PCB                    PIC X.                           
089200 01  U2-W222-WDK7E-PCB                   PIC X.                           
089300 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
089400 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
089500 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
089600 01  U2-W222-UTUP1-WDK7-PCB              PIC X.                           
089700 01  U2-W222-UTUP1-WDB6-PCB              PIC X.                           
089800 01  U2-W222-UTUP1-UTIL-WDK6-PCB         PIC X.                           
089900 01  U2-W222-UTUP1-UTIL-WDK7-PCB         PIC X.                           
090000 01  U2-W222-UTUP1-UTIL-WDB6-PCB         PIC X.                           
090100     EJECT                                                                
090200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK7-PCB WDK7-2-PCB           
090300     WDK7-3-PCB                                                           
090400     WDL7-PCB WDL4-PCB WDD3-PCB WDE3-PCB WDA5-PCB ORDQ-PCB                
090500     WDK6-PCB WDN6-PCB WDD7-PCB ERSB-PCB INLA-PCB WDK9-PCB                
090600     LEVA-PCB WDL6-PCB                                                    
090700     WDB6-PCB WDB6-NEXT-PCB 2261-PCB                                      
090800     UTIL-WDK6-PCB UTIL-WDK7-PCB UTIL-WDB6-PCB                            
090900     REFL1-2501-PCB                                                       
091000     REFL1-WDB6-PCB                                                       
091100     REFL1-WDK7-PCB                                                       
091200     REFL1-UTIL-WDK6-PCB                                                  
091300     REFL1-UTIL-WDK7-PCB                                                  
091400     REFL1-UTIL-WDB6-PCB                                                  
091500     UTUP1-WDK7-PCB                                                       
091600     UTUP1-WDB6-PCB                                                       
091700     UTUP1-UTIL-WDK6-PCB                                                  
091800     UTUP1-UTIL-WDK7-PCB                                                  
091900     UTUP1-UTIL-WDB6-PCB                                                  
092000     U2-WDK6-PCB                                                          
092100     U2-WDB6-PCB                                                          
092200     U2-PBTO-W222-WDK6-PCB                                                
092300     U2-PBTO-W222-WDK7-PCB                                                
092400     U2-PBTO-W222-ARTM-PCB                                                
092500     U2-PBTO-W222-REFL1-2501-PCB                                          
092600     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
092700     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
092800     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
092900     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
093000     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
093100     U2-PBTO-W222-WDB6-PCB                                                
093200     U2-PBTO-W222-WDD7-PCB                                                
093300     U2-PBTO-W222-WDK7E-PCB                                               
093400     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
093500     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
093600     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
093700     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
093800     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
093900     U2-REFL2-2501-PCB                                                    
094000     U2-REFL2-WDB6-PCB                                                    
094100     U2-REFL2-UTIL-WDK6-PCB                                               
094200     U2-REFL2-UTIL-WDK7-PCB                                               
094300     U2-REFL2-UTIL-WDB6-PCB                                               
094400     U2-W222-WDK6-PCB                                                     
094500     U2-W222-WDK7-PCB                                                     
094600     U2-W222-ARTM-PCB                                                     
094700     U2-W222-2501-PCB                                                     
094800     U2-W222-WDB6R-PCB                                                    
094900     U2-W222-WDK7R-PCB                                                    
095000     U2-W222-WDB6-PCB                                                     
095100     U2-W222-WDD7-PCB                                                     
095200     U2-W222-WDK7E-PCB                                                    
095300     U2-W222-UTIL-WDK6-PCB                                                
095400     U2-W222-UTIL-WDK7-PCB                                                
095500     U2-W222-UTIL-WDB6-PCB                                                
095600     U2-W222-UTUP1-WDK7-PCB                                               
095700     U2-W222-UTUP1-WDB6-PCB                                               
095800     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
095900     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
096000     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
096100 MAIN SECTION.                                                            
096200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK7-PCB WDK7-2-PCB           
096300     WDK7-3-PCB                                                           
096400     WDL7-PCB WDL4-PCB WDD3-PCB WDE3-PCB WDA5-PCB ORDQ-PCB                
096500     WDK6-PCB WDN6-PCB WDD7-PCB ERSB-PCB INLA-PCB WDK9-PCB                
096600     LEVA-PCB WDL6-PCB                                                    
096700     WDB6-PCB WDB6-NEXT-PCB 2261-PCB                                      
096800     UTIL-WDK6-PCB UTIL-WDK7-PCB UTIL-WDB6-PCB                            
096900     REFL1-2501-PCB                                                       
097000     REFL1-WDB6-PCB                                                       
097100     REFL1-WDK7-PCB                                                       
097200     REFL1-UTIL-WDK6-PCB                                                  
097300     REFL1-UTIL-WDK7-PCB                                                  
097400     REFL1-UTIL-WDB6-PCB                                                  
097500     UTUP1-WDK7-PCB                                                       
097600     UTUP1-WDB6-PCB                                                       
097700     UTUP1-UTIL-WDK6-PCB                                                  
097800     UTUP1-UTIL-WDK7-PCB                                                  
097900     UTUP1-UTIL-WDB6-PCB                                                  
098000     U2-WDK6-PCB                                                          
098100     U2-WDB6-PCB                                                          
098200     U2-PBTO-W222-WDK6-PCB                                                
098300     U2-PBTO-W222-WDK7-PCB                                                
098400     U2-PBTO-W222-ARTM-PCB                                                
098500     U2-PBTO-W222-REFL1-2501-PCB                                          
098600     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
098700     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
098800     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
098900     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
099000     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
099100     U2-PBTO-W222-WDB6-PCB                                                
099200     U2-PBTO-W222-WDD7-PCB                                                
099300     U2-PBTO-W222-WDK7E-PCB                                               
099400     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
099500     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
099600     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
099700     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
099800     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
099900     U2-REFL2-2501-PCB                                                    
100000     U2-REFL2-WDB6-PCB                                                    
100100     U2-REFL2-UTIL-WDK6-PCB                                               
100200     U2-REFL2-UTIL-WDK7-PCB                                               
100300     U2-REFL2-UTIL-WDB6-PCB                                               
100400     U2-W222-WDK6-PCB                                                     
100500     U2-W222-WDK7-PCB                                                     
100600     U2-W222-ARTM-PCB                                                     
100700     U2-W222-2501-PCB                                                     
100800     U2-W222-WDB6R-PCB                                                    
100900     U2-W222-WDK7R-PCB                                                    
101000     U2-W222-WDB6-PCB                                                     
101100     U2-W222-WDD7-PCB                                                     
101200     U2-W222-WDK7E-PCB                                                    
101300     U2-W222-UTIL-WDK6-PCB                                                
101400     U2-W222-UTIL-WDK7-PCB                                                
101500     U2-W222-UTIL-WDB6-PCB                                                
101600     U2-W222-UTUP1-WDK7-PCB                                               
101700     U2-W222-UTUP1-WDB6-PCB                                               
101800     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
101900     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
102000     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
102100                                                                          
102200     PERFORM IMS-GET-MSG                                                  
102300     IF SEGMENT-FINNS                                                     
102400       PERFORM A-INIT                                                     
102500       PERFORM B-KOLLA-NYCKLAR                                            
102600       IF NYCKLAR-OK                                                      
102700         IF MFS-UPDATE OR                                                 
102710            MFS-UPD-V                                                     
102800           PERFORM I-KOLLA-INPUT                                          
102900           PERFORM L-KOLLA-INPUT-NYCKLAR                                  
103000           IF INDATA-OK                                                   
103100             PERFORM H-UPD-WDK6-WDK7                                      
103200             IF WS-IDREFTYP = 'A'                                         
103300             OR WS-IDREFTYP = 'B'                                         
103400             OR WS-IDREFTYP = 'C'                                         
103500             OR WS-IDREFTYP = 'L'                                         
103600               PERFORM N-UPD-WDE3-WDK7                                    
103700             END-IF                                                       
103800                                                                          
103900             MOVE INF-UPDATE-DONE                                         
104000                             TO MED-IDMFSINF                              
104100             CALL WMEDKONV USING MED-WMEDAREA                             
104200             MOVE MED-TEMFSINF                                            
104300                             TO MOD-TEMFSINF                              
104400           ELSE                                                           
104500*   MOD-FÄLT SOM ÄVEN ÄR INFÄLT SKA INTE SKRIVAS ÖVER                     
104600*   I F-HAEMTA-INFO                                                       
104700             MOVE NEJ        TO SW-HAEMTA-INPUT-FAELT                     
104800             MOVE 'PROPOSAL NOT REVIEWED'                                 
104900                             TO MOD-ORDERSTATUS                           
105000             MOVE INF-UPDATE-NOT-DONE                                     
105100                             TO MED-IDMFSINF                              
105200             CALL WMEDKONV USING MED-WMEDAREA                             
105300             MOVE MED-TEMFSINF                                            
105400                             TO MOD-TEMFSINF                              
105500           END-IF                                                         
105600         ELSE                                                             
105700                                                                          
105800*   INITIERA MOD-PURCHQTY                                                 
105900           MOVE ZERO         TO WS-RED-PURCHQTY                           
106000           MOVE WS-RED-PURCHQTY                                           
106100                             TO MOD-PURCHQTY                              
106200                                                                          
106300           IF MFS-FIRST                                                   
106400             PERFORM C-FOEREG-SIDA                                        
106500           ELSE                                                           
106600             IF MFS-NEXT                                                  
106700               PERFORM D-NAESTA-SIDA                                      
106800             ELSE                                                         
106900               PERFORM E-SAMMA-SIDA                                       
107000             END-IF                                                       
107100           END-IF                                                         
107200         END-IF                                                           
107300                                                                          
107400*   UPPDATERA BILDEN                                                      
107500         IF W-IDARTNR > ZERO                                              
107600           PERFORM S1-SECURITY-CHECK-PARTNO                               
107700           PERFORM F-HAEMTA-INFO                                          
107800         ELSE                                                             
107900           MOVE MFS-RENSA-FAELT                                           
108000                             TO MOD-IDARTNR-UT                            
108100                                MOD-IDREFTYP-UT                           
108200                                MOD-ORDERSTATUS                           
108300         END-IF                                                           
108400         PERFORM J-FYLL-I-ANT-REVIEW                                      
108500                                                                          
108600         MOVE W-IDARTNR         TO WS-MSGI-IDARTNR-ENTER                  
108700         IF MOD-ORDERSTATUS = 'REVIEWED'                                  
108800            MOVE 'R'         TO WS-MSGI-ORDER-ENTER                       
108900         ELSE                                                             
109000            MOVE 'N'         TO WS-MSGI-ORDER-ENTER                       
109100         END-IF                                                           
109200         IF WS-MSGI-IDARTNR-ENTER = WS-MSGI-IDARTNR-PF7                   
109300           MOVE ZERO            TO WS-MSGI-IDARTNR-PF7                    
109400           MOVE SPACE           TO WS-MSGI-ORDER-PF7                      
109500         END-IF                                                           
109600* ---    UPPDATERA MSGI-SPAR-AREA                                         
109700         MOVE '002'             TO MSGI-KDCALL                            
109800         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
109900         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
110000         MOVE '2372'            TO MSGI-IDTRANS                           
110100         MOVE WS-MSGI-AREA-2372 TO MSGI-SPAR-AREA                         
110200         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
110300                                                                          
110400* ---    UPPDATERA MSGI-IDARTNR                                           
110500         MOVE ALL '+'        TO MSGI-WMSGINIT                             
110600         MOVE '001'          TO MSGI-KDCALL                               
110700         MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                         
110800         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
110900         MOVE '2372'         TO MSGI-IDTRANS                              
111000         MOVE W-IDARTNR      TO WS-IDARTNR-NUM                            
111100         MOVE WS-IDARTNR     TO MSGI-IDARTNR                              
111200         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
111300                                                                          
111400       END-IF                                                             
111500                                                                          
111600       IF BLOCKED-SECURITY-CHECK                                          
111700*        --- DETTA VÄRDE SÄTTS I SEKTION S1- DÄR KONTROLL GÖRS PÅ         
111800*            IFALL USER FÅR SE INFO OM VISS ARTIKEL                       
111900         PERFORM MFS-RENSA-FAELT-IN                                       
112000         PERFORM MFS-RENSA-FAELT-UT                                       
112100         MOVE MFS-RENSA-FAELT  TO MOD-PURCHQTY                            
112200                                  MOD-KVPB-REF                            
112300                                  MOD-KVPBREOI                            
112400                                  MOD-FLREFBEO                            
112500                                  MOD-FLAGGA-FCD                          
112600                                  MOD-COMMENT(1)                          
112700                                  MOD-COMMENT(2)                          
112800         MOVE MFS-STAENG-FAELT TO MOD-PURCHQTY-ATTR                       
112900                                  MOD-KVPB-REF-ATTR                       
113000                                  MOD-FLREFBEO-ATTR                       
113100                                  MOD-KVPBREOI-ATTR                       
113200                                  MOD-FLAGGA-FCD-ATTR                     
113300                                  MOD-COMMENT-ATTR(1)                     
113400                                  MOD-COMMENT-ATTR(2)                     
113500       END-IF                                                             
113600                                                                          
113700       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O37201 + 4                      
113800       PERFORM IMS-INSERT-MSG                                             
113900     END-IF                                                               
114000                                                                          
114100     MOVE ZERO TO RETURN-CODE                                             
114200     GOBACK                                                               
114300     .                                                                    
114400     EJECT                                                                
114500                                                                          
114600 A-INIT SECTION.                                                          
114700                                                                          
114800     IF MSG-DUBBLA-TRANSKODER                                             
114900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
115000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
115100       IF MFS-IDTRANS = '2371'                                            
115200         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
115300                             TO MID-W2I37101                              
115400       ELSE                                                               
115500         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
115600                             TO MID-W2I37201                              
115700       END-IF                                                             
115800     ELSE                                                                 
115900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
116000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
116100       IF MFS-IDTRANS = '2371'                                            
116200         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
116300                             TO MID-W2I37101                              
116400       ELSE                                                               
116500         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
116600                             TO MID-W2I37201                              
116700       END-IF                                                             
116800     END-IF                                                               
116900                                                                          
117000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
117100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
117200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
117300                                                                          
117400     MOVE LOW-VALUE TO MSG-AREA                                           
117500     MOVE 'W2O372N1' TO MFS-IDMOD                                         
117600     MOVE '2372' TO MOD-IDTRANS                                           
117700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
117800                                                                          
117900     IF EGEN-MID OR HELP-MID                                              
118000       CONTINUE                                                           
118100     ELSE                                                                 
118200       MOVE SPACE TO MFS-KDTRTYP                                          
118300     END-IF                                                               
118400                                                                          
118500     MOVE +2      TO SPRAK-IX                                             
118600     MOVE 'GB ' TO MED-IDSKYLT                                            
118700                                                                          
118800     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
118900                                                                          
119000     ACCEPT DAGENS-DATUM FROM DATE                                        
119100                                                                          
119200     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
119300     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
119400                                                                          
119500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
119600                     DAT-O-TIDATUM DAT-KDSVAR                             
119700                                                                          
119800     IF DAT-KDSVAR-OK                                                     
119900****             HÄMTA SEKELSIFFROR                                       
120000                                                                          
120100       MOVE DAT-TISEKEL       TO DAGENS-AAR(1:2)                          
120200       MOVE DAT-TIAARP        TO DAGENS-PER                               
120300       MOVE DAT-TIVV          TO DAGENS-VECKA                             
120400                                                                          
120500     ELSE                                                                 
120600         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
120700         DELIMITED BY SIZE INTO FELTEXT                                   
120800         CALL FELLOG                                                      
120900     END-IF                                                               
121000                                                                          
121100     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
121200                                                                          
121300     MOVE 'AARP  '           TO DAT-KDDATFORM                             
121400     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
121500                                                                          
121600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
121700                     DAT-O-TIDATUM DAT-KDSVAR                             
121800                                                                          
121900     IF DAT-KDSVAR-OK                                                     
122000****             RÄKNA UT VECKA I AKTUELL PERIOD                          
122100                                                                          
122200       COMPUTE WS-ANTAL-VECKOR = DAGENS-VECKA - DAT-TIVV + 1              
122300                                                                          
122400     ELSE                                                                 
122500         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
122600         DELIMITED BY SIZE INTO FELTEXT                                   
122700         CALL FELLOG                                                      
122800     END-IF                                                               
122900                                                                          
123000                                                                          
123100*****   INPUT/OUTPUT FÄLTEN INITERAS                                      
123200     PERFORM MFS-LAES-IN-IGEN                                             
123300*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
123400     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
123500     .                                                                    
123600     EJECT                                                                
123700                                                                          
123800 B-KOLLA-NYCKLAR SECTION.                                                 
123900                                                                          
124000     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
124100                                MOD-IDDC-IN                               
124200                                MOD-IDTYPE-IN                             
124300                                MOD-IDPERSON-BUY-IN                       
124400                                MOD-IDSTATUS-IN                           
124500                                MOD-IDREFTYP-IN                           
124600     MOVE ALL '+'            TO MSGI-WMSGINIT                             
124700     MOVE '001'              TO MSGI-KDCALL                               
124800     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
124900     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
125000     MOVE '2372'             TO MSGI-IDTRANS                              
125100                                                                          
125200     IF EGEN-MID                                                          
125300     OR (MID-IDARTNR-IN NUMERIC                                           
125400     AND MID-IDARTNR-IN > ZERO)                                           
125500        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
125600        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
125700     END-IF                                                               
125800                                                                          
125900     MOVE SPACE              TO MSGI-SPAR-AREA                            
126000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
126100     MOVE MSGI-IDARTNR       TO WS-IDARTNR                                
126200                                                                          
126300     MOVE MSGI-IDDC-KEY      TO IDDC-WS                                   
126400                                W-IDDC-301                                
126500                                W-IDDC-MIN                                
126600                                W-IDDC-MAX                                
126700                                W-IDDC-B6                                 
126800                                WS-IDDC                                   
126900     PERFORM IMS-GU-WDB601                                                
127000     MOVE DCS-IDLANDX2       TO W-IDLAND                                  
127100                                WS-IDLAND-HEAD                            
127200                                                                          
127300     IF WS-IDARTNR NUMERIC                                                
127400         MOVE WS-IDARTNR     TO W-IDARTNR                                 
127500     END-IF                                                               
127600                                                                          
127700     MOVE MSGI-SPAR-AREA(1:55)  TO WS-A2                                  
127800     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
127900     INSPECT IDDC-WS REPLACING LEADING SPACE BY ZERO                      
128000                                                                          
128100     IF EGEN-MID                                                          
128200                                                                          
128300       IF MSGI-SPAR-AREA(1:4) = '2372'                                    
128400         MOVE MSGI-SPAR-AREA TO WS-MSGI-AREA-2372                         
128500       END-IF                                                             
128600       MOVE JA TO NYCKLAR-SW                                              
128700                                                                          
128800*      -- KONTROLL AV IDARTNR                                             
128900                                                                          
129000       IF WS-IDARTNR NUMERIC                                              
129100         MOVE WS-IDARTNR-NUM                                              
129200                             TO W-IDARTNR-MIN                             
129300                                W-IDARTNR-301                             
129400                                W-IDARTNR                                 
129500       ELSE                                                               
129600         MOVE NEJ            TO NYCKLAR-SW                                
129700       END-IF                                                             
129800                                                                          
129900       MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                            
130000                                                                          
130100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
130200       INSPECT MOD-IDARTNR-UT REPLACING LEADING '+'  BY SPACE             
130300                                                                          
130400*      -- KONTROLL AV IDDC                                                
130500                                                                          
130600       MOVE IDDC-WS      TO MOD-IDDC-UT                                   
130700                            WS-IDDC                                       
130800                                                                          
130900       IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS           
131000       OR DCS-NDC-SA                                                      
131100          MOVE IDDC-WS   TO W-IDDC                                        
131200       ELSE                                                               
131300          MOVE NEJ           TO NYCKLAR-SW                                
131400       END-IF                                                             
131500                                                                          
131600       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
131700       INSPECT MOD-IDDC-UT REPLACING LEADING '+'  BY SPACE                
131800                                                                          
131900       PERFORM IMS-GU-WDK711                                              
132000       IF SEGMENT-FINNS                                                   
132100          MOVE SLAG-IDDC-REF      TO WS-IDDC-REF                          
132200                                     REFILL-WS-IDDC                       
132300          IF REFILL-CDC-SE OR REFILL-NDC                                  
132400             SET GODK-IDDC-REF-JA TO TRUE                                 
132500          END-IF                                                          
132600          IF SLAG-IDDC-REF = SPACE AND NDC-CN                             
132700             SET NOT-REFILL-PART  TO TRUE                                 
132800             MOVE NEJ             TO NYCKLAR-SW                           
132900          ELSE                                                            
133000             SET REFILL-PART      TO TRUE                                 
133100             IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                    
133200                MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                     
133300             ELSE                                                         
133400                IF SLAG-IDDC-REF > SPACES                                 
133500                  MOVE SLAG-IDDC-REF TO W-IDDC-B616                       
133600                  PERFORM IMS-GU-WDB616                                   
133700                  IF SEGMENT-FINNS                                        
133800                     MOVE B6-REF-IDDISTR-REFILL TO WS-IDDISTR             
133900                  ELSE                                                    
134000                     MOVE NEJ     TO NYCKLAR-SW                           
134100                     SET DISTRICT-NOT-FOUND TO TRUE                       
134200                  END-IF                                                  
134300                END-IF                                                    
134400             END-IF                                                       
134500          END-IF                                                          
134600       END-IF                                                             
134700                                                                          
134800*      -- KONTROLL AV TYP                                                 
134900*      -- TYP ANVÄNDS FÖR SÖKNING AV REFILLORDERFÖRSLAG                   
135000                                                                          
135100       IF MID-IDTYPE-IN = ALL '+'                                         
135200         INSPECT MID-IDTYPE-UT REPLACING LEADING '+' BY SPACE             
135300         IF MID-IDTYPE-UT = 'AIRCR'                                       
135400           MOVE 'C'          TO WS-IDTYPE                                 
135500         ELSE                                                             
135600           MOVE MID-IDTYPE-UT                                             
135700                             TO WS-IDTYPE                                 
135800         END-IF                                                           
135900       ELSE                                                               
136000         MOVE MID-IDTYPE-IN  TO WS-IDTYPE                                 
136100                                MID-IDREFTYP-UT                           
136200       END-IF                                                             
136300                                                                          
136400       IF WS-IDTYPE = 'A'                                                 
136500       OR WS-IDTYPE = 'B'                                                 
136600       OR WS-IDTYPE = 'C'                                                 
136700       OR WS-IDTYPE = 'L'                                                 
136800         IF WS-IDTYPE = 'A'                                               
136900           MOVE 'AIR'        TO MOD-IDTYPE-UT                             
137000         END-IF                                                           
137100         IF WS-IDTYPE = 'C'                                               
137200           MOVE 'AIRCR'      TO MOD-IDTYPE-UT                             
137300         END-IF                                                           
137400         IF WS-IDTYPE = 'B'                                               
137500           MOVE 'BOAT'       TO MOD-IDTYPE-UT                             
137600         END-IF                                                           
137700         IF WS-IDTYPE = 'L'                                               
137800           MOVE 'LOCAL'      TO MOD-IDTYPE-UT                             
137900         END-IF                                                           
138000       ELSE                                                               
138100         IF MID-IDTYPE-IN NOT = ALL '+'                                   
138200           MOVE NEJ          TO NYCKLAR-SW                                
138300           MOVE MED-1        TO MOD-TEMFSINF                              
138400         END-IF                                                           
138500       END-IF                                                             
138600       INSPECT MOD-IDTYPE-UT REPLACING LEADING '+'  BY SPACE              
138700                                                                          
138800*      -- KONTROLL AV IDPERSON-BUY                                        
138900                                                                          
139000       IF MID-IDPERSON-BUY-IN = ALL '+'                                   
139100         INSPECT MID-IDPERSON-BUY-UT                                      
139200                                   REPLACING LEADING '+' BY SPACE         
139300         MOVE MID-IDPERSON-BUY-UT                                         
139400                             TO WS-IDPERSON-BUY                           
139500       ELSE                                                               
139600           MOVE MID-IDPERSON-BUY-IN                                       
139700                             TO WS-IDPERSON-BUY                           
139800       END-IF                                                             
139900       INSPECT WS-IDPERSON-BUY REPLACING LEADING SPACE BY ZERO            
140000       IF WS-IDPERSON-BUY-NUM NUMERIC                                     
140100         MOVE WS-IDPERSON-BUY-NUM                                         
140200                               TO W-IDPERSON-BUY                          
140300                                  WS-IDPERSON-BUY-RED                     
140400                                                                          
140500         MOVE WS-IDPERSON-BUY-RED                                         
140600                               TO MOD-IDPERSON-BUY-UT                     
140700         INSPECT MOD-IDPERSON-BUY-UT REPLACING LEADING '+'                
140800                                     BY SPACE                             
140900       ELSE                                                               
141000         MOVE NEJ TO NYCKLAR-SW                                           
141100       END-IF                                                             
141200                                                                          
141300*      -- KONTROLL AV STATUS                                              
141400                                                                          
141500       IF MID-IDSTATUS-IN = ALL '+'                                       
141600         INSPECT MID-IDSTATUS-UT REPLACING LEADING '+' BY SPACE           
141700         MOVE MID-IDSTATUS-UT  TO WS-STATUS                               
141800       ELSE                                                               
141900         MOVE MID-IDSTATUS-IN  TO WS-STATUS                               
142000       END-IF                                                             
142100                                                                          
142200       IF WS-STATUS = 'R'                                                 
142300       OR WS-STATUS = 'N'                                                 
142400         IF WS-STATUS = 'R'                                               
142500           MOVE 'REVIEWED'   TO MOD-IDSTATUS-UT                           
142600         END-IF                                                           
142700         IF WS-STATUS = 'N'                                               
142800           MOVE 'NOT REVIEWED'                                            
142900                             TO MOD-IDSTATUS-UT                           
143000         END-IF                                                           
143100       ELSE                                                               
143200         IF MID-IDSTATUS-IN NOT = ALL '+'                                 
143300           MOVE NEJ TO NYCKLAR-SW                                         
143400           MOVE MED-2        TO MOD-TEMFSINF                              
143500         END-IF                                                           
143600       END-IF                                                             
143700       INSPECT MOD-IDSTATUS-UT REPLACING LEADING '+'  BY SPACE            
143800                                                                          
143900*      -- KONTROLL AV IDREFTYP                                            
144000*      -- IDREFTYP ANVÄNDS FÖR ATT ANGE VILKEN ORDERTYP                   
144100*      -- SOM SKA SKAPAS                                                  
144200                                                                          
144300       IF MID-IDREFTYP-IN = ALL '+'                                       
144400         IF MID-IDREFTYP-UT (1:1) = 'B'                                   
144500         OR MID-IDREFTYP-UT (1:1) = 'A'                                   
144600         OR MID-IDREFTYP-UT       = 'AIRCR'                               
144700         OR MID-IDREFTYP-UT (1:1) = 'L'                                   
144800           IF MID-IDREFTYP-UT = 'AIRCR'                                   
144900             MOVE 'C'        TO WS-IDREFTYP                               
145000           ELSE                                                           
145100             MOVE MID-IDREFTYP-UT                                         
145200                             TO WS-IDREFTYP                               
145300           END-IF                                                         
145400         ELSE                                                             
145500           MOVE WS-IDTYPE    TO WS-IDREFTYP                               
145600         END-IF                                                           
145700       ELSE                                                               
145800         MOVE MID-IDREFTYP-IN                                             
145900                             TO WS-IDREFTYP                               
146000       END-IF                                                             
146100       IF WS-IDREFTYP = 'B'                                               
146200       OR WS-IDREFTYP = 'A'                                               
146300       OR WS-IDREFTYP = 'C'                                               
146400       OR WS-IDREFTYP = 'L'                                               
146500         IF WS-IDREFTYP = 'B'                                             
146600           MOVE 'BOAT'       TO MOD-IDREFTYP-UT                           
146700         END-IF                                                           
146800         IF WS-IDREFTYP = 'A'                                             
146900           MOVE 'AIR'        TO MOD-IDREFTYP-UT                           
147000         END-IF                                                           
147100         IF WS-IDREFTYP = 'C'                                             
147200           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
147300         END-IF                                                           
147400         IF WS-IDREFTYP = 'L'                                             
147500           MOVE 'LOCAL'      TO MOD-IDREFTYP-UT                           
147600         END-IF                                                           
147700       ELSE                                                               
147800         IF MID-IDREFTYP-IN NOT = ALL '+'                                 
147900           MOVE NEJ          TO NYCKLAR-SW                                
148000           MOVE MED-7        TO MOD-TEMFSINF                              
148100         END-IF                                                           
148200       END-IF                                                             
148300       INSPECT MOD-IDREFTYP-UT REPLACING LEADING '+'  BY SPACE            
148400                                                                          
148500                                                                          
148600     ELSE                                                                 
148700       IF  MFS-IDTRANS = '2371'                                           
148800                                                                          
148900         MOVE +1             TO IX                                        
149000         PERFORM UNTIL IX > +13                                           
149100         OR MID-SELECT-2371 (IX) NOT = '+'                                
149200           ADD +1            TO IX                                        
149300         END-PERFORM                                                      
149400                                                                          
149500         MOVE ZERO           TO WS-IDPERSON-BUY                           
149600         MOVE SPACE          TO WS-IDTYPE                                 
149700                                WS-IDREFTYP                               
149800                                                                          
149900         IF IX > +13                                                      
150000           CONTINUE                                                       
150100         ELSE                                                             
150200           INSPECT MID-IDPERSON-2371 (IX) REPLACING                       
150300                                     LEADING SPACE BY ZERO                
150400             IF MID-IDTYPE-2371-IN = '+'                                  
150500               IF MID-IDTYPE-2371-UT = 'AIR'                              
150600                 MOVE 'A'    TO WS-IDTYPE                                 
150700                                WS-IDREFTYP                               
150800               END-IF                                                     
150900               IF MID-IDTYPE-2371-UT = 'AIRCR'                            
151000                 MOVE 'C'    TO WS-IDTYPE                                 
151100                                WS-IDREFTYP                               
151200               END-IF                                                     
151300               IF MID-IDTYPE-2371-UT = 'BOAT'                             
151400                 MOVE 'B'    TO WS-IDTYPE                                 
151500                                WS-IDREFTYP                               
151600               END-IF                                                     
151700               IF MID-IDTYPE-2371-UT = 'LOCAL'                            
151800                 MOVE 'L'    TO WS-IDTYPE                                 
151900                                WS-IDREFTYP                               
152000               END-IF                                                     
152100             ELSE                                                         
152200               MOVE MID-IDTYPE-2371-IN                                    
152300                             TO WS-IDTYPE                                 
152400                                WS-IDREFTYP                               
152500             END-IF                                                       
152600             MOVE MID-IDPERSON-2371 (IX)                                  
152700                             TO WS-IDPERSON-BUY                           
152800         END-IF                                                           
152900                                                                          
153000         MOVE '7'            TO MFS-IDPFK                                 
153100         MOVE SPACE          TO MFS-KDTRTYP                               
153200                                                                          
153300         MOVE 'N'            TO WS-STATUS                                 
153400         INSPECT WS-IDPERSON-BUY REPLACING                                
153500                                     LEADING SPACE BY ZERO                
153600         IF WS-IDPERSON-BUY-NUM NUMERIC                                   
153700           MOVE WS-IDPERSON-BUY-NUM                                       
153800                             TO W-IDPERSON-BUY                            
153900                                WS-IDPERSON-BUY-RED                       
154000                                                                          
154100           MOVE WS-IDPERSON-BUY-RED                                       
154200                             TO MOD-IDPERSON-BUY-UT                       
154300           INSPECT MOD-IDPERSON-BUY-UT REPLACING                          
154400                                     LEADING '+'      BY SPACE            
154500         ELSE                                                             
154600           MOVE NEJ TO NYCKLAR-SW                                         
154700         END-IF                                                           
154800                                                                          
154900*      -- KONTROLL AV IDDC                                                
155000         IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS         
155100         OR DCS-NDC-SA                                                    
155200            MOVE IDDC-WS     TO W-IDDC                                    
155300         ELSE                                                             
155400            MOVE NEJ         TO NYCKLAR-SW                                
155500         END-IF                                                           
155600         MOVE IDDC-WS        TO MOD-IDDC-UT                               
155700                                WS-IDDC                                   
155800                                                                          
155900         IF WS-IDTYPE = 'A'                                               
156000         OR WS-IDTYPE = 'B'                                               
156100         OR WS-IDTYPE = 'C'                                               
156200         OR WS-IDTYPE = 'L'                                               
156300           IF WS-IDTYPE = 'A'                                             
156400             MOVE 'AIR'      TO MOD-IDTYPE-UT                             
156500                                MOD-IDREFTYP-UT                           
156600           END-IF                                                         
156700           IF WS-IDTYPE = 'C'                                             
156800             MOVE 'AIRCR'    TO MOD-IDTYPE-UT                             
156900                                MOD-IDREFTYP-UT                           
157000           END-IF                                                         
157100           IF WS-IDTYPE = 'B'                                             
157200             MOVE 'BOAT'     TO MOD-IDTYPE-UT                             
157300                                MOD-IDREFTYP-UT                           
157400           END-IF                                                         
157500           IF WS-IDTYPE = 'L'                                             
157600             MOVE 'LOCAL'    TO MOD-IDTYPE-UT                             
157700                                MOD-IDREFTYP-UT                           
157800           END-IF                                                         
157900         ELSE                                                             
158000           MOVE NEJ TO NYCKLAR-SW                                         
158100         END-IF                                                           
158200         INSPECT MOD-IDTYPE-UT REPLACING                                  
158300                                   LEADING '+' BY SPACE                   
158400                                                                          
158500         MOVE 'N'            TO WS-STATUS                                 
158600         MOVE 'NOT REVIEWED'                                              
158700                             TO MOD-IDSTATUS-UT                           
158800                                                                          
158900       ELSE                                                               
159000*      -- KONTROLL AV IDARTNR                                             
159100                                                                          
159200         IF WS-IDARTNR NUMERIC                                            
159300           MOVE WS-IDARTNR-NUM                                            
159400                             TO W-IDARTNR-MIN                             
159500                                W-IDARTNR-MAX                             
159600                                W-IDARTNR-301                             
159700                                W-IDARTNR                                 
159800                                MID-IDARTNR-IN                            
159900         ELSE                                                             
160000           MOVE NEJ          TO NYCKLAR-SW                                
160100         END-IF                                                           
160200                                                                          
160300         MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                            
160400                                                                          
160500         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
160600         INSPECT MOD-IDARTNR-UT REPLACING LEADING '+' BY SPACE            
160700                                                                          
160800*      -- KONTROLL AV IDDC                                                
160900         IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS         
161000         OR DCS-NDC-SA                                                    
161100            MOVE IDDC-WS    TO W-IDDC                                     
161200         ELSE                                                             
161300            MOVE NEJ        TO NYCKLAR-SW                                 
161400         END-IF                                                           
161500         MOVE IDDC-WS        TO MOD-IDDC-UT                               
161600                                WS-IDDC                                   
161700                                                                          
161800         IF MSGI-SPAR-AREA(1:4) = '2372'                                  
161900           MOVE MSGI-SPAR-AREA                                            
162000                             TO WS-MSGI-AREA-2372                         
162100           IF WS-MSGI-IDTYPE = 'A'                                        
162200           OR WS-MSGI-IDTYPE = 'B'                                        
162300           OR WS-MSGI-IDTYPE = 'C'                                        
162400           OR WS-MSGI-IDTYPE = 'L'                                        
162500             IF WS-MSGI-IDTYPE = 'A'                                      
162600               MOVE 'AIR'      TO MOD-IDTYPE-UT                           
162700                                  MOD-IDREFTYP-UT                         
162800             END-IF                                                       
162900             IF WS-MSGI-IDTYPE = 'C'                                      
163000               MOVE 'AIRCR'    TO MOD-IDTYPE-UT                           
163100                                  MOD-IDREFTYP-UT                         
163200             END-IF                                                       
163300             IF WS-MSGI-IDTYPE = 'B'                                      
163400               MOVE 'BOAT'     TO MOD-IDTYPE-UT                           
163500                                  MOD-IDREFTYP-UT                         
163600             END-IF                                                       
163700             IF WS-MSGI-IDTYPE = 'L'                                      
163800               MOVE 'LOCAL'    TO MOD-IDTYPE-UT                           
163900                                  MOD-IDREFTYP-UT                         
164000             END-IF                                                       
164100             MOVE WS-MSGI-IDTYPE                                          
164200                               TO MID-IDTYPE-IN                           
164300                                  MID-IDREFTYP-IN                         
164400                                  WS-IDTYPE                               
164500                                  WS-IDREFTYP                             
164600           ELSE                                                           
164700             MOVE '+'          TO MID-IDTYPE-IN                           
164800                                  MID-IDREFTYP-IN                         
164900           END-IF                                                         
165000         ELSE                                                             
165100           MOVE '+'            TO MID-IDTYPE-IN                           
165200                                  MID-IDREFTYP-IN                         
165300         END-IF                                                           
165400                                                                          
165500         MOVE 'N'            TO WS-STATUS                                 
165600         MOVE 'NOT REVIEWED'                                              
165700                             TO MOD-IDSTATUS-UT                           
165800       END-IF                                                             
165900                                                                          
166000       PERFORM IMS-GU-WDK711                                              
166100       IF SEGMENT-FINNS                                                   
166200          IF SLAG-IDDC-REF = SPACE AND NDC-CN                             
166300             SET NOT-REFILL-PART  TO TRUE                                 
166400             MOVE NEJ             TO NYCKLAR-SW                           
166500          ELSE                                                            
166600             SET REFILL-PART      TO TRUE                                 
166700             IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                    
166800                MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                     
166900             ELSE                                                         
167000                IF SLAG-IDDC-REF > SPACES                                 
167100                  MOVE SLAG-IDDC-REF TO W-IDDC-B616                       
167200                  PERFORM IMS-GU-WDB616                                   
167300                  IF SEGMENT-FINNS                                        
167400                     MOVE B6-REF-IDDISTR-REFILL TO WS-IDDISTR             
167500                  ELSE                                                    
167600                     MOVE NEJ     TO NYCKLAR-SW                           
167700                     SET DISTRICT-NOT-FOUND TO TRUE                       
167800                  END-IF                                                  
167900                END-IF                                                    
168000             END-IF                                                       
168100          END-IF                                                          
168200       END-IF                                                             
168300     END-IF                                                               
168400***LYNK PARTS ONLY IN EUROPE                                              
168500     MOVE NEJ     TO SW-LYNK-PART                                         
168600     PERFORM IMS-GU-WDK601                                                
168700     IF SEGMENT-FINNS                                                     
168810       IF  ART-KDPRODSL > 30                                              
168820       AND ART-KDPRODSL < 40                                              
168900       AND NDC                                                            
169000         MOVE NEJ          TO NYCKLAR-SW                                  
169100         MOVE JA           TO SW-LYNK-PART                                
169200       END-IF                                                             
169300     END-IF                                                               
169400                                                                          
169500     IF NYCKLAR-FEL                                                       
169600       IF NOT-REFILL-PART                                                 
169700          MOVE ERR-NOT-REFILL-PART                                        
169800                             TO MED-IDMFSFEL                              
169900       ELSE                                                               
170000          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
170100       END-IF                                                             
170200       IF DISTRICT-NOT-FOUND                                              
170300          MOVE MED-15        TO MOD-TEMFSFEL                              
170400       ELSE                                                               
170500         IF LYNK-PART                                                     
170600           MOVE MED-16        TO MOD-TEMFSFEL                             
170700         ELSE                                                             
170800           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
170900           CALL WMEDKONV USING MED-WMEDAREA                               
171000           MOVE MED-MFSFEL    TO MOD-TEMFSFEL                             
171100         END-IF                                                           
171200       END-IF                                                             
171300       PERFORM MFS-RENSA-FAELT-IN                                         
171400       PERFORM MFS-RENSA-FAELT-UT                                         
171500     ELSE                                                                 
171600       IF WS-IDPERSON-BUY NUMERIC                                         
171700         MOVE WS-IDPERSON-BUY                                             
171800                             TO W-IDPERSON-BUY                            
171900         MOVE WS-IDPERSON-BUY-NUM                                         
172000                             TO W-IDPERSON-BUY-MIN                        
172100                                W-IDPERSON-BUY-MAX                        
172200       ELSE                                                               
172300         MOVE ZERO           TO WS-IDPERSON-BUY                           
172400                                W-IDPERSON-BUY                            
172500       END-IF                                                             
172600       MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                            
172700                                W-KDREFTYP-MAX                            
172800       MOVE WS-IDTYPE        TO WS-MSGI-IDTYPE                            
172900     END-IF                                                               
173000     .                                                                    
173100     EJECT                                                                
173200                                                                          
173300 C-FOEREG-SIDA SECTION.                                                   
173400     MOVE NEJ                TO SW-TRAEFF                                 
173500                                                                          
173600     PERFORM CA-BLAEDDRA-BAK                                              
173700                                                                          
173800     IF SW-TRAEFF-JA                                                      
173900       CONTINUE                                                           
174000     ELSE                                                                 
174100       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
174200       CALL WMEDKONV USING MED-WMEDAREA                                   
174300       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
174400       IF EGEN-MID                                                        
174500         MOVE ZERO           TO W-IDARTNR                                 
174600       END-IF                                                             
174700       PERFORM MFS-RENSA-FAELT-IN                                         
174800       PERFORM MFS-RENSA-FAELT-UT                                         
174900     END-IF                                                               
175000     .                                                                    
175100     EJECT                                                                
175200                                                                          
175300 CA-BLAEDDRA-BAK SECTION.                                                 
175400                                                                          
175500     MOVE NEJ                TO SW-TRAEFF                                 
175600****   BLÄDDRA TILLBAKA TILL FÖREGÅENDE ARTIKEL *******                   
175700****   (OM DET FINNS NÅGON)                     *******                   
175800     MOVE WS-MSGI-IDARTNR-PF7                                             
175900                             TO W-IDARTNR                                 
176000                                W-IDARTNR-301                             
176100                                W-IDARTNR-MIN                             
176200                                                                          
176300     PERFORM IMS-GU-WDE301-MIN-MAX                                        
176400                                                                          
176500     PERFORM UNTIL SEGMENT-SAKNAS                                         
176600     OR (REF-IDARTNR = WS-MSGI-IDARTNR-PF7                                
176700     AND  REF-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                          
176800     AND  REF-KDREFTYP = WS-IDTYPE                                        
176900     AND   ((WS-MSGI-ORDER-PF7 = 'N'                                      
177000        AND REF-KDREFORS = 'P')                                           
177100       OR (WS-MSGI-ORDER-PF7   = 'R'                                      
177200        AND REF-KDREFORS = 'O')))                                         
177300                                                                          
177400          PERFORM IMS-GN-WDE301-MIN-MAX                                   
177500                                                                          
177600     END-PERFORM                                                          
177700                                                                          
177800     IF SEGMENT-FINNS                                                     
177900*       --- CHECK IDLEVNR-SECURITY                                        
178000        MOVE REF-IDARTNR TO W-IDARTNR                                     
178100        PERFORM S1-SECURITY-CHECK-PARTNO                                  
178200     END-IF                                                               
178300                                                                          
178400     IF SEGMENT-FINNS                                                     
178500                                                                          
178600       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
178700                                                                          
178800     ELSE                                                                 
178900       MOVE ZERO             TO W-IDARTNR                                 
179000                                W-IDARTNR-301                             
179100                                W-IDARTNR-MIN                             
179200                                                                          
179300       PERFORM IMS-GU-WDE301-MIN-MAX                                      
179400                                                                          
179500       PERFORM UNTIL SEGMENT-SAKNAS                                       
179600                                                                          
179700       OR (REF-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                         
179800       AND (REF-KDREFTYP = WS-IDTYPE                                      
179900       AND ((WS-STATUS  = 'N'                                             
180000        AND REF-KDREFORS = 'P')                                           
180100       OR (WS-STATUS    = 'R'                                             
180200        AND REF-KDREFORS = 'O'))))                                        
180300                                                                          
180400           ADD +1            TO WS-ANTAL-POSTER                           
180500                                                                          
180600           PERFORM IMS-GN-WDE301-MIN-MAX                                  
180700                                                                          
180800       END-PERFORM                                                        
180900                                                                          
181000       IF SEGMENT-FINNS                                                   
181100*         --- CHECK IDLEVNR-SECURITY                                      
181200          MOVE REF-IDARTNR TO W-IDARTNR                                   
181300          PERFORM S1-SECURITY-CHECK-PARTNO                                
181400       END-IF                                                             
181500                                                                          
181600       IF SEGMENT-FINNS                                                   
181700                                                                          
181800         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
181900                                                                          
182000       END-IF                                                             
182100     END-IF                                                               
182200     PERFORM UNTIL SEGMENT-SAKNAS                                         
182300     OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                                
182400**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
182500**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
182600**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
182700       IF REF-KDREFTYP = WS-IDTYPE                                        
182800         PERFORM M-UPD-MOD-FAELT                                          
182900       END-IF                                                             
183000                                                                          
183100       PERFORM IMS-GN-WDE301-MIN-MAX                                      
183200     END-PERFORM                                                          
183300     .                                                                    
183400     EJECT                                                                
183500                                                                          
183600 D-NAESTA-SIDA SECTION.                                                   
183700     MOVE WS-IDPERSON-BUY    TO W-IDPERSON-BUY-MIN                        
183800     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
183900     MOVE WS-IDARTNR         TO W-IDARTNR-MIN                             
184000     MOVE ZERO               TO W-IDDISTR-MIN                             
184100     MOVE NEJ                TO SW-TRAEFF                                 
184200                                                                          
184300     PERFORM DA-BLAEDDRA-FRAM                                             
184400                                                                          
184500     IF SW-TRAEFF-JA                                                      
184600       CONTINUE                                                           
184700     ELSE                                                                 
184800       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
184900       CALL WMEDKONV USING MED-WMEDAREA                                   
185000       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
185100       MOVE ZERO             TO W-IDARTNR                                 
185200       PERFORM MFS-RENSA-FAELT-IN                                         
185300       PERFORM MFS-RENSA-FAELT-UT                                         
185400     END-IF                                                               
185500     .                                                                    
185600     EJECT                                                                
185700                                                                          
185800 DA-BLAEDDRA-FRAM SECTION.                                                
185900                                                                          
186000     PERFORM IMS-GU-WDE301-PF8                                            
186100                                                                          
186200     PERFORM UNTIL SEGMENT-SAKNAS                                         
186300     OR  (REF-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                          
186400     AND  REF-IDARTNR > W-IDARTNR                                         
186500     AND (REF-KDREFTYP = WS-IDTYPE                                        
186600     AND ((WS-STATUS        = 'N'                                         
186700     AND       REF-KDREFORS = 'P')                                        
186800     OR      (WS-STATUS     = 'R'                                         
186900     AND  REF-KDREFORS      = 'O'))))                                     
187000                                                                          
187100         PERFORM IMS-GN-WDE301-PF8                                        
187200     END-PERFORM                                                          
187300                                                                          
187400     IF SEGMENT-FINNS                                                     
187500*       --- CHECK IDLEVNR-SECURITY                                        
187600        MOVE REF-IDARTNR TO W-IDARTNR                                     
187700        PERFORM S1-SECURITY-CHECK-PARTNO                                  
187800     END-IF                                                               
187900                                                                          
188000     IF SEGMENT-FINNS                                                     
188100       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
188200     ELSE                                                                 
188300       MOVE ZERO             TO W-IDARTNR                                 
188400     END-IF                                                               
188500                                                                          
188600     MOVE WS-MSGI-IDARTNR-ENTER                                           
188700                             TO WS-MSGI-IDARTNR-PF7                       
188800     MOVE WS-MSGI-ORDER-ENTER                                             
188900                             TO WS-MSGI-ORDER-PF7                         
189000                                                                          
189100     PERFORM UNTIL SEGMENT-SAKNAS                                         
189200     OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                                
189300**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
189400**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
189500**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
189600       IF REF-KDREFTYP = WS-IDTYPE                                        
189700         PERFORM M-UPD-MOD-FAELT                                          
189800       END-IF                                                             
189900                                                                          
190000       PERFORM IMS-GN-WDE301-PF8                                          
190100     END-PERFORM                                                          
190200     .                                                                    
190300     EJECT                                                                
190400                                                                          
190500 E-SAMMA-SIDA SECTION.                                                    
190600                                                                          
190700     IF MID-IDARTNR-IN       = ALL '+'                                    
190800     AND MID-IDDC-IN         = ALL '+'                                    
190900     AND MID-IDTYPE-IN       = ALL '+'                                    
191000     AND MID-IDPERSON-BUY-IN = ALL '+'                                    
191100     AND MID-IDSTATUS-IN     = ALL '+'                                    
191200     AND MID-IDREFTYP-IN     = ALL '+'                                    
191300       IF  MID-KVPB-REF      = ALL '+'                                    
191400       OR  MID-KVPBREOI      = ALL '+'                                    
191500         CONTINUE                                                         
191600*----     INGEN INFO LIGGER I BILDEN                                      
191700*----     SKA SKRIVAS UT I F-HAEMTA                                       
191800*----                                                                     
191900       ELSE                                                               
192000         MOVE NEJ            TO SW-HAEMTA-INPUT-FAELT                     
192100       END-IF                                                             
192200                                                                          
192300       PERFORM EB-SIMULERA                                                
192400       IF MID-INPUT      NOT = ALL '+'                                    
192500         PERFORM I-KOLLA-INPUT                                            
192600         PERFORM EE-BERAKNA-REFPKT                                        
192700         MOVE WS-PURCHQTY       TO WS-PURCHQTY-SIM                        
192800       END-IF                                                             
192900     ELSE                                                                 
193000                                                                          
193100       IF  (MID-IDARTNR-IN NOT = ALL '+'                                  
193200       AND MID-IDTYPE-IN  NOT = ALL '+')                                  
193300       OR MID-IDREFTYP-IN NOT = ALL '+'                                   
193400                                                                          
193500         PERFORM EC-SOEK-NY-ARTIKEL-TYP                                   
193600                                                                          
193700         IF SW-TRAEFF-JA                                                  
193800           CONTINUE                                                       
193900         ELSE                                                             
194000           PERFORM MFS-RENSA-FAELT-IN                                     
194100           PERFORM MFS-RENSA-FAELT-UT                                     
194200           MOVE URVAL-SAKNAS TO MED-IDMFSFEL                              
194300           CALL WMEDKONV USING MED-WMEDAREA                               
194400           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
194500         END-IF                                                           
194600                                                                          
194700       ELSE                                                               
194800          IF  MID-IDARTNR-IN NOT = ALL '+'                                
194900          OR  MID-IDDC-IN    NOT = ALL '+'                                
195000          AND MID-IDTYPE-IN      = ALL '+'                                
195100          AND MID-IDREFTYP-IN    = ALL '+'                                
195200                                                                          
195300            PERFORM ED-SOEK-NY-ARTIKEL                                    
195400                                                                          
195500            IF SW-TRAEFF-JA                                               
195600              CONTINUE                                                    
195700            ELSE                                                          
195800              PERFORM MFS-RENSA-FAELT-IN                                  
195900              PERFORM MFS-RENSA-FAELT-UT                                  
196000              MOVE URVAL-SAKNAS TO MED-IDMFSFEL                           
196100              CALL WMEDKONV USING MED-WMEDAREA                            
196200              MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                           
196300            END-IF                                                        
196400                                                                          
196500          ELSE                                                            
196600            IF MID-IDPERSON-BUY-IN NOT = ALL '+'                          
196700            OR MID-IDTYPE-IN NOT = ALL '+'                                
196800            OR MID-IDSTATUS-IN NOT = ALL '+'                              
196900                                                                          
197000              PERFORM EA-NY-BUY-TYPE-LEV-STAT                             
197100                                                                          
197200              IF SW-TRAEFF-JA                                             
197300                CONTINUE                                                  
197400              ELSE                                                        
197500                PERFORM MFS-RENSA-FAELT-IN                                
197600                PERFORM MFS-RENSA-FAELT-UT                                
197700                MOVE URVAL-SAKNAS TO MED-IDMFSFEL                         
197800                CALL WMEDKONV USING MED-WMEDAREA                          
197900                MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                         
198000              END-IF                                                      
198100            END-IF                                                        
198200          END-IF                                                          
198300       END-IF                                                             
198400     END-IF                                                               
198500     .                                                                    
198600     EJECT                                                                
198700                                                                          
198800 EA-NY-BUY-TYPE-LEV-STAT SECTION.                                         
198900                                                                          
199000     MOVE NEJ                TO SW-TRAEFF                                 
199100     MOVE ZERO               TO W-IDARTNR                                 
199200     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-UT                            
199300     PERFORM IMS-GU-WDE301-MIN-MAX                                        
199400                                                                          
199500     PERFORM UNTIL SEGMENT-SAKNAS                                         
199600     OR  (REF-KDREFTYP = WS-IDTYPE                                        
199700       AND ((WS-STATUS   = 'N'                                            
199800      AND    REF-KDREFORS = 'P')                                          
199900       OR  (WS-STATUS    = 'R'                                            
200000       AND  REF-KDREFORS = 'O')))                                         
200100            PERFORM IMS-GN-WDE301-MIN-MAX                                 
200200                                                                          
200300     END-PERFORM                                                          
200400                                                                          
200500     IF SEGMENT-FINNS                                                     
200600*       --- CHECK IDLEVNR-SECURITY                                        
200700        MOVE REF-IDARTNR TO W-IDARTNR                                     
200800        PERFORM S1-SECURITY-CHECK-PARTNO                                  
200900     END-IF                                                               
201000                                                                          
201100     IF SEGMENT-FINNS                                                     
201200       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
201300     END-IF                                                               
201400                                                                          
201500     PERFORM UNTIL SEGMENT-SAKNAS                                         
201600     OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                                
201700       IF REF-KDREFTYP = WS-IDTYPE                                        
201800         PERFORM M-UPD-MOD-FAELT                                          
201900       END-IF                                                             
202000                                                                          
202100       PERFORM IMS-GN-WDE301-MIN-MAX                                      
202200     END-PERFORM                                                          
202300     .                                                                    
202400     EJECT                                                                
202500                                                                          
202600 EB-SIMULERA SECTION.                                                     
202700                                                                          
202800     MOVE NEJ                TO SW-TRAEFF                                 
202900     MOVE WS-MSGI-IDARTNR-ENTER                                           
203000                             TO W-IDARTNR                                 
203100                                W-IDARTNR-301                             
203200                                W-IDARTNR-MIN                             
203300     PERFORM IMS-GU-WDE301-MIN-MAX                                        
203400                                                                          
203500     PERFORM UNTIL SEGMENT-SAKNAS                                         
203600     OR (REF-IDARTNR = W-IDARTNR-MIN                                      
203700     AND  REF-KDREFTYP = WS-IDREFTYP)                                     
203800            PERFORM IMS-GN-WDE301-MIN-MAX                                 
203900                                                                          
204000     END-PERFORM                                                          
204100                                                                          
204200     IF SEGMENT-FINNS                                                     
204300*       --- CHECK IDLEVNR-SECURITY                                        
204400        MOVE REF-IDARTNR TO W-IDARTNR                                     
204500        PERFORM S1-SECURITY-CHECK-PARTNO                                  
204600     END-IF                                                               
204700                                                                          
204800     IF SEGMENT-FINNS                                                     
204900       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
205000     END-IF                                                               
205100                                                                          
205200     PERFORM UNTIL SEGMENT-SAKNAS                                         
205300     OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                                
205400       IF REF-KDREFTYP = WS-IDREFTYP                                      
205500                                                                          
205600         MOVE REF-KDREFTXT   TO WS-REF-KDREFTXT                           
205710         IF REF-KDREFORS = 'P'                                            
205800            MOVE 'PROPOSAL NOT REVIEWED'                                  
205900                             TO MOD-ORDERSTATUS                           
206000         ELSE                                                             
206100            MOVE 'REVIEWED'  TO MOD-ORDERSTATUS                           
206200         END-IF                                                           
206300                                                                          
206400         IF REF-KDREFTYP = 'B'                                            
206500           MOVE 'BOAT '      TO MOD-IDREFTYP-UT                           
206600         END-IF                                                           
206700         IF REF-KDREFTYP = 'A'                                            
206800           MOVE 'AIR  '      TO MOD-IDREFTYP-UT                           
206900         END-IF                                                           
207000         IF REF-KDREFTYP = 'C'                                            
207100           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
207200         END-IF                                                           
207300         IF REF-KDREFTYP = 'L'                                            
207400           MOVE 'LOCAL'      TO MOD-IDREFTYP-UT                           
207500         END-IF                                                           
207600                                                                          
207700       END-IF                                                             
207800                                                                          
207900       PERFORM IMS-GN-WDE301-MIN-MAX                                      
208000     END-PERFORM                                                          
208100     .                                                                    
208200     EJECT                                                                
208300                                                                          
208400 EC-SOEK-NY-ARTIKEL-TYP SECTION.                                          
208500                                                                          
208600     MOVE NEJ                TO SW-TRAEFF                                 
208700*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
208800     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
208900     MOVE W-IDARTNR          TO W-IDARTNR-MIN                             
209000     PERFORM IMS-GU-WDE301-MIN-MAX                                        
209100                                                                          
209200     PERFORM UNTIL SEGMENT-SAKNAS                                         
209300     OR (REF-IDARTNR = W-IDARTNR-MIN                                      
209400     AND  REF-KDREFTYP = WS-IDREFTYP)                                     
209500                                                                          
209600       PERFORM IMS-GN-WDE301-MIN-MAX                                      
209700     END-PERFORM                                                          
209800                                                                          
209900     IF SEGMENT-FINNS                                                     
210000*       --- CHECK IDLEVNR-SECURITY                                        
210100        MOVE REF-IDARTNR TO W-IDARTNR                                     
210200        PERFORM S1-SECURITY-CHECK-PARTNO                                  
210300     END-IF                                                               
210400                                                                          
210500     IF SEGMENT-FINNS                                                     
210600       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
210700       MOVE REF-IDDC         TO W-IDDC-301                                
210800       MOVE REF-IDARTNR      TO W-IDARTNR                                 
210900                                W-IDARTNR-301                             
211000                                W-IDARTNR-MIN                             
211100                                W-IDARTNR-MAX                             
211200                                                                          
211300       PERFORM UNTIL SEGMENT-SAKNAS                                       
211400       OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                              
211500                                                                          
211600**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
211700**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
211800**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
211900         IF REF-KDREFTYP = WS-IDREFTYP                                    
212000           PERFORM M-UPD-MOD-FAELT                                        
212100         END-IF                                                           
212200                                                                          
212300         PERFORM IMS-GN-WDE301-MIN-MAX                                    
212400       END-PERFORM                                                        
212500     ELSE                                                                 
212600       PERFORM IMS-GU-WDK711                                              
212700       IF SEGMENT-FINNS                                                   
212800         MOVE SLAG-IDPERSON-BUY                                           
212900                               TO WS-IDPERSON-BUY-NUM                     
213000                                  WS-IDPERSON-BUY-RED                     
213100         MOVE WS-IDPERSON-BUY-RED                                         
213200                               TO MOD-IDPERSON-BUY-UT                     
213300       END-IF                                                             
213400     END-IF                                                               
213500     .                                                                    
213600     EJECT                                                                
213700                                                                          
213800 ED-SOEK-NY-ARTIKEL SECTION.                                              
213900                                                                          
214000     MOVE NEJ                TO SW-TRAEFF                                 
214100*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
214200     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
214300     MOVE LOW-VALUE          TO W-WDE301KY-MIN-X                          
214400     MOVE IDDC-WS            TO W-IDDC-MIN                                
214500     MOVE HIGH-VALUE         TO W-WDE301KY-MAX-X                          
214600     MOVE IDDC-WS            TO W-IDDC-MAX                                
214700     PERFORM IMS-GU-WDE301-MIN-MAX                                        
214800                                                                          
214900     MOVE 9                  TO WS-KTRL-PRIO                              
215000                                                                          
215100     PERFORM UNTIL SEGMENT-SAKNAS                                         
215200       IF REF-IDARTNR = W-IDARTNR                                         
215300       AND REF-IDDC = W-IDDC                                              
215400         IF (REF-KDREFTYP = 'A' OR 'C')                                   
215500         AND REF-KDREFORS = 'O'                                           
215600         AND WS-KTRL-PRIO > 1                                             
215700*------    HÖGST PRIORITET SPARAS                                         
215800*------                                                                   
215900           MOVE REF-KDREFTYP TO WS-IDREFTYP                               
216000           MOVE 1            TO WS-KTRL-PRIO                              
216100                                                                          
216200         ELSE                                                             
216300           IF REF-KDREFTYP = 'B'                                          
216400           AND REF-KDREFORS = 'O'                                         
216500           AND WS-KTRL-PRIO > 2                                           
216600*------    HÖGST PRIORITET SPARAS                                         
216700*------                                                                   
216800             MOVE REF-KDREFTYP TO WS-IDREFTYP                             
216900             MOVE 2          TO WS-KTRL-PRIO                              
217000                                                                          
217100           ELSE                                                           
217200             IF REF-KDREFTYP = 'L'                                        
217300             AND REF-KDREFORS = 'O'                                       
217400             AND WS-KTRL-PRIO > 3                                         
217500*------    HÖGST PRIORITET SPARAS                                         
217600*------                                                                   
217700               MOVE REF-KDREFTYP TO WS-IDREFTYP                           
217800               MOVE 3        TO WS-KTRL-PRIO                              
217900                                                                          
218000             ELSE                                                         
218100               IF (REF-KDREFTYP = 'A' OR 'C')                             
218200               AND REF-KDREFORS = 'P'                                     
218300               AND WS-KTRL-PRIO > 4                                       
218400*------    HÖGST PRIORITET SPARAS                                         
218500*------                                                                   
218600                 MOVE REF-KDREFTYP TO WS-IDREFTYP                         
218700                 MOVE 4      TO WS-KTRL-PRIO                              
218800                                                                          
218900               ELSE                                                       
219000                 IF REF-KDREFTYP = 'B'                                    
219100                 AND REF-KDREFORS = 'P'                                   
219200                 AND WS-KTRL-PRIO > 5                                     
219300*------    HÖGST PRIORITET SPARAS                                         
219400*------                                                                   
219500                   MOVE REF-KDREFTYP TO WS-IDREFTYP                       
219600                   MOVE 5    TO WS-KTRL-PRIO                              
219700                                                                          
219800                 ELSE                                                     
219900                   IF REF-KDREFTYP = 'L'                                  
220000                   AND REF-KDREFORS = 'P'                                 
220100                   AND WS-KTRL-PRIO > 6                                   
220200*------    HÖGST PRIORITET SPARAS                                         
220300*------                                                                   
220400                     MOVE REF-KDREFTYP TO WS-IDREFTYP                     
220500                     MOVE 6  TO WS-KTRL-PRIO                              
220600                                                                          
220700                   END-IF                                                 
220800                 END-IF                                                   
220900               END-IF                                                     
221000             END-IF                                                       
221100           END-IF                                                         
221200         END-IF                                                           
221300       END-IF                                                             
221400       PERFORM IMS-GN-WDE301-MIN-MAX                                      
221500     END-PERFORM                                                          
221600                                                                          
221700     IF WS-KTRL-PRIO NOT = 9                                              
221800*---    TRÄFF PÅ SÖKT ARTIKEL                                             
221900*---                                                                      
222000       PERFORM IMS-GU-WDE301-MIN-MAX                                      
222100       PERFORM UNTIL SEGMENT-SAKNAS                                       
222200       OR (REF-IDARTNR  = W-IDARTNR                                       
222300       AND REF-IDDC     = W-IDDC                                          
222400       AND REF-KDREFTYP = WS-IDREFTYP)                                    
222500*------    LÄS FRAM TILL SÖKT ARTIKEL/REFTYP                              
222600*------                                                                   
222700         PERFORM IMS-GN-WDE301-MIN-MAX                                    
222800       END-PERFORM                                                        
222900                                                                          
223000       IF SEGMENT-FINNS                                                   
223100*         --- CHECK IDLEVNR-SECURITY                                      
223200          MOVE REF-IDARTNR TO W-IDARTNR                                   
223300          PERFORM S1-SECURITY-CHECK-PARTNO                                
223400       END-IF                                                             
223500                                                                          
223600       IF SEGMENT-FINNS                                                   
223700         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
223800         MOVE REF-IDPERSON-BUY                                            
223900                             TO WS-IDPERSON-BUY-NUM                       
224000       ELSE                                                               
224100*------    DETTA FALL SKA INTE INTRÄFFA                                   
224200*------                                                                   
224300         MOVE 'GE'           TO STATUS-WS                                 
224400       END-IF                                                             
224500     ELSE                                                                 
224600       PERFORM IMS-GU-WDK711                                              
224700       IF SEGMENT-FINNS                                                   
224800         MOVE SLAG-IDPERSON-BUY                                           
224900                             TO WS-IDPERSON-BUY-NUM                       
225000                                WS-IDPERSON-BUY-RED                       
225100         MOVE WS-IDPERSON-BUY-RED                                         
225200                             TO MOD-IDPERSON-BUY-UT                       
225300       END-IF                                                             
225400       MOVE 'GE'             TO STATUS-WS                                 
225500     END-IF                                                               
225600                                                                          
225700     PERFORM UNTIL SEGMENT-SAKNAS                                         
225800     OR (WS-SPARA-IDARTNR NOT = REF-IDARTNR                               
225900     AND W-IDDC              = REF-IDDC)                                  
226000                                                                          
226100**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
226200**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
226300**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
226400       IF REF-KDREFTYP = WS-IDREFTYP                                      
226500         PERFORM M-UPD-MOD-FAELT                                          
226600       END-IF                                                             
226700                                                                          
226800       PERFORM IMS-GN-WDE301-MIN-MAX                                      
226900     END-PERFORM                                                          
227000     .                                                                    
227100     EJECT                                                                
227200                                                                          
227300 EE-BERAKNA-REFPKT SECTION.                                               
227400                                                                          
227500     IF WS-KVPB-TOT > ZERO                                                
227600       PERFORM IMS-GU-WDK711                                              
227700       IF SEGMENT-FINNS                                                   
227800                                                                          
227900         MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                              
228000         MOVE DAGENS-DATUM    TO TMP2-YYMMDD                              
228100         PERFORM WY2000P1                                                 
228200         IF TMP1-YYMMDD   >= TMP2-YYMMDD                                  
228300*--- KVREFPKT FRÅN BASEN GÄLLER PGA MANUELLT DATUM ÄR SATT                
228400           MOVE SLAG-KVREFPKT                                             
228500                           TO MOD-KVREFPKT                                
228600           PERFORM IMS-GU-WDK727                                          
228700           IF SEGMENT-FINNS                                               
228800             IF PROG-KVPB-JUST(1) > ZERO                                  
228900               MOVE MFS-ADD-LYS-UPP-FAELT                                 
229000                          TO MOD-KVREFPKT-ATTR                            
229100             END-IF                                                       
229200           END-IF                                                         
229300           MOVE SLAG-KVREFBER                                             
229400                           TO MOD-KVREFBER                                
229500         ELSE                                                             
229600***        CALL W271REFL TO SIMULATE REFILLING POINT & QTY                
229700***        FOR SIMULATING ALWAYS PASS YES TO FLSIM                        
229800***                                                                       
229900           MOVE JA         TO WS-FLSIM                                    
230000           PERFORM S90-CALL-W271REFL                                      
230100                                                                          
230200           MOVE W271-REFL-KVREFPKT                                        
230300                           TO MOD-KVREFPKT                                
230400           PERFORM IMS-GU-WDK727                                          
230500           IF SEGMENT-FINNS                                               
230600             IF PROG-KVPB-JUST(1) > ZERO                                  
230700               MOVE MFS-ADD-LYS-UPP-FAELT                                 
230800                           TO MOD-KVREFPKT-ATTR                           
230900             END-IF                                                       
231000           END-IF                                                         
231100           MOVE W271-REFL-KVREFBER                                        
231200                           TO MOD-KVREFBER                                
231300                                                                          
231400         END-IF                                                           
231500       ELSE                                                               
231600         MOVE ZERO         TO MOD-KVREFPKT                                
231700         MOVE ZERO         TO MOD-KVREFBER                                
231800       END-IF                                                             
231900     ELSE                                                                 
232000       MOVE ZERO           TO MOD-KVREFPKT                                
232100       MOVE ZERO           TO MOD-KVREFBER                                
232200     END-IF                                                               
232300                                                                          
232400     .                                                                    
232500     EJECT                                                                
232600                                                                          
232700 F-HAEMTA-INFO SECTION.                                                   
232800                                                                          
232900*    ARTIKELINFORMATION                                                   
233000     PERFORM IMS-GU-WDK601                                                
233100     IF SEGMENT-FINNS                                                     
233200       MOVE ART-IDFKNGRP     TO MOD-IDFKNGRP                              
233300       MOVE ART-KDPRODSL     TO MOD-KDPRODSL                              
233400       MOVE ART-TIFINLV      TO MOD-TIFINLV                               
233500       MOVE ART-TIURPROD     TO MOD-TIURPROD                              
233600                                                                          
233700*      ARTIKEL C-LAGER INFO                                               
233800       PERFORM IMS-GNP-WDK611                                             
233900       IF SEGMENT-FINNS                                                   
234000         MOVE SPACE          TO WS-IDLAND-SEND                            
234100         MOVE CLAG-KDERS     TO MOD-KDERS                                 
234200                                WS-KDERS                                  
234300         MOVE CLAG-REDIRLEV  TO MOD-REDIRLEV                              
234400         IF NOT (NDC-CN OR NDC-NA)                                        
234500           MOVE CLAG-PRARTSTD TO MOD-PRARTSTD                             
234600           MOVE 'Std'         TO MOD-PRICE-TYPE                           
234700         ELSE                                                             
234800           PERFORM IMS-GU-WDK712                                          
234900           IF SEGMENT-FINNS                                               
235000             MOVE LART-PRMATRL TO MOD-PRARTSTD                            
235100             MOVE 'Mtr'        TO MOD-PRICE-TYPE                          
235200           END-IF                                                         
235300         END-IF                                                           
235400         MOVE CLAG-KVQPACK-0 TO MOD-KVQPACK-0                             
235500         MOVE CLAG-KVQPACK-1 TO MOD-KVQPACK-1                             
235600         MOVE CLAG-KVQPACK-3 TO WS-KVQPACK-3                              
235700         MOVE CLAG-KVQPACK-4 TO MOD-KVQPACK-4                             
235800         MOVE IDDC-WS        TO W-IDDC                                    
235900         PERFORM IMS-GU-WDK711                                            
236000         IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                        
236100            MOVE CLAG-KVPALL TO MOD-KVPALL                                
236200            MOVE CLAG-VKART  TO MOD-VKART                                 
236300                                WS-VKART                                  
236400            MOVE CLAG-VLARTNTO TO MOD-VLARTNTO                            
236500                                  WS-VLARTNTO                             
236600            MOVE CLAG-IDDC-REF TO WS-IDDC-REF-K7                          
236700            COMPUTE WS-PREADV-QTY    = CLAG-KVAKS-PAV +                   
236800                                       CLAG-KVBEART                       
236900         ELSE                                                             
237000            IF SLAG-IDDC-REF NOT = SPACE                                  
237100               MOVE SLAG-IDDC-REF TO W-IDDC                               
237200                                     WS-IDDC                              
237300               PERFORM IMS-GU-WDK722                                      
237400               IF SEGMENT-FINNS                                           
237500                  MOVE XLAG-KVPALL TO MOD-KVPALL                          
237600               END-IF                                                     
237700            ELSE                                                          
237800**FLYTTAR 0 FÖR ATT INTE FÅ TRÄFF FÖR LOKAL ANSKAFFADE VIA REFILL         
237900** I s03-SEARCH-IDLAND                                                    
238000               MOVE ZERO  TO W-IDDC                                       
238100               MOVE ZERO     TO MOD-KVPALL                                
238200            END-IF                                                        
238300                                                                          
238400** KOLLAR VILKET LAND SOM REFILLAR FÖR ATT VISA VIKT & VOLYM              
238500            PERFORM S03-SEARCH-IDLAND                                     
238600                                                                          
238700            PERFORM IMS-GU-WDK712                                         
238800            IF SEGMENT-FINNS                                              
238900               IF LART-VKART > 0                                          
239000                 MOVE LART-VKART TO MOD-VKART                             
239100                                     WS-VKART                             
239200               ELSE                                                       
239300                 MOVE CLAG-VKART TO MOD-VKART                             
239400                                     WS-VKART                             
239500               END-IF                                                     
239600               IF LART-VLARTNTO > 0                                       
239700                 MOVE LART-VLARTNTO TO MOD-VLARTNTO                       
239800                                       WS-VLARTNTO                        
239900               ELSE                                                       
240000                 MOVE CLAG-VLARTNTO TO MOD-VLARTNTO                       
240100                                       WS-VLARTNTO                        
240200               END-IF                                                     
240300               IF LART-KVQPACK-3 > ZERO                                   
240400                 MOVE LART-KVQPACK-3   TO WS-KVQPACK-3                    
240500               END-IF                                                     
240600            ELSE                                                          
240700               MOVE CLAG-VKART  TO MOD-VKART                              
240800                                   WS-VKART                               
240900               MOVE CLAG-VLARTNTO TO MOD-VLARTNTO                         
241000                                     WS-VLARTNTO                          
241100            END-IF                                                        
241200**FLYTTA TILLBAKA DC/LAND FÖR DC IFYLLT I HUVUDRADEN                      
241300            PERFORM IMS-GU-WDK711                                         
241400            IF SEGMENT-FINNS                                              
241500              MOVE SLAG-IDDC-REF TO WS-IDDC-REF-K7                        
241600              COMPUTE WS-PREADV-QTY = SLAG-KVAKS-PAV +                    
241700                                      SLAG-KVBEART                        
241800            END-IF                                                        
241900            MOVE IDDC-WS    TO W-IDDC                                     
242000                               WS-IDDC                                    
242100            MOVE WS-IDLAND-HEAD TO W-IDLAND                               
242200         END-IF                                                           
242300         MOVE WS-KVQPACK-3    TO MOD-KVQPACK-3                            
242400         MOVE ZERO           TO WS-KR-VIKT                                
242500                                WS-KR-VIKT-RED                            
242600                                WS-KR-VOLYM                               
242700                                WS-KR-VOLYM-RED                           
242800*        ARTIKELREGISTER ORDER ENTRY                                      
242900         PERFORM IMS-GU-WDK901                                            
243000         IF SEGMENT-FINNS                                                 
243100           COMPUTE WS-KVOKS-TOT = WDK9-ART-KVOKS-BULK +                   
243200                                   WDK9-ART-KVOKS-DAG +                   
243300                                   WDK9-ART-KVOKS-VOR                     
243400         ELSE                                                             
243500           MOVE ZERO        TO WS-KVOKS-TOT                               
243600         END-IF                                                           
243700                                                                          
243800*                                                                         
243900         IF W-IDDC  NOT = W-IDDC-B6                                       
244000            MOVE W-IDDC  TO W-IDDC-B6                                     
244100            PERFORM IMS-GU-WDB601                                         
244200         END-IF                                                           
244300                                                                          
244400*                                                                         
244500         IF DCS-NDC-CN OR DCS-NDC-PF OR DCS-NDC-OTHERS                    
244600         OR DCS-NDC-SA                                                    
244700           CONTINUE                                                       
244800         ELSE                                                             
244900           MOVE MFS-CLOSE-FIELD   TO MOD-KVPBREOI-ATTR                    
245000         END-IF                                                           
245100                                                                          
245200         PERFORM IMS-GU-WDK711                                            
245300         IF SEGMENT-FINNS                                                 
245400            IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                     
245500               COMPUTE WS-AVAILABLE ROUNDED =                             
245600                  CLAG-KVLS - CLAG-KVRESS - WS-KVOKS-TOT                  
245700               COMPUTE WS-KVAKS ROUNDED =                                 
245800                  CLAG-KVAKS-CDC + CLAG-KVAKS-T                           
245900               MOVE CLAG-KVROS    TO WS-KVROS-CDC                         
246000            ELSE                                                          
246100               MOVE W-IDDC        TO WS-IDDC-SAVED                        
246200               MOVE SLAG-IDDC-REF TO W-IDDC                               
246300               PERFORM IMS-GU-WDK711                                      
246400               IF SEGMENT-FINNS                                           
246500                  COMPUTE WS-AVAILABLE ROUNDED =                          
246600                     SLAG-KVLS - SLAG-KVRESS - SLAG-KVOKS-BULK -          
246700                     SLAG-KVOKS-DAG                                       
246800                  COMPUTE WS-KVROS-CDC ROUNDED =                          
246900                     SLAG-KVROS-BULK + SLAG-KVROS-DAG                     
247000                  MOVE SLAG-KVAKS-SDC TO WS-KVAKS                         
247100                  MOVE ZERO TO WS-KVPB                                    
247200                  ADD SLAG-KVPB-REF  TO WS-KVPB                           
247300                  ADD SLAG-KVPBREOI  TO WS-KVPB                           
247400               END-IF                                                     
247500               MOVE WS-IDDC-SAVED TO W-IDDC                               
247600            END-IF                                                        
247700         ELSE                                                             
247800           MOVE W-IDDC        TO WS-IDDC-SAVED                            
247900           MOVE DCS-IDDC-REF  TO W-IDDC                                   
248000           IF DCS-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                       
248100              COMPUTE WS-AVAILABLE ROUNDED =                              
248200                 CLAG-KVLS - CLAG-KVRESS - WS-KVOKS-TOT                   
248300              COMPUTE WS-KVAKS ROUNDED =                                  
248400                 CLAG-KVAKS-CDC + CLAG-KVAKS-T                            
248500              MOVE CLAG-KVROS     TO WS-KVROS-CDC                         
248600           ELSE                                                           
248700              MOVE DCS-IDDC-REF TO W-IDDC                                 
248800              PERFORM IMS-GU-WDK711                                       
248900              IF SEGMENT-FINNS                                            
249000                 COMPUTE WS-AVAILABLE ROUNDED =                           
249100                    SLAG-KVLS - SLAG-KVRESS - SLAG-KVOKS-BULK -           
249200                    SLAG-KVOKS-DAG                                        
249300                 COMPUTE WS-KVROS-CDC ROUNDED =                           
249400                    SLAG-KVROS-BULK + SLAG-KVROS-DAG                      
249500                 MOVE SLAG-KVAKS-SDC TO WS-KVAKS                          
249600                 MOVE ZERO TO WS-KVPB                                     
249700                 ADD SLAG-KVPB-REF   TO WS-KVPB                           
249800                 ADD SLAG-KVPBREOI   TO WS-KVPB                           
249900              END-IF                                                      
250000            END-IF                                                        
250100            MOVE WS-IDDC-SAVED TO W-IDDC                                  
250200         END-IF                                                           
250300         MOVE WS-AVAILABLE   TO MOD-AVAIL                                 
250400         MOVE WS-KVAKS       TO MOD-KVAKS-CDC                             
250500         MOVE WS-KVROS-CDC   TO MOD-KVROS-CDC                             
250600         MOVE W-IDDC         TO WS-SPARA-IDDC-FD-HAEMTA                   
250700                                                                          
250800         IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                        
250900           PERFORM FD-HAEMTA-KVPB                                         
251000         END-IF                                                           
251100                                                                          
251200         MOVE WS-SPARA-IDDC-FD-HAEMTA                                     
251300                             TO W-IDDC                                    
251400         PERFORM IMS-GU-WDK711                                            
251500         MOVE WS-KVPB        TO MOD-KVPB-CDC                              
251600         IF CLAG-KVUTRS > ZERO                                            
251700           MOVE CLAG-KVUTRS  TO WS-TEMF-UTRSALDO-NUM                      
251800           MOVE WS-TEMF-UTRSALDO-NUM                                      
251900                                 TO WS-TEMF-UTRSALDO                      
252000           MOVE WS-TEMF-RED-INVBAL                                        
252100                                 TO WS-TEMFSINF-TEXT                      
252200         END-IF                                                           
252300         MOVE MFS-RENSA-FAELT                                             
252400                             TO MOD-REPLACES                              
252500         IF ART-FLERS = JA                                                
252600            MOVE ART-IDARTNR TO W-IDARTNR-MIN7                            
252700                                W-IDARTNR-MAX7                            
252800            PERFORM IMS-GU-WDD7-ERSB01-MINMAX                             
252900            IF SEGMENT-FINNS                                              
253000               IF ERSB01-ERS-IDARTNR NOT = ZERO                           
253100                  MOVE ERSB01-ERS-IDARTNR                                 
253200                             TO MOD-REPLACES                              
253300                  INSPECT MOD-REPLACES REPLACING                          
253400                                        LEADING ZERO BY SPACE             
253500                  PERFORM IMS-GN-WDD7-ERSB01-MINMAX                       
253600                  IF SEGMENT-FINNS                                        
253700                  AND ERSB01-ERS-IDARTNR NOT = ZERO                       
253800                    MOVE 'VARIOUS'                                        
253900                             TO MOD-REPLACES                              
254000                  END-IF                                                  
254100               END-IF                                                     
254200            END-IF                                                        
254300         END-IF                                                           
254400                                                                          
254500         PERFORM S01-LAES-WDA5-ENTER                                      
254600         MOVE WS-RESTKVANT                                                
254700                             TO MOD-KVROS-NDC-CDC                         
254800                                                                          
254900         IF WS-KDERS                    >  0                              
255000           IF WS-KDERS                  <  29                             
255100             IF MOD-TEMFSFEL = SPACE                                      
255200                 MOVE ARTIKEL-ERSATT                                      
255300                               TO MED-IDMFSFEL                            
255400                 CALL WMEDKONV USING MED-WMEDAREA                         
255500                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
255600             END-IF                                                       
255700           ELSE                                                           
255800             MOVE ARTIKEL-UTGANGEN                                        
255900                             TO MED-IDMFSFEL                              
256000             CALL WMEDKONV USING MED-WMEDAREA                             
256100             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
256200           END-IF                                                         
256300         END-IF                                                           
256400                                                                          
256500*    --- BILDENS 2 'NÄRMST FÖRESTÅENDE ETA' RADER                         
256600*    --- PLUS SENASTE R32                                                 
256700         PERFORM IMS-GU-WDL601                                            
256800         IF SEGMENT-FINNS                                                 
256900           PERFORM IMS-GNP-WDL611                                         
257000           MOVE ZERO                   TO WS-TIINLINL                     
257100           PERFORM UNTIL SEGMENT-SAKNAS                                   
257200             IF WDL6-INL-IDDC = W-IDDC                                    
257300               MOVE WDL6-INL-TIINLINL                                     
257400                             TO TMP1-YYMMDD                               
257500               MOVE WS-TIINLINL                                           
257600                             TO TMP2-YYMMDD                               
257700               PERFORM WY2000P1                                           
257800               IF TMP1-YYMMDD > TMP2-YYMMDD                               
257900                 MOVE WDL6-INL-TIINLINL                                   
258000                             TO WS-TIINLINL                               
258100               END-IF                                                     
258200               IF (WDL6-INL-IDPTYP = '310' OR 'R31' OR 'R30')             
258300               AND(WDL6-INL-KDRT = ZERO)                                  
258400                 MOVE WDL6-INL-TIBERANK                                   
258500                             TO TMP1-YYMMDD                               
258600                 MOVE WS-SECOND-ETA                                       
258700                             TO TMP2-YYMMDD                               
258800                 PERFORM WY2000P1                                         
258900                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
259000                 AND WS-SECOND-ETA NOT = 999999                           
259100                   CONTINUE                                               
259200                 ELSE                                                     
259300                   MOVE WDL6-INL-TIBERANK                                 
259400                             TO TMP1-YYMMDD                               
259500                   MOVE WS-FIRST-ETA                                      
259600                             TO TMP2-YYMMDD                               
259700                   PERFORM WY2000P1                                       
259800                   IF (TMP1-YYMMDD < TMP2-YYMMDD                          
259900                   AND WS-FIRST-ETA NOT = 999999)                         
260000                   OR WS-FIRST-ETA = 999999                               
260100                     MOVE WS-FIRST-ETA TO WS-SECOND-ETA                   
260200                     MOVE WS-FIRST-ETA-QTY                                
260300                                       TO WS-SECOND-ETA-QTY               
260400                     MOVE ZERO         TO WS-FIRST-ETA                    
260500                                          WS-FIRST-ETA-QTY                
260600                     MOVE WDL6-INL-TIBERANK                               
260700                                       TO WS-FIRST-ETA                    
260800                     MOVE WDL6-INL-KVAVIS                                 
260900                                       TO WS-FIRST-ETA-QTY                
261000                   ELSE                                                   
261100                     IF WDL6-INL-TIBERANK = WS-FIRST-ETA                  
261200                       ADD WDL6-INL-KVAVIS                                
261300                                       TO WS-FIRST-ETA-QTY                
261400                     ELSE                                                 
261500                       IF WDL6-INL-TIBERANK = WS-SECOND-ETA               
261600                         ADD WDL6-INL-KVAVIS                              
261700                                       TO WS-SECOND-ETA-QTY               
261800                       ELSE                                               
261900                         MOVE WDL6-INL-TIBERANK                           
262000                                       TO WS-SECOND-ETA                   
262100                         MOVE WDL6-INL-KVAVIS                             
262200                                       TO WS-SECOND-ETA-QTY               
262300                       END-IF                                             
262400                     END-IF                                               
262500                   END-IF                                                 
262600                 END-IF                                                   
262700               END-IF                                                     
262800             END-IF                                                       
262900             PERFORM IMS-GNP-WDL611                                       
263000           END-PERFORM                                                    
263100           MOVE WS-TIINLINL            TO MOD-TIINLINL                    
263200           IF WS-FIRST-ETA < 999999                                       
263300           AND WS-FIRST-ETA > ZERO                                        
263400* -- VISA ETA ENDAST FÖR JAPAN/AUSTRALIEN TILLS SDC:ERNA FÅR EGEN         
263500* -- ETA BERÄKNING.                                                       
263600             IF W-IDDC  NOT = W-IDDC-B6                                   
263700                MOVE W-IDDC  TO W-IDDC-B6                                 
263800                PERFORM IMS-GU-WDB601                                     
263900             END-IF                                                       
264000             IF DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS                
264100             OR DCS-NDC-SA                                                
264200               MOVE WS-FIRST-ETA           TO MOD-TIBERANK(1)             
264300             ELSE                                                         
264400               MOVE MFS-RENSA-FAELT        TO MOD-TIBERANK(1)             
264500             END-IF                                                       
264600           ELSE                                                           
264700             MOVE MFS-RENSA-FAELT        TO MOD-TIBERANK(1)               
264800           END-IF                                                         
264900           IF WS-SECOND-ETA < 999999                                      
265000           AND WS-SECOND-ETA > ZERO                                       
265100             IF W-IDDC  NOT = W-IDDC-B6                                   
265200                MOVE W-IDDC  TO W-IDDC-B6                                 
265300                PERFORM IMS-GU-WDB601                                     
265400             END-IF                                                       
265500             IF DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS                
265600             OR DCS-NDC-SA                                                
265700               MOVE WS-SECOND-ETA          TO MOD-TIBERANK(2)             
265800             ELSE                                                         
265900               MOVE MFS-RENSA-FAELT        TO MOD-TIBERANK(2)             
266000             END-IF                                                       
266100           ELSE                                                           
266200             MOVE MFS-RENSA-FAELT        TO MOD-TIBERANK(2)               
266300           END-IF                                                         
266400           IF WS-FIRST-ETA-QTY > 0                                        
266500             MOVE WS-FIRST-ETA-QTY       TO MOD-KVAVIS-SUM(1)             
266600           ELSE                                                           
266700             MOVE MFS-RENSA-FAELT        TO MOD-KVAVIS-SUM(1)             
266800           END-IF                                                         
266900           IF WS-SECOND-ETA-QTY > 0                                       
267000             MOVE WS-SECOND-ETA-QTY      TO MOD-KVAVIS-SUM(2)             
267100           ELSE                                                           
267200             MOVE MFS-RENSA-FAELT        TO MOD-KVAVIS-SUM(2)             
267300           END-IF                                                         
267400         END-IF                                                           
267500       ELSE                                                               
267600         MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                              
267700         CALL WMEDKONV USING MED-WMEDAREA                                 
267800         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
267900         PERFORM MFS-RENSA-FAELT-IN                                       
268000         PERFORM MFS-RENSA-FAELT-UT                                       
268100       END-IF                                                             
268200     ELSE                                                                 
268300       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
268400       CALL WMEDKONV USING MED-WMEDAREA                                   
268500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
268600       PERFORM MFS-RENSA-FAELT-IN                                         
268700       PERFORM MFS-RENSA-FAELT-UT                                         
268800     END-IF                                                               
268900                                                                          
269000     MOVE MFS-RENSA-FAELT    TO MOD-REPL-BY                               
269100     PERFORM IMS-GU-WDD701                                                
269200     IF SEGMENT-FINNS                                                     
269300        PERFORM IMS-GNP-WDD702                                            
269400        IF SEGMENT-FINNS                                                  
269500           MOVE WDD702-IDARTNR-TILLK                                      
269600                             TO MOD-REPL-BY                               
269700           INSPECT MOD-REPL-BY REPLACING LEADING ZERO BY SPACE            
269800           PERFORM IMS-GNP-WDD702                                         
269900           IF SEGMENT-FINNS                                               
270000             MOVE 'VARIOUS'  TO MOD-REPL-BY                               
270100           END-IF                                                         
270200        END-IF                                                            
270300     END-IF                                                               
270400                                                                          
270500     MOVE SPACE              TO MOD-BEART                                 
270600     PERFORM IMS-GU-WDD301-BSEQ                                           
270700     IF SEGMENT-FINNS                                                     
270800       MOVE 'GB'  TO W-IDSKYLT                                            
270900       PERFORM IMS-GNP-WDD311                                             
271000       IF SEGMENT-FINNS                                                   
271100         MOVE WDD311-TEXT-BEART                                           
271200                             TO MOD-BEART                                 
271300       END-IF                                                             
271400     END-IF                                                               
271500                                                                          
271600     PERFORM IMS-GU-WDK711                                                
271700     IF SEGMENT-FINNS                                                     
271800       PERFORM FB-BEHANDLA-DC                                             
271900     END-IF                                                               
272000**To get Air Freight cost factor                                          
272100*WDB601 KEY - RECEIVING DC                                                
272200*WDB616 KEY - REFILL DC                                                   
272300     MOVE MOD-IDDC-UT   TO W-IDDC                                         
272400     MOVE MOD-IDDC-REF  TO W-IDDC-B616                                    
272401                                                                          
272410     PERFORM S06-CALC-AIR-COST                                            
272420                                                                          
273600*     ---- VISA DET DYRASTE ALTERNATIVET                                  
273700     IF WS-KR-VIKT-RED > WS-KR-VOLYM-RED                                  
273800        MOVE WS-KR-VIKT-RED TO MOD-AIR-COST-SEK                           
273900     ELSE                                                                 
274000        MOVE WS-KR-VOLYM-RED                                              
274100                            TO MOD-AIR-COST-SEK                           
274200     END-IF                                                               
274300                                                                          
274400**                                                                        
274500     INSPECT MOD-AIR-COST-SEK REPLACING LEADING ZERO BY SPACE             
274600                                                                          
274700     PERFORM IMS-GU-WDL711                                                
274800     IF SEGMENT-FINNS                                                     
274900       PERFORM IMS-GU-WDL411                                              
275000       IF SEGMENT-SAKNAS                                                  
275100******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
275200******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
275300         INITIALIZE OIHD-WDL411                                           
275400       END-IF                                                             
275500                                                                          
275600       IF DC-TIREFEFT > ZERO                                              
275700         MOVE DC-TIREFEFT                                                 
275800                             TO MOD-TIREFEFT                              
275900       ELSE                                                               
276000         MOVE MFS-RENSA-FAELT                                             
276100                             TO MOD-TIREFEFT                              
276200       END-IF                                                             
276300       PERFORM FA-BEHANDLA-ORDERINGGANG                                   
276400     END-IF                                                               
276500                                                                          
276600     MOVE W-IDDC           TO W-IDDC-2261                                 
276700     PERFORM IMS-GHU-WDGX2262                                             
276800     IF SEGMENT-FINNS                                                     
276900       MOVE 2262-TEREFMED(1:36)                                           
277000                             TO MOD-COMMENT(1)                            
277100       MOVE 2262-TEREFMED(37:36)                                          
277200                             TO MOD-COMMENT(2)                            
277300     ELSE                                                                 
277400       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT(1)                            
277500                                MOD-COMMENT(2)                            
277600     END-IF                                                               
277700                                                                          
277800     MOVE +1                 TO IX                                        
277900     PERFORM IMS-GU-WDN601                                                
278000     IF SEGMENT-FINNS                                                     
278100       PERFORM IMS-GNP-WDN611                                             
278200     END-IF                                                               
278300     PERFORM UNTIL SEGMENT-SAKNAS                                         
278400     OR              IX > 3                                               
278500       MOVE WDN6-KAT-BEMASTER (1:3)                                       
278600                             TO MOD-MODEL (IX)                            
278700       PERFORM IMS-GNP-WDN611                                             
278800       ADD +1                TO IX                                        
278900     END-PERFORM                                                          
279000                                                                          
279100     IF WS-IDDC-REF-K7 = SPACE                                            
279200       MOVE W-IDARTNR        TO W-IDARTNR-HSEQ                            
279300       MOVE ZERO             TO WS-KVAVIS                                 
279400       PERFORM IMS-GN-INLA11-W6D1SEQ                                      
279500       PERFORM UNTIL SEGMENT-SAKNAS                                       
279600         IF INLA-ART-IDDC   = WS-IDDC-REF                                 
279700         AND INLA-ART-IDLOPNRM = ZERO                                     
279800         AND INLA-ART-FLFEL  = NEJ                                        
279900             ADD INLA-ART-KVAVIS                                          
280000                               TO WS-KVAVIS                               
280100         END-IF                                                           
280200         PERFORM IMS-GN-INLA11-W6D1SEQ                                    
280300       END-PERFORM                                                        
280400     ELSE                                                                 
280500       MOVE WS-PREADV-QTY  TO WS-KVAVIS                                   
280600     END-IF                                                               
280700     MOVE WS-KVAVIS          TO MOD-KVAVIS                                
280800*                                                                         
280900*    CHECK SOURCING MARKET FOR THE PART                                   
281000     PERFORM FE-CHECK-ART-SOURCE                                          
281100*                                                                         
281200     IF INDATA-OK                                                         
281300     AND NOT MFS-UPDATE                                                   
281310     AND NOT MFS-UPD-V                                                    
281400       MOVE WS-TEMFSINF      TO MOD-TEMFSINF                              
281500     END-IF                                                               
281600*                                                                         
281700*     MFS-ALFA-FAELT-FEL FLYTTAS TILL ATTRIBUTET MOD-PURCHQTY-ATTR        
281800*     ENBART FÖR ATT FÅ UPPLYST FÄLT PLUS CURSORPLACERING                 
281900*     INTE FÖR ATT DET ÄR NÅGOT FEL                                       
282000*                                                                         
282100     IF WS-PURCHQTY > ZERO                                                
282200       MOVE MFS-ALFA-FAELT-FEL                                            
282300                             TO MOD-PURCHQTY-ATTR                         
282400     END-IF                                                               
282500*                                                                         
282600*     EFTER UPPDATERING SKA CURSOR STÅ PÅ FÄLTET IDARTNR-IN               
282700*                                                                         
282800     IF MFS-UPDATE OR MFS-UPD-V                                           
282900       MOVE MFS-ADD-SAETT-CURSOR                                          
283000                             TO MOD-IDARTNR-IN-ATTR                       
283100     END-IF                                                               
283200     .                                                                    
283300     EJECT                                                                
283400                                                                          
283500 FA-BEHANDLA-ORDERINGGANG     SECTION.                                    
283600                                                                          
283700     PERFORM FAA-HAMTA-VV-I-PER                                           
283800                                                                          
283900*    -- OBS 4-STÄLLIGT ÅR                                                 
284000     MOVE DAGENS-AAR         TO MOD-IAAR                                  
284100     SUBTRACT 1 FROM DAGENS-AAR GIVING WS-FOREG-AAR                       
284200     MOVE WS-FOREG-AAR       TO MOD-FOREG-AAR                             
284300                                                                          
284400     MOVE ZERO               TO WS-KVOI-SUM                               
284500     MOVE +1                 TO IX                                        
284600     PERFORM UNTIL IX > +12                                               
284700       ADD OIHD-KVOI (1, IX) TO WS-KVOI-SUM                               
284800       ADD OIHD-KVOI-REFILL (1, IX)                                       
284900                             TO WS-KVOI-SUM                               
285000       ADD +1                TO IX                                        
285100     END-PERFORM                                                          
285200     MOVE WS-KVOI-SUM        TO MOD-KVOI-FOREG-AAR                        
285300                                                                          
285400     MOVE ZERO               TO WS-KVOI-SUM                               
285500     MOVE +1                 TO IX                                        
285600                                                                          
285700*  --- HÄMTA ALLA VECKOR TOM FÖRRA PERIODEN                               
285800                                                                          
285900     IF DAGENS-PP > 1                                                     
286000        PERFORM UNTIL IX > WS-SISTA-V (12)                                
286100          ADD DC-KVOI-RULL (IX) TO WS-KVOI-SUM                            
286200          ADD DC-KVOI-REF-RULL (IX)                                       
286300                             TO WS-KVOI-SUM                               
286400          ADD +1             TO IX                                        
286500        END-PERFORM                                                       
286600     END-IF                                                               
286700     MOVE WS-KVOI-SUM        TO MOD-KVOI-IAAR                             
286800                                                                          
286900                                                                          
287000*  --- FYLL PÅ TABELLEN MED OI                                            
287100                                                                          
287200     MOVE +1                 TO IX                                        
287300     PERFORM UNTIL IX        >  12                                        
287400       MOVE WS-FORSTA-V(IX)  TO IX-VV                                     
287500       PERFORM UNTIL IX-VV   >  WS-SISTA-V(IX)                            
287600         ADD DC-KVOI-RULL(IX-VV)                                          
287700                             TO WS-KVOI(IX)                               
287800         ADD DC-KVOI-REF-RULL(IX-VV)                                      
287900                             TO WS-KVOI(IX)                               
288000         ADD +1              TO IX-VV                                     
288100       END-PERFORM                                                        
288200       ADD +1                TO IX                                        
288300     END-PERFORM                                                          
288400                                                                          
288500*    --- FLYTTA UT TABELLEN TILL MOD:EN,                                  
288600*    --- DE SENASTE TOLV PERIODERNA VISAS                                 
288700                                                                          
288800     MOVE +1                 TO IX                                        
288900     MOVE +1                 TO MOD-IX                                    
289000     PERFORM UNTIL IX > +12                                               
289100       MOVE WS-PER(IX)       TO MOD-TIPP(MOD-IX)                          
289200       INSPECT MOD-TIPP(MOD-IX) REPLACING LEADING ZERO BY SPACE           
289300       MOVE WS-FORSTA-V(IX)                                               
289400                             TO WS-FOM                                    
289500       MOVE WS-SISTA-V(IX)   TO WS-TOM                                    
289600       MOVE WS-FOM-TOM       TO MOD-TIVV-FOM-TOM(MOD-IX)                  
289700       MOVE WS-KVOI(IX)      TO MOD-KVOI-RULL(MOD-IX)                     
289800       MOVE ZERO             TO WS-KVOI(IX)                               
289900       ADD +1                TO IX                                        
290000                                MOD-IX                                    
290100     END-PERFORM                                                          
290200                                                                          
290300*    --- LÄGG UT KVOI FÖR AKTUELL PERIOD                                  
290400*                                                                         
290500                                                                          
290600     MOVE +1                 TO IX                                        
290700     MOVE ZERO               TO WS-KVOI-SUM                               
290800     PERFORM UNTIL IX > +5                                                
290900       ADD DC-KVOI-INNEV(IX)    TO WS-KVOI-SUM                            
291000       ADD DC-KVOI-PP-INNEV(IX) TO WS-KVOI-SUM                            
291100       ADD DC-KVOI-REF-INNEV(IX)                                          
291200                                TO WS-KVOI-SUM                            
291300       ADD +1                   TO IX                                     
291400     END-PERFORM                                                          
291500     MOVE WS-ANTAL-VECKOR       TO MOD-VECKA                              
291600     MOVE WS-KVOI-SUM           TO MOD-KVOI-INNEV                         
291700     .                                                                    
291800     EJECT                                                                
291900                                                                          
292000 FAA-HAMTA-VV-I-PER SECTION.                                              
292100                                                                          
292200     MOVE +1                 TO IX                                        
292300     MOVE DAGENS-PER         TO WS-TIAAPER                                
292400     IF TIAA = 00                                                         
292500       MOVE 99 TO TIAA                                                    
292600     ELSE                                                                 
292700       SUBTRACT 1 FROM TIAA                                               
292800     END-IF                                                               
292900                                                                          
293000*    --- TA FRAM HUR MÅNGA VECKOR DET VAR FÖREGÅENDE ÅR                   
293100     MOVE TIAA               TO WS-AAR                                    
293200     MOVE 53                 TO WS-VV                                     
293300     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
293400     MOVE TIAAVV             TO DAT-I-TIDATUM                             
293500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
293600                         DAT-O-TIDATUM DAT-KDSVAR                         
293700     IF DAT-KDSVAR-OK                                                     
293800       MOVE 53               TO WS-ANT-VV                                 
293900     ELSE                                                                 
294000       MOVE 52               TO WS-ANT-VV                                 
294100     END-IF                                                               
294200                                                                          
294300*    --- FYLL I VECKONR FÖR PERIODERNA                                    
294400                                                                          
294500     MOVE 'AARP  '           TO DAT-KDDATFORM                             
294600     MOVE TIAAPER            TO DAT-I-TIDATUM                             
294700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
294800                         DAT-O-TIDATUM DAT-KDSVAR                         
294900     IF DAT-KDSVAR-OK                                                     
295000       IF PER                = 1                                          
295100         MOVE 1              TO WS-PER(IX)                                
295200                                WS-FORSTA-V(IX)                           
295300       ELSE                                                               
295400         MOVE PER            TO WS-PER(IX)                                
295500         MOVE DAT-TIVV       TO WS-FORSTA-V(IX)                           
295600       END-IF                                                             
295700     ELSE                                                                 
295800       MOVE 'FELAKTIGT DATUM - DATKONV2' TO FELTEXT                       
295900       CALL FELLOG                                                        
296000     END-IF                                                               
296100                                                                          
296200     PERFORM UNTIL IX        >  12                                        
296300       ADD +1                TO PER                                       
296400       IF PER                >  12                                        
296500         ADD +1              TO TIAA                                      
296600         MOVE 01             TO PER                                       
296700       END-IF                                                             
296800                                                                          
296900       MOVE TIAAPER          TO DAT-I-TIDATUM                             
297000       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
297100                           DAT-O-TIDATUM DAT-KDSVAR                       
297200       IF DAT-KDSVAR-OK                                                   
297300         IF PER              =  1                                         
297400           MOVE WS-ANT-VV    TO WS-SISTA-V(IX)                            
297500         ELSE                                                             
297600           COMPUTE WS-SISTA-V(IX) = DAT-TIVV - 1                          
297700         END-IF                                                           
297800         ADD +1              TO IX                                        
297900         IF IX               <= 12                                        
298000           MOVE PER          TO WS-PER(IX)                                
298100           IF PER            =  1                                         
298200             MOVE +1         TO WS-FORSTA-V(IX)                           
298300           ELSE                                                           
298400             MOVE DAT-TIVV   TO WS-FORSTA-V(IX)                           
298500           END-IF                                                         
298600         END-IF                                                           
298700       ELSE                                                               
298800         MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                     
298900         CALL FELLOG                                                      
299000       END-IF                                                             
299100     END-PERFORM                                                          
299200*    --- OM VECKOR I FÖRSTA OCH SISTA PERIODEN ÖVERLAPPAR,                
299300*    --- RÄTTA I FÖRSTA (DVS DEN ÄLDSTA) PERIODEN.                        
299400     IF WS-FORSTA-V(1)        = WS-SISTA-V(12)                            
299500     OR WS-FORSTA-V(1)        = WS-SISTA-V(12) - 1                        
299600       COMPUTE WS-FORSTA-V(1) = WS-SISTA-V(12) + 1                        
299700     END-IF                                                               
299800     .                                                                    
299900     EJECT                                                                
300000                                                                          
300100 FB-BEHANDLA-DC    SECTION.                                               
300200                                                                          
300300     IF SW-HAEMTA-INPUT-FAELT-JA                                          
300400       MOVE SLAG-FLFLYG      TO WS-FLAGGA-FCD                             
300500       IF SLAG-FLFLYG = JA                                                
300600       AND MSGI-IDLAND-SPR = 'GB'                                         
300700         MOVE YES            TO MOD-FLAGGA-FCD                            
300800       ELSE                                                               
300900         MOVE SLAG-FLFLYG    TO MOD-FLAGGA-FCD                            
301000       END-IF                                                             
301100       MOVE SLAG-KVPB-REF    TO WS-RED-KVPB-REF                           
301200                                WS-KVPB-REF                               
301300       MOVE SLAG-KVPBREOI    TO WS-RED-KVPBREOI                           
301400                                WS-KVPBREOI                               
301500       COMPUTE WS-KVPB-TOT = WS-KVPB-REF + WS-KVPBREOI                    
301600       MOVE WS-RED-KVPB-REF  TO MOD-KVPB-REF                              
301700       MOVE WS-RED-KVPBREOI  TO MOD-KVPBREOI                              
301800       MOVE MFS-ADD-LAES-IN-FAELT                                         
301900                             TO MOD-KVPB-REF-ATTR                         
302000       IF DCS-NDC-CN OR NDC-JP-61                                         
302100         MOVE MFS-ADD-LAES-IN-FAELT  tO MOD-KVPBREOI-ATTR                 
302200       END-IF                                                             
302300       IF SLAG-FLREFBEO = JA                                              
302400       AND MSGI-IDLAND-SPR = 'GB'                                         
302500         MOVE YES            TO MOD-FLREFBEO                              
302600       ELSE                                                               
302700         MOVE SLAG-FLREFBEO  TO MOD-FLREFBEO                              
302800       END-IF                                                             
302900       MOVE MFS-ADD-LAES-IN-FAELT                                         
303000                             TO MOD-FLREFBEO-ATTR                         
303100       MOVE SLAG-KVREFBER  TO MOD-KVREFBER                                
303200       MOVE SLAG-KVREFPKT  TO MOD-KVREFPKT                                
303300       PERFORM IMS-GU-WDK727                                              
303400       IF SEGMENT-FINNS                                                   
303500         IF PROG-KVPB-JUST(1) > ZERO                                      
303600           MOVE MFS-ADD-LYS-UPP-FAELT                                     
303700                      TO MOD-KVREFPKT-ATTR                                
303800         END-IF                                                           
303900       END-IF                                                             
304000     END-IF                                                               
304100     IF SLAG-TIORDREG > ZERO                                              
304200       MOVE SLAG-TIORDREG    TO MOD-TIORDREG                              
304300     ELSE                                                                 
304400       MOVE MFS-RENSA-FAELT  TO MOD-TIORDREG                              
304500     END-IF                                                               
304600     MOVE SLAG-ADLAGOMR    TO MOD-ADLAGOMR                                
304700     MOVE SLAG-ADGANG      TO MOD-ADGANG                                  
304800     MOVE SLAG-ADPLATS     TO MOD-ADPLATS                                 
304900     MOVE SLAG-FLWILSON    TO MOD-FLWILSON                                
305000     IF SLAG-FLFLYG = JA                                                  
305100     AND MSGI-IDLAND-SPR = 'GB'                                           
305200       MOVE YES            TO MOD-FLAGGA-FCD                              
305300     ELSE                                                                 
305400       MOVE SLAG-FLFLYG    TO MOD-FLAGGA-FCD                              
305500     END-IF                                                               
305600     MOVE SLAG-FLORDSP-EJRO                                               
305700                           TO MOD-FLORDSP-EJRO                            
305800                                                                          
305900     PERFORM IMS-GU-WDK712                                                
306000     IF SEGMENT-FINNS AND LART-DAPUBL > ZERO                              
306100       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
306200       MOVE LART-DAPUBL(3:6)   TO DAT-I-TIDATUM                           
306300                                                                          
306400       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
306500                     DAT-O-TIDATUM DAT-KDSVAR                             
306600                                                                          
306700       IF DAT-KDSVAR-OK                                                   
306800         MOVE DAT-TIAAVVD    TO MOD-DAPUBL                                
306900       ELSE                                                               
307000         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
307100         DELIMITED BY SIZE INTO FELTEXT                                   
307200         CALL FELLOG                                                      
307300       END-IF                                                             
307400     END-IF                                                               
307500                                                                          
307600     IF SEGMENT-FINNS                                                     
307700       MOVE LART-FLREFERAL     TO WS-FLREFERAL                            
307800     ELSE                                                                 
307900       MOVE NEJ                TO WS-FLREFERAL                            
308000     END-IF                                                               
308100     IF SLAG-FLORDSP = JA                                                 
308200       IF WS-FLREFERAL = JA                                               
308300         MOVE 'T'            TO MOD-FREEZECODE                            
308400       ELSE                                                               
308500         MOVE 'F'            TO MOD-FREEZECODE                            
308600       END-IF                                                             
308700     ELSE                                                                 
308800       IF SLAG-FLSPBULK = JA                                              
308900         IF WS-FLREFERAL = JA                                             
309000           MOVE 'T'          TO MOD-FREEZECODE                            
309100         ELSE                                                             
309200           MOVE 'P'          TO MOD-FREEZECODE                            
309300         END-IF                                                           
309400       ELSE                                                               
309500         IF WS-FLREFERAL = JA                                             
309600           MOVE 'R'          TO MOD-FREEZECODE                            
309700         ELSE                                                             
309800           MOVE '-'          TO MOD-FREEZECODE                            
309900         END-IF                                                           
310000       END-IF                                                             
310100     END-IF                                                               
310200                                                                          
310300     MOVE SLAG-IDDC-REF      TO MOD-IDDC-REF                              
310400                                WS-IDDC-REF                               
310500     COMPUTE WS-BALANCE ROUNDED = SLAG-KVLS -                             
310600                 SLAG-KVROS-BULK - SLAG-KVROS-DAG -                       
310700                 SLAG-KVOKS-BULK - SLAG-KVOKS-DAG                         
310800     MOVE WS-BALANCE         TO MOD-BALANCE                               
310900     COMPUTE WS-ORDERED ROUNDED = SLAG-KVBEART + SLAG-KVAKS-PAV           
311000     MOVE WS-ORDERED         TO MOD-ORDERED                               
311100     COMPUTE WS-KVROS ROUNDED = SLAG-KVROS-BULK + SLAG-KVROS-DAG          
311200     MOVE WS-KVROS           TO MOD-KVROS                                 
311300     COMPUTE WS-SUPERWEEK ROUNDED =                                       
311400           (SLAG-KVLS +                                                   
311500            SLAG-KVAKS-SDC + SLAG-KVAKS-PAV + SLAG-KVBEART -              
311600            SLAG-KVOKS-BULK - SLAG-KVOKS-DAG -                            
311700            SLAG-KVROS-BULK - SLAG-KVROS-DAG +                            
311800            WS-PURCHQTY-SIM ) /                                           
311900           (WS-KVPB-TOT / 4.33)                                           
312000                  ON SIZE ERROR                                           
312100                      MOVE +999   TO WS-SUPERWEEK                         
312200     END-COMPUTE                                                          
312300                                                                          
312400     IF WS-SUPERWEEK > +999                                               
312500       MOVE +999             TO WS-SUPERWEEK                              
312600     END-IF                                                               
312700                                                                          
312800     IF WS-SUPERWEEK < ZERO                                               
312900       MOVE ZERO             TO WS-SUPERWEEK                              
313000     END-IF                                                               
313100                                                                          
313200     IF WS-BALANCE > ZERO                                                 
313300     OR WS-KVPB-TOT > ZERO                                                
313400       CONTINUE                                                           
313500     ELSE                                                                 
313600       MOVE ZERO             TO WS-SUPERWEEK                              
313700     END-IF                                                               
313800                                                                          
313900     MOVE WS-SUPERWEEK                                                    
314000                             TO MOD-SUPERWEEK                             
314100     IF SLAG-KDLEVSP > ZERO                                               
314200       MOVE 'F'              TO MOD-QUALBLOCK                             
314300     ELSE                                                                 
314400       IF SLAG-KVSPARR-KVAL > ZERO                                        
314500         MOVE 'P'            TO MOD-QUALBLOCK                             
314600       ELSE                                                               
314700         MOVE '-'            TO MOD-QUALBLOCK                             
314800       END-IF                                                             
314900     END-IF                                                               
315000                                                                          
315100     MOVE +1                 TO IX                                        
315200     MOVE NEJ                TO SW-SEASON                                 
315300     PERFORM UNTIL IX > +12                                               
315400     OR SW-SEASON-JA                                                      
315500       IF SLAG-RESEASON (IX) NOT = +1.00                                  
315600         MOVE JA             TO SW-SEASON                                 
315700       END-IF                                                             
315800       ADD +1                TO IX                                        
315900     END-PERFORM                                                          
316000     IF SW-SEASON = JA                                                    
316100       MOVE 'Y'              TO MOD-SEASON                                
316200     ELSE                                                                 
316300       MOVE 'N'              TO MOD-SEASON                                
316400     END-IF                                                               
316500                                                                          
316600     MOVE SLAG-KVROS-DAG     TO MOD-CRIT-KVROS-NDC-CDC                    
316700                                                                          
316800     IF SLAG-FLPB-FLYTT = JA                                              
316900       MOVE 'REPL'           TO WS-TEMFSINF-NDC                           
317000     END-IF                                                               
317100                                                                          
317200     IF WS-REF-KDREFTXT > ZERO                                            
317300       MOVE +1               TO IX                                        
317400       PERFORM UNTIL IX > REF-TEXT-TABMAX                                 
317500       OR WS-REF-KDREFTXT = REF-TEXT-KDREFTEXT (IX)                       
317600         ADD +1              TO IX                                        
317700       END-PERFORM                                                        
317800       IF IX > REF-TEXT-TABMAX                                            
317900         CONTINUE                                                         
318000       ELSE                                                               
318100         IF REF-TEXT-KDREFTEXT (IX) = 70                                  
318200           CONTINUE                                                       
318300         ELSE                                                             
318400             MOVE REF-TEXT (IX) TO WS-TEMFSINF-NDC                        
318500         END-IF                                                           
318600       END-IF                                                             
318700     END-IF                                                               
318800     MOVE SLAG-KVAKS-SDC       TO MOD-KVAKS-SDC                           
318900     .                                                                    
319000     EJECT                                                                
319100                                                                          
319200 FD-HAEMTA-KVPB SECTION.                                                  
319300                                                                          
319400     COMPUTE WS-KVPB ROUNDED =                                            
319500               (CLAG-KVPB-SATS + CLAG-KVPB-SEP + CLAG-KVPB-TPO)           
319600                                                                          
319700     PERFORM IMS-GN-WDB601                                                
319800     PERFORM UNTIL SEGMENT-SAKNAS                                         
319900        IF NEXT-DCS-CDC OR NEXT-DCS-DDC                                   
320000           CONTINUE                                                       
320100        ELSE                                                              
320200           MOVE NEXT-DCS-IDDC TO W-IDDC                                   
320300           PERFORM IMS-GU-WDK711                                          
320400           IF SEGMENT-FINNS                                               
320500*  ----  SLAG-KVPB ÄR PROGNOS FÖR EN MÅNADSPERIOD                         
320600              ADD SLAG-KVPB-REF   TO WS-KVPB                              
320700           END-IF                                                         
320800        END-IF                                                            
320900        PERFORM IMS-GN-WDB601                                             
321000     END-PERFORM                                                          
321100     .                                                                    
321200     EJECT                                                                
321300 FE-CHECK-ART-SOURCE SECTION.                                             
321400                                                                          
321500*  ----  CHECK MARKET WHERE PART IS SOURCED                               
321600*  ----  FOR CHECKING SOURCE MARKET CALL W271UTIL WITH KDCAL 001          
321700                                                                          
321800     INITIALIZE  UTIL-W271UTIL                                            
321900     MOVE 001                   TO UTIL-KDCALL                            
322000     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
322100                                                                          
322200     CALL W271UTIL USING UTIL-W271UTIL                                    
322300                         UTIL-WDK6-PCB                                    
322400                         UTIL-WDK7-PCB                                    
322500                         UTIL-WDB6-PCB                                    
322600                                                                          
322700     IF UTIL-KDSVAR-OK                                                    
322800        MOVE UTIL-TEXT          TO WS-TEMFSINF-SOURCE                     
322900     END-IF                                                               
323000     .                                                                    
323100     EJECT                                                                
323200 H-UPD-WDK6-WDK7 SECTION.                                                 
323300                                                                          
323400     PERFORM IMS-GU-WDK611                                                
323500     PERFORM IMS-GHU-WDK711                                               
323600     IF SEGMENT-FINNS                                                     
323700       PERFORM HA-UPPDATERA-WDK7                                          
323800       PERFORM IMS-REPL-WDK711                                            
323900     ELSE                                                                 
324000       IF WS-KVPB-TOT > ZERO                                              
324100         PERFORM HB-NY-WDK7                                               
324200         PERFORM IMS-GHU-WDK711                                           
324300         IF SEGMENT-FINNS                                                 
324400           PERFORM HA-UPPDATERA-WDK7                                      
324500           PERFORM IMS-REPL-WDK711                                        
324600         END-IF                                                           
324700       END-IF                                                             
324800     END-IF                                                               
324900                                                                          
325000***  CALL SECTION BELOW TO UPDATE REFILLING PT & QTY TO WDK7              
325100*                                                                         
325200     PERFORM HD-UPD-REFL1-OUTPUT                                          
325300                                                                          
325400     IF KVPB-PLAN-UPD-JA                                                  
325500        PERFORM HC-RECALCULATE-PBPLAN                                     
325600     END-IF                                                               
325700     .                                                                    
325800     EJECT                                                                
325900                                                                          
326000 HA-UPPDATERA-WDK7 SECTION.                                               
326100                                                                          
326200     IF (MID-COMMENT-1  NOT = ALL '+')                                    
326300     OR (MID-COMMENT-2  NOT = ALL '+')                                    
326400                                                                          
326500       MOVE W-IDDC         TO W-IDDC-2261                                 
326600       PERFORM IMS-GHU-WDGX2262                                           
326700       IF MID-COMMENT-1 NOT = ALL '+'                                     
326800         MOVE MID-COMMENT-1 TO 2262-TEREFMED (1:36)                       
326900       ELSE                                                               
327000         MOVE SPACE         TO 2262-TEREFMED (1:36)                       
327100       END-IF                                                             
327200       IF MID-COMMENT-2 NOT = ALL '+'                                     
327300         MOVE MID-COMMENT-2 TO 2262-TEREFMED (37:36)                      
327400         MOVE SPACE         TO 2262-TEREFMED (73:3)                       
327500       ELSE                                                               
327600         MOVE SPACE         TO 2262-TEREFMED (37:39)                      
327700       END-IF                                                             
327800                                                                          
327900       IF SEGMENT-SAKNAS                                                  
328000         MOVE W-IDARTNR       TO 2262-IDARTNR                             
328100         PERFORM IMS-ISRT-WDGX2262                                        
328200       ELSE                                                               
328300         PERFORM IMS-REPL-WDGX2262                                        
328400       END-IF                                                             
328500     END-IF                                                               
328600* -- OM MAN HÖJER PB SÄTTS FLREFNYO TILL NEJ SÅVIDA MAN                   
328700* -- INTE SAMTIDIGT NOLLAT ETT FÖRSLAG. DÅ BEHÅLLER                       
328800* -- FLREFNYO SIN TIDIGARE STATUS                                         
328900     IF (WS-KVPB-TOT > ZERO)                                              
329000     OR (MID-FLAGGA-FCD NOT = ALL '+')                                    
329100       IF WS-KVPB-REF > SLAG-KVPB-REF                                     
329200       OR WS-KVPBREOI > SLAG-KVPBREOI                                     
329300          MOVE NEJ           TO SLAG-FLREFNYO                             
329400       END-IF                                                             
329500     END-IF                                                               
329600                                                                          
329700     IF WS-FLAGGA-FCD NOT = SPACE                                         
329800       MOVE WS-FLAGGA-FCD    TO SLAG-FLFLYG                               
329900*To exclude LOCK logic for the new buyer steering rule                    
330000       IF DCS-KDDCSTYR-BUY > ZERO                                         
330100*                                                                         
330200          IF WS-FLAGGA-FCD = JA                                           
330300            MOVE 1              TO SLAG-IDREFTAB                          
330400            MOVE 'J'            to SLAG-FLTABUPD                          
330500          ELSE                                                            
330600            IF WS-FLAGGA-FCD = NEJ                                        
330610            OR WS-FLAGGA-FCD = 'S'                                        
330700              MOVE ZERO         TO SLAG-IDREFTAB                          
330800              MOVE 'N'          to SLAG-FLTABUPD                          
330900            END-IF                                                        
331000          END-IF                                                          
331100       END-IF                                                             
331200     END-IF                                                               
331300                                                                          
331400     IF NY-WDK7-JA                                                        
331500       CONTINUE                                                           
331600     ELSE                                                                 
331700       IF MID-FLREFBEO = ALL '+'                                          
331800          CONTINUE                                                        
331900       ELSE                                                               
332000          IF MID-FLREFBEO = YES                                           
332100            MOVE JA            TO SLAG-FLREFBEO                           
332200          ELSE                                                            
332300            MOVE MID-FLREFBEO                                             
332400                                  TO SLAG-FLREFBEO                        
332500          END-IF                                                          
332600       END-IF                                                             
332700     END-IF                                                               
332800                                                                          
332900     IF WS-KVPB-REF = SLAG-KVPB-REF                                       
333000       CONTINUE                                                           
333100     ELSE                                                                 
333200       MOVE DAGENS-DATUM     TO SLAG-TIREFMPB                             
333300       MOVE JA               TO SW-KVPB-SEP                               
333400     END-IF                                                               
333500                                                                          
333600     IF WS-KVPBREOI = SLAG-KVPBREOI                                       
333700       CONTINUE                                                           
333800     ELSE                                                                 
333900       MOVE DAGENS-DATUM     TO SLAG-TIPBREOI                             
334000       MOVE JA               TO SW-KVPB-SEP                               
334100     END-IF                                                               
334200                                                                          
334300     MOVE WS-KVPB-REF                                                     
334400                             TO SLAG-KVPB-REF                             
334500                                SLAG-KVPB-HIST                            
334600     MOVE WS-KVPBREOI                                                     
334700                             TO SLAG-KVPBREOI                             
334800                                SLAG-KVPBREOI-HIST                        
334900                                                                          
335000     IF SW-KVPB-SEP-JA                                                    
335100       IF SLAG-IDDC-REF = WC-CDC-SE                                       
335200*****   IF REFILLED FROM CDC AND WE CHANGE FORECAST WE NEED TO            
335300*****   UDPATE CREF-KVPB-PLAN IF PART IS REFILLED TO CDC FROM             
335400*****   ANOTHER DC                                                        
335500         IF CLAG-IDDC-REF NOT = SPACE                                     
335600            MOVE JA          TO SW-KVPB-PLAN                              
335700         END-IF                                                           
335800       END-IF                                                             
335900     END-IF                                                               
336000     .                                                                    
336100     EJECT                                                                
336200                                                                          
336300 HB-NY-WDK7 SECTION.                                                      
336400                                                                          
336500     MOVE JA           TO NY-WDK7-SW                                      
336600                                                                          
336700     MOVE ALL '+'      TO WDK7-W005WDK7                                   
336800     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
336900     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
337000     MOVE IDDC-WS      TO WDK7-IDDC-KFB                                   
337100                          WDK7-IDDC                                       
337200     MOVE WS-KVPB-REF  TO WDK7-KVPB-REF                                   
337300     MOVE WS-KVPBREOI  TO WDK7-KVPBREOI                                   
337400     MOVE AKTIV        TO WDK7-KDREFSTA                                   
337500     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
337600                                       WDK6-PCB WDK7-PCB                  
337700                                                                          
337800     IF MSGI-IDDC-KEY NOT = W-IDDC-B6                                     
337900        MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                   
338000        PERFORM IMS-GU-WDB601                                             
338100     END-IF                                                               
338200     IF WS-KVPB-REF > ZERO                                                
338300       IF CLAG-IDDC-REF NOT = SPACE                                       
338400          MOVE JA            TO SW-KVPB-PLAN                              
338500       END-IF                                                             
338600     END-IF                                                               
338700     .                                                                    
338800     EJECT                                                                
338900                                                                          
339000 HC-RECALCULATE-PBPLAN SECTION.                                           
339100                                                                          
339200*--- THIS SECTION CALLS UTILITY PROGRAM W272UTUP TO UPDATE                
339300*--- KVPB-PLAN IN WDK6. ITS MANDATORY TO CALL THE UTILITY                 
339400*--- USING CORRECT KDCALL VALUE.                                          
339500                                                                          
339600     INITIALIZE W272-UTUP-W272UTUP                                        
339700     MOVE 1                     TO W272-UTUP-KDCALL                       
339800     MOVE W-IDARTNR             TO W272-UTUP-IDARTNR                      
339900     MOVE W-IDDC                TO W272-UTUP-IDDC                         
340000     IF CLAG-IDDC-REF = SPACE                                             
340100       CALL FELLOG                                                        
340200     ELSE                                                                 
340300       MOVE CLAG-IDDC-REF       TO W272-UTUP-IDDC-REF                     
340400     END-IF                                                               
340500                                                                          
340600     CALL W272UTUP USING W272-UTUP-W272UTUP                               
340700                         U2-WDK6-PCB                                      
340800                         U2-WDB6-PCB                                      
340900                         U2-PBTO-W222-WDK6-PCB                            
341000                         U2-PBTO-W222-WDK7-PCB                            
341100                         U2-PBTO-W222-ARTM-PCB                            
341200                         U2-PBTO-W222-REFL1-2501-PCB                      
341300                         U2-PBTO-W222-REFL1-WDB6R-PCB                     
341400                         U2-PBTO-W222-REFL1-WDK7R-PCB                     
341500                         U2-PBTO-W222-WDB6-PCB                            
341600                         U2-PBTO-W222-WDD7-PCB                            
341700                         U2-PBTO-W222-WDK7E-PCB                           
341800                         U2-PBTO-W222-REFL1-UTIL-K6-PCB                   
341900                         U2-PBTO-W222-REFL1-UTIL-K7-PCB                   
342000                         U2-PBTO-W222-REFL1-UTIL-B6-PCB                   
342100                         U2-PBTO-W222-UTUP1-WDK7-PCB                      
342200                         U2-PBTO-W222-UTUP1-WDB6-PCB                      
342300                         U2-PBTO-W222-UTUP1-UTIL-K6-PCB                   
342400                         U2-PBTO-W222-UTUP1-UTIL-K7-PCB                   
342500                         U2-PBTO-W222-UTUP1-UTIL-B6-PCB                   
342600                         U2-REFL2-2501-PCB                                
342700                         U2-REFL2-WDB6-PCB                                
342800                         U2-REFL2-UTIL-WDK6-PCB                           
342900                         U2-REFL2-UTIL-WDK7-PCB                           
343000                         U2-REFL2-UTIL-WDB6-PCB                           
343100                         U2-W222-WDK6-PCB                                 
343200                         U2-W222-WDK7-PCB                                 
343300                         U2-W222-ARTM-PCB                                 
343400                         U2-W222-2501-PCB                                 
343500                         U2-W222-WDB6R-PCB                                
343600                         U2-W222-WDK7R-PCB                                
343700                         U2-W222-WDB6-PCB                                 
343800                         U2-W222-WDD7-PCB                                 
343900                         U2-W222-WDK7E-PCB                                
344000                         U2-W222-UTIL-WDK6-PCB                            
344100                         U2-W222-UTIL-WDK7-PCB                            
344200                         U2-W222-UTIL-WDB6-PCB                            
344300                         U2-W222-UTUP1-WDK7-PCB                           
344400                         U2-W222-UTUP1-WDB6-PCB                           
344500                         U2-W222-UTUP1-UTIL-WDK6-PCB                      
344600                         U2-W222-UTUP1-UTIL-WDK7-PCB                      
344700                         U2-W222-UTUP1-UTIL-WDB6-PCB                      
344800                                                                          
344900     IF W272-UTUP-KDSVAR-OK                                               
345000        CONTINUE                                                          
345100     ELSE                                                                 
345200        DISPLAY 'W272UTUP-ERROR :' W272-UTUP-TEXT                         
345300        CALL FELLOG                                                       
345400     END-IF                                                               
345500     .                                                                    
345600     EJECT                                                                
345700                                                                          
345800 HD-UPD-REFL1-OUTPUT SECTION.                                             
345900                                                                          
346000***  THIS SECTION CALLS W271REFL TO GET THE CALULATED                     
346100***  REFILLING POINT AND REFILLING QUANTITY                               
346200***  CAN BE USED FOR UPDATE ON WDK7 OR JUST SIMULATION                    
346300***                                                                       
346400     PERFORM IMS-GHU-WDK711                                               
346500     IF SEGMENT-FINNS                                                     
346600***     FOR UPDATES SIMULATION FLAG FLSIM SHOULD BE NO                    
346700        MOVE NEJ                  TO WS-FLSIM                             
346800        PERFORM S90-CALL-W271REFL                                         
346900*                                                                         
347000        MOVE SLAG-TIREFPKT        TO TMP1-YYMMDD                          
347100        MOVE DAGENS-DATUM         TO TMP2-YYMMDD                          
347200        PERFORM WY2000P1                                                  
347300        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
347400                                                                          
347500*--- INGEN UPPDATERING AV KVREFPKT PGA MANUELLT DATUM ÄR SATT             
347600          CONTINUE                                                        
347700        ELSE                                                              
347800          MOVE W271-REFL-KVREFPKT TO SLAG-KVREFPKT                        
347900        END-IF                                                            
348000                                                                          
348100        MOVE SLAG-TIREFPAF        TO TMP1-YYMMDD                          
348200        MOVE DAGENS-DATUM         TO TMP2-YYMMDD                          
348300        PERFORM WY2000P1                                                  
348400        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
348500                                                                          
348600*--- INGEN UPPDATERING AV KVREFBER PGA MANUELLT DATUM ÄR SATT             
348700          CONTINUE                                                        
348800        ELSE                                                              
348900          MOVE W271-REFL-KVREFBER TO SLAG-KVREFBER                        
349000        END-IF                                                            
349100                                                                          
349200        MOVE W271-REFL-KVREFOVL   TO SLAG-KVREFOVL                        
349300                                                                          
349400        IF  SLAG-KDREFSTA          = PASSIV                               
349500        AND (SLAG-KVPB-REF         > ZERO                                 
349600          OR SLAG-KVPBREOI         > ZERO)                                
349700          MOVE AKTIV              TO SLAG-KDREFSTA                        
349800          MOVE DAGENS-DATUM       TO SLAG-TIREFSTA                        
349900        END-IF                                                            
350000*                                                                         
350100        PERFORM IMS-REPL-WDK711                                           
350200     END-IF                                                               
350300     .                                                                    
350400     EJECT                                                                
350500                                                                          
350600 I-KOLLA-INPUT SECTION.                                                   
350700                                                                          
350800     PERFORM IMS-GU-WDK611                                                
350900     IF SEGMENT-FINNS                                                     
350910       MOVE CLAG-VKART         TO WS-VKART                                
350920       MOVE CLAG-VLARTNTO      TO WS-VLARTNTO                             
350930                                                                          
351000       MOVE NEJ              TO SW-KTRL-ERS                               
351100       IF MID-PURCHQTY = ALL '+'                                          
351200         CONTINUE                                                         
351300       ELSE                                                               
351400         INSPECT MID-PURCHQTY                                             
351500                        REPLACING LEADING SPACE BY ZERO                   
351600         IF MID-PURCHQTY > ZERO                                           
351700           MOVE JA         TO SW-KTRL-ERS                                 
351800         END-IF                                                           
351900       END-IF                                                             
352000*                                                                         
352100       PERFORM IMS-GU-WDK712                                              
352101                                                                          
352102       IF SEGMENT-FINNS                                                   
352103         IF LART-VKART > 0                                                
352104            MOVE LART-VKART    TO WS-VKART                                
352105         END-IF                                                           
352106         IF LART-VLARTNTO > 0                                             
352107            MOVE LART-VLARTNTO TO WS-VLARTNTO                             
352108         END-IF                                                           
352109       END-IF                                                             
352110                                                                          
352220       MOVE CLAG-KDERS     TO WS-KDERS-INOM                               
352300*                                                                         
352400       IF SW-KTRL-ERS-JA                                                  
352500        IF CLAG-KDERS               < 10                                  
352600          IF CLAG-PRARTSTD = ZERO                                         
352700            MOVE PRIS-SAKNAS                                              
352800                           TO MED-IDMFSFEL                                
352900            CALL WMEDKONV USING MED-WMEDAREA                              
353000            MOVE MED-TEMFSFEL                                             
353100                           TO MOD-TEMFSFEL                                
353200            MOVE NEJ          TO INDATA-SW                                
353300          END-IF                                                          
353400        ELSE                                                              
353500          IF NDC AND WS-IDDC-REF NOT = '11'                               
353600             IF CLAG-KDERS NOT = +52                                      
353700                 PERFORM S04-GET-WDK711-SEND-DC                           
353800                 IF WS-KVDISP-SEND-DC > 0                                 
353900                     CONTINUE                                             
354000                 ELSE                                                     
354100                     MOVE ARTIKEL-ERSATT                                  
354200                                      TO MED-IDMFSFEL                     
354300                     CALL WMEDKONV USING MED-WMEDAREA                     
354400                     MOVE MED-TEMFSFEL                                    
354500                                      TO MOD-TEMFSFEL                     
354600                     MOVE NEJ         TO INDATA-SW                        
354700                 END-IF                                                   
354800             ELSE                                                         
354900                 MOVE ARTIKEL-UTGANGEN                                    
355000                                      TO MED-IDMFSFEL                     
355100                 CALL WMEDKONV USING MED-WMEDAREA                         
355200                 MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                     
355300                 MOVE NEJ             TO INDATA-SW                        
355400             END-IF                                                       
355500          ELSE                                                            
355600             IF CLAG-KDERS              = +29 OR +52                      
355700               MOVE ARTIKEL-UTGANGEN                                      
355800                                TO MED-IDMFSFEL                           
355900               CALL WMEDKONV USING MED-WMEDAREA                           
356000               MOVE MED-TEMFSFEL                                          
356100                                TO MOD-TEMFSFEL                           
356200             ELSE                                                         
356300               MOVE ARTIKEL-ERSATT                                        
356400                                TO MED-IDMFSFEL                           
356500               CALL WMEDKONV USING MED-WMEDAREA                           
356600               MOVE MED-TEMFSFEL                                          
356700                                TO MOD-TEMFSFEL                           
356800             END-IF                                                       
356900             MOVE NEJ           TO INDATA-SW                              
357000          END-IF                                                          
357100        END-IF                                                            
357200       END-IF                                                             
357300     ELSE                                                                 
357400       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
357500       CALL WMEDKONV USING MED-WMEDAREA                                   
357600       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
357700       MOVE NEJ              TO INDATA-SW                                 
357800     END-IF                                                               
357900                                                                          
358000     IF MID-KVPB-REF = ALL '+'                                            
358100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPB-REF-ATTR                    
358200     ELSE                                                                 
358300*      KONVERTERA IFRÅN FRITT FORMAT TILL 6 + 1 DECIMAL                   
358400       MOVE MID-KVPB-REF   TO DEC-IDFRIDATA                               
358500                              MOD-KVPB-REF                                
358600       MOVE 6              TO DEC-KVHELTAL                                
358700       MOVE 1              TO DEC-KVDECIMAL                               
358800       CALL WDECEDIT USING DEC-WDECAREA                                   
358900       IF DEC-KDSVAR-OK                                                   
359000         MOVE MFS-ADD-LAES-IN-FAELT                                       
359100                           TO MOD-KVPB-REF-ATTR                           
359200         MOVE DEC-IDEDITDATA                                              
359300                           TO WS-RED-KVPB-REF                             
359400                              WS-KVPB-REF                                 
359500         MOVE WS-RED-KVPB-REF                                             
359600                           TO MOD-KVPB-REF                                
359700       ELSE                                                               
359800         MOVE MED-3        TO MOD-TEMFSFEL                                
359900         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
360000                           TO MOD-KVPB-REF-ATTR                           
360100         MOVE NEJ          TO INDATA-SW                                   
360200         MOVE MID-KVPB-REF TO MOD-KVPB-REF                                
360300       END-IF                                                             
360400     END-IF                                                               
360500                                                                          
360600     IF MID-KVPBREOI = ALL '+'                                            
360700       IF NDC-CN OR NDC-JP-61                                             
360800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPBREOI-ATTR                  
360900       END-IF                                                             
361000     ELSE                                                                 
361100       MOVE MID-KVPBREOI   TO DEC-IDFRIDATA                               
361200                              MOD-KVPBREOI                                
361300       MOVE 6              TO DEC-KVHELTAL                                
361400       MOVE 1              TO DEC-KVDECIMAL                               
361500       CALL WDECEDIT USING DEC-WDECAREA                                   
361600       IF DEC-KDSVAR-OK                                                   
361700         MOVE MFS-ADD-LAES-IN-FAELT                                       
361800                           TO MOD-KVPBREOI-ATTR                           
361900         MOVE DEC-IDEDITDATA                                              
362000                           TO WS-KVPBREOI                                 
362100                             WS-RED-KVPBREOI                              
362200         MOVE WS-RED-KVPBREOI                                             
362300                           TO MOD-KVPBREOI                                
362400       ELSE                                                               
362500         MOVE ERR-CORR-HILITE-FLDS                                        
362600                            TO MED-IDMFSFEL                               
362700         CALL WMEDKONV USING MED-WMEDAREA                                 
362800         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
362900         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
363000                           TO MOD-KVPBREOI-ATTR                           
363100         MOVE NEJ          TO INDATA-SW                                   
363200         MOVE MID-KVPBREOI TO MOD-KVPBREOI                                
363300       END-IF                                                             
363400     END-IF                                                               
363500     COMPUTE WS-KVPB-TOT = WS-KVPB-REF + WS-KVPBREOI                      
363600                                                                          
363700     IF WS-KVPB-TOT > ZERO                                                
363800       PERFORM IMS-GU-WDK711                                              
363900       IF SEGMENT-FINNS                                                   
364000         IF WS-KVPB-REF > ZERO                                            
364100           MOVE MFS-ADD-LAES-IN-FAELT                                     
364200                             TO MOD-KVPB-REF-ATTR                         
364300         END-IF                                                           
364400         IF WS-KVPBREOI > ZERO                                            
364500           MOVE MFS-ADD-LAES-IN-FAELT                                     
364600                             TO MOD-KVPBREOI-ATTR                         
364700         END-IF                                                           
364800       ELSE                                                               
364900         IF NDC-CN                                                        
365000            IF CLAG-PRARTSTD  = ZERO                                      
365100               MOVE PRIS-SAKNAS    TO MED-IDMFSFEL                        
365200               CALL WMEDKONV    USING MED-WMEDAREA                        
365300               MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                        
365400               MOVE NEJ            TO INDATA-SW                           
365500            END-IF                                                        
365600         END-IF                                                           
365700       END-IF                                                             
365800     END-IF                                                               
365900                                                                          
366000     IF MID-PURCHQTY = ALL '+'                                            
366100       MOVE ZERO           TO WS-RED-PURCHQTY                             
366200                              WS-PURCHQTY                                 
366300       MOVE WS-RED-PURCHQTY                                               
366400                           TO MOD-PURCHQTY                                
366500     ELSE                                                                 
366600       INSPECT MID-PURCHQTY                                               
366700                        REPLACING LEADING SPACE BY ZERO                   
366800       IF MID-PURCHQTY NUMERIC                                            
366900         MOVE MID-PURCHQTY                                                
367000                           TO WS-RED-PURCHQTY                             
367100                              WS-PURCHQTY                                 
367200         MOVE WS-RED-PURCHQTY                                             
367300                           TO MOD-PURCHQTY                                
367400                                                                          
367500         IF  MID-PURCHQTY IS NUMERIC                                      
367600         AND MID-PURCHQTY > ZERO                                          
367700           PERFORM IMS-GU-WDK711                                          
367800           IF SEGMENT-FINNS                                               
367900             IF ((WS-IDREFTYP = 'A'                                       
368000             OR   WS-IDREFTYP = 'C'                                       
368100             OR   WS-IDREFTYP = 'B')                                      
368200             AND GODK-IDDC-REF-JA)                                        
368300             OR  (WS-IDREFTYP = 'L'                                       
368400             AND NOT (GODK-IDDC-REF-JA                                    
368500                 OR SLAG-IDLEVNR = 'BP2TW'))                              
368600               MOVE MFS-ADD-LAES-IN-FAELT                                 
368700                             TO MOD-PURCHQTY-ATTR                         
368800             ELSE                                                         
368900               MOVE MED-8  TO MOD-TEMFSFEL                                
369000               MOVE NEJ    TO INDATA-SW                                   
369100               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
369200                             TO MOD-PURCHQTY-ATTR                         
369300             END-IF                                                       
369310             IF MFS-UPDATE           AND                                  
369320               (WS-IDREFTYP = 'A' OR WS-IDREFTYP = 'C')                   
369330                MOVE MOD-IDDC-UT        TO W-IDDC                         
369340                MOVE WS-IDDC-REF        TO W-IDDC-B616                    
369350                MOVE ZEROES             TO WS-REAIRCO                     
369360                                           WS-PRFRAKT                     
369370                                           WS-AIR-COST-SEK                
369380                PERFORM  S06-CALC-AIR-COST                                
369390                IF WS-KR-VIKT-RED > WS-KR-VOLYM-RED                       
369391                   MOVE WS-KR-VIKT-RED  TO WS-AIR-COST-SEK                
369392                ELSE                                                      
369393                   MOVE WS-KR-VOLYM-RED TO WS-AIR-COST-SEK                
369395                END-IF                                                    
369396                COMPUTE WS-AIR-COST-SEK = WS-AIR-COST-SEK                 
369397                                       * WS-PURCHQTY                      
369398                                                                          
369399                IF WS-AIR-COST-SEK > WS-PRFRAKT                           
369400                   MOVE ERR-HIGH-AIR-COST TO MED-IDMFSFEL                 
369401                   CALL WMEDKONV        USING MED-WMEDAREA                
369402                   MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                   
369403                   MOVE NEJ             TO INDATA-SW                      
369404                END-IF                                                    
369405             END-IF                                                       
369406                                                                          
369410             IF WS-IDREFTYP = 'L'                                         
369500                IF NDC-CN                                                 
369600                   MOVE NEJ    TO INDATA-SW                               
369700                   MOVE MED-8  TO MOD-TEMFSFEL                            
369800                ELSE                                                      
369900*-----                                                                    
370000*-----   FÖR LOKALA LEVERANSER MÅSTE                                      
370100*-----   DET FINNAS ETT GODKÄNT PRIS                                      
370200*-----                                                                    
370300                  PERFORM IMS-GU-WDK601                                   
370400                  IF SEGMENT-FINNS                                        
370500                    COMPUTE W-DAPRLIST-21 =                               
370600                             99999999 - DAGENS-DATUM-SEKEL                
370700                    MOVE SLAG-IDLEVNR TO W-IDLEVNR-21                     
370800                    PERFORM IMS-GNP-WDK621                                
370900                    PERFORM UNTIL SEGMENT-SAKNAS OR                       
371000                      PRL-KDSTATUS-PR = 1                                 
371100                      PERFORM IMS-GNP-WDK621                              
371200                    END-PERFORM                                           
371300                  END-IF                                                  
371400                                                                          
371500                  IF SEGMENT-FINNS                                        
371600***       GODKÄNT PRIS ***************                                    
371700                    CONTINUE                                              
371800                  ELSE                                                    
371900                    MOVE MED-9 TO MOD-TEMFSFEL                            
372000                    MOVE NEJ TO INDATA-SW                                 
372100                    MOVE MFS-ADD-LAES-IN-FAELT-HI                         
372200                                TO MOD-PURCHQTY-ATTR                      
372300                  END-IF                                                  
372400               END-IF                                                     
372500             ELSE                                                         
372600               IF CLAG-KVQPACK-1 > ZERO                                   
372700***    KONTROLLERAR ATT KÖPET ÄR EN JÄMN                                  
372800***    MULTIPEL AV Q1                                                     
372900                 DIVIDE MID-PURCHQTY                                      
373000                           BY CLAG-KVQPACK-1                              
373100                           GIVING WS-SLASK                                
373200                           REMAINDER WS-REST                              
373300               END-IF                                                     
373400               IF WS-REST > ZERO                                          
373500*                UNEVEN MULTIPEL OF Q1                                    
373600                 MOVE MED-10                                              
373700                           TO MOD-TEMFSFEL                                
373800                 MOVE NEJ TO INDATA-SW                                    
373900                                                                          
374000                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
374100                           TO MOD-PURCHQTY-ATTR                           
374200               END-IF                                                     
374300             END-IF                                                       
374400           ELSE                                                           
374500             MOVE ARTIKEL-SAKNAS                                          
374600                           TO MED-IDMFSFEL                                
374700             CALL WMEDKONV USING MED-WMEDAREA                             
374800             MOVE MED-TEMFSFEL                                            
374900                           TO MOD-TEMFSFEL                                
375000             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
375100                           TO MOD-PURCHQTY-ATTR                           
375200             MOVE NEJ      TO INDATA-SW                                   
375300           END-IF                                                         
375400                                                                          
375500         ELSE                                                             
375600           MOVE MFS-ADD-LAES-IN-FAELT                                     
375700                           TO MOD-PURCHQTY-ATTR                           
375800         END-IF                                                           
375900                                                                          
376000       ELSE                                                               
376100         MOVE MED-4        TO MOD-TEMFSFEL                                
376200         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
376300                           TO MOD-PURCHQTY-ATTR                           
376400         MOVE MID-PURCHQTY                                                
376500                           TO MOD-PURCHQTY                                
376600         MOVE NEJ          TO INDATA-SW                                   
376700       END-IF                                                             
376800     END-IF                                                               
376900                                                                          
377000     IF MID-FLREFBEO = ALL '+'                                            
377100       MOVE MFS-RENSA-FAELT                                               
377200                           TO MOD-FLREFBEO                                
377300     ELSE                                                                 
377400       MOVE MID-FLREFBEO                                                  
377500                           TO MOD-FLREFBEO                                
377600       PERFORM IMS-GU-WDK711                                              
377700       IF SEGMENT-FINNS                                                   
377800         IF GODK-IDDC-REF-JA                                              
377900*----                                                                     
378000*---- EJ LOKAL ARTIKEL                                                    
378100*----                                                                     
378200           IF MID-FLREFBEO = JA                                           
378300           OR MID-FLREFBEO = YES                                          
378400           OR MID-FLREFBEO = NEJ                                          
378500           OR MID-FLREFBEO = 'S'                                          
378600             MOVE MFS-ADD-LAES-IN-FAELT                                   
378700                           TO MOD-FLREFBEO-ATTR                           
378800           ELSE                                                           
378900             MOVE MED-5    TO MOD-TEMFSFEL                                
379000             MOVE NEJ      TO INDATA-SW                                   
379100             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
379200                             TO MOD-FLREFBEO-ATTR                         
379300           END-IF                                                         
379400         ELSE                                                             
379500*----                                                                     
379600*---- LOKAL ARTIKEL                                                       
379700*----                                                                     
379800           IF MID-FLREFBEO = NEJ                                          
379900             MOVE MFS-ADD-LAES-IN-FAELT                                   
380000                             TO MOD-FLREFBEO-ATTR                         
380100           ELSE                                                           
380200             MOVE MED-5      TO MOD-TEMFSFEL                              
380300             MOVE NEJ        TO INDATA-SW                                 
380400             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
380500                             TO MOD-FLREFBEO-ATTR                         
380600           END-IF                                                         
380700         END-IF                                                           
380800       END-IF                                                             
380900     END-IF                                                               
381000                                                                          
381100     IF W-IDDC  NOT = W-IDDC-B6                                           
381200        MOVE W-IDDC  TO W-IDDC-B6                                         
381300        PERFORM IMS-GU-WDB601                                             
381400     END-IF                                                               
381500     IF MID-FLAGGA-FCD = ALL '+'                                          
381600       IF WS-IDREFTYP = 'B'                                               
381700         IF SLAG-FLFLYG = 'J'                                             
381800           MOVE MED-11         TO MOD-TEMFSFEL                            
381900           MOVE NEJ            TO INDATA-SW                               
382000           MOVE MFS-ROER-EJ-FAELT                                         
382100                               TO MOD-FLAGGA-FCD-ATTR                     
382200         END-IF                                                           
382201       ELSE                                                               
382210         IF WS-IDREFTYP = 'A' or 'C'                                      
382220           IF SLAG-FLFLYG = 'S'                                           
382230             MOVE MED-17       TO MOD-TEMFSFEL                            
382240             MOVE NEJ          TO INDATA-SW                               
382250             MOVE MFS-ROER-EJ-FAELT                                       
382260                                 TO MOD-FLAGGA-FCD-ATTR                   
382270           END-IF                                                         
382280         END-IF                                                           
382300       END-IF                                                             
382400     ELSE                                                                 
382500       IF MID-FLAGGA-FCD = 'Y' OR 'J'                                     
382600         IF DCS-NDC-PF                                                    
382700         OR DCS-NDC-CN                                                    
382800         OR DCS-NDC-SA                                                    
382900         OR DCS-NDC-OTHERS                                                
383000           IF WS-IDREFTYP = 'B'                                           
383100           AND MID-PURCHQTY > ZERO                                        
383200              MOVE NEJ          TO INDATA-SW                              
383300              MOVE MFS-ROER-EJ-FAELT                                      
383400                                TO MOD-FLAGGA-FCD-ATTR                    
383500           ELSE                                                           
383600              IF GODK-IDDC-REF-JA                                         
383700                MOVE 'J'            TO WS-FLAGGA-FCD                      
383800                IF MSGI-IDLAND-SPR = 'GB'                                 
383900                  MOVE YES          TO MOD-FLAGGA-FCD                     
384000                ELSE                                                      
384100                  MOVE JA           TO MOD-FLAGGA-FCD                     
384200                END-IF                                                    
384300              ELSE                                                        
384400                MOVE MED-14         TO MOD-TEMFSFEL                       
384500                MOVE NEJ            TO INDATA-SW                          
384600                MOVE MFS-ADD-LAES-IN-FAELT-HI                             
384700                                    TO MOD-FLAGGA-FCD-ATTR                
384800              END-IF                                                      
384900           END-IF                                                         
385000         ELSE                                                             
385100           MOVE MED-12       TO MOD-TEMFSFEL                              
385200           MOVE NEJ          TO INDATA-SW                                 
385300           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
385400                             TO MOD-FLAGGA-FCD-ATTR                       
385500         END-IF                                                           
385600       ELSE                                                               
385700         IF MID-FLAGGA-FCD = 'N'                                          
385800           MOVE 'N'            TO WS-FLAGGA-FCD                           
385900           MOVE WS-FLAGGA-FCD  TO MOD-FLAGGA-FCD                          
386000         END-IF                                                           
386010         IF MID-FLAGGA-FCD = 'S'                                          
386020           MOVE 'S'            TO WS-FLAGGA-FCD                           
386030           MOVE WS-FLAGGA-FCD  TO MOD-FLAGGA-FCD                          
386040         END-IF                                                           
386100       END-IF                                                             
386200     END-IF                                                               
386300     IF MID-COMMENT-1   = ALL '+'                                         
386400       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT(1)                            
386500*                               MID-COMMENT-1                             
386600     ELSE                                                                 
386700       MOVE MID-COMMENT-1    TO MOD-COMMENT(1)                            
386800     END-IF                                                               
386900     MOVE MFS-ADD-LAES-IN-FAELT                                           
387000                             TO MOD-COMMENT-ATTR(1)                       
387100     IF MID-COMMENT-2   = ALL '+'                                         
387200       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT(2)                            
387300*                               MID-COMMENT-2                             
387400     ELSE                                                                 
387500       MOVE MID-COMMENT-2    TO MOD-COMMENT(2)                            
387600     END-IF                                                               
387700     MOVE MFS-ADD-LAES-IN-FAELT                                           
387800                             TO MOD-COMMENT-ATTR(2)                       
387900     .                                                                    
388000     EJECT                                                                
388100                                                                          
388200 J-FYLL-I-ANT-REVIEW SECTION.                                             
388300                                                                          
388400     MOVE ZERO               TO WS-ANT-REVIEW                             
388500     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
388600                                W-KDREFTYP-MAX                            
388700     MOVE ZERO               TO W-IDARTNR-MIN                             
388800                                                                          
388900     PERFORM IMS-GU-WDE301-MIN-MAX                                        
389000                                                                          
389100     PERFORM UNTIL SEGMENT-SAKNAS                                         
389200                                                                          
389300         IF  REF-KDREFTYP     = WS-IDTYPE                                 
389400         AND REF-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                       
389500         AND REF-KDREFORS     = 'P'                                       
389600         AND REF-IDDC         = IDDC-WS                                   
389700            ADD +1           TO WS-ANT-REVIEW                             
389800            MOVE REF-IDARTNR                                              
389900                             TO WS-SPARA-IDARTNR                          
390000                                                                          
390100            PERFORM UNTIL SEGMENT-SAKNAS                                  
390200            OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                         
390300                                                                          
390400               PERFORM IMS-GN-WDE301-MIN-MAX                              
390500            END-PERFORM                                                   
390600                                                                          
390700         ELSE                                                             
390800            PERFORM IMS-GN-WDE301-MIN-MAX                                 
390900         END-IF                                                           
391000                                                                          
391100     END-PERFORM                                                          
391200     MOVE WS-ANT-REVIEW      TO MOD-ANT-REVIEW                            
391300     .                                                                    
391400     EJECT                                                                
391500                                                                          
391600 K-FYLL-I-NYCKEL-FAELT SECTION.                                           
391700                                                                          
391800     MOVE REF-IDARTNR        TO WS-SPARA-IDARTNR                          
391900                                W-IDARTNR                                 
392000                                MOD-IDARTNR-UT                            
392100     MOVE JA                 TO SW-TRAEFF                                 
392200     MOVE REF-IDDC           TO WS-SPARA-IDDC                             
392300                                W-IDDC                                    
392400     MOVE REF-IDPERSON-BUY TO WS-IDPERSON-BUY-RED                         
392500     MOVE WS-IDPERSON-BUY-RED                                             
392600                             TO MOD-IDPERSON-BUY-UT                       
392700     IF REF-KDREFTYP = 'A'                                                
392800        MOVE 'AIR'           TO MOD-IDTYPE-UT                             
392900                                WS-IDTYPE                                 
393000     END-IF                                                               
393100     IF REF-KDREFTYP = 'C'                                                
393200        MOVE 'AIRCR'         TO MOD-IDTYPE-UT                             
393300        MOVE 'C'             TO WS-IDTYPE                                 
393400     END-IF                                                               
393500     IF REF-KDREFTYP = 'B'                                                
393600        MOVE 'BOAT'          TO MOD-IDTYPE-UT                             
393700                                WS-IDTYPE                                 
393800     END-IF                                                               
393900     IF REF-KDREFTYP = 'L'                                                
394000        MOVE 'LOCAL'         TO MOD-IDTYPE-UT                             
394100                                WS-IDTYPE                                 
394200     END-IF                                                               
394300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
394400     .                                                                    
394500     EJECT                                                                
394600                                                                          
394700 L-KOLLA-INPUT-NYCKLAR SECTION.                                           
394800                                                                          
394900     IF NOT (MID-IDARTNR-IN       = ALL '+'                               
395000        AND  MID-IDDC-IN          = ALL '+'                               
395100        AND  MID-IDTYPE-IN        = ALL '+'                               
395200        AND  MID-IDPERSON-BUY-IN  = ALL '+'                               
395300        AND  MID-IDSTATUS-IN      = ALL '+')                              
395400                                                                          
395500       MOVE NEJ              TO INDATA-SW                                 
395600     END-IF                                                               
395700     .                                                                    
395800     EJECT                                                                
395900                                                                          
396000 M-UPD-MOD-FAELT SECTION.                                                 
396100                                                                          
396200     MOVE REF-KDREFTXT       TO WS-REF-KDREFTXT                           
402401*         KOD FÖR REFILL VARNINGSTEXT                                     
402402     IF REF-KDREFORS = 'P'                                                
402403        MOVE 'PROPOSAL NOT REVIEWED'                                      
402404                             TO MOD-ORDERSTATUS                           
402405        MOVE 'NOT REVIEWED'                                               
402406                             TO MOD-IDSTATUS-UT                           
402407        MOVE 'N'             TO WS-STATUS                                 
402408     ELSE                                                                 
402409        MOVE 'REVIEWED'      TO MOD-ORDERSTATUS                           
402410                                                                          
402411     END-IF                                                               
402412                                                                          
402413     IF REF-KDREFTYP = 'B'                                                
402414       MOVE 'BOAT '          TO MOD-IDREFTYP-UT                           
402415     END-IF                                                               
402416     IF REF-KDREFTYP = 'A'                                                
402417       MOVE 'AIR  '          TO MOD-IDREFTYP-UT                           
402418     END-IF                                                               
402419     IF REF-KDREFTYP = 'C'                                                
402420       MOVE 'AIRCR'          TO MOD-IDREFTYP-UT                           
402421     END-IF                                                               
402422     IF REF-KDREFTYP = 'L'                                                
402423       MOVE 'LOCAL'          TO MOD-IDREFTYP-UT                           
402424     END-IF                                                               
402425*--- LÄGG UT Y ELLER N I BULK-AIR FÄLTET FÖR ATT VISA VILKEN              
402426*--- TYP AV FLYGKÖP SOM GJORTS TIDIGARE (ENDAST DC 62)                    
402427                                                                          
402428     MOVE REF-KVBEART        TO WS-RED-PURCHQTY                           
402429                                WS-PURCHQTY                               
402430                                WS-PURCHQTY-SIM                           
402431     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY                              
402432     MOVE MFS-ADD-LAES-IN-FAELT                                           
402433                             TO MOD-PURCHQTY-ATTR                         
402434     .                                                                    
402435     EJECT                                                                
402436                                                                          
402437 N-UPD-WDE3-WDK7 SECTION.                                                 
402438                                                                          
402439                                                                          
402440     MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                              
402441     MOVE W-IDARTNR        TO W-IDARTNR-MIN                               
402442     MOVE ZERO             TO W-IDDISTR-MIN                               
402443                                                                          
402444     PERFORM IMS-GU-WDE301-MIN-MAX                                        
402445                                                                          
402446     PERFORM UNTIL SEGMENT-SAKNAS                                         
402447       OR (REF-IDPERSON-BUY = W-IDPERSON-BUY-MIN                          
402448       AND REF-KDREFTYP     = W-KDREFTYP-MIN                              
402449       AND REF-IDARTNR      = W-IDARTNR-MIN)                              
402450                                                                          
402451       PERFORM IMS-GN-WDE301-MIN-MAX                                      
402452                                                                          
402453     END-PERFORM                                                          
402454                                                                          
402455     IF SEGMENT-FINNS                                                     
402456                                                                          
402457       PERFORM IMS-GHU-WDK711                                             
402458                                                                          
402459       IF REF-KDREFORS = 'O'                                              
402460         SUBTRACT REF-KVBEART                                             
402461                           FROM SLAG-KVBEART                              
402462       END-IF                                                             
402500                                                                          
402600       IF MID-PURCHQTY IS NUMERIC                                         
402700       AND MID-PURCHQTY > ZERO                                            
402800         ADD MID-PURCHQTY  TO SLAG-KVBEART                                
402900         MOVE DAGENS-DATUM TO SLAG-TIORDREG                               
403000       END-IF                                                             
403100                                                                          
403200       PERFORM IMS-REPL-WDK711                                            
403300       MOVE REF-IDDC       TO W-IDDC-301                                  
403400       MOVE REF-IDPERSON-BUY                                              
403500                           TO W-IDPERSON-BUY                              
403600       MOVE REF-KDREFTYP   TO W-KDREFTYP                                  
403700       MOVE REF-IDARTNR    TO W-IDARTNR-301                               
403800       MOVE REF-IDDISTR    TO W-IDDISTR                                   
403900                                                                          
404000       PERFORM IMS-GHU-WDE301                                             
404100                                                                          
404200       MOVE WS-IDREFTYP    TO REF-KDREFTYP                                
404300       IF MID-PURCHQTY IS NUMERIC                                         
404400       AND MID-PURCHQTY > ZERO                                            
404500         MOVE MID-PURCHQTY                                                
404600                           TO REF-KVBEART                                 
404700         MOVE 'O '         TO REF-KDREFORS                                
404800         MOVE SLAG-IDLEVNR TO REF-IDLEVNR                                 
404900         MOVE WS-KDFRAKT   TO REF-KDFRAKT                                 
405000         PERFORM NA-EV-CROSS-DOCKING                                      
405100         PERFORM IMS-REPL-WDE301                                          
405200       ELSE                                                               
405300         PERFORM IMS-DLET-WDE301                                          
405400* --     OM MAN NOLLAT KÖPFÖRSLAGET SÄTTS FLREFNYO TILL JA                
405500         PERFORM IMS-GHU-WDK711                                           
405600         IF GODK-IDDC-REF-JA                                              
405900           IF SEGMENT-FINNS                                               
406000             MOVE JA       TO SLAG-FLREFNYO                               
406100             PERFORM IMS-REPL-WDK711                                      
406200           END-IF                                                         
406300         END-IF                                                           
406400       END-IF                                                             
406500     ELSE                                                                 
406600       IF MID-PURCHQTY IS NUMERIC                                         
406700       AND MID-PURCHQTY > ZERO                                            
406800                                                                          
406900         MOVE WS-IDREFTYP  TO REF-KDREFTYP                                
407000         MOVE W-IDDC                                                      
407100                           TO REF-IDDC                                    
407200         MOVE W-IDARTNR    TO REF-IDARTNR                                 
407300         PERFORM IMS-GHU-WDK711                                           
407400         ADD MID-PURCHQTY                                                 
407500                           TO SLAG-KVBEART                                
407600         MOVE DAGENS-DATUM TO SLAG-TIORDREG                               
407700         PERFORM IMS-REPL-WDK711                                          
407800         MOVE SLAG-ADART   TO REF-ADART-SDC                               
407900         MOVE CLAG-ADART   TO REF-ADART-CDC                               
408000         MOVE WS-IDDISTR   TO REF-IDDISTR                                 
408100         MOVE MID-PURCHQTY                                                
408200                           TO REF-KVBEART                                 
408300         MOVE 'O'          TO REF-KDREFORS                                
408400         MOVE SLAG-IDLEVNR TO REF-IDLEVNR                                 
408500         MOVE WS-IDPERSON-BUY-NUM                                         
408600                           TO REF-IDPERSON-BUY                            
408700         MOVE ZERO         TO REF-IDKUNDNR                                
408800                              REF-KDREFTXT                                
408900                              REF-KVBEART-CD                              
409000                              REF-ADLAGOMR-CD                             
409100                              REF-ADGANG-CD                               
409200                              REF-ADPLATS-CD                              
409300         MOVE WS-KDFRAKT   TO REF-KDFRAKT                                 
409310         MOVE SLAG-IDDC-REF TO REF-IDDC-REF                               
409400         PERFORM NA-EV-CROSS-DOCKING                                      
409500         PERFORM IMS-ISRT-WDE301                                          
409600       END-IF                                                             
409700     END-IF                                                               
409800     MOVE 'REVIEWED'         TO MOD-ORDERSTATUS                           
409900     .                                                                    
410000     EJECT                                                                
410100                                                                          
410200 NA-EV-CROSS-DOCKING  SECTION.                                            
410300                                                                          
410400     IF SLAG-ADLAGOMR-CD > ZERO                                           
410500                                                                          
410600       MOVE 1                TO IX-CD                                     
410700       PERFORM UNTIL IX-CD > 4                                            
410800       OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX-CD)                     
410900         ADD 1               TO IX-CD                                     
411000       END-PERFORM                                                        
411100                                                                          
411200       IF IX-CD > 4                                                       
411300*    SKA INTE KUNNA INTRÄFFA                                              
411400         CONTINUE                                                         
411500       ELSE                                                               
411600                                                                          
411700         IF (CLAG-KVLS-CD (IX-CD) - CLAG-KVRESS-CD (IX-CD))               
411800                             < CLAG-KVQPACK-3                             
411900           CONTINUE                                                       
412000         ELSE                                                             
412100           DIVIDE REF-KVBEART BY CLAG-KVQPACK-3                           
412200                                 GIVING WS-HELTAL-BEST                    
412300           COMPUTE WS-SALDO =                                             
412400                   CLAG-KVLS-CD (IX-CD) - CLAG-KVRESS-CD (IX-CD)          
412500           DIVIDE WS-SALDO       BY CLAG-KVQPACK-3                        
412600                                 GIVING WS-HELTAL-SALDO                   
412700           IF WS-HELTAL-BEST > WS-HELTAL-SALDO                            
412800             COMPUTE REF-KVBEART-CD =                                     
412900                             WS-HELTAL-SALDO * CLAG-KVQPACK-3             
413000           ELSE                                                           
413100             COMPUTE REF-KVBEART-CD =                                     
413200                             WS-HELTAL-BEST * CLAG-KVQPACK-3              
413300           END-IF                                                         
413400           MOVE CLAG-ADLAGOMR-CD (IX-CD)                                  
413500                             TO REF-ADLAGOMR-CD                           
413600           MOVE CLAG-ADGANG-CD (IX-CD)                                    
413700                             TO REF-ADGANG-CD                             
413800           MOVE CLAG-ADPLATS-CD (IX-CD)                                   
413900                             TO REF-ADPLATS-CD                            
414000                                                                          
414100           PERFORM IMS-GHU-WDK611                                         
414200           COMPUTE CLAG-KVRESS-CD (IX-CD) =                               
414300                   CLAG-KVRESS-CD (IX-CD) + REF-KVBEART-CD                
414400           PERFORM IMS-REPL-WDK6                                          
414500                                                                          
414600         END-IF                                                           
414700       END-IF                                                             
414800     END-IF                                                               
414900     .                                                                    
415000     EJECT                                                                
415100                                                                          
415200                                                                          
415300 S01-LAES-WDA5-ENTER SECTION.                                             
415400                                                                          
415500     MOVE W-IDARTNR          TO W-IDARTNR-N3-MIN                          
415600                                W-IDARTNR-N3-MAX                          
415700                                                                          
415800     PERFORM IMS-GN-WDA5-ORDQ01                                           
415900                                                                          
416000     MOVE ZERO               TO WS-RESTKVANT                              
416100     PERFORM UNTIL SEGMENT-SAKNAS                                         
416200                                                                          
416300        MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                             
416400        MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                            
416500        MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                            
416600        MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                             
416700        MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                             
416800                                                                          
416900        IF SEQA-IDDISTR = WS-IDDISTR                                      
417000            PERFORM IMS-GU-WDA501                                         
417100            IF SEGMENT-FINNS                                              
417200            AND RAD-KDSTARAD = '2'                                        
417300            AND RAD-IDDISTR = WS-IDDISTR                                  
417400*  SUMMERA RESTORDERKVANTITET                                             
417500                ADD RAD-KVART   TO WS-RESTKVANT                           
417600            END-IF                                                        
417700        END-IF                                                            
417800                                                                          
417900        PERFORM IMS-GN-WDA5-ORDQ01                                        
418000                                                                          
418100     END-PERFORM                                                          
418200     .                                                                    
418300     EJECT                                                                
418400                                                                          
418500                                                                          
418600 S02-GET-BESPRIS SECTION.                                                 
418700     IF NDC-CN OR NDC-NA                                                  
418800       PERFORM S02A-GET-BESPRIS                                           
418900     ELSE                                                                 
419000       PERFORM S02B-GET-BESPRIS                                           
419100     END-IF                                                               
419200     .                                                                    
419300     EJECT                                                                
419400                                                                          
419500 S02A-GET-BESPRIS SECTION.                                                
419600                                                                          
419700     IF NDC-CN                                                            
419800       MOVE 'CN'             TO W-IDLAND                                  
419900     END-IF                                                               
420000     IF NDC-US                                                            
420100       MOVE 'US'             TO W-IDLAND                                  
420200     END-IF                                                               
420300     IF NDC-CA                                                            
420400       MOVE 'CA'             TO W-IDLAND                                  
420500     END-IF                                                               
420600     PERFORM IMS-GU-WDK712-BESPRIS                                        
420700     IF SEGMENT-FINNS                                                     
420800       MOVE LART-PRMATRL     TO WS-PRARTBES                               
420900     ELSE                                                                 
421000       MOVE ZERO             TO WS-PRARTBES                               
421100     END-IF                                                               
421200     .                                                                    
421300     EJECT                                                                
421400                                                                          
421500 S02B-GET-BESPRIS SECTION.                                                
421600                                                                          
421700     MOVE CLAG-PRARTSTD         TO WS-PRARTBES                            
421800     .                                                                    
421900     EJECT                                                                
422000 S1-SECURITY-CHECK-PARTNO SECTION.                                        
422100     SKIP2                                                                
422200*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
422300     PERFORM IMS-GU-WDK601                                                
422400     IF  SEGMENT-FINNS                                                    
422500       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
422600       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
422700       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
422800*        --- USER GRANTED                                                 
422900         SET PASSED-SECURITY-CHECK TO TRUE                                
423000       ELSE                                                               
423100         SET BLOCKED-SECURITY-CHECK TO TRUE                               
423200       END-IF                                                             
423300     END-IF                                                               
423400     .                                                                    
423500     EJECT                                                                
423600                                                                          
423700 S03-SEARCH-IDLAND SECTION.                                               
423800                                                                          
423900     SEARCH ALL DC-LAND                                                   
424000       AT END                                                             
424100         MOVE SPACE          TO W-IDLAND                                  
424200       WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC                              
424300         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
424400                                            WS-IDLAND-SEND                
424500     END-SEARCH                                                           
424600     .                                                                    
424700     EJECT                                                                
424800* GET WDK711 DETAILS FOR SENDING DC                                       
424900* GU-WDK711 CALL IS TO GET SENDING DC DETAILS                             
425000 S04-GET-WDK711-SEND-DC SECTION.                                          
425100                                                                          
425200     MOVE W-IDDC                        TO W-RECEIVING-IDDC               
425300     MOVE SLAG-IDDC-REF                 TO W-IDDC                         
425400     MOVE ZEROES                        TO WS-KVDISP-SEND-DC              
425500                                                                          
425600     PERFORM IMS-GU-WDK711                                                
425700     IF SEGMENT-FINNS                                                     
425800        COMPUTE WS-KVDISP-SEND-DC  = SLAG-KVLS         +                  
425900                                     SLAG-KVBEART      +                  
426000                                     SLAG-KVAKS-SDC    +                  
426100                                     SLAG-KVAKS-PAV    -                  
426200                                     SLAG-KVOKS-DAG    -                  
426300                                     SLAG-KVOKS-BULK   -                  
426400                                     SLAG-KVROS-DAG    -                  
426500                                     SLAG-KVROS-BULK   -                  
426600                                     SLAG-KVSPARR-KVAL -                  
426700                                     SLAG-KVRESS                          
426800     END-IF                                                               
426900                                                                          
427000     MOVE W-RECEIVING-IDDC              TO W-IDDC                         
427100     .                                                                    
427200     EJECT                                                                
427201                                                                          
427340 S06-CALC-AIR-COST SECTION.                                               
427350                                                                          
427360     PERFORM IMS-GU-WDB616                                                
427370     IF SEGMENT-FINNS                                                     
427380            MOVE B6-REF-REAIRCO        TO WS-REAIRCO                      
427390            MOVE B6-REF-PRFRAKT        TO WS-PRFRAKT                      
427391     END-IF                                                               
427392**Air cost calculation                                                    
427393     COMPUTE WS-KR-VIKT ROUNDED =                                         
427394                        (WS-VKART * WS-REAIRCO ) / 1000                   
427395     COMPUTE WS-KR-VIKT-RED ROUNDED = WS-KR-VIKT * 1                      
427396     COMPUTE WS-KR-VOLYM ROUNDED = (WS-VLARTNTO *                         
427397                          WS-REAIRCO * 167 / 1000000)                     
427398     COMPUTE WS-KR-VOLYM-RED ROUNDED = WS-KR-VOLYM * 1                    
427399                                                                          
427400     .                                                                    
427401     EJECT                                                                
427402                                                                          
427410 S90-CALL-W271REFL SECTION.                                               
427500                                                                          
427600     INITIALIZE  W271-REFL-W271REFL                                       
427700     MOVE ZERO                 TO W271-REFL-NDC-KVDAGAR-TBT-DC            
427800     MOVE W-IDDC               TO W271-REFL-IDDC                          
427900     MOVE W-IDARTNR            TO W271-REFL-IDARTNR                       
428000     MOVE SLAG-IDDC-REF        TO W271-REFL-IDDC-REF                      
428100     MOVE SLAG-IDREFTAB        TO W271-REFL-IDREFTAB                      
428200     MOVE SLAG-FLWILSON        TO W271-REFL-FLWILSON                      
428300                                                                          
428400     PERFORM S02-GET-BESPRIS                                              
428500     MOVE WS-PRARTBES          TO W271-REFL-PRARTBES                      
428600                                                                          
428700     MOVE 1 TO IX                                                         
428800     PERFORM UNTIL IX > 12                                                
428900        MOVE SLAG-RESEASON(IX) TO W271-REFL-RESEASON(IX)                  
429000        ADD 1 TO IX                                                       
429100     END-PERFORM                                                          
429200                                                                          
429300     MOVE SLAG-IDLEVNR         TO W271-REFL-IN-IDLEVNR-DC                 
429400     MOVE SLAG-FLFLYG          TO W271-REFL-FLFLYG                        
429500                                                                          
429600     MOVE SLAG-TIREFPKT        TO TMP1-YYMMDD                             
429700     MOVE DAGENS-DATUM         TO TMP2-YYMMDD                             
429800     PERFORM WY2000P1                                                     
429900                                                                          
430000     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
430100       MOVE SLAG-KVREFPKT      TO W271-REFL-IN-KVREFPKT                   
430200     ELSE                                                                 
430300       MOVE ZERO               TO W271-REFL-IN-KVREFPKT                   
430400     END-IF                                                               
430500                                                                          
430600     MOVE SLAG-TIREFPAF        TO TMP1-YYMMDD                             
430700     MOVE DAGENS-DATUM         TO TMP2-YYMMDD                             
430800     PERFORM WY2000P1                                                     
430900                                                                          
431000     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
431100       MOVE SLAG-KVREFBER      TO W271-REFL-IN-KVREFBER                   
431200     ELSE                                                                 
431300       MOVE ZERO               TO W271-REFL-IN-KVREFBER                   
431400     END-IF                                                               
431500                                                                          
431600     IF GODK-IDDC-REF-JA                                                  
431700        MOVE ZERO TO W271-REFL-NDC-KVDAGAR-TBT-DC                         
431800     ELSE                                                                 
431900        IF SLAG-KVDAGAR-MANLT   > ZERO                                    
432000           MOVE SLAG-KVDAGAR-MANLT                                        
432100                               TO W271-REFL-NDC-KVDAGAR-TBT-DC            
432200        ELSE                                                              
432300           MOVE SLAG-IDLEVNR   TO W-IDLEVNR                               
432400           PERFORM IMS-GU-LEVA16                                          
432500           IF SEGMENT-FINNS                                               
432600              MOVE NDC-KVDAGAR-TBT                                        
432700                               TO W271-REFL-NDC-KVDAGAR-TBT-DC            
432800           ELSE                                                           
432900              MOVE 1           TO W271-REFL-NDC-KVDAGAR-TBT-DC            
433000           END-IF                                                         
433100        END-IF                                                            
433200     END-IF                                                               
433300*                                                                         
433400***  BELOW CHECK ONLY IF SIMUNATION IS REQUESTED AND NOT FOR UPD          
433500*                                                                         
433600     IF WS-FLSIM = JA                                                     
433700        MOVE JA                TO W271-REFL-IN-FLSIM                      
433800        MOVE WS-KVPB-REF       TO W271-REFL-IN-KVPB-REF                   
433900        MOVE WS-KVPBREOI       TO W271-REFL-IN-KVPBREOI                   
434000     END-IF                                                               
434100                                                                          
434200     PERFORM S100-CALL-W271UTUP                                           
434300                                                                          
434400     CALL W271REFL USING W271-REFL-W271REFL                               
434500                         REFL1-2501-PCB                                   
434600                         REFL1-WDB6-PCB                                   
434700                         REFL1-WDK7-PCB                                   
434800                         REFL1-UTIL-WDK6-PCB                              
434900                         REFL1-UTIL-WDK7-PCB                              
435000                         REFL1-UTIL-WDB6-PCB                              
435100                                                                          
435200     .                                                                    
435300     EJECT                                                                
435400                                                                          
435500 S100-CALL-W271UTUP SECTION.                                              
435600                                                                          
435700***  THIS SECTION CALLS W271UTUP TO GET LEAD TIME                         
435800***  ADJUSTED DEMAND - ( LEAD TIME ADJUSTED FROM CURRENT WEEK)            
435900*                                                                         
436000     INITIALIZE W271-UTUP-W271UTUP                                        
436100     MOVE W-IDARTNR                 TO W271-UTUP-IDARTNR                  
436200     MOVE W-IDDC                    TO W271-UTUP-IDDC                     
436300     MOVE SLAG-IDDC-REF             TO W271-UTUP-IDDC-REF                 
436400     MOVE DAGENS-DATUM              TO W271-UTUP-TIAAMMDD                 
436500     MOVE W271-REFL-NDC-KVDAGAR-TBT-DC                                    
436600                                    TO W271-UTUP-LEADTIME                 
436700     MOVE 004                       TO W271-UTUP-KDCALL                   
436800*                                                                         
436900***  BELOW CHECK ONLY IF SIMUNATION IS REQUESTED AND NOT FOR UPD          
437000*                                                                         
437100     IF WS-FLSIM = JA                                                     
437200        MOVE JA                     TO W271-UTUP-FLSIM                    
437300        MOVE WS-KVPB-REF            TO W271-UTUP-KVPB-REF                 
437400        MOVE WS-KVPBREOI            TO W271-UTUP-KVPBREOI                 
437500     END-IF                                                               
437600*                                                                         
437700     CALL W271UTUP USING W271-UTUP-W271UTUP                               
437800                         UTUP1-WDK7-PCB                                   
437900                         UTUP1-WDB6-PCB                                   
438000                         UTUP1-UTIL-WDK6-PCB                              
438100                         UTUP1-UTIL-WDK7-PCB                              
438200                         UTUP1-UTIL-WDB6-PCB                              
438300     IF W271-UTUP-KDSVAR-OK                                               
438400        MOVE W271-UTUP-LEADTID-BEHOV TO W271-REFL-IN-LEADTID-BEHOV        
438500     ELSE                                                                 
438600        MOVE 'FEL FRÅN W20372 S100-'                                      
438700                                    TO FELTEXT                            
438800        CALL FELLOG                                                       
438900     END-IF                                                               
439000     .                                                                    
439100     EJECT                                                                
439200                                                                          
439300 MFS-RENSA-FAELT-IN SECTION.                                              
439400                                                                          
439500*    --- ALLA INDATA-FÄLT                                                 
439600     MOVE MFS-RENSA-FAELT    TO MOD-KVPB-REF                              
439700                                MOD-PURCHQTY                              
439800                                MOD-FLREFBEO                              
439900                                MOD-KVPBREOI                              
440000     MOVE 1                  TO IX                                        
440100     PERFORM UNTIL IX > 2                                                 
440200       MOVE MFS-RENSA-FAELT  TO                                           
440300                                MOD-COMMENT(IX)                           
440400       ADD 1                 TO IX                                        
440500     END-PERFORM                                                          
440600     MOVE ZERO               TO WS-RED-PURCHQTY                           
440700     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY                              
440800     .                                                                    
440900     EJECT                                                                
441000                                                                          
441100 MFS-RENSA-FAELT-UT SECTION.                                              
441200                                                                          
441300*    --- ALLA UTDATA-FÄLT                                                 
441400     MOVE MFS-RENSA-FAELT    TO MOD-BEART                                 
441500                                MOD-ANT-REVIEW                            
441600                                MOD-FOREG-AAR                             
441700                                MOD-IAAR                                  
441800                                MOD-KVOI-IAAR                             
441900                                MOD-KVOI-FOREG-AAR                        
442000                                MOD-VECKA                                 
442100                                MOD-KVOI-INNEV                            
442200                                MOD-FLWILSON                              
442300                                MOD-FREEZECODE                            
442400                                MOD-VKART                                 
442500                                MOD-VLARTNTO                              
442600                                MOD-TIREFEFT                              
442700                                MOD-TIINLINL                              
442800                                MOD-TIORDREG                              
442900                                MOD-ADLAGOMR                              
443000                                MOD-ADGANG                                
443100                                MOD-ADPLATS                               
443200                                MOD-KVREFPKT                              
443300                                MOD-KVREFBER                              
443400                                MOD-BALANCE                               
443500                                MOD-KVAKS-SDC                             
443600                                MOD-ORDERED                               
443700                                MOD-KVROS                                 
443800                                MOD-CRIT-KVROS-NDC-CDC                    
443900                                MOD-SUPERWEEK                             
444000                                MOD-QUALBLOCK                             
444100                                MOD-IDFKNGRP                              
444200                                MOD-KDPRODSL                              
444300                                MOD-REPLACES                              
444400                                MOD-TIFINLV                               
444500                                MOD-TIURPROD                              
444600                                MOD-REPL-BY                               
444700                                MOD-KDERS                                 
444800                                MOD-KVQPACK-0                             
444900                                MOD-PRARTSTD                              
445000                                MOD-KVQPACK-1                             
445100                                MOD-AIR-COST-SEK                          
445200                                MOD-KVQPACK-3                             
445300                                MOD-KVQPACK-4                             
445400                                MOD-KVAVIS                                
445500                                MOD-DAPUBL                                
445600                                MOD-FLORDSP-EJRO                          
445700                                MOD-KVPALL                                
445800                                MOD-AVAIL                                 
445900                                MOD-KVAKS-CDC                             
446000                                MOD-KVROS-CDC                             
446100                                MOD-SEASON                                
446200                                MOD-KVPB-CDC                              
446300                                MOD-KVROS-NDC-CDC                         
446400                                MOD-FLREFBEO                              
446500                                MOD-REDIRLEV                              
446600                                                                          
446700     MOVE 1                  TO IX                                        
446800     PERFORM UNTIL IX > 3                                                 
446900       MOVE MFS-RENSA-FAELT  TO MOD-MODEL (IX)                            
447000                                                                          
447100       ADD 1                 TO IX                                        
447200     END-PERFORM                                                          
447300                                                                          
447400     MOVE 1                  TO IX                                        
447500     PERFORM UNTIL IX > 12                                                
447600       MOVE MFS-RENSA-FAELT  TO MOD-TIPP (IX)                             
447700                                MOD-TIVV-FOM-TOM (IX)                     
447800                                MOD-KVOI-RULL (IX)                        
447900       ADD 1                 TO IX                                        
448000     END-PERFORM                                                          
448100     .                                                                    
448200     SKIP3                                                                
448300                                                                          
448400 MFS-LAES-IN-IGEN SECTION.                                                
448500                                                                          
448600*    --- ALLA INDATA-FÄLT                                                 
448700                                                                          
448800     MOVE MFS-ADD-LAES-IN-FAELT                                           
448900                             TO MOD-IDARTNR-IN-ATTR                       
449000                                MOD-KVPB-REF-ATTR                         
449100                                MOD-KVPBREOI-ATTR                         
449200                                MOD-PURCHQTY-ATTR                         
449300                                MOD-FLREFBEO-ATTR                         
449400     MOVE 1                  TO IX                                        
449500     PERFORM UNTIL IX > 2                                                 
449600       MOVE MFS-ADD-LAES-IN-FAELT                                         
449700                             TO MOD-COMMENT-ATTR(IX)                      
449800       ADD 1                 TO IX                                        
449900     END-PERFORM                                                          
450000     .                                                                    
450100     EJECT                                                                
450200* --- IMS SEKTIONER ---                                                   
450300     SKIP3                                                                
450400                                                                          
450500 IMS-GET-MSG SECTION.                                                     
450600                                                                          
450700     MOVE '  QC' TO GODK-STATUSKODER                                      
450800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
450900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
451000     PERFORM IMS-STATUSKONTROLL                                           
451100     .                                                                    
451200     SKIP3                                                                
451300 IMS-INSERT-MSG SECTION.                                                  
451400                                                                          
451500     IF ENGLISH-TEXT                                                      
451600       MOVE 'N' TO MFS-KDHUVOMR                                           
451700     END-IF                                                               
451800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
451900     MOVE SPACE TO GODK-STATUSKODER                                       
452000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
452100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
452200     PERFORM IMS-STATUSKONTROLL                                           
452300     .                                                                    
452400     EJECT                                                                
452500 IMS-GU-WDK711 SECTION.                                                   
452600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
452700          DELIMITED BY SIZE INTO SSA1                                     
452800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
452900          DELIMITED BY SIZE INTO SSA2                                     
453000     MOVE '  GE' TO GODK-STATUSKODER                                      
453100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1   SSA2        
453200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
453300     PERFORM IMS-STATUSKONTROLL                                           
453400     .                                                                    
453500     SKIP3                                                                
453600 IMS-GHU-WDK711 SECTION.                                                  
453700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
453800          DELIMITED BY SIZE INTO SSA1                                     
453900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
454000          DELIMITED BY SIZE INTO SSA2                                     
454100     MOVE '  GE' TO GODK-STATUSKODER                                      
454200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK711                   
454300                            SSA1 SSA2                                     
454400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
454500     PERFORM IMS-STATUSKONTROLL                                           
454600     .                                                                    
454700     SKIP3                                                                
454800 IMS-GU-WDK712 SECTION.                                                   
454900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
455000          DELIMITED BY SIZE INTO SSA1                                     
455100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
455200          DELIMITED BY SIZE INTO SSA2                                     
455300     MOVE '  GE' TO GODK-STATUSKODER                                      
455400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
455500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
455600     PERFORM IMS-STATUSKONTROLL                                           
455700     .                                                                    
455800     SKIP3                                                                
455900 IMS-GU-WDK712-BESPRIS SECTION.                                           
456000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
456100          DELIMITED BY SIZE INTO SSA1                                     
456200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
456300          DELIMITED BY SIZE INTO SSA2                                     
456400     MOVE '  GE' TO GODK-STATUSKODER                                      
456500     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-AREA-WDK712 SSA1 SSA2        
456600     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
456700     PERFORM IMS-STATUSKONTROLL                                           
456800     .                                                                    
456900     SKIP3                                                                
457000 IMS-GU-WDK727 SECTION.                                                   
457100                                                                          
457200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
457300          DELIMITED BY SIZE INTO SSA1                                     
457400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
457500          DELIMITED BY SIZE INTO SSA2                                     
457600     STRING 'WDK727  (KDSEGKEY =1)'                                       
457700          DELIMITED BY SIZE INTO SSA3                                     
457800     MOVE '  GE'            TO GODK-STATUSKODER                           
457900     CALL CBLTDLI USING GU WDK7-3-PCB DLI-IO-AREA-WDK727                  
458000                        SSA1 SSA2 SSA3                                    
458100     MOVE WDK7-3-STATUS-CODE TO STATUS-WS                                 
458200     PERFORM IMS-STATUSKONTROLL                                           
458300     .                                                                    
458400     EJECT                                                                
458500                                                                          
458600 IMS-REPL-WDK711 SECTION.                                                 
458700     MOVE '  ' TO GODK-STATUSKODER                                        
458800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
458900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
459000     PERFORM IMS-STATUSKONTROLL                                           
459100     .                                                                    
459200     EJECT                                                                
459300 IMS-GU-WDK722 SECTION.                                                   
459400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
459500          DELIMITED BY SIZE INTO SSA1                                     
459600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
459700          DELIMITED BY SIZE INTO SSA2                                     
459800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
459900          DELIMITED BY SIZE INTO SSA3                                     
460000     MOVE '  GE' TO GODK-STATUSKODER                                      
460100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2   SSA3        
460200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
460300     PERFORM IMS-STATUSKONTROLL                                           
460400     .                                                                    
460500     EJECT                                                                
460600 IMS-GU-WDD301-BSEQ SECTION.                                              
460700                                                                          
460800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
460900          DELIMITED BY SIZE INTO SSA1                                     
461000     MOVE '  GE' TO GODK-STATUSKODER                                      
461100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD301 SSA1               
461200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
461300     PERFORM IMS-STATUSKONTROLL                                           
461400     .                                                                    
461500     SKIP3                                                                
461600 IMS-GNP-WDD311 SECTION.                                                  
461700                                                                          
461800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
461900          DELIMITED BY SIZE INTO SSA1                                     
462000     MOVE '  GE' TO GODK-STATUSKODER                                      
462100     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-WDD311 SSA1              
462200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
462300     PERFORM IMS-STATUSKONTROLL                                           
462400     .                                                                    
462500     EJECT                                                                
462600 IMS-GHU-WDE301 SECTION.                                                  
462700                                                                          
462800     STRING 'WDE301  (WDE301KY =' W-WDE301KY-X ')'                        
462900          DELIMITED BY SIZE INTO SSA1                                     
463000     MOVE '  ' TO GODK-STATUSKODER                                        
463100     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-AREA-WDE301 SSA1              
463200     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
463300     PERFORM IMS-STATUSKONTROLL                                           
463400     .                                                                    
463500     SKIP3                                                                
463600 IMS-GU-WDE301-MIN-MAX SECTION.                                           
463700                                                                          
463800     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
463900                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
464000          DELIMITED BY SIZE INTO SSA1                                     
464100     MOVE '  GE' TO GODK-STATUSKODER                                      
464200     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
464300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
464400     PERFORM IMS-STATUSKONTROLL                                           
464500     .                                                                    
464600     SKIP3                                                                
464700 IMS-GN-WDE301-MIN-MAX SECTION.                                           
464800                                                                          
464900     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
465000                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
465100          DELIMITED BY SIZE INTO SSA1                                     
465200     MOVE '  GE' TO GODK-STATUSKODER                                      
465300     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
465400     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
465500     PERFORM IMS-STATUSKONTROLL                                           
465600     .                                                                    
465700     SKIP3                                                                
465800 IMS-GU-WDE301-PF8 SECTION.                                               
465900                                                                          
466000     STRING 'WDE301  (WDE301KY >' W-WDE301KY-MIN-X                        
466100                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
466200          DELIMITED BY SIZE INTO SSA1                                     
466300     MOVE '  GE' TO GODK-STATUSKODER                                      
466400     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
466500     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
466600     PERFORM IMS-STATUSKONTROLL                                           
466700     .                                                                    
466800     SKIP3                                                                
466900 IMS-GN-WDE301-PF8 SECTION.                                               
467000                                                                          
467100     STRING 'WDE301  (WDE301KY >' W-WDE301KY-MIN-X                        
467200                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
467300          DELIMITED BY SIZE INTO SSA1                                     
467400     MOVE '  GE' TO GODK-STATUSKODER                                      
467500     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
467600     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
467700     PERFORM IMS-STATUSKONTROLL                                           
467800     .                                                                    
467900     SKIP3                                                                
468000 IMS-ISRT-WDE301 SECTION.                                                 
468100                                                                          
468200     MOVE 'WDE301   ' TO SSA1                                             
468300     MOVE '  ' TO GODK-STATUSKODER                                        
468400     CALL CBLTDLI USING ISRT WDE3-PCB DLI-IO-AREA-WDE301 SSA1             
468500     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
468600     PERFORM IMS-STATUSKONTROLL                                           
468700     .                                                                    
468800     SKIP3                                                                
468900 IMS-REPL-WDE301 SECTION.                                                 
469000                                                                          
469100     MOVE '  ' TO GODK-STATUSKODER                                        
469200     CALL CBLTDLI USING REPL WDE3-PCB DLI-IO-AREA-WDE301                  
469300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
469400     PERFORM IMS-STATUSKONTROLL                                           
469500     .                                                                    
469600     EJECT                                                                
469700 IMS-DLET-WDE301 SECTION.                                                 
469800                                                                          
469900     MOVE '  ' TO GODK-STATUSKODER                                        
470000     CALL CBLTDLI USING DLET WDE3-PCB DLI-IO-AREA-WDE301                  
470100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
470200     PERFORM IMS-STATUSKONTROLL                                           
470300     .                                                                    
470400     SKIP3                                                                
470500 IMS-GN-WDA5-ORDQ01 SECTION.                                              
470600                                                                          
470700     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN                          
470800                    '&WDA5A1KY<=' W-WDA5A1KY-MAX  ')'                     
470900            DELIMITED BY SIZE INTO SSA1                                   
471000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
471100     CALL CBLTDLI USING GN ORDQ-PCB DLI-IO-AREA-ORDQ01 SSA1               
471200     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
471300     PERFORM IMS-STATUSKONTROLL                                           
471400     .                                                                    
471500     EJECT                                                                
471600 IMS-GU-WDA501 SECTION.                                                   
471700                                                                          
471800     STRING 'WDA501  (WDA501KY =' W-WDA501KY ')'                          
471900            DELIMITED BY SIZE INTO SSA1                                   
472000     MOVE '  GE' TO GODK-STATUSKODER                                      
472100     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-AREA-WDA501 SSA1               
472200     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
472300     PERFORM IMS-STATUSKONTROLL                                           
472400     .                                                                    
472500     SKIP2                                                                
472600 IMS-GU-WDK601 SECTION.                                                   
472700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
472800          DELIMITED BY SIZE INTO SSA1                                     
472900     MOVE '  GE' TO GODK-STATUSKODER                                      
473000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
473100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
473200     PERFORM IMS-STATUSKONTROLL                                           
473300     .                                                                    
473400     SKIP3                                                                
473500 IMS-GU-WDK611 SECTION.                                                   
473600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
473700          DELIMITED BY SIZE INTO SSA1                                     
473800     MOVE 'WDK611  '       TO SSA2                                        
473900     MOVE '  GE' TO GODK-STATUSKODER                                      
474000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
474100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
474200     PERFORM IMS-STATUSKONTROLL                                           
474300     .                                                                    
474400     SKIP3                                                                
474500 IMS-GNP-WDK611 SECTION.                                                  
474600     MOVE 'WDK611  '       TO SSA1                                        
474700     MOVE '  GE' TO GODK-STATUSKODER                                      
474800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
474900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
475000     PERFORM IMS-STATUSKONTROLL                                           
475100     .                                                                    
475200     EJECT                                                                
475300 IMS-GNP-WDK621-first SECTION.                                            
475400     STRING 'WDK621  *F(DAPRLIST>=' W-DAPRLIST-21-N                       
475500                      '&IDLEVNR  =' W-IDLEVNR-21-X ')'                    
475600          DELIMITED BY SIZE INTO SSA1                                     
475700     MOVE '  GE' TO GODK-STATUSKODER                                      
475800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
475900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
476000     PERFORM IMS-STATUSKONTROLL                                           
476100     SKIP3                                                                
476200     .                                                                    
476300     EJECT                                                                
476400 IMS-GNP-WDK621 SECTION.                                                  
476500     STRING 'WDK621  (DAPRLIST>=' W-DAPRLIST-21-N                         
476600                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
476700          DELIMITED BY SIZE INTO SSA1                                     
476800     MOVE '  GE' TO GODK-STATUSKODER                                      
476900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
477000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
477100     PERFORM IMS-STATUSKONTROLL                                           
477200     SKIP3                                                                
477300     .                                                                    
477400     EJECT                                                                
477500 IMS-GU-WDL601      SECTION.                                              
477600                                                                          
477700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
477800          DELIMITED BY SIZE INTO SSA1                                     
477900     MOVE '  GE' TO GODK-STATUSKODER                                      
478000     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
478100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
478200     PERFORM IMS-STATUSKONTROLL                                           
478300     .                                                                    
478400     SKIP2                                                                
478500 IMS-GNP-WDL611      SECTION.                                             
478600                                                                          
478700     STRING 'WDL611     '                                                 
478800          DELIMITED BY SIZE INTO SSA1                                     
478900     MOVE '  GE' TO GODK-STATUSKODER                                      
479000     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
479100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
479200     PERFORM IMS-STATUSKONTROLL                                           
479300     .                                                                    
479400     SKIP2                                                                
479500 IMS-GU-WDN601 SECTION.                                                   
479600     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
479700            DELIMITED BY SIZE INTO SSA1                                   
479800     MOVE '  GE' TO GODK-STATUSKODER                                      
479900     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-AREA-WDN601 SSA1               
480000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
480100     PERFORM IMS-STATUSKONTROLL                                           
480200     .                                                                    
480300     SKIP3                                                                
480400 IMS-GNP-WDN611 SECTION.                                                  
480500     MOVE 'WDN611   ' TO SSA1                                             
480600     MOVE '  GE' TO GODK-STATUSKODER                                      
480700     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-AREA-WDN611 SSA1              
480800     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
480900     PERFORM IMS-STATUSKONTROLL                                           
481000     .                                                                    
481100     EJECT                                                                
481200 IMS-GU-WDD701      SECTION.                                              
481300     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
481400            DELIMITED BY SIZE INTO SSA1                                   
481500     MOVE '  GE' TO GODK-STATUSKODER                                      
481600     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-AREA-WDD701 SSA1               
481700     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
481800     PERFORM IMS-STATUSKONTROLL                                           
481900     .                                                                    
482000     SKIP2                                                                
482100 IMS-GNP-WDD702      SECTION.                                             
482200     STRING 'WDD702  (FLTEXT   =N)'                                       
482300            DELIMITED BY SIZE INTO SSA1                                   
482400     MOVE '  GE' TO GODK-STATUSKODER                                      
482500     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-AREA-WDD702 SSA1              
482600     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
482700     PERFORM IMS-STATUSKONTROLL                                           
482800     .                                                                    
482900     SKIP2                                                                
483000 IMS-GU-WDD7-ERSB01-MINMAX SECTION.                                       
483100     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
483200                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
483300            DELIMITED BY SIZE INTO SSA1                                   
483400     MOVE '  GE' TO GODK-STATUSKODER                                      
483500     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA-ERSB01 SSA1               
483600     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
483700     PERFORM IMS-STATUSKONTROLL                                           
483800     .                                                                    
483900     EJECT                                                                
484000 IMS-GN-WDD7-ERSB01-MINMAX SECTION.                                       
484100     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
484200                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
484300            DELIMITED BY SIZE INTO SSA1                                   
484400     MOVE '  GE' TO GODK-STATUSKODER                                      
484500     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA-ERSB01 SSA1               
484600     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
484700     PERFORM IMS-STATUSKONTROLL                                           
484800     .                                                                    
484900     EJECT                                                                
485000 IMS-GN-INLA11-W6D1SEQ SECTION.                                           
485100     STRING 'W6INLA11(W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
485200            DELIMITED BY SIZE INTO SSA1                                   
485300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
485400     CALL CBLTDLI USING GN INLA-PCB DLI-IO-AREA-INLA11 SSA1               
485500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
485600     PERFORM IMS-STATUSKONTROLL                                           
485700     .                                                                    
485800     EJECT                                                                
485900 IMS-GU-WDL711          SECTION.                                          
486000     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
486100          DELIMITED BY SIZE INTO SSA1                                     
486200     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
486300          DELIMITED BY SIZE INTO SSA2                                     
486400     MOVE '  GE' TO GODK-STATUSKODER                                      
486500     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-WDL711 SSA1 SSA2          
486600     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
486700     PERFORM IMS-STATUSKONTROLL                                           
486800     .                                                                    
486900     SKIP3                                                                
487000 IMS-GU-WDL411          SECTION.                                          
487100     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
487200          DELIMITED BY SIZE INTO SSA1                                     
487300     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
487400          DELIMITED BY SIZE INTO SSA2                                     
487500     MOVE '  GE' TO GODK-STATUSKODER                                      
487600     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-AREA-WDL411 SSA1 SSA2          
487700     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
487800     PERFORM IMS-STATUSKONTROLL                                           
487900     .                                                                    
488000     SKIP3                                                                
488100 IMS-GU-WDK901         SECTION.                                           
488200     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
488300            DELIMITED BY SIZE INTO SSA1                                   
488400     MOVE '  GE' TO GODK-STATUSKODER                                      
488500     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-WDK901 SSA1               
488600     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
488700     PERFORM IMS-STATUSKONTROLL                                           
488800     .                                                                    
488900     SKIP3                                                                
489000 IMS-GU-LEVA16 SECTION.                                                   
489100                                                                          
489200     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
489300          DELIMITED BY SIZE INTO SSA1                                     
489400     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
489500          DELIMITED BY SIZE INTO SSA2                                     
489600     MOVE '  GE' TO GODK-STATUSKODER                                      
489700     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA16 SSA1 SSA2               
489800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
489900     PERFORM IMS-STATUSKONTROLL                                           
490000     .                                                                    
490100     EJECT                                                                
490200                                                                          
490300 IMS-GHU-WDK611 SECTION.                                                  
490400     STRING 'WDK61101(IDARTNR  =' W-IDARTNR-X ')'                         
490500          DELIMITED BY SIZE INTO SSA1                                     
490600     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
490700     MOVE '  GE' TO GODK-STATUSKODER                                      
490800     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
490900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
491000     PERFORM IMS-STATUSKONTROLL                                           
491100     .                                                                    
491200     EJECT                                                                
491300                                                                          
491400 IMS-REPL-WDK6 SECTION.                                                   
491500                                                                          
491600     MOVE '  ' TO GODK-STATUSKODER                                        
491700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
491800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
491900     PERFORM IMS-STATUSKONTROLL                                           
492000     .                                                                    
492100     SKIP3                                                                
492200     EJECT                                                                
492300                                                                          
492400 IMS-GU-WDB601    SECTION.                                                
492500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
492600          DELIMITED BY SIZE INTO SSA1                                     
492700     MOVE '  GE' TO GODK-STATUSKODER                                      
492800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
492900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
493000     PERFORM IMS-STATUSKONTROLL                                           
493100     IF SEGMENT-SAKNAS                                                    
493200        MOVE SPACE TO DCS-KDDC                                            
493300     END-IF                                                               
493400     .                                                                    
493500     EJECT                                                                
493600 IMS-GU-WDB616    SECTION.                                                
493700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
493800          DELIMITED BY SIZE INTO SSA1                                     
493900     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
494000          DELIMITED BY SIZE INTO SSA2                                     
494100     MOVE '  GE' TO GODK-STATUSKODER                                      
494200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
494300     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
494400     PERFORM IMS-STATUSKONTROLL                                           
494500     .                                                                    
494600     EJECT                                                                
494700 IMS-GN-WDB601 SECTION.                                                   
494800     MOVE 'WDB601   ' TO SSA1                                             
494900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
495000     CALL CBLTDLI USING GN WDB6-NEXT-PCB                                  
495100                           DLI-IO-AREA-B601-NEXT SSA1                     
495200     MOVE WDB6-NEXT-STATUS-CODE TO STATUS-WS                              
495300     PERFORM IMS-STATUSKONTROLL                                           
495400     .                                                                    
495500     EJECT                                                                
495600 IMS-GHU-WDGX2262 SECTION.                                                
495700                                                                          
495800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
495900          DELIMITED BY SIZE INTO SSA1                                     
496000     STRING 'WDGX2262(IDARTNR  =' W-IDARTNR-X ')'                         
496100          DELIMITED BY SIZE INTO SSA2                                     
496200     MOVE '  GE' TO GODK-STATUSKODER                                      
496300     CALL CBLTDLI USING GHU 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2            
496400     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
496500     PERFORM IMS-STATUSKONTROLL                                           
496600     .                                                                    
496700     SKIP3                                                                
496800 IMS-ISRT-WDGX2262   SECTION.                                             
496900                                                                          
497000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
497100          DELIMITED BY SIZE INTO SSA1                                     
497200     MOVE 'WDGX2262 ' TO SSA2                                             
497300     MOVE '  ' TO GODK-STATUSKODER                                        
497400     CALL CBLTDLI USING ISRT 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2           
497500     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
497600     PERFORM IMS-STATUSKONTROLL                                           
497700     .                                                                    
497800     SKIP3                                                                
497900 IMS-REPL-WDGX2262 SECTION.                                               
498000                                                                          
498100     MOVE '  ' TO GODK-STATUSKODER                                        
498200     CALL CBLTDLI USING REPL 2261-PCB DLI-IO-WDGX2262                     
498300     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
498400     PERFORM IMS-STATUSKONTROLL                                           
498500     .                                                                    
498600     EJECT                                                                
498700                                                                          
498800 IMS-STATUSKONTROLL SECTION.                                              
498900                                                                          
499000     SET STATUS-IX TO 1                                                   
499100     SEARCH GODK-STATUS                                                   
499200       AT END                                                             
499300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
499400         DELIMITED BY SIZE INTO FELTEXT                                   
499500         CALL FELLOG                                                      
499600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
499700         CONTINUE                                                         
499800     END-SEARCH                                                           
499900     .                                                                    
500000     EJECT                                                                
500100*    -COPY WY2000P1                                                       
