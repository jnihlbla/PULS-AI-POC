000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2038200.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   JAN 2006.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        MANUELLA KÖP MELLAN DC                                           
001000*                                                                         
001100*        ORDERFÖRSLAG FRÅN REFILLSYSTEMET KOMMER UPP PÅ                   
001200*        DENNA BILD VILKET MAN KAN ACCEPTERA, ÄNDRA ELLER                 
001300*        FÖRKASTA                                                         
001400*        DESSUTOM KAN MAN LÄGGA MANUELLA ORDERBESTÄLLNINGAR HÄR           
001500*                                                                         
001510*                                                                         
001600*        ENTER :    ANVÄNDS VID SIMULERING                                
001700*        PF 11 :    UPPDATERING                                           
001800*                   1) ETT ORDERFÖRSLAG MED KVANTITET IFYLLD              
001900*                      KOMMER ATT GENERERA EN ORDER                       
002000*                   2) ETT ORDERFÖRSLAG MED NOLL I KVANTITET              
002100*                      KOMMER ATT TAS BORT FRÅN WDE3 (FYSISKT)            
002200*                   3) VID EN NY BESTÄLLNING KOMMER EN POST PÅ            
002300*                      WDE3 ATT LÄGGAS UPP VILKET KOMMER ATT              
002400*                      GENERERA EN ORDER                                  
002500*                   4) DATAELEMENTEN KVPB-REF, FLREFBEO                   
002600*                      SAMT TEARTNOT KOMMER ATT UPPDATERAS                
002700*                      SÄTTS MANUELL PROGNOS PÅ                           
002800*                      PASSIV ARTIKEL AKTIVERAS DEN                       
002900*                                                                         
003000*        PF 7          BLÄDDRING BAKÅT                                    
003100*                      KAN BLÄDDRA EN ARTIKEL TILLBAKA                    
003200*        PF 8          BLÄDDRING FRAMÅT (ETT ORDERFÖRSLAG LIGGER          
003300*                      KVAR PÅ WDE3 SOM ICKE BEHANDLAT OM MAN EJ          
003400*                      UPPDATERAR MED PF11 FÖRST, MED KVANT = 0)          
003500*                                                                         
003600*        PROGRAMMET UPPDATERAR WDK7                                       
003700*                              WDP7                                       
003800*                              WDE3                                       
003900*        PROGRAMMET LÄSER      WDD3                                       
004000*                              WDK6                                       
004100*                              WDN6                                       
004200*                              WDD7                                       
004300*                              WDK9                                       
004400*                              WDL7                                       
004500*                              WDE3B  (WDE3) SEK. INDEX                   
004600*                                                                         
004700*    INDATA.                                                              
004800*        TRANSAKTION: W2T382                                              
004900*        MID:         W2I38201                                            
005000*                                                                         
005100*    UTDATA.                                                              
005200*        MOD:         W2O38201                                            
005300*                                                                         
005500*   ÄNDRINGAR:                                                            
005600* 09-SEP-2021 STORY 2224095 REMOVED WS-TIERSDAT-VIPS CTRL.                
005700*             ADDED KVDISP-SEND-DC CHECK FOR ALL KDERS PARTS              
005800*             EXCEPT KDERS = 52.                                          
005900                                                                          
006000     SKIP3                                                                
006100 ENVIRONMENT DIVISION.                                                    
006200     EJECT                                                                
006300 DATA DIVISION.                                                           
006400 WORKING-STORAGE SECTION.                                                 
006500*    -COPY WY2000W1                                                       
006600     SKIP3                                                                
006700 77  IDPGM                       PIC X(08)   VALUE 'W2038200'.            
006800                                                                          
006900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007100                                                                          
007200 77  YES                         PIC X       VALUE 'Y'.                   
007300 77  JA                          PIC X       VALUE 'J'.                   
007400 77  NEJ                         PIC X       VALUE 'N'.                   
007500                                                                          
007600*01  -COPY WWDCKONS                                                       
007700                                                                          
007800 77  REFILL-PART-SW              PIC X       VALUE 'J'.                   
007900     88  REFILL-PART                         VALUE 'J'.                   
008000     88  NOT-REFILL-PART                     VALUE 'N'.                   
008100                                                                          
008200 77  ERSATT-SW                   PIC X       VALUE 'N'.                   
008300     88  ERSATT-PART                         VALUE 'J'.                   
008400                                                                          
008500 77  AKTIV                       PIC X       VALUE 'A'.                   
008600 77  PASSIV                      PIC X       VALUE 'P'.                   
008700 77  DEFINITIV                   PIC S9      VALUE +1  COMP-3.            
008800 77  DC-MAX-2382                 PIC 9(3)    VALUE 6.                     
008900 77  MOD-IX                      PIC 9(3)    VALUE ZERO.                  
009000 77  IX                          PIC 9(3)    VALUE ZERO.                  
009100 77  IX2                         PIC 9(3)    VALUE ZERO.                  
009200 77  INDX                        PIC S9(3)   VALUE ZERO.                  
009300 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
009400 77  IX-TILL                     PIC 9(3)    VALUE ZERO.                  
009500 77  IX-VV                       PIC 9(2)    VALUE ZERO.                  
009600 77  IX-CD                       PIC 9(3)    VALUE ZERO.                  
009700 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
009800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009900 77  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
010000 01  DAGENS-PER                  PIC 9(4)    VALUE ZERO.                  
010100 01  DAG-PER REDEFINES DAGENS-PER.                                        
010200         05 DAGENS-AA            PIC 9(2).                                
010300         05 DAGENS-PP            PIC 9(2).                                
010400 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
010500 77  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
010600                                                                          
010700 77  SW-KVPB-SEP                 PIC X       VALUE ' '.                   
010800     88  SW-KVPB-SEP-JA                      VALUE 'J'.                   
010900     88  SW-KVPB-SEP-NEJ                     VALUE 'N'.                   
011000                                                                          
011100 77  SW-KVPB-PLAN                PIC X       VALUE ' '.                   
011200     88  KVPB-PLAN-UPD-JA                    VALUE 'J'.                   
011300     88  KVPB-PLAN-UPD-NEJ                   VALUE 'N'.                   
011400                                                                          
011500 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
011600     88  TRAFF-OK                            VALUE 'J'.                   
011700     88  NO-TRAFF                            VALUE 'N'.                   
011800                                                                          
011900 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
012000     88  LYNK-PART                           VALUE 'J'.                   
012100                                                                          
012200 01  WS.                                                                  
012300  05 WS-TEMFSINF.                                                         
012400    10 WS-TEMFSINF-SOURCE        PIC X(08)   VALUE SPACE.                 
012500    10 FILLER                    PIC X(03)   VALUE SPACE.                 
012600    10 FILLER                    OCCURS 6.                                
012700      15 WS-TEMFSINF-DC          PIC X(7)    VALUE SPACE.                 
012800      15 FILLER                  PIC X       VALUE SPACE.                 
012900    10 WS-TEMFSINF-TEXT          PIC X(13)   VALUE SPACE.                 
013000                                                                          
013100  05 WS-TEMF-RED-OS.                                                      
013200    10 WS-TEMF-TEXT-OS           PIC X(3)    VALUE SPACE.                 
013300    10 WS-TEMF-OS-NDC            OCCURS 6                                 
013400                                 PIC X(3)    VALUE SPACE.                 
013500  05 WS-TEMF-RED-INVBAL.                                                  
013600    10 FILLER                    PIC X(5)    VALUE 'INVB '.               
013700    10 WS-TEMF-UTRSALDO          PIC -(6)9.                               
013800    10 FILLER                    PIC X       VALUE SPACE.                 
013900                                                                          
014000  05 WS-TEMF-UTRSALDO-NUM        PIC S9(7).                               
014100                                                                          
014200  05 WS-ANT-VV                   PIC  9(2)   VALUE ZERO.                  
014300  05 WS-TIAAVV.                                                           
014400    10 WS-AAR                    PIC  9(2)   VALUE ZERO.                  
014500    10 WS-VV                     PIC  9(2)   VALUE ZERO.                  
014600  05 TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                                 
014700  05 WS-TIAAPER.                                                          
014800    10 TIAA                      PIC  9(2)   VALUE ZERO.                  
014900    10 PER                       PIC  9(2)   VALUE ZERO.                  
015000  05    TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
015100  05 WS-FOM-TOM.                                                          
015200    10 WS-FOM                    PIC  X(2)   VALUE ZERO.                  
015300    10 WS-STRECK                 PIC  X(1)   VALUE '-'.                   
015400    10 WS-TOM                    PIC  X(2)   VALUE ZERO.                  
015500*                                                                         
015600*   WS-TABELL ÄR EN RULLANDE TABELL DÄR                                   
015700*   IX = 1 ÄR DAGENS PERIOD ETT ÅR TILLBAKA                               
015800*   IX = 12 ÄR FÖRRA PERIODEN                                             
015900*                                                                         
016000  05 WS-TABELL    OCCURS 12.                                              
016100    10 WS-PER                    PIC  9(2)   VALUE ZERO.                  
016200    10 WS-FORSTA-V               PIC  9(2)   VALUE ZERO.                  
016300    10 WS-SISTA-V                PIC  9(2)   VALUE ZERO.                  
016400    10 WS-KVOI                   PIC S9(7)   VALUE ZERO.                  
016500  05 WS-BALANCE                  PIC S9(7)   VALUE ZERO.                  
016600  05 WS-TRANSF-BALANCE           PIC S9(7)   VALUE ZERO.                  
016700  05 WS-REST                     PIC S9(7)   VALUE ZERO.                  
016800  05 WS-SLASK                    PIC S9(7)   VALUE ZERO.                  
016900  05 WS-KDERS                    PIC 9(3)    VALUE ZERO.                  
017000  05 WS-FOREG-AAR                PIC  9(4)   VALUE ZERO.                  
017100  05 WS-KVROS                    PIC S9(7)   VALUE ZERO.                  
017200  05 WS-ANTAL-POSTER             PIC S9(7)   VALUE ZERO.                  
017300  05 WS-ANTAL-VECKOR             PIC S9(3)   VALUE ZERO.                  
017400  05 WS-KVOI-SUM                 PIC S9(7)   VALUE ZERO.                  
017500  05 WS-KVAVIS                   PIC S9(9)   VALUE ZERO.                  
017600  05 WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
017700  05 WS-DAPRLIST                 PIC 9(8)    VALUE ZERO.                  
017800  05 WS-SPARA-IDARTNR            PIC 9(9)    VALUE ZERO.                  
017900  05 WS-KVQPACK-3                PIC S9(5)   VALUE ZERO COMP-3.           
018000  05 FL-PRARTBES                 PIC X(1)    VALUE 'N'.                   
018100  05 WS-FLSIM                    PIC X       VALUE SPACES.                
018200  05 WS-PRARTBES                 PIC S9(7)V9(2)                           
018300                                             VALUE ZERO COMP-3.           
018400  05 WS-AVER-COST-NUM            PIC 9(7)V9(2)                            
018500                                             VALUE ZERO.                  
018600  05 WS-AVER-COST-RED            PIC Z(4)9.9(2).                          
018700  05 WS-KR-VIKT                  PIC S9(9)V9(2)                           
018800                                             VALUE ZERO.                  
018900  05 WS-KR-VIKT-RED              PIC  9(9)   VALUE ZERO.                  
019000  05 WS-KR-VOLYM                 PIC S9(9)V9(2)                           
019100                                             VALUE ZERO.                  
019200  05 WS-KR-VOLYM-RED             PIC  9(9)   VALUE ZERO.                  
019300  05 WS-KVAKS                    PIC S9(9)V9(2)                           
019400                                             VALUE ZERO.                  
019500  05 WS-KVPB                     PIC S9(9)V9(2)                           
019600                                             VALUE ZERO.                  
019700  05 WS-TOTAL-ANTAL              PIC 9(9)    VALUE ZERO.                  
019800  05 WS-INDEX                    PIC S9V9(2) VALUE ZERO.                  
019900  05 WS-DASPSEA                  PIC 9(7)    VALUE ZERO.                  
020000  05 WS-OSAKERHET                PIC 9(2)V9  VALUE ZERO.                  
020100  05 WS-SIMIX-SUM                PIC S9(2)V9(2)                           
020200                                             VALUE ZERO.                  
020300  05 WS-SIMIX                    PIC S9(2)V9(2)                           
020400                                             VALUE ZERO.                  
020500  05 WS-SIMIX-X                  PIC X(4).                                
020600  05 WS-SIMIX-N                  REDEFINES WS-SIMIX-X                     
020700                                 PIC 9.9(2).                              
020800  05 WS-SUPERWEEK                OCCURS 6                                 
020900                                 PIC S9(9)V9(2)                           
021000                                             VALUE ZERO.                  
021100  05 WS-AVAILABLE                PIC S9(7)   VALUE ZERO.                  
021200  05 WS-ORDERED                  PIC  9(7)   VALUE ZERO.                  
021300  05 WS-KVAKS-SDC                PIC  9(7)   VALUE ZERO.                  
021400  05 WS-PURCHQTY                 OCCURS 6                                 
021500                                 PIC  9(7)   VALUE ZERO.                  
021600  05 WS-PURCHQTY-SIM             OCCURS 6                                 
021700                                 PIC  9(7)   VALUE ZERO.                  
021800  05 WS-PURCHQTY-SUM             OCCURS 6                                 
021900                                 PIC  9(7)   VALUE ZERO.                  
022000  05 W-PURCHQTY                  OCCURS 6                                 
022100                                 PIC  9(7)   VALUE ZERO.                  
022200  05 WS-RED-PURCHQTY             PIC Z(6)9   VALUE ZERO.                  
022300  05 WS-KVPBREOI                 OCCURS 6                                 
022400                                 PIC  9(6)V9 VALUE ZERO.                  
022500  05 WS-KVPB-REF                 OCCURS 6                                 
022600                                 PIC  9(6)V9 VALUE ZERO.                  
022700  05 WS-RED-KVPB-REF             PIC Z(4)9.9 VALUE ZERO.                  
022800  05 WS-RED-KVPB-REF-NUM         PIC 9(5)V9 VALUE ZERO.                   
022900  05 WS-OVERLAGER                OCCURS 6                                 
023000                                 PIC X       VALUE 'N'.                   
023100  05 WS-ANT-REVIEW               PIC S9(5)   VALUE ZERO.                  
023200  05 WS-KTRL-PRIO                PIC 9(2)    VALUE ZERO.                  
023300  05 WS-COUNT                    PIC 9       VALUE ZERO.                  
023400  05 WS-RESTKVANT                OCCURS 6                                 
023500                                 PIC S9(7)   VALUE ZERO.                  
023600  05 WS-IDREFTYP                 PIC X       VALUE SPACE.                 
023700  05 WS-KVOKS-TOT                PIC S9(7)   VALUE ZERO COMP-3.           
023800  05 WS-HELTAL-BEST              PIC S9(7)   VALUE ZERO COMP-3.           
023900  05 WS-HELTAL-SALDO             PIC S9(7)   VALUE ZERO COMP-3.           
024000  05 WS-SALDO                    PIC S9(7)   VALUE ZERO COMP-3.           
024100  05 WS-IDLOPNR-DC               PIC S9(7)   VALUE ZERO COMP-3.           
024200  05 WSA-IDDC                    PIC X(2)    VALUE SPACE.                 
024300  05 WSA-IDDC-NUM                REDEFINES WSA-IDDC                       
024400                                 PIC 9(2).                                
024500  05 WS-DC-TABELL.                                                        
024600   10  WS-DC-NR                  OCCURS 6                                 
024700                                 PIC X(2).                                
024800  05 WS-DC-MAX                   PIC 9(7)    VALUE ZERO.                  
024900  05 WS-IDDISTR                  OCCURS 6                                 
025000                                 PIC 9(4)    VALUE ZERO.                  
025100  05 WS-IDDC-FROM                OCCURS 6                                 
025200                                 PIC X(2)    VALUE SPACE.                 
025300  05 WS-IDDC-MED                 OCCURS 6                                 
025400                                 PIC X(7)    VALUE SPACE.                 
025500  05 WS-IDKUNDNR                 OCCURS 6                                 
025600                                 PIC S9(7)   VALUE ZERO COMP-3.           
025700  05 WS-TAB-REFTXT               OCCURS 6.                                
025800     07 WS-REF-KDREFTXT          PIC 9(2)    VALUE ZERO.                  
025910                                                                          
025920  05 WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
025930  05 WS-TIINLINL                 OCCURS 6                                 
025940                                 PIC 9(6)    VALUE ZERO.                  
025950  05 WS-AREA                     PIC X(4)    VALUE SPACE.                 
025960  05 WS-ADLAGOMR                 PIC 9(2)    VALUE ZERO.                  
025970  05 WS-KDARBTYP-X3              PIC X(3)    VALUE 'ESC'.                 
025980  05 WS-CDC-11                   PIC X(2)    VALUE '11'.                  
025990  05 WS-TIERSDAT-VIPS            PIC 9(5)    VALUE ZEROES.                
026000  05 WS-KVDISP-SEND-DC           PIC S9(7)   VALUE ZERO COMP-3.           
026001  05 WS-VKART                    PIC S9(7)   VALUE ZERO COMP-3.           
026002  05 WS-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO COMP-3.        
026010  05 WS-REAIRCO                  PIC S9(6)V9(1) VALUE ZERO COMP-3.        
026020  05 WS-PRFRAKT                  PIC S9(7)      VALUE ZERO COMP-3.        
026030  05 WS-AIR-COST-SEK             PIC 9(7) VALUE ZERO.                     
026100                                                                          
026200*********************************************************                 
026300*    WS-MSGI-AREA-2382                                                    
026400*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
026500*           (I MSGI-SPAR-AREA)                                            
026600*********************************************************                 
026700  05 WS-MSGI-AREA-2382.                                                   
026800    10 WS-MSGI-IDTRANS-2382      PIC X(4)    VALUE '2382'.                
026900    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
027000    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
027100    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
027200    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
027300    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
027400                                                                          
027500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
027600                                                                          
027700  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
027800  05 WS-IDARTNR-NUM              REDEFINES WS-IDARTNR                     
027900                                 PIC 9(9).                                
028000  05 WS-IDPERSON-BUY             PIC X(3)    VALUE SPACE.                 
028100  05 WS-IDPERSON-BUY-NUM         REDEFINES WS-IDPERSON-BUY                
028200                                 PIC 9(3).                                
028300  05 WS-IDPERSON-BUY-RED         PIC Z(2)9.                               
028400  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
028500  05 WS-STATUS                   PIC X       VALUE SPACE.                 
028600  05 WS-DISPLAY                  PIC X(30)   VALUE SPACE.                 
028700  05 FILLER                      PIC X(16)   VALUE                        
028800                                             'WS-PGM-SEKTION'.            
028900  05 WS-PGM-SEKTION              PIC X(24)   VALUE SPACE.                 
029000  05 FILLER                      PIC X(16)   VALUE                        
029100                                             'WS-IMS-SEKTION'.            
029200  05 WS-IMS-SEKTION              PIC X(24)   VALUE SPACE.                 
029300  05 FILLER                      PIC X(16)   VALUE                        
029400                                             'WS-DB2-SEKTION'.            
029500  05 WS-DB2-SEKTION              PIC X(24)   VALUE SPACE.                 
029600  05 WS-MEDD-ERS.                                                         
029700    07 WS-MEDD-ERSKOD            PIC 9(2)    VALUE ZERO.                  
029800    07 FILLER                    PIC X(1)    VALUE SPACE.                 
029900    07 WS-MEDD-TEXT              PIC X(23)   VALUE SPACE.                 
030000  05 WS-FLERSATT-X2X3            PIC X(1)    VALUE SPACE.                 
030100     EJECT                                                                
030200 01 NYCKLAR-TP4TRAN.                                                      
030300     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
030400     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
030500                                                                          
030600 01   WS-TESTFAELT.                                                       
030700   03 WS-A                       PIC X(7)    VALUE SPACE.                 
030800   03 FILLER                     PIC X       VALUE '/'.                   
030900   03 WS-B                       PIC X(7)    VALUE SPACE.                 
031000   03 FILLER                     PIC X       VALUE '/'.                   
031100   03 WS-C                       PIC X(7)    VALUE SPACE.                 
031200   03 FILLER                     PIC X       VALUE '/'.                   
031300   03 WS-D                       PIC X(7)    VALUE SPACE.                 
031400   03 FILLER                     PIC X       VALUE '/'.                   
031500 01   WS-TESTFAELT-2.                                                     
031600   03 WS-TIVV-1-TF2              PIC 9(2)    VALUE ZERO.                  
031700   03 FILLER                     PIC X       VALUE '/'.                   
031800   03 WS-KVOI-1-TF2              PIC 9(2)    VALUE ZERO.                  
031900   03 FILLER                     PIC X       VALUE '/'.                   
032000   03 WS-TIVV-2-TF2              PIC 9(2)    VALUE ZERO.                  
032100   03 FILLER                     PIC X       VALUE '/'.                   
032200   03 WS-KVOI-2-TF2              PIC 9(2)    VALUE ZERO.                  
032300   03 FILLER                     PIC X       VALUE '/'.                   
032400   03 WS-TIVV-3-TF2              PIC 9(2)    VALUE ZERO.                  
032500   03 FILLER                     PIC X       VALUE '/'.                   
032600   03 WS-KVOI-3-TF2              PIC 9(2)    VALUE ZERO.                  
032700   03 FILLER                     PIC X       VALUE '/'.                   
032800   03 WS-TIVV-4-TF2              PIC 9(2)    VALUE ZERO.                  
032900   03 FILLER                     PIC X       VALUE '/'.                   
033000   03 WS-KVOI-4-TF2              PIC 9(2)    VALUE ZERO.                  
033100   03 FILLER                     PIC X       VALUE '/'.                   
033200   03 WS-TIVV-5-TF2              PIC 9(2)    VALUE ZERO.                  
033300   03 FILLER                     PIC X       VALUE '/'.                   
033400   03 WS-KVOI-5-TF2              PIC 9(2)    VALUE ZERO.                  
033500   03 FILLER                     PIC X       VALUE '/'.                   
033600   03 WS-KVOI-SUM-TF2            PIC 9(2)    VALUE ZERO.                  
033700   03 FILLER                     PIC X       VALUE '/'.                   
033800   03 WS-ANTAL-DC41-TF2          PIC 9(1)    VALUE ZERO.                  
033900   03 FILLER                     PIC X       VALUE '/'.                   
034000   03 WS-ANTAL-DC42-TF2          PIC 9(1)    VALUE ZERO.                  
034100   03 FILLER                     PIC X       VALUE '/'.                   
034200   03 WS-ANTAL-DC43-TF2          PIC 9(1)    VALUE ZERO.                  
034300   03 FILLER                     PIC X       VALUE '/'.                   
034400   03 WS-ANTAL-DC51-TF2          PIC 9(1)    VALUE ZERO.                  
034500 01   WS-TESTFAELT-3.                                                     
034600   03 WS-KR-VIKT-TF3             PIC 9(9)    VALUE ZERO.                  
034700   03 FILLER                     PIC X       VALUE '/'.                   
034800   03 WS-KR-VOLYM-TF3            PIC 9(9)    VALUE ZERO.                  
034900 01   WS-TESTFAELT-4.                                                     
035000   03 WS-FL1                     PIC X       VALUE SPACE.                 
035100   03 FILLER                     PIC X       VALUE '/'.                   
035200   03 WS-FL2                     PIC X       VALUE SPACE.                 
035300   03 FILLER                     PIC X       VALUE '/'.                   
035400   03 WS-FL3                     PIC X       VALUE SPACE.                 
035500   03 FILLER                     PIC X       VALUE '/'.                   
035600   03 WS-FL4                     PIC X       VALUE SPACE.                 
035700   03 FILLER                     PIC X       VALUE '/'.                   
035800   03 WS-FL5                     PIC X       VALUE SPACE.                 
035900   03 FILLER                     PIC X       VALUE '/'.                   
036000   03 WS-FL6                     PIC X       VALUE SPACE.                 
036100   03 FILLER                     PIC X       VALUE '/'.                   
036200   03 WS-FL7                     PIC X       VALUE SPACE.                 
036300   03 FILLER                     PIC X       VALUE '/'.                   
036400   03 WS-FL8                     PIC X       VALUE SPACE.                 
036500 01   WS-TESTFAELT-5.                                                     
036600   03 WS-STOCK-5                 PIC 9(5)    VALUE ZERO.                  
036700   03 FILLER                     PIC X       VALUE '/'.                   
036800   03 WS-KVAKS-5                 PIC 9(5)    VALUE ZERO.                  
036900   03 FILLER                     PIC X       VALUE '/'.                   
037000   03 WS-ORDERED-5               PIC 9(5)    VALUE ZERO.                  
037100   03 FILLER                     PIC X       VALUE '/'.                   
037200   03 WS-PURCHQTY-5              PIC 9(5)    VALUE ZERO.                  
037300   03 FILLER                     PIC X       VALUE '/'.                   
037400   03 WS-KVPB-REF-5              PIC 9(5)    VALUE ZERO.                  
037500 01   WS-TESTFAELT-6.                                                     
037600   03 WS-FL1-6                   PIC X       VALUE SPACE.                 
037700   03 FILLER                     PIC X       VALUE '/'.                   
037800   03 WS-FL2-6                   PIC X       VALUE SPACE.                 
037900   03 FILLER                     PIC X       VALUE '/'.                   
038000   03 WS-FL3-6                   PIC X       VALUE SPACE.                 
038100   03 FILLER                     PIC X       VALUE '/'.                   
038200   03 WS-FL4-6                   PIC X       VALUE SPACE.                 
038300   03 FILLER                     PIC X       VALUE '/'.                   
038400   03 WS-FL5-6                   PIC X       VALUE SPACE.                 
038500   03 FILLER                     PIC X       VALUE '/'.                   
038600   03 WS-FL6-6                   PIC X       VALUE SPACE.                 
038700   03 FILLER                     PIC X       VALUE '/'.                   
038800   03 WS-FL7-6                   PIC X       VALUE SPACE.                 
038900   03 FILLER                     PIC X       VALUE '/'.                   
039000   03 WS-FL8-6                   PIC X       VALUE SPACE.                 
039100   03 FILLER                     PIC X       VALUE '/'.                   
039200                                                                          
039300 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
039400     88  SW-TRAEFF-JA                        VALUE 'J'.                   
039500     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
039600                                                                          
039700 77  SW-SEASON                   PIC X       VALUE ' '.                   
039800     88  SW-SEASON-JA                        VALUE 'J'.                   
039900     88  SW-SEASON-NEJ                       VALUE 'N'.                   
040000                                                                          
040100 77  SW-KTRL-ERS                 PIC X       VALUE ' '.                   
040200     88  SW-KTRL-ERS-JA                      VALUE 'J'.                   
040300     88  SW-KTRL-ERS-NEJ                     VALUE 'N'.                   
040400                                                                          
040500 77  SW-GILTIGT-DC               PIC X       VALUE ' '.                   
040600     88  SW-GILTIGT-DC-JA                    VALUE 'J'.                   
040700     88  SW-GILTIGT-DC-NEJ                   VALUE 'N'.                   
040800                                                                          
040900 77  SW-HAEMTA-INPUT-FAELT       PIC X       VALUE 'J'.                   
041000     88  SW-HAEMTA-INPUT-FAELT-JA            VALUE 'J'.                   
041100     88  SW-HAEMTA-INPUT-FAELT-NEJ           VALUE 'N'.                   
041200                                                                          
041300 77  CRITICAL-AIR-SW             PIC X       VALUE 'N'.                   
041400     88  CRITICAL-AIR-JA                     VALUE 'J'.                   
041500     88  CRITICAL-AIR-NEJ                    VALUE 'N'.                   
041600                                                                          
041700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
041800     88  INDATA-OK                           VALUE 'J'.                   
041900     88  INDATA-FEL                          VALUE 'N'.                   
042000                                                                          
042100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
042200     88  NYCKLAR-OK                          VALUE 'J'.                   
042300     88  NYCKLAR-FEL                         VALUE 'N'.                   
042400                                                                          
042500 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
042600     88  SIM-INDEX-JA                        VALUE 'J'.                   
042700     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
042800                                                                          
042900 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
043000     88  SIM-ANTAL-JA                        VALUE 'J'.                   
043100     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
043200                                                                          
043300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
043400     88  EGEN-MID                            VALUE '2382'.                
043500     88  GODK-MID                            VALUE '2381' '2382'          
043600                                                   '2353' '2354'          
043700                                                   '2355' '2356'          
043800                                                   '2357' '2358'          
043900                                                   '2359'.                
044000     88  HELP-MID                            VALUE '0551'.                
044100                                                                          
044200 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
044300     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
044400     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
044500                                                                          
044600     EJECT                                                                
044700 01  FILLER                      PIC X(16) VALUE 'REFILLFÖRSLAG'.         
044800     SKIP3                                                                
044900*01  -COPY W271RTXT                                                       
045000     EJECT                                                                
045100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
045200 01  GENERELLA-SUBPROGRAM.                                                
045300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
045400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
045500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
045600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
045700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
045800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
045900     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
046000     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
046100     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
046200     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
046300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
046400     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
046500     EJECT                                                                
046600*01  -COPY WWDC99                                                         
046700*01  -COPY WWDC99        -PRE REFILL-                                     
046800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
046900*01 -COPY WMEDAREA                                                        
047000     EJECT                                                                
047100*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
047200*01  -COPY WDECAREA                                                       
047300     EJECT                                                                
047400*    --- PARAMETRAR TILL W271REFL                                         
047500*01 -COPY W271REFL       -PRE W271-                                       
047600     EJECT                                                                
047700*    --- PARAMETRAR TILL W271UTIL                                         
047800*01 -COPY W271UTIL                                                        
047900     EJECT                                                                
048000*    --- PARAMETRAR TILL W271UTUP                                         
048100*01 -COPY W271UTUP       -PRE W271-                                       
048200     EJECT                                                                
048300*    --- PARAMETRAR TILL W272UTUP                                         
048400*01 -COPY W272UTUP       -PRE W272-                                       
048500     EJECT                                                                
048600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
048700*   -COPY W005WDK7                                                        
048800     EJECT                                                                
048900 01  MESSAGE-CODES.                                                       
049000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
049100     03  CONFLICT                PIC X(3)    VALUE '002'.                 
049200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
049300     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
049400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
049500     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
049600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
049700     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
049800     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
049900     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
050000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
050100     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
050200     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
050300     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
050400     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
050500     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
050600     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
050610     03  ERR-HIGH-AIR-COST       PIC X(3)    VALUE '369'.                 
050700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
050800     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
050900     03  INF-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
051000                                                                          
051100                                                                          
051200                                                                          
051300 01  MEDDELANDE.                                                          
051400     03  MED-1                  PIC X(30)                                 
051500         VALUE 'TYPE : A,B,C,L OR T           '.                          
051600     03  MED-2                  PIC X(30)                                 
051700         VALUE 'STATUS : R OR N               '.                          
051800     03  MED-3                  PIC X(30)                                 
051900         VALUE 'FORECAST WRONG                '.                          
052000     03  MED-4                  PIC X(30)                                 
052100         VALUE 'PURCHQTY WRONG                '.                          
052200     03  MED-5                  PIC X(30)                                 
052300         VALUE 'AUT REFILL ORDERING WRONG     '.                          
052400     03  MED-6                  PIC X(30)                                 
052500         VALUE 'CAN NOT UPDATE WITH NEW KEY   '.                          
052600     03  MED-7                  PIC X(30)                                 
052700         VALUE 'OT: A,B,L OR T                '.                          
052800     03  MED-8                  PIC X(30)                                 
052900         VALUE 'CONFLICT OT/VENDOR            '.                          
053000     03  MED-9                  PIC X(30)                                 
053100         VALUE 'NO VALID PRICE                '.                          
053200     03  MED-10                 PIC X(30)                                 
053300         VALUE 'UNEVEN MULTIPEL OF Q1         '.                          
053400     03  MED-11                 PIC X(30)                                 
053500         VALUE 'DC NOT A VALID TRANSFER       '.                          
053600     03  MED-12                 PIC X(30)                                 
053700         VALUE 'PURCHQTY WRONG TRANSFER       '.                          
053800     03  MED-13                 PIC X(30)                                 
053900         VALUE 'TRANSFER NOT ALLOWED          '.                          
054000     03  MED-14                 PIC X(30)                                 
054100         VALUE 'MIXED OT NOT ALLOWED          '.                          
054200     03  MED-15                 PIC X(30)                                 
054300         VALUE 'DISTRICT NOT FOUND            '.                          
054400     03  MED-16                  PIC X(30)                                
054500         VALUE 'LYNK & CO PART                '.                          
054510     03  MED-17                 PIC X(30)                                 
054520         VALUE 'PART LOCKED FROM AIRFREIGHT   '.                          
054600                                                                          
054700     EJECT                                                                
054800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
054900*01  -COPY WDATAREA                                                       
055000     EJECT                                                                
055100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
055200*                                                                         
055300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
055400     SKIP3                                                                
055500*01 -COPY WMSGINIT                                                        
055600     EJECT                                                                
055700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
055800*                                                                         
055900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
056000     SKIP3                                                                
056100*01  MID -COPY W2I38201                                                   
056200     EJECT                                                                
056300*    --- VID HOPP FRÅN 2381 ANVÄNDS W2I38101                              
056400*    ---                                                                  
056500*01  MID -COPY W2I38101                                                   
056600     EJECT                                                                
056700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
056800     SKIP3                                                                
056900*01  -COPY WMSGAREA                                                       
057000     EJECT                                                                
057100     03  MOD REDEFINES MSG-AREA.                                          
057200*      05  -COPY W2O38201                                                 
057300     EJECT                                                                
057400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
057500     SKIP3                                                                
057600*01  -COPY WMFSAREA                                                       
057700     EJECT                                                                
057800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
057900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
058000                                                                          
058100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
058200 01  DB2-WS.                                                              
058300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
058400         88  CURSOR-OK                      VALUE 000.                    
058500         88  LINES-FOUND                    VALUE 000.                    
058600         88  LINES-MISSING                  VALUE 100.                    
058700         88  RESOURCE-WRONG                 VALUE 904.                    
058800     03  GOOD-SQLCODECODES.                                               
058900         05  GOOD-SQLCODE OCCURS 5                                        
059000             INDEXED BY SQLCODE-IX PIC 9(3).                              
059100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
059200     EJECT                                                                
059300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
059400*                                                                         
059500     EJECT                                                                
059600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
059700     SKIP3                                                                
059800 01  NYCKLAR-TILL-DLI.                                                    
059900     03  W-IDARTNR-X.                                                     
060000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
060100     03  W-IDDC-X.                                                        
060200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
060300     03  W-RECEIVING-IDDC        PIC X(2)    VALUE SPACE.                 
060400     03  W-IDDC-B6-X.                                                     
060500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
060600     03  W-IDDC-REF-B6-X.                                                 
060700         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
060800     03  W-IDDC-B616-X.                                                   
060900         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
061000     03  W-IDLAND-X.                                                      
061100         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
061200     03  W-IDUSER-X.                                                      
061300         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
061400     03  W-IDSKYLT-X.                                                     
061500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
061600     03  W-IDLEVNR-21-X.                                                  
061700         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
061800     03  W-DAPRLIST-21-N.                                                 
061900         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
062000                                                                          
062100     03  W-KDNOTTYP-X.                                                    
062200         05  W-KDNOTTYP           PIC  S9(01)  COMP-3 VALUE ZERO.         
062300                                                                          
062400     03 W-WDE301KY-X.                                                     
062500         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
062600         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
062700         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
062800         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
062900         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
063000                                                                          
063100     03 W-WDE301KY-MIN-X.                                                 
063200         05  W-IDDC-301-MIN      PIC X(2)  VALUE SPACE.                   
063300         05  W-IDPERSON-BUY-301-MIN                                       
063400                                 PIC S9(3) VALUE ZERO COMP-3.             
063500         05  W-KDREFTYP-301-MIN  PIC X     VALUE SPACE.                   
063600         05  W-IDARTNR-301-MIN   PIC S9(9) VALUE ZERO COMP-3.             
063700         05  W-IDDISTR-301-MIN   PIC S9(5) VALUE ZERO COMP-3.             
063800                                                                          
063900     03 W-WDE301KY-MAX-X.                                                 
064000         05  W-IDDC-301-MAX      PIC X(2)  VALUE HIGH-VALUE.              
064100         05  W-IDPERSON-BUY-301-MAX                                       
064200                                 PIC S9(3) VALUE +999 COMP-3.             
064300         05  W-KDREFTYP-301-MAX  PIC X     VALUE HIGH-VALUE.              
064400         05  W-IDARTNR-301-MAX   PIC S9(9)                                
064500                                         VALUE +999999999 COMP-3.         
064600         05  W-IDDISTR-301-MAX   PIC S9(5) VALUE +99999 COMP-3.           
064700                                                                          
064800     03 W-WDE3B1KY-MIN-X.                                                 
064900         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
065000         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
065100         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
065200         05  FILLER              PIC X(2)  VALUE LOW-VALUE.               
065300                                                                          
065400     03 W-WDE3B1KY-MAX-X.                                                 
065500         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
065600         05  W-IDARTNR-MAX       PIC S9(9)                                
065700                                          VALUE +999999999 COMP-3.        
065800         05  W-IDDC-MAX          PIC X(2)  VALUE HIGH-VALUE.              
065900         05  FILLER              PIC X(2)  VALUE HIGH-VALUE.              
066000                                                                          
066100                                                                          
066200   03  W-WDN611KY-X.                                                      
066300     05  W-IDFORDON              PIC S9(2)   VALUE ZERO  COMP-3.          
066400     05  W-TIOMBRYT-1            PIC S9(7)   VALUE ZERO  COMP-3.          
066500     SKIP2                                                                
066600                                                                          
066700   03  W-WDD7A1KY-MIN.                                                    
066800     05  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
066900     05  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
067000     05  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
067100                                                                          
067200   03  W-WDD7A1KY-MAX.                                                    
067300     05  W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.                  
067400     05  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.            
067500     05  FILLER            PIC S9(3)  COMP-3 VALUE +999.                  
067600                                                                          
067700   03  W-IDARTNR-TILLK-X.                                                 
067800     05  W-IDARTNR-TILLK         PIC S9(9)   COMP-3.                      
067900   03  W-IDARTNR-ERS-LOW-X.                                               
068000     05  W-IDARTNR-ERS-LOW       PIC S9(9)   COMP-3  VALUE ZERO.          
068100   03  W-IDARTNR-ERS-HIGH-X.                                              
068200     05  W-IDARTNR-ERS-HIGH      PIC S9(9)   COMP-3                       
068300                                  VALUE +999999999.                       
068400                                                                          
068500   03  W-W6D1HSEQ-X.                                                      
068600     05  W-IDARTNR-HSEQ  PIC S9(9)   VALUE ZERO  COMP-3.                  
068700                                                                          
068800   03  W-KDSEGKEY-X.                                                      
068900         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
069000                                                                          
069100   03  W-IDLEVNR-X.                                                       
069200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
069300                                                                          
069400     SKIP2                                                                
069500*    --- STATUS-KOD FRÅN IMS                                              
069600 01  STATUS-WS                   PIC XX.                                  
069700     88  SEGMENT-FINNS                       VALUE '  '.                  
069800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
069900     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
070000                                                   'GB'.                  
070100     SKIP2                                                                
070200 01  GODK-STATUSKODER.                                                    
070300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
070400     SKIP3                                                                
070500 01  SSA1                        PIC X(128).                              
070600 01  SSA2                        PIC X(128).                              
070700 01  SSA3                        PIC X(128).                              
070800     EJECT                                                                
070900*    --- IMS FUNKTIONSKODER                                               
071000*01  -COPY W0003                                                          
071100     EJECT                                                                
071200*    ---  DLI INPUT-OUTPUT AREA                                           
071300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
071400     SKIP3                                                                
071500 01  DLI-IO-AREA-WDK701.                                                  
071600*        05  -COPY WDK701                                                 
071700     EJECT                                                                
071800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
071900     SKIP3                                                                
072000 01  DLI-IO-AREA-WDK711.                                                  
072100*        05  -COPY WDK711                                                 
072200     EJECT                                                                
072300 01  FILLER                  PIC X(24)                                    
072400                                 VALUE 'DLI-IO-WDK711-TRANS'.             
072500     SKIP3                                                                
072600 01  DLI-IO-AREA-WDK711-TRANS.                                            
072700*        05  -COPY WDK711    -PRE TRANS-                                  
072800     EJECT                                                                
072900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK727'.             
073000     SKIP3                                                                
073100 01  DLI-IO-AREA-WDK727.                                                  
073200*        05  -COPY WDK727                                                 
073300     EJECT                                                                
073400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
073500     SKIP3                                                                
073600 01  DLI-IO-AREA-WDK712.                                                  
073700*        05  -COPY WDK712                                                 
073800     EJECT                                                                
073900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
074000     SKIP3                                                                
074100 01  DLI-IO-AREA-WDL711.                                                  
074200*        05  -COPY WDL711                                                 
074300     EJECT                                                                
074400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
074500     SKIP3                                                                
074600 01  DLI-IO-AREA-WDD301.                                                  
074700*        05  -COPY WDD301                                                 
074800     EJECT                                                                
074900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
075000     SKIP3                                                                
075100 01  DLI-IO-AREA-WDD311.                                                  
075200*        05  -COPY WDD311                                                 
075300     EJECT                                                                
075400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE301'.             
075500     SKIP3                                                                
075600 01  DLI-IO-AREA-WDE301.                                                  
075700*        05  -COPY WDE301                                                 
075800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE3B1'.             
075900     SKIP3                                                                
076000 01  DLI-IO-AREA-WDE3B1.                                                  
076100*        05  -COPY WDE3B1                                                 
076200     EJECT                                                                
076300     EJECT                                                                
076400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
076500     SKIP3                                                                
076600 01  DLI-IO-AREA-WDK601.                                                  
076700*        05  -COPY WDK601                                                 
076800     EJECT                                                                
076900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
077000     SKIP3                                                                
077100 01  DLI-IO-AREA-WDK611.                                                  
077200*        05  -COPY WDK611                                                 
077300     EJECT                                                                
077400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK621'.             
077500     SKIP3                                                                
077600 01  DLI-IO-AREA-WDK621.                                                  
077700*        05  -COPY WDK621                                                 
077800     EJECT                                                                
077900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDN601'.            
078000     SKIP3                                                                
078100 01  DLI-IO-AREA-WDN601.                                                  
078200*  03     -COPY WDN601                                                    
078300     EJECT                                                                
078400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDN611'.            
078500     SKIP3                                                                
078600 01  DLI-IO-AREA-WDN611.                                                  
078700*  03     -COPY WDN611                                                    
078800     EJECT                                                                
078900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD701'.            
079000     SKIP3                                                                
079100 01  DLI-IO-AREA-WDD701.                                                  
079200*  03   -COPY WDD701  -PRE WDD701-                                        
079300     EJECT                                                                
079400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD702'.            
079500     SKIP3                                                                
079600 01  DLI-IO-AREA-WDD702.                                                  
079700*  03   -COPY WDD702  -PRE WDD702-                                        
079800     EJECT                                                                
079900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD7A1'.            
080000     SKIP3                                                                
080100 01  DLI-IO-AREA-WDD7A1.                                                  
080200*  03   -COPY WDD7A1  -PRE WDD7A1-                                        
080300     EJECT                                                                
080400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-INLA11'.            
080500     SKIP3                                                                
080600 01  DLI-IO-AREA-INLA11.                                                  
080700*  03  W6INLA11 -COPY W6D111 -PRE INLA-                                   
080800     EJECT                                                                
080900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDK901'.            
081000     SKIP3                                                                
081100 01  DLI-IO-AREA-WDK901.                                                  
081200*  03  -COPY WDK901  -PRE WDK9-                                           
081300     EJECT                                                                
081400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF101'.                      
081500 01  DLI-IO-WDF101.                                                       
081600*    03  -COPY WDF101                                                     
081700     EJECT                                                                
081800                                                                          
081900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
082000 01   DLI-IO-AREA-B601.                                                   
082100*     03  -COPY WDB601                                                    
082200     EJECT                                                                
082300                                                                          
082400 01  FILLER               PIC X(16)   VALUE 'WDB601-2 AREA'.              
082500 01   DLI-IO-AREA-2-B601.                                                 
082600*     03  -COPY WDB601 -PRE B6-                                           
082700     EJECT                                                                
082800                                                                          
082900 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
083000 01   DLI-IO-AREA-B616.                                                   
083100*     03  -COPY WDB616 -PRE B6-                                           
083200     EJECT                                                                
083300                                                                          
083400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL601'.            
083500 01  DLI-IO-AREA-WDL601.                                                  
083600*  03  WDL601 -COPY WDL601 -PRE WDL6-                                     
083700     EJECT                                                                
083800                                                                          
083900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL611'.            
084000 01  DLI-IO-AREA-WDL611.                                                  
084100*  03  WDL611 -COPY WDL611                                                
084200     EJECT                                                                
084300                                                                          
084400 01  FILLER                      PIC X(16)  VALUE 'TP5IDDC-AREA'.         
084500                                                                          
084600*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
084700     EJECT                                                                
084800 01  FILLER                      PIC X(16)  VALUE 'TP5COMM-AREA'.         
084900                                                                          
085000*01  -COPY TP5COMM -PRE TP5COMM-                                          
085100     EJECT                                                                
085200 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
085300                                                                          
085400*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
085500     EJECT                                                                
085600     EXEC SQL INCLUDE TP5IDDC END-EXEC.                                   
085700     EJECT                                                                
085800     EXEC SQL INCLUDE TP5COMM END-EXEC.                                   
085900     EJECT                                                                
086000     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
086100     EJECT                                                                
086200 LINKAGE SECTION.                                                         
086300                                                                          
086400*01  -COPY W0009   -PRE MSG-                                              
086500     EJECT                                                                
086600*01  -COPY W0008  -PRE  WDP7-                                             
086700     05  FILLER                          PIC X.                           
086800     EJECT                                                                
086900*01  -COPY W0008  -PRE  WDK7-                                             
087000     05  FILLER                          PIC X.                           
087100     EJECT                                                                
087200*01  -COPY W0008  -PRE WDK7I-                                             
087300     05  FILLER                          PIC X.                           
087400     EJECT                                                                
087500*01  -COPY W0008  -PRE WDK72-                                             
087600     05  FILLER                          PIC X.                           
087700     EJECT                                                                
087800*01  -COPY W0008  -PRE  WDL7-                                             
087900     05  FILLER                          PIC X.                           
088000     EJECT                                                                
088100*01  -COPY W0008  -PRE  WDD3-                                             
088200     05  FILLER                          PIC X.                           
088300     EJECT                                                                
088400*01  -COPY W0008  -PRE WDE3-                                              
088500     05  FILLER                          PIC X.                           
088600     EJECT                                                                
088700*01  -COPY W0008  -PRE WDE3B-                                             
088800     05  FILLER                          PIC X.                           
088900     EJECT                                                                
089000*01  -COPY W0008  -PRE WDK6-                                              
089100     05  FILLER                          PIC X.                           
089200     EJECT                                                                
089300*01  -COPY W0008  -PRE WDN6-                                              
089400     05  FILLER                          PIC X.                           
089500     EJECT                                                                
089600*01  -COPY W0008  -PRE WDD7-                                              
089700     05  FILLER                          PIC X.                           
089800     EJECT                                                                
089900*01  -COPY W0008  -PRE WDD7A1-                                            
090000     05  FILLER                          PIC X.                           
090100     EJECT                                                                
090200*01  -COPY W0008  -PRE WDK9-                                              
090300     05  FILLER                          PIC X.                           
090400     EJECT                                                                
090500*01  -COPY W0008  -PRE WDB6-                                              
090600     05  FILLER                          PIC X.                           
090700     EJECT                                                                
090800*01  -COPY W0008  -PRE WDB6-GN-                                           
090900     05  FILLER                          PIC X.                           
091000     EJECT                                                                
091100*01  -COPY W0008  -PRE  WDL6-                                             
091200     05  FILLER                          PIC X.                           
091300     EJECT                                                                
091400*01  -COPY W0008  -PRE UTIL-WDK6-                                         
091500     05  FILLER                          PIC X.                           
091600     EJECT                                                                
091700*01  -COPY W0008  -PRE UTIL-WDK7-                                         
091800     05  FILLER                          PIC X.                           
091900     EJECT                                                                
092000*01  -COPY W0008  -PRE UTIL-WDB6-                                         
092100     05  FILLER                          PIC X.                           
092200     EJECT                                                                
092300*****W271REFL**********                                                   
092400 01  REFL1-2501-PCB                      PIC X.                           
092500 01  REFL1-WDB6-PCB                      PIC X.                           
092600 01  REFL1-WDK7-PCB                      PIC X.                           
092700 01  REFL1-UTIL-WDK6-PCB                 PIC X.                           
092800 01  REFL1-UTIL-WDK7-PCB                 PIC X.                           
092900 01  REFL1-UTIL-WDB6-PCB                 PIC X.                           
093000*****W271UTUP**********                                                   
093100 01  UTUP1-WDK7-PCB                      PIC X.                           
093200 01  UTUP1-WDB6-PCB                      PIC X.                           
093300 01  UTUP1-UTIL-WDK6-PCB                 PIC X.                           
093400 01  UTUP1-UTIL-WDK7-PCB                 PIC X.                           
093500 01  UTUP1-UTIL-WDB6-PCB                 PIC X.                           
093600*****W272UTUP**********                                                   
093700 01  U2-WDK6-PCB                         PIC X.                           
093800 01  U2-WDB6-PCB                         PIC X.                           
093900 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
094000 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
094100 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
094200 01  U2-PBTO-W222-REFL1-2501-PCB         PIC X.                           
094300 01  U2-PBTO-W222-REFL1-WDB6R-PCB        PIC X.                           
094400 01  U2-PBTO-W222-REFL1-WDK7R-PCB        PIC X.                           
094500 01  U2-PBTO-W222-REFL1-UTIL-K6-PCB      PIC X.                           
094600 01  U2-PBTO-W222-REFL1-UTIL-K7-PCB      PIC X.                           
094700 01  U2-PBTO-W222-REFL1-UTIL-B6-PCB      PIC X.                           
094800 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
094900 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
095000 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
095100 01  U2-PBTO-W222-UTUP1-WDK7-PCB         PIC X.                           
095200 01  U2-PBTO-W222-UTUP1-WDB6-PCB         PIC X.                           
095300 01  U2-PBTO-W222-UTUP1-UTIL-K6-PCB      PIC X.                           
095400 01  U2-PBTO-W222-UTUP1-UTIL-K7-PCB      PIC X.                           
095500 01  U2-PBTO-W222-UTUP1-UTIL-B6-PCB      PIC X.                           
095600 01  U2-REFL2-2501-PCB                   PIC X.                           
095700 01  U2-REFL2-WDB6-PCB                   PIC X.                           
095800 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
095900 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
096000 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
096100 01  U2-W222-WDK6-PCB                    PIC X.                           
096200 01  U2-W222-WDK7-PCB                    PIC X.                           
096300 01  U2-W222-ARTM-PCB                    PIC X.                           
096400 01  U2-W222-2501-PCB                    PIC X.                           
096500 01  U2-W222-WDB6R-PCB                   PIC X.                           
096600 01  U2-W222-WDK7R-PCB                   PIC X.                           
096700 01  U2-W222-WDB6-PCB                    PIC X.                           
096800 01  U2-W222-WDD7-PCB                    PIC X.                           
096900 01  U2-W222-WDK7E-PCB                   PIC X.                           
097000 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
097100 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
097200 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
097300 01  U2-W222-UTUP1-WDK7-PCB              PIC X.                           
097400 01  U2-W222-UTUP1-WDB6-PCB              PIC X.                           
097500 01  U2-W222-UTUP1-UTIL-WDK6-PCB         PIC X.                           
097600 01  U2-W222-UTUP1-UTIL-WDK7-PCB         PIC X.                           
097700 01  U2-W222-UTUP1-UTIL-WDB6-PCB         PIC X.                           
097800     EJECT                                                                
097900                                                                          
098000 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK7-PCB WDK7I-PCB            
098100     WDK72-PCB WDL7-PCB WDD3-PCB WDE3-PCB WDE3B-PCB                       
098200     WDK6-PCB  WDN6-PCB WDD7-PCB WDD7A1-PCB WDK9-PCB                      
098300     WDB6-PCB  WDB6-GN-PCB   WDL6-PCB                                     
098400     UTIL-WDK6-PCB UTIL-WDK7-PCB UTIL-WDB6-PCB                            
098500     REFL1-2501-PCB                                                       
098600     REFL1-WDB6-PCB                                                       
098700     REFL1-WDK7-PCB                                                       
098800     REFL1-UTIL-WDK6-PCB                                                  
098900     REFL1-UTIL-WDK7-PCB                                                  
099000     REFL1-UTIL-WDB6-PCB                                                  
099100     UTUP1-WDK7-PCB                                                       
099200     UTUP1-WDB6-PCB                                                       
099300     UTUP1-UTIL-WDK6-PCB                                                  
099400     UTUP1-UTIL-WDK7-PCB                                                  
099500     UTUP1-UTIL-WDB6-PCB                                                  
099600     U2-WDK6-PCB                                                          
099700     U2-WDB6-PCB                                                          
099800     U2-PBTO-W222-WDK6-PCB                                                
099900     U2-PBTO-W222-WDK7-PCB                                                
100000     U2-PBTO-W222-ARTM-PCB                                                
100100     U2-PBTO-W222-REFL1-2501-PCB                                          
100200     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
100300     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
100400     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
100500     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
100600     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
100700     U2-PBTO-W222-WDB6-PCB                                                
100800     U2-PBTO-W222-WDD7-PCB                                                
100900     U2-PBTO-W222-WDK7E-PCB                                               
101000     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
101100     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
101200     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
101300     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
101400     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
101500     U2-REFL2-2501-PCB                                                    
101600     U2-REFL2-WDB6-PCB                                                    
101700     U2-REFL2-UTIL-WDK6-PCB                                               
101800     U2-REFL2-UTIL-WDK7-PCB                                               
101900     U2-REFL2-UTIL-WDB6-PCB                                               
102000     U2-W222-WDK6-PCB                                                     
102100     U2-W222-WDK7-PCB                                                     
102200     U2-W222-ARTM-PCB                                                     
102300     U2-W222-2501-PCB                                                     
102400     U2-W222-WDB6R-PCB                                                    
102500     U2-W222-WDK7R-PCB                                                    
102600     U2-W222-WDB6-PCB                                                     
102700     U2-W222-WDD7-PCB                                                     
102800     U2-W222-WDK7E-PCB                                                    
102900     U2-W222-UTIL-WDK6-PCB                                                
103000     U2-W222-UTIL-WDK7-PCB                                                
103100     U2-W222-UTIL-WDB6-PCB                                                
103200     U2-W222-UTUP1-WDK7-PCB                                               
103300     U2-W222-UTUP1-WDB6-PCB                                               
103400     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
103500     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
103600     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
103700                                                                          
103800 MAIN SECTION.                                                            
103900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK7-PCB WDK7I-PCB            
104000     WDK72-PCB WDL7-PCB WDD3-PCB WDE3-PCB WDE3B-PCB                       
104100     WDK6-PCB  wDN6-PCB WDD7-PCB WDD7A1-PCB WDK9-PCB                      
104200     WDB6-PCB  wDB6-GN-PCB   WDL6-PCB                                     
104300     UTIL-WDK6-PCB UTIL-WDK7-PCB UTIL-WDB6-PCB                            
104400     REFL1-2501-PCB                                                       
104500     REFL1-WDB6-PCB                                                       
104600     REFL1-WDK7-PCB                                                       
104700     REFL1-UTIL-WDK6-PCB                                                  
104800     REFL1-UTIL-WDK7-PCB                                                  
104900     REFL1-UTIL-WDB6-PCB                                                  
105000     UTUP1-WDK7-PCB                                                       
105100     UTUP1-WDB6-PCB                                                       
105200     UTUP1-UTIL-WDK6-PCB                                                  
105300     UTUP1-UTIL-WDK7-PCB                                                  
105400     UTUP1-UTIL-WDB6-PCB                                                  
105500     U2-WDK6-PCB                                                          
105600     U2-WDB6-PCB                                                          
105700     U2-PBTO-W222-WDK6-PCB                                                
105800     U2-PBTO-W222-WDK7-PCB                                                
105900     U2-PBTO-W222-ARTM-PCB                                                
106000     U2-PBTO-W222-REFL1-2501-PCB                                          
106100     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
106200     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
106300     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
106400     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
106500     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
106600     U2-PBTO-W222-WDB6-PCB                                                
106700     U2-PBTO-W222-WDD7-PCB                                                
106800     U2-PBTO-W222-WDK7E-PCB                                               
106900     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
107000     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
107100     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
107200     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
107300     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
107400     U2-REFL2-2501-PCB                                                    
107500     U2-REFL2-WDB6-PCB                                                    
107600     U2-REFL2-UTIL-WDK6-PCB                                               
107700     U2-REFL2-UTIL-WDK7-PCB                                               
107800     U2-REFL2-UTIL-WDB6-PCB                                               
107900     U2-W222-WDK6-PCB                                                     
108000     U2-W222-WDK7-PCB                                                     
108100     U2-W222-ARTM-PCB                                                     
108200     U2-W222-2501-PCB                                                     
108300     U2-W222-WDB6R-PCB                                                    
108400     U2-W222-WDK7R-PCB                                                    
108500     U2-W222-WDB6-PCB                                                     
108600     U2-W222-WDD7-PCB                                                     
108700     U2-W222-WDK7E-PCB                                                    
108800     U2-W222-UTIL-WDK6-PCB                                                
108900     U2-W222-UTIL-WDK7-PCB                                                
109000     U2-W222-UTIL-WDB6-PCB                                                
109100     U2-W222-UTUP1-WDK7-PCB                                               
109200     U2-W222-UTUP1-WDB6-PCB                                               
109300     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
109400     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
109500     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
109600                                                                          
109700     PERFORM IMS-GET-MSG                                                  
109800     IF SEGMENT-FINNS                                                     
109900       PERFORM A-INIT                                                     
110000       PERFORM B-KOLLA-NYCKLAR                                            
110100       IF NYCKLAR-OK                                                      
110200         IF DCS-CHINA                                                     
110300           MOVE 'Mtrsek'     TO MOD-PRICE-TYPE                            
110400         ELSE                                                             
110500           MOVE 'Stdsek'     TO MOD-PRICE-TYPE                            
110600         END-IF                                                           
110700                                                                          
110800         IF MFS-UPDATE OR                                                 
110810            MFS-UPD-V                                                     
110900           PERFORM I-KOLLA-INPUT                                          
111000           PERFORM L-KOLLA-INPUT-NYCKLAR                                  
111100           IF INDATA-OK                                                   
111200             PERFORM H-UPD-WDK6-WDK7                                      
111300             IF WS-IDREFTYP = 'A'                                         
111400             OR WS-IDREFTYP = 'C'                                         
111500             OR WS-IDREFTYP = 'B'                                         
111600             OR WS-IDREFTYP = 'L'                                         
111700             OR WS-IDREFTYP = 'T'                                         
111800               PERFORM N-UPD-WDE3-WDK7                                    
111900             END-IF                                                       
112000                                                                          
112100             MOVE INF-UPDATE-DONE                                         
112200                             TO MED-IDMFSINF                              
112300             CALL WMEDKONV USING MED-WMEDAREA                             
112400             MOVE MED-TEMFSINF                                            
112500                             TO MOD-TEMFSINF                              
112600           ELSE                                                           
112700*   MOD-FÄLT SOM ÄVEN ÄR INFÄLT SKA INTE SKRIVAS ÖVER                     
112800*   I F-HAEMTA-INFO                                                       
112900             MOVE NEJ        TO SW-HAEMTA-INPUT-FAELT                     
113000             MOVE 'PROPOSAL NOT REVIEWED'                                 
113100                             TO MOD-ORDERSTATUS                           
113200             MOVE INF-UPDATE-NOT-DONE                                     
113300                             TO MED-IDMFSINF                              
113400             CALL WMEDKONV USING MED-WMEDAREA                             
113500             MOVE MED-TEMFSINF                                            
113600                             TO MOD-TEMFSINF                              
113700           END-IF                                                         
113800         ELSE                                                             
113900*   INITIERA MOD-PURCHQTY                                                 
114000           MOVE ZERO         TO WS-RED-PURCHQTY                           
114100           MOVE 1            TO IX-DC                                     
114200           PERFORM UNTIL IX-DC > WS-DC-MAX                                
114300                                                                          
114400             MOVE WS-RED-PURCHQTY                                         
114500                             TO MOD-PURCHQTY (IX-DC)                      
114600             ADD 1           TO IX-DC                                     
114700           END-PERFORM                                                    
114800           IF MFS-FIRST                                                   
114900             PERFORM C-FOEREG-SIDA                                        
115000           ELSE                                                           
115100             IF MFS-NEXT                                                  
115200               PERFORM D-NAESTA-SIDA                                      
115300             ELSE                                                         
115400               PERFORM E-SAMMA-SIDA                                       
115500             END-IF                                                       
115600           END-IF                                                         
115700         END-IF                                                           
115800*   UPPDATERA BILDEN                                                      
115900         IF W-IDARTNR > ZERO                                              
116000           PERFORM S1-SECURITY-CHECK-PARTNO                               
116100           PERFORM F-HAEMTA-INFO                                          
116200         ELSE                                                             
116300           MOVE MFS-RENSA-FAELT                                           
116400                             TO MOD-IDARTNR-UT                            
116500                                MOD-IDREFTYP-UT                           
116600                                MOD-ORDERSTATUS                           
116700         END-IF                                                           
116800         PERFORM J-FYLL-I-ANT-REVIEW                                      
116900                                                                          
117000         MOVE W-IDARTNR         TO WS-MSGI-IDARTNR-ENTER                  
117100         IF MOD-ORDERSTATUS = 'REVIEWED'                                  
117200            MOVE 'R'         TO WS-MSGI-ORDER-ENTER                       
117300         ELSE                                                             
117400            MOVE 'N'         TO WS-MSGI-ORDER-ENTER                       
117500         END-IF                                                           
117600         IF WS-MSGI-IDARTNR-ENTER = WS-MSGI-IDARTNR-PF7                   
117700           MOVE ZERO            TO WS-MSGI-IDARTNR-PF7                    
117800           MOVE SPACE           TO WS-MSGI-ORDER-PF7                      
117900         END-IF                                                           
118000* ---    UPPDATERA MSGI-SPAR-AREA                                         
118100         MOVE '002'             TO MSGI-KDCALL                            
118200         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
118300         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
118400         MOVE '2382'            TO MSGI-IDTRANS                           
118500         MOVE WS-MSGI-AREA-2382 TO MSGI-SPAR-AREA                         
118600         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
118700                                                                          
118800* ---    UPPDATERA MSGI-IDARTNR                                           
118900         MOVE ALL '+'        TO MSGI-WMSGINIT                             
119000         MOVE '001'          TO MSGI-KDCALL                               
119100         MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                         
119200         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
119300         MOVE '2382'         TO MSGI-IDTRANS                              
119400         MOVE W-IDARTNR      TO WS-IDARTNR-NUM                            
119500         MOVE WS-IDARTNR     TO MSGI-IDARTNR                              
119600         MOVE WSA-IDDC        TO MSGI-IDDC-KEY                            
119700         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
119800                                                                          
119900       END-IF                                                             
120000                                                                          
120100       IF BLOCKED-SECURITY-CHECK                                          
120200*        --- DETTA VÄRDE SÄTTS I SEKTION S1- DÄR KONTROLL GÖRS PÅ         
120300*            IFALL USER FÅR SE INFO OM VISS ARTIKEL                       
120400         PERFORM MFS-RENSA-FAELT-IN                                       
120500         PERFORM MFS-RENSA-FAELT-UT                                       
120600         PERFORM MFS-RENSA-OBEHOERIGA-FAELT-IN                            
120700         PERFORM MFS-STAENG-OBEHOERIGA-FAELT-IN                           
120800       END-IF                                                             
120900                                                                          
121000       PERFORM Z-JUSTERA-MOD                                              
121100                                                                          
121200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O38201 + 4                      
121300       PERFORM IMS-INSERT-MSG                                             
121400     END-IF                                                               
121500                                                                          
121600     MOVE ZERO TO RETURN-CODE                                             
121700     GOBACK                                                               
121800     .                                                                    
121900     EJECT                                                                
122000 A-INIT SECTION.                                                          
122100                                                                          
122200     IF MSG-DUBBLA-TRANSKODER                                             
122300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
122400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
122500       IF MFS-IDTRANS = '2381'                                            
122600         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
122700                             TO MID-W2I38101                              
122800       ELSE                                                               
122900         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
123000                             TO MID-W2I38201                              
123100       END-IF                                                             
123200     ELSE                                                                 
123300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
123400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
123500       IF MFS-IDTRANS = '2381'                                            
123600         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
123700                             TO MID-W2I38101                              
123800       ELSE                                                               
123900         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
124000                             TO MID-W2I38201                              
124100       END-IF                                                             
124200     END-IF                                                               
124300                                                                          
124400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
124500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
124600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
124700                                                                          
124800     MOVE LOW-VALUE TO MSG-AREA                                           
124900     MOVE 'W2O382N1' TO MFS-IDMOD                                         
125000     MOVE '2382' TO MOD-IDTRANS                                           
125100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
125200                                                                          
125300     IF EGEN-MID OR HELP-MID                                              
125400       CONTINUE                                                           
125500     ELSE                                                                 
125600       MOVE SPACE TO MFS-KDTRTYP                                          
125700     END-IF                                                               
125800                                                                          
125900     MOVE +2      TO SPRAK-IX                                             
126000     MOVE 'GB ' TO MED-IDSKYLT                                            
126100                                                                          
126200     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
126300                                                                          
126400     ACCEPT DAGENS-DATUM FROM DATE                                        
126500                                                                          
126600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
126700     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
126800                                                                          
126900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
127000                     DAT-O-TIDATUM DAT-KDSVAR                             
127100                                                                          
127200     IF DAT-KDSVAR-OK                                                     
127300****             HÄMTA SEKELSIFFROR                                       
127400                                                                          
127500       MOVE DAT-TISEKEL       TO DAGENS-AAR(1:2)                          
127600       MOVE DAT-TIAARP        TO DAGENS-PER                               
127700       MOVE DAT-TIVV          TO DAGENS-VECKA                             
127800                                                                          
127900     ELSE                                                                 
128000         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
128100         DELIMITED BY SIZE INTO FELTEXT                                   
128200         CALL FELLOG                                                      
128300     END-IF                                                               
128400                                                                          
128500     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
128600                                                                          
128700     MOVE 'AARP  '           TO DAT-KDDATFORM                             
128800     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
128900                                                                          
129000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
129100                     DAT-O-TIDATUM DAT-KDSVAR                             
129200                                                                          
129300     IF DAT-KDSVAR-OK                                                     
129400****             RÄKNA UT VECKA I AKTUELL PERIOD                          
129500                                                                          
129600       COMPUTE WS-ANTAL-VECKOR = DAGENS-VECKA - DAT-TIVV + 1              
129700                                                                          
129800     ELSE                                                                 
129900         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
130000         DELIMITED BY SIZE INTO FELTEXT                                   
130100         CALL FELLOG                                                      
130200     END-IF                                                               
130300                                                                          
130400*****   INPUT/OUTPUT FÄLTEN INITERAS                                      
130500     PERFORM MFS-LAES-IN-IGEN                                             
130600*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
130700     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
130800     .                                                                    
130900     EJECT                                                                
131000 B-KOLLA-NYCKLAR SECTION.                                                 
131100     MOVE 'B-KOLLA-NYCKLAR     ' TO  WS-PGM-SEKTION                       
131200                                                                          
131300     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
131400                                MOD-IDSTATUS-IN                           
131500                                MOD-IDTYPE-IN                             
131600                                MOD-IDREFTYP-IN                           
131700     MOVE ALL '+'            TO MSGI-WMSGINIT                             
131800     MOVE '001'              TO MSGI-KDCALL                               
131900     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
132000     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
132100     MOVE '2382'             TO MSGI-IDTRANS                              
132200     IF EGEN-MID                                                          
132300     OR (MID-IDARTNR-IN NUMERIC                                           
132400     AND MID-IDARTNR-IN > ZERO)                                           
132500        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
132600        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
132700     END-IF                                                               
132800     MOVE SPACE              TO MSGI-SPAR-AREA                            
132900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
133000     MOVE MSGI-IDARTNR     TO WS-IDARTNR                                  
133100     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
133200     IF WS-IDARTNR NUMERIC                                                
133300       MOVE WS-IDARTNR-NUM                                                
133400                              TO W-IDARTNR                                
133500     ELSE                                                                 
133600       MOVE NEJ               TO NYCKLAR-SW                               
133700     END-IF                                                               
133800                                                                          
133900     MOVE MSGI-IDDC-KEY       TO WSA-IDDC                                 
134000                                 W-IDDC-301                               
134100                                 WS-IDDC                                  
134200*                                                                         
134300     MOVE WSA-IDDC            TO W-IDDC-B6                                
134400     PERFORM IMS-GU-WDB601                                                
134500     IF SEGMENT-SAKNAS                                                    
134600        MOVE NEJ              TO NYCKLAR-SW                               
134700     END-IF                                                               
134800                                                                          
134900     IF EGEN-MID                                                          
135000                                                                          
135100       IF MSGI-SPAR-AREA(1:4) = '2382'                                    
135200         MOVE MSGI-SPAR-AREA TO WS-MSGI-AREA-2382                         
135300         IF MFS-FIRST                                                     
135400            MOVE WS-MSGI-IDARTNR-PF7 TO WS-IDARTNR                        
135500            INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO            
135600            IF WS-IDARTNR NUMERIC                                         
135700               MOVE WS-IDARTNR-NUM   TO W-IDARTNR                         
135800            END-IF                                                        
135900            MOVE MSGI-IDDC-KEY       TO WSA-IDDC                          
136000                                        W-IDDC-301                        
136100         END-IF                                                           
136200       END-IF                                                             
136300       MOVE JA TO NYCKLAR-SW                                              
136400                                                                          
136500                                                                          
136600*      -- KONTROLL AV IDARTNR                                             
136700                                                                          
136800       IF WS-IDARTNR NUMERIC                                              
136900         MOVE WS-IDARTNR-NUM                                              
137000                             TO W-IDARTNR-MIN                             
137100       ELSE                                                               
137200         MOVE NEJ            TO NYCKLAR-SW                                
137300       END-IF                                                             
137400                                                                          
137500       IF WS-IDARTNR NUMERIC                                              
137600         MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                            
137700       ELSE                                                               
137800         MOVE NEJ            TO NYCKLAR-SW                                
137900       END-IF                                                             
138000                                                                          
138100       IF WS-IDARTNR-NUM NUMERIC                                          
138200         MOVE WS-IDARTNR-NUM TO W-IDARTNR-301                             
138300                                W-IDARTNR                                 
138400       ELSE                                                               
138500         MOVE NEJ            TO NYCKLAR-SW                                
138600       END-IF                                                             
138700                                                                          
138800       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
138900       INSPECT MOD-IDARTNR-UT REPLACING LEADING '+'  BY SPACE             
139000                                                                          
139100                                                                          
139200*      -- KONTROLL AV IDDC                                                
139300                                                                          
139400       IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS           
139500       OR DCS-NDC-SA                                                      
139600          MOVE WSA-IDDC   TO MOD-IDDC-UT                                  
139700                            W-IDDC                                        
139800       ELSE                                                               
139900          MOVE NEJ       TO NYCKLAR-SW                                    
140000          MOVE WSA-IDDC   TO MOD-IDDC-UT                                  
140100       END-IF                                                             
140200                                                                          
140300       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
140400       INSPECT MOD-IDDC-UT REPLACING LEADING '+'  BY SPACE                
140500                                                                          
140600                                                                          
140700*      -- KONTROLL AV STATUS                                              
140800                                                                          
140900       IF MID-IDSTATUS-IN = ALL '+'                                       
141000         INSPECT MID-IDSTATUS-UT REPLACING LEADING '+' BY SPACE           
141100         MOVE MID-IDSTATUS-UT  TO WS-STATUS                               
141200       ELSE                                                               
141300         MOVE MID-IDSTATUS-IN  TO WS-STATUS                               
141400       END-IF                                                             
141500                                                                          
141600       IF WS-STATUS = 'R'                                                 
141700       OR WS-STATUS = 'N'                                                 
141800         IF WS-STATUS = 'R'                                               
141900           MOVE 'REVIEWED'   TO MOD-IDSTATUS-UT                           
142000         END-IF                                                           
142100         IF WS-STATUS = 'N'                                               
142200           MOVE 'NOT REVIEWED'                                            
142300                             TO MOD-IDSTATUS-UT                           
142400         END-IF                                                           
142500       ELSE                                                               
142600         IF MID-IDSTATUS-IN NOT = ALL '+'                                 
142700           MOVE NEJ TO NYCKLAR-SW                                         
142800           MOVE MED-2        TO MOD-TEMFSINF                              
142900         END-IF                                                           
143000       END-IF                                                             
143100       INSPECT MOD-IDSTATUS-UT REPLACING LEADING '+'  BY SPACE            
143200                                                                          
143300                                                                          
143400*      -- KONTROLL AV TYP                                                 
143500*      -- TYP ANVÄNDS FÖR SÖKNING AV REFILLORDERFÖRSLAG                   
143600                                                                          
143700       IF MID-IDTYPE-IN = ALL '+'                                         
143800         INSPECT MID-IDTYPE-UT REPLACING LEADING '+' BY SPACE             
143900         IF MID-IDTYPE-UT = 'AIRCR'                                       
144000           MOVE 'C'          TO WS-IDTYPE                                 
144100         ELSE                                                             
144200           MOVE MID-IDTYPE-UT                                             
144300                             TO WS-IDTYPE                                 
144400         END-IF                                                           
144500       ELSE                                                               
144600         MOVE MID-IDTYPE-IN  TO WS-IDTYPE                                 
144700                                MID-IDREFTYP-UT                           
144800       END-IF                                                             
144900                                                                          
145000       IF WS-IDTYPE = 'A'                                                 
145100       OR WS-IDTYPE = 'C'                                                 
145200       OR WS-IDTYPE = 'B'                                                 
145300       OR WS-IDTYPE = 'L'                                                 
145400       OR WS-IDTYPE = 'T'                                                 
145500         IF WS-IDTYPE = 'A'                                               
145600           MOVE 'AIR'        TO MOD-IDTYPE-UT                             
145700         END-IF                                                           
145800         IF WS-IDTYPE = 'C'                                               
145900           MOVE 'AIRCR'      TO MOD-IDTYPE-UT                             
146000         END-IF                                                           
146100         IF WS-IDTYPE = 'B'                                               
146200           MOVE 'BOAT'       TO MOD-IDTYPE-UT                             
146300         END-IF                                                           
146400         IF WS-IDTYPE = 'L'                                               
146500           MOVE 'LOCAL'      TO MOD-IDTYPE-UT                             
146600         END-IF                                                           
146700         IF WS-IDTYPE = 'T'                                               
146800           MOVE 'TRANS'      TO MOD-IDTYPE-UT                             
146900         END-IF                                                           
147000       ELSE                                                               
147100         IF MID-IDTYPE-IN NOT = ALL '+'                                   
147200           MOVE NEJ          TO NYCKLAR-SW                                
147300           MOVE MED-1        TO MOD-TEMFSINF                              
147400         END-IF                                                           
147500       END-IF                                                             
147600       INSPECT MOD-IDTYPE-UT REPLACING LEADING '+'  BY SPACE              
147700                                                                          
147800*      -- KONTROLL AV IDREFTYP                                            
147900*      -- IDREFTYP ANVÄNDS FÖR ATT ANGE VILKEN ORDERTYP                   
148000*      -- SOM SKA SKAPAS                                                  
148100                                                                          
148200       IF MID-IDREFTYP-IN = ALL '+'                                       
148300         IF MID-IDREFTYP-UT = 'AIR  '                                     
148400         OR MID-IDREFTYP-UT = 'AIRCR'                                     
148500         OR MID-IDREFTYP-UT = 'BOAT '                                     
148600         OR MID-IDREFTYP-UT = 'LOCAL'                                     
148700         OR MID-IDREFTYP-UT = 'TRANS'                                     
148800           IF MID-IDREFTYP-UT = 'AIR  '                                   
148900             MOVE 'A'        TO WS-IDREFTYP                               
149000           END-IF                                                         
149100           IF MID-IDREFTYP-UT = 'AIRCR'                                   
149200             MOVE 'C'        TO WS-IDREFTYP                               
149300           END-IF                                                         
149400           IF MID-IDREFTYP-UT = 'BOAT '                                   
149500             MOVE 'B'        TO WS-IDREFTYP                               
149600           END-IF                                                         
149700           IF MID-IDREFTYP-UT = 'LOCAL'                                   
149800             MOVE 'L'        TO WS-IDREFTYP                               
149900           END-IF                                                         
150000           IF MID-IDREFTYP-UT = 'TRANS'                                   
150100             MOVE 'T'        TO WS-IDREFTYP                               
150200           END-IF                                                         
150300         ELSE                                                             
150400           MOVE WS-IDTYPE    TO WS-IDREFTYP                               
150500         END-IF                                                           
150600       ELSE                                                               
150700         MOVE MID-IDREFTYP-IN                                             
150800                             TO WS-IDREFTYP                               
150900       END-IF                                                             
151000       IF WS-IDREFTYP = 'B'                                               
151100       OR WS-IDREFTYP = 'A'                                               
151200       OR WS-IDREFTYP = 'C'                                               
151300       OR WS-IDREFTYP = 'L'                                               
151400       OR WS-IDREFTYP = 'T'                                               
151500         IF WS-IDREFTYP = 'B'                                             
151600           MOVE 'BOAT'       TO MOD-IDREFTYP-UT                           
151700         END-IF                                                           
151800         IF WS-IDREFTYP = 'A'                                             
151900           MOVE 'AIR'        TO MOD-IDREFTYP-UT                           
152000         END-IF                                                           
152100         IF WS-IDREFTYP = 'C'                                             
152200           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
152300         END-IF                                                           
152400         IF WS-IDREFTYP = 'L'                                             
152500           MOVE 'LOCAL'      TO MOD-IDREFTYP-UT                           
152600         END-IF                                                           
152700         IF WS-IDREFTYP = 'T'                                             
152800           MOVE 'TRANS'      TO MOD-IDREFTYP-UT                           
152900         END-IF                                                           
153000       ELSE                                                               
153100         IF MID-IDREFTYP-IN NOT = ALL '+'                                 
153200           MOVE NEJ          TO NYCKLAR-SW                                
153300           MOVE MED-7        TO MOD-TEMFSINF                              
153400         END-IF                                                           
153500       END-IF                                                             
153600       INSPECT MOD-IDREFTYP-UT REPLACING LEADING '+'  BY SPACE            
153700                                                                          
153800                                                                          
153900     ELSE                                                                 
154000                                                                          
154100       IF  MFS-IDTRANS = '2381'                                           
154200                                                                          
154300         MOVE ZERO           TO W-IDARTNR                                 
154400         MOVE +1             TO IX                                        
154500         PERFORM UNTIL IX > +13                                           
154600         OR MID-SELECT-2381 (IX) NOT = '+'                                
154700           ADD +1            TO IX                                        
154800         END-PERFORM                                                      
154900                                                                          
155000         MOVE SPACE          TO WS-IDTYPE                                 
155100                                WS-IDREFTYP                               
155200                                                                          
155300         IF IX > +13                                                      
155400           CONTINUE                                                       
155500         ELSE                                                             
155600             IF MID-IDTYPE-2381-IN = '+'                                  
155700               IF MID-IDTYPE-2381-UT = 'AIR  '                            
155800                 MOVE 'A'    TO WS-IDTYPE                                 
155900                                WS-IDREFTYP                               
156000               END-IF                                                     
156100               IF MID-IDTYPE-2381-UT = 'AIRCR'                            
156200                 MOVE 'C'    TO WS-IDTYPE                                 
156300                                WS-IDREFTYP                               
156400               END-IF                                                     
156500               IF MID-IDTYPE-2381-UT = 'BOAT '                            
156600                 MOVE 'B'    TO WS-IDTYPE                                 
156700                                WS-IDREFTYP                               
156800               END-IF                                                     
156900               IF MID-IDTYPE-2381-UT = 'LOCAL'                            
157000                 MOVE 'L'    TO WS-IDTYPE                                 
157100                                WS-IDREFTYP                               
157200               END-IF                                                     
157300               IF MID-IDTYPE-2381-UT = 'TRANS'                            
157400                 MOVE 'T'    TO WS-IDTYPE                                 
157500                                WS-IDREFTYP                               
157600               END-IF                                                     
157700             ELSE                                                         
157800               MOVE MID-IDTYPE-2381-IN                                    
157900                             TO WS-IDTYPE                                 
158000                                WS-IDREFTYP                               
158100             END-IF                                                       
158200             MOVE MID-IDDC-2381 (IX, 1)                                   
158300                             TO WSA-IDDC                                  
158400                                MOD-IDDC-UT                               
158500                                MSGI-IDDC-KEY                             
158600         END-IF                                                           
158700                                                                          
158800         MOVE '7'            TO MFS-IDPFK                                 
158900         MOVE SPACE          TO MFS-KDTRTYP                               
159000                                                                          
159100         MOVE 'N'            TO WS-STATUS                                 
159200                                                                          
159300         IF WS-IDTYPE = 'A'                                               
159400         OR WS-IDTYPE = 'C'                                               
159500         OR WS-IDTYPE = 'B'                                               
159600         OR WS-IDTYPE = 'L'                                               
159700         OR WS-IDTYPE = 'T'                                               
159800           IF WS-IDTYPE = 'A'                                             
159900             MOVE 'AIR'      TO MOD-IDTYPE-UT                             
160000                                MOD-IDREFTYP-UT                           
160100           END-IF                                                         
160200           IF WS-IDTYPE = 'C'                                             
160300             MOVE 'AIRCR'    TO MOD-IDTYPE-UT                             
160400                                MOD-IDREFTYP-UT                           
160500           END-IF                                                         
160600           IF WS-IDTYPE = 'B'                                             
160700             MOVE 'BOAT'     TO MOD-IDTYPE-UT                             
160800                                MOD-IDREFTYP-UT                           
160900           END-IF                                                         
161000           IF WS-IDTYPE = 'L'                                             
161100             MOVE 'LOCAL'    TO MOD-IDTYPE-UT                             
161200                                MOD-IDREFTYP-UT                           
161300           END-IF                                                         
161400           IF WS-IDTYPE = 'T'                                             
161500             MOVE 'TRANS'    TO MOD-IDTYPE-UT                             
161600                                MOD-IDREFTYP-UT                           
161700           END-IF                                                         
161800         ELSE                                                             
161900           MOVE NEJ TO NYCKLAR-SW                                         
162000         END-IF                                                           
162100         INSPECT MOD-IDTYPE-UT REPLACING                                  
162200                                   LEADING '+' BY SPACE                   
162300                                                                          
162400         MOVE 'N'            TO WS-STATUS                                 
162500         MOVE 'NOT REVIEWED'                                              
162600                             TO MOD-IDSTATUS-UT                           
162700                                                                          
162800                                                                          
162900       ELSE                                                               
163000*      -- KONTROLL AV IDARTNR                                             
163100                                                                          
163200         IF WS-IDARTNR NUMERIC                                            
163300           MOVE WS-IDARTNR-NUM                                            
163400                             TO W-IDARTNR-MIN                             
163500                                W-IDARTNR-301                             
163600                                W-IDARTNR                                 
163700                                MID-IDARTNR-IN                            
163800           IF DCS-NDC-CN                                                  
163900              MOVE WS-IDARTNR-NUM                                         
164000                             TO W-IDARTNR                                 
164100              PERFORM IMS-GU-WDK711                                       
164200              IF  SEGMENT-FINNS                                           
164300              AND SLAG-IDDC-REF = SPACE                                   
164400                 MOVE NEJ    TO NYCKLAR-SW                                
164500                                REFILL-PART-SW                            
164600              END-IF                                                      
164700           END-IF                                                         
164800         ELSE                                                             
164900           MOVE NEJ          TO NYCKLAR-SW                                
165000         END-IF                                                           
165100                                                                          
165200         MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                            
165300                                                                          
165400         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
165500         INSPECT MOD-IDARTNR-UT REPLACING LEADING '+' BY SPACE            
165600*                                                                         
165700         MOVE WSA-IDDC       TO MOD-IDDC-UT                               
165800         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
165900         INSPECT MOD-IDDC-UT REPLACING LEADING '+'  BY SPACE              
166000*                                                                         
166100                                                                          
166200         IF MSGI-SPAR-AREA(1:4) = '2382'                                  
166300           MOVE MSGI-SPAR-AREA                                            
166400                             TO WS-MSGI-AREA-2382                         
166500           IF WS-MSGI-IDTYPE = 'A'                                        
166600           OR WS-MSGI-IDTYPE = 'C'                                        
166700           OR WS-MSGI-IDTYPE = 'B'                                        
166800           OR WS-MSGI-IDTYPE = 'L'                                        
166900           OR WS-MSGI-IDTYPE = 'T'                                        
167000             IF WS-MSGI-IDTYPE = 'A'                                      
167100               MOVE 'AIR'    TO MOD-IDTYPE-UT                             
167200                                  MOD-IDREFTYP-UT                         
167300             END-IF                                                       
167400             IF WS-MSGI-IDTYPE = 'C'                                      
167500               MOVE 'AIRCR'  TO MOD-IDTYPE-UT                             
167600                                  MOD-IDREFTYP-UT                         
167700             END-IF                                                       
167800             IF WS-MSGI-IDTYPE = 'B'                                      
167900               MOVE 'BOAT'   TO MOD-IDTYPE-UT                             
168000                                  MOD-IDREFTYP-UT                         
168100             END-IF                                                       
168200             IF WS-MSGI-IDTYPE = 'L'                                      
168300               MOVE 'LOCAL'  TO MOD-IDTYPE-UT                             
168400                                  MOD-IDREFTYP-UT                         
168500             END-IF                                                       
168600             IF WS-MSGI-IDTYPE = 'T'                                      
168700               MOVE 'TRANS'  TO MOD-IDTYPE-UT                             
168800                                  MOD-IDREFTYP-UT                         
168900             END-IF                                                       
169000             MOVE WS-MSGI-IDTYPE                                          
169100                               TO MID-IDTYPE-IN                           
169200                                  MID-IDREFTYP-IN                         
169300                                  WS-IDTYPE                               
169400                                  WS-IDREFTYP                             
169500           ELSE                                                           
169600             MOVE '+'          TO MID-IDTYPE-IN                           
169700                                  MID-IDREFTYP-IN                         
169800           END-IF                                                         
169900         ELSE                                                             
170000           MOVE '+'            TO MID-IDTYPE-IN                           
170100                                  MID-IDREFTYP-IN                         
170200         END-IF                                                           
170300                                                                          
170400         MOVE 'N'            TO WS-STATUS                                 
170500         MOVE 'NOT REVIEWED'                                              
170600                             TO MOD-IDSTATUS-UT                           
170700                                                                          
170800       END-IF                                                             
170900                                                                          
171000     END-IF                                                               
171100                                                                          
171200     IF NYCKLAR-OK                                                        
171300        PERFORM S101-GET-REFILLDC-OCH-DISTRIKT                            
171400     END-IF                                                               
171500                                                                          
171600***LYNK PARTS ONLY IN EUROPE                                              
171700     MOVE NEJ     TO SW-LYNK-PART                                         
171800     PERFORM IMS-GU-WDK601                                                
171900     IF SEGMENT-FINNS                                                     
172010       IF  ART-KDPRODSL > 30                                              
172020       AND ART-KDPRODSL < 40                                              
172100       AND NDC                                                            
172200         MOVE NEJ          TO NYCKLAR-SW                                  
172300         MOVE JA           TO SW-LYNK-PART                                
172400       END-IF                                                             
172500     END-IF                                                               
172600                                                                          
172700     IF NYCKLAR-FEL                                                       
172800       IF NOT-REFILL-PART                                                 
172900          MOVE INF-NOT-REFILL-PART                                        
173000                             TO MED-IDMFSINF                              
173100          CALL WMEDKONV  USING  MED-WMEDAREA                              
173200          MOVE MED-TEMFSINF  TO MOD-TEMFSINF                              
173300       END-IF                                                             
173400       IF LYNK-PART                                                       
173500         MOVE MED-16          TO MOD-TEMFSFEL                             
173600       ELSE                                                               
173700         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
173800         CALL WMEDKONV USING MED-WMEDAREA                                 
173900         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
174000       END-IF                                                             
174100       PERFORM MFS-RENSA-FAELT-IN                                         
174200       PERFORM MFS-RENSA-FAELT-UT                                         
174300     ELSE                                                                 
174400       MOVE ZERO             TO WS-IDPERSON-BUY                           
174500                                W-IDPERSON-BUY                            
174600       MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                            
174700                                W-KDREFTYP-MAX                            
174800       MOVE WS-IDTYPE        TO WS-MSGI-IDTYPE                            
174900     END-IF                                                               
175000     .                                                                    
175100     EJECT                                                                
175200 C-FOEREG-SIDA SECTION.                                                   
175300     MOVE 'C-FOEREG-SIDA       ' TO  WS-PGM-SEKTION                       
175400     MOVE NEJ                TO SW-TRAEFF                                 
175500     PERFORM CA-BLAEDDRA-BAK                                              
175600     IF SW-TRAEFF-JA                                                      
175700       PERFORM S101-GET-REFILLDC-OCH-DISTRIKT                             
175800     ELSE                                                                 
175900       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
176000       CALL WMEDKONV USING MED-WMEDAREA                                   
176100       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
176200       IF EGEN-MID                                                        
176300         MOVE ZERO           TO W-IDARTNR                                 
176400       END-IF                                                             
176500       PERFORM MFS-RENSA-FAELT-IN                                         
176600       PERFORM MFS-RENSA-FAELT-UT                                         
176700     END-IF                                                               
176800     .                                                                    
176900     EJECT                                                                
177000 CA-BLAEDDRA-BAK SECTION.                                                 
177100     MOVE 'CA-BLAEDDRA-BAK     ' TO  WS-PGM-SEKTION                       
177200                                                                          
177300     MOVE NEJ                TO SW-TRAEFF                                 
177400****   BLÄDDRA TILLBAKA TILL FÖREGÅENDE ARTIKEL *******                   
177500****   (OM DET FINNS NÅGON)                     *******                   
177600     MOVE WS-MSGI-IDARTNR-PF7                                             
177700                             TO W-IDARTNR                                 
177800                                W-IDARTNR-301                             
177900                                W-IDARTNR-MIN                             
178000     MOVE WS-DC-NR (1)       TO W-IDDC-MIN                                
178100     MOVE WS-DC-NR (WS-DC-MAX)                                            
178200                             TO W-IDDC-MAX                                
178300                                                                          
178400     PERFORM IMS-GU-WDE3B1-MIN-MAX                                        
178500     PERFORM S2-KOLLA-GILTIGT-DC                                          
178600                                                                          
178700     PERFORM UNTIL SEGMENT-SAKNAS                                         
178800     OR (SEQB-IDARTNR = WS-MSGI-IDARTNR-PF7                               
178900     AND SW-GILTIGT-DC-JA                                                 
179000     AND  SEQB-KDREFTYP = WS-IDTYPE                                       
179100     AND   ((WS-MSGI-ORDER-PF7 = 'N'                                      
179200        AND SEQB-KDREFORS = 'P')                                          
179300       OR (WS-MSGI-ORDER-PF7   = 'R'                                      
179400        AND SEQB-KDREFORS = 'O')))                                        
179500          PERFORM IMS-GN-WDE3B1-MIN-MAX                                   
179600          PERFORM S2-KOLLA-GILTIGT-DC                                     
179700                                                                          
179800     END-PERFORM                                                          
179900                                                                          
180000     IF SEGMENT-FINNS                                                     
180100       IF SEQB-IDDC NOT = DCS-IDDC                                        
180200          MOVE SEQB-IDDC        TO W-IDDC-B6                              
180300          PERFORM IMS-GU-WDB601                                           
180400       END-IF                                                             
180500                                                                          
180600*       --- CHECK IDLEVNR-SECURITY                                        
180700        MOVE SEQB-IDARTNR TO W-IDARTNR                                    
180800        PERFORM S1-SECURITY-CHECK-PARTNO                                  
180900     END-IF                                                               
181000                                                                          
181100     IF SEGMENT-FINNS                                                     
181200       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
181300                                                                          
181400     ELSE                                                                 
181500       MOVE ZERO             TO W-IDARTNR                                 
181600                                W-IDARTNR-301                             
181700                                W-IDARTNR-MIN                             
181800       PERFORM IMS-GU-WDE3B1-MIN-MAX                                      
181900                                                                          
182000       PERFORM S2-KOLLA-GILTIGT-DC                                        
182100                                                                          
182200       PERFORM UNTIL SEGMENT-SAKNAS                                       
182300       OR (SW-GILTIGT-DC-JA                                               
182400       AND (SEQB-KDREFTYP = WS-IDTYPE                                     
182500       AND ((WS-STATUS  = 'N'                                             
182600        AND SEQB-KDREFORS = 'P')                                          
182700       OR (WS-STATUS    = 'R'                                             
182800        AND SEQB-KDREFORS = 'O'))))                                       
182900                                                                          
183000           PERFORM IMS-GN-WDE3B1-MIN-MAX                                  
183100           PERFORM S2-KOLLA-GILTIGT-DC                                    
183200                                                                          
183300       END-PERFORM                                                        
183400                                                                          
183500       IF SEGMENT-FINNS                                                   
183600         IF SEQB-IDDC NOT = DCS-IDDC                                      
183700            MOVE SEQB-IDDC        TO W-IDDC-B6                            
183800            PERFORM IMS-GU-WDB601                                         
183900         END-IF                                                           
184000*         --- CHECK IDLEVNR-SECURITY                                      
184100          MOVE SEQB-IDARTNR TO W-IDARTNR                                  
184200          PERFORM S1-SECURITY-CHECK-PARTNO                                
184300       END-IF                                                             
184400                                                                          
184500       IF SEGMENT-FINNS                                                   
184600         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
184700       END-IF                                                             
184800     END-IF                                                               
184900                                                                          
185000     IF SEGMENT-FINNS                                                     
185100        MOVE SEQB-KDREFTYP      TO W-KDREFTYP-MIN                         
185200        MOVE SEQB-IDARTNR       TO W-IDARTNR-MIN                          
185300        MOVE WS-DC-NR (1)       TO W-IDDC-MIN                             
185400        PERFORM IMS-GU-WDE3B1-MIN-MAX                                     
185500**********                                                                
185600        PERFORM UNTIL SEGMENT-SAKNAS                                      
185700        OR NOT (WS-SPARA-IDARTNR = SEQB-IDARTNR                           
185800        AND SW-GILTIGT-DC-JA)                                             
185900**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
186000**********      ARTNR FÖR DC MED RÄTT KDREFTYP                            
186100**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
186200          IF SEQB-KDREFTYP = WS-IDTYPE                                    
186300**********                                                                
186400            IF SEQB-IDDC = WS-DC-NR (1)                                   
186500              MOVE +1           TO IX-DC                                  
186600            END-IF                                                        
186700**********                                                                
186800            IF SEQB-IDDC = WS-DC-NR (2)                                   
186900              MOVE +2           TO IX-DC                                  
187000            END-IF                                                        
187100**********                                                                
187200            IF SEQB-IDDC = WS-DC-NR (3)                                   
187300              MOVE +3           TO IX-DC                                  
187400            END-IF                                                        
187500**********                                                                
187600            IF SEQB-IDDC = WS-DC-NR (4)                                   
187700              MOVE +4           TO IX-DC                                  
187800            END-IF                                                        
187900**********                                                                
188000            IF SEQB-IDDC = WS-DC-NR (5)                                   
188100              MOVE +5           TO IX-DC                                  
188200            END-IF                                                        
188300**********                                                                
188400            IF SEQB-IDDC = WS-DC-NR (6)                                   
188500              MOVE +6           TO IX-DC                                  
188600            END-IF                                                        
188700                                                                          
188800            MOVE SEQB-IDWDE301                                            
188900                                TO W-WDE301KY-X                           
189000            PERFORM IMS-GU-WDE301                                         
189100                                                                          
189200            PERFORM M-UPD-MOD-FAELT                                       
189300          END-IF                                                          
189400                                                                          
189500          PERFORM IMS-GN-WDE3B1-MIN-MAX                                   
189600          PERFORM S2-KOLLA-GILTIGT-DC                                     
189700                                                                          
189800          PERFORM UNTIL SEGMENT-SAKNAS                                    
189900          OR (WS-SPARA-IDARTNR = SEQB-IDARTNR                             
190000          AND SW-GILTIGT-DC-JA)                                           
190100            PERFORM IMS-GN-WDE3B1-MIN-MAX                                 
190200            PERFORM S2-KOLLA-GILTIGT-DC                                   
190300          END-PERFORM                                                     
190400                                                                          
190500        END-PERFORM                                                       
190600     END-IF                                                               
190700     IF SEGMENT-FINNS                                                     
190800       IF SEQB-IDDC NOT = DCS-IDDC                                        
190900          MOVE SEQB-IDDC          TO W-IDDC-B6                            
191000          PERFORM IMS-GU-WDB601                                           
191100       END-IF                                                             
191200     END-IF                                                               
191300     .                                                                    
191400     EJECT                                                                
191500 D-NAESTA-SIDA SECTION.                                                   
191600     MOVE 'D-NAESTA-SIDA       ' TO  WS-PGM-SEKTION                       
191700     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
191800     MOVE WS-IDARTNR         TO W-IDARTNR-MIN                             
191900     MOVE NEJ                TO SW-TRAEFF                                 
192000     PERFORM DA-BLAEDDRA-FRAM                                             
192100     IF SW-TRAEFF-JA                                                      
192200       PERFORM S101-GET-REFILLDC-OCH-DISTRIKT                             
192300*      CONTINUE                                                           
192400     ELSE                                                                 
192500       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
192600       CALL WMEDKONV USING MED-WMEDAREA                                   
192700       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
192800       MOVE ZERO             TO W-IDARTNR                                 
192900       PERFORM MFS-RENSA-FAELT-IN                                         
193000       PERFORM MFS-RENSA-FAELT-UT                                         
193100     END-IF                                                               
193200     .                                                                    
193300     EJECT                                                                
193400 DA-BLAEDDRA-FRAM SECTION.                                                
193500     MOVE 'DA-BLAEDDRA-FRAM    ' TO  WS-PGM-SEKTION                       
193600                                                                          
193700     PERFORM IMS-GU-WDE3B1-PF8                                            
193800                                                                          
193900     PERFORM S2-KOLLA-GILTIGT-DC                                          
194000                                                                          
194100     PERFORM UNTIL SEGMENT-SAKNAS                                         
194200     OR (SW-GILTIGT-DC-JA                                                 
194300     AND  SEQB-IDARTNR > W-IDARTNR                                        
194400     AND (SEQB-KDREFTYP = WS-IDTYPE                                       
194500     AND ((WS-STATUS        = 'N'                                         
194600     AND SEQB-KDREFORS = 'P')                                             
194700     OR      (WS-STATUS     = 'R'                                         
194800     AND SEQB-KDREFORS = 'O'))))                                          
194900                                                                          
195000         PERFORM IMS-GN-WDE3B1-PF8                                        
195100         PERFORM S2-KOLLA-GILTIGT-DC                                      
195200                                                                          
195300     END-PERFORM                                                          
195400                                                                          
195500     IF SEGMENT-FINNS                                                     
195600       IF SEQB-IDDC NOT = DCS-IDDC                                        
195700          MOVE SEQB-IDDC        TO W-IDDC-B6                              
195800          PERFORM IMS-GU-WDB601                                           
195900       END-IF                                                             
196000*       --- CHECK IDLEVNR-SECURITY                                        
196100        MOVE SEQB-IDARTNR TO W-IDARTNR                                    
196200        PERFORM S1-SECURITY-CHECK-PARTNO                                  
196300     END-IF                                                               
196400                                                                          
196500     IF SEGMENT-FINNS                                                     
196600       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
196700     ELSE                                                                 
196800       MOVE ZERO             TO W-IDARTNR                                 
196900     END-IF                                                               
197000     MOVE WS-MSGI-IDARTNR-ENTER                                           
197100                             TO WS-MSGI-IDARTNR-PF7                       
197200     MOVE WS-MSGI-ORDER-ENTER                                             
197300                             TO WS-MSGI-ORDER-PF7                         
197400                                                                          
197500     IF SEGMENT-FINNS                                                     
197600        MOVE SEQB-KDREFTYP   TO W-KDREFTYP-MIN                            
197700        MOVE SEQB-IDARTNR    TO W-IDARTNR-MIN                             
197800        MOVE WS-DC-NR (1)    TO W-IDDC-MIN                                
197900        PERFORM IMS-GU-WDE3B1-MIN-MAX                                     
198000**********                                                                
198100        PERFORM UNTIL SEGMENT-SAKNAS                                      
198200        OR NOT (WS-SPARA-IDARTNR = SEQB-IDARTNR                           
198300        AND SW-GILTIGT-DC-JA)                                             
198400**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
198500**********      ARTNR FÖR DC MED RÄTT KDREFTYP                            
198600**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
198700          IF SEQB-KDREFTYP = WS-IDTYPE                                    
198800**********                                                                
198900            IF SEQB-IDDC = WS-DC-NR (1)                                   
199000              MOVE +1           TO IX-DC                                  
199100            END-IF                                                        
199200**********                                                                
199300            IF SEQB-IDDC = WS-DC-NR (2)                                   
199400              MOVE +2           TO IX-DC                                  
199500            END-IF                                                        
199600**********                                                                
199700            IF SEQB-IDDC = WS-DC-NR (3)                                   
199800              MOVE +3           TO IX-DC                                  
199900            END-IF                                                        
200000**********                                                                
200100            IF SEQB-IDDC = WS-DC-NR (4)                                   
200200              MOVE +4           TO IX-DC                                  
200300            END-IF                                                        
200400**********                                                                
200500            IF SEQB-IDDC = WS-DC-NR (5)                                   
200600              MOVE +5           TO IX-DC                                  
200700            END-IF                                                        
200800**********                                                                
200900            IF SEQB-IDDC = WS-DC-NR (6)                                   
201000              MOVE +6           TO IX-DC                                  
201100            END-IF                                                        
201200                                                                          
201300            MOVE SEQB-IDWDE301                                            
201400                                TO W-WDE301KY-X                           
201500            PERFORM IMS-GU-WDE301                                         
201600                                                                          
201700            PERFORM M-UPD-MOD-FAELT                                       
201800          END-IF                                                          
201900                                                                          
202000          PERFORM IMS-GN-WDE3B1-PF8                                       
202100          PERFORM S2-KOLLA-GILTIGT-DC                                     
202200          IF SW-GILTIGT-DC-JA                                             
202300            CONTINUE                                                      
202400          ELSE                                                            
202500            PERFORM UNTIL SW-GILTIGT-DC-JA OR SEGMENT-SAKNAS              
202600              PERFORM IMS-GN-WDE3B1-PF8                                   
202700              PERFORM S2-KOLLA-GILTIGT-DC                                 
202800            END-PERFORM                                                   
202900          END-IF                                                          
203000                                                                          
203100        END-PERFORM                                                       
203200     END-IF                                                               
203300     IF SEGMENT-FINNS                                                     
203400       IF SEQB-IDDC NOT = DCS-IDDC                                        
203500          MOVE SEQB-IDDC          TO W-IDDC-B6                            
203600          PERFORM IMS-GU-WDB601                                           
203700       END-IF                                                             
203800     END-IF                                                               
203900     .                                                                    
204000     EJECT                                                                
204100 E-SAMMA-SIDA SECTION.                                                    
204200     MOVE 'E-SAMMA-SIDA        ' TO  WS-PGM-SEKTION                       
204300                                                                          
204400     IF MID-IDARTNR-IN       = ALL '+'                                    
204500     AND MID-IDDC-IN         = ALL '+'                                    
204600     AND MID-IDTYPE-IN       = ALL '+'                                    
204700     AND MID-IDSTATUS-IN     = ALL '+'                                    
204800     AND MID-IDREFTYP-IN     = ALL '+'                                    
204900       IF  MID-KVPB-REF (1)  = ALL '+'                                    
205000       AND MID-KVPB-REF (2)  = ALL '+'                                    
205100       AND MID-KVPB-REF (3)  = ALL '+'                                    
205200       AND MID-KVPB-REF (4)  = ALL '+'                                    
205300       AND MID-KVPB-REF (5)  = ALL '+'                                    
205400       AND MID-KVPB-REF (6)  = ALL '+'                                    
205500         CONTINUE                                                         
205600*----     INGEN INFO LIGGER I BILDEN                                      
205700*----     SKA SKRIVAS UT I F-HAEMTA                                       
205800*----                                                                     
205900       ELSE                                                               
206000         MOVE NEJ            TO SW-HAEMTA-INPUT-FAELT                     
206100       END-IF                                                             
206200                                                                          
206300       IF MID-INPUT      NOT = ALL '+'                                    
206400         PERFORM EB-SIMULERA                                              
206500         PERFORM I-KOLLA-INPUT                                            
206600         PERFORM EE-BERAKNA-REFPKT                                        
206700         MOVE WS-PURCHQTY (1)                                             
206800                             TO WS-PURCHQTY-SIM (1)                       
206900         MOVE WS-PURCHQTY (2)                                             
207000                             TO WS-PURCHQTY-SIM (2)                       
207100         MOVE WS-PURCHQTY (3)                                             
207200                             TO WS-PURCHQTY-SIM (3)                       
207300         MOVE WS-PURCHQTY (4)                                             
207400                             TO WS-PURCHQTY-SIM (4)                       
207500         MOVE WS-PURCHQTY (5)                                             
207600                             TO WS-PURCHQTY-SIM (5)                       
207700         MOVE WS-PURCHQTY (6)                                             
207800                             TO WS-PURCHQTY-SIM (6)                       
207900       END-IF                                                             
208000                                                                          
208100     ELSE                                                                 
208200                                                                          
208300       IF (MID-IDARTNR-IN NOT = ALL '+'                                   
208400       AND MID-IDTYPE-IN  NOT = ALL '+')                                  
208500       OR MID-IDREFTYP-IN NOT = ALL '+'                                   
208600                                                                          
208700         PERFORM EC-SOEK-NY-ARTIKEL-TYP                                   
208800                                                                          
208900         IF SW-TRAEFF-JA                                                  
209000           CONTINUE                                                       
209100         ELSE                                                             
209200           PERFORM MFS-RENSA-FAELT-IN                                     
209300           PERFORM MFS-RENSA-FAELT-UT                                     
209400           MOVE URVAL-SAKNAS TO MED-IDMFSFEL                              
209500           CALL WMEDKONV USING MED-WMEDAREA                               
209600           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
209700         END-IF                                                           
209800                                                                          
209900       ELSE                                                               
210000                                                                          
210100           IF (MID-IDARTNR-IN NOT = ALL '+'                               
210200           OR  MID-IDDC-IN    NOT = ALL '+')                              
210300           AND MID-IDTYPE-IN      = ALL '+'                               
210400           AND MID-IDREFTYP-IN    = ALL '+'                               
210500                                                                          
210600             IF WS-DC-MAX > 0                                             
210700               PERFORM ED-SOEK-NY-ARTIKEL                                 
210800             ELSE                                                         
210900               MOVE NEJ      TO SW-TRAEFF                                 
211000             END-IF                                                       
211100                                                                          
211200             IF SW-TRAEFF-JA                                              
211300               CONTINUE                                                   
211400             ELSE                                                         
211500               PERFORM MFS-RENSA-FAELT-IN                                 
211600               PERFORM MFS-RENSA-FAELT-UT                                 
211700               MOVE URVAL-SAKNAS TO MED-IDMFSFEL                          
211800               CALL WMEDKONV USING MED-WMEDAREA                           
211900               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
212000             END-IF                                                       
212100                                                                          
212200           ELSE                                                           
212300                                                                          
212400             IF MID-IDTYPE-IN NOT = ALL '+'                               
212500             OR MID-IDSTATUS-IN NOT = ALL '+'                             
212600                                                                          
212700               PERFORM EA-NY-TYPE-LEV-STAT                                
212800                                                                          
212900               IF SW-TRAEFF-JA                                            
213000                 CONTINUE                                                 
213100               ELSE                                                       
213200                 PERFORM MFS-RENSA-FAELT-IN                               
213300                 PERFORM MFS-RENSA-FAELT-UT                               
213400                 MOVE URVAL-SAKNAS TO MED-IDMFSFEL                        
213500                 CALL WMEDKONV USING MED-WMEDAREA                         
213600                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
213700               END-IF                                                     
213800                                                                          
213900                                                                          
214000             END-IF                                                       
214100           END-IF                                                         
214200       END-IF                                                             
214300     END-IF                                                               
214400     .                                                                    
214500     EJECT                                                                
214600                                                                          
214700 EA-NY-TYPE-LEV-STAT SECTION.                                             
214800     MOVE 'EA-NY-TYPE-LEV-STAT ' TO  WS-PGM-SEKTION                       
214900                                                                          
215000     MOVE NEJ                TO SW-TRAEFF                                 
215100     MOVE ZERO               TO W-IDARTNR                                 
215200                                W-IDARTNR-MIN                             
215300     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-UT                            
215400     PERFORM IMS-GU-WDE3B1-MIN-MAX                                        
215500     PERFORM S2-KOLLA-GILTIGT-DC                                          
215600                                                                          
215700     PERFORM UNTIL SEGMENT-SAKNAS                                         
215800     OR  (SEQB-KDREFTYP = WS-IDTYPE                                       
215900       AND SW-GILTIGT-DC-JA                                               
216000       AND ((WS-STATUS         = 'N'                                      
216100       AND  SEQB-KDREFORS = 'P')                                          
216200       OR  (WS-STATUS          = 'R'                                      
216300       AND  SEQB-KDREFORS = 'O')))                                        
216400            PERFORM IMS-GN-WDE3B1-MIN-MAX                                 
216500            PERFORM S2-KOLLA-GILTIGT-DC                                   
216600                                                                          
216700     END-PERFORM                                                          
216800                                                                          
216900     IF SEGMENT-FINNS                                                     
217000       IF SEQB-IDDC NOT = DCS-IDDC                                        
217100          MOVE SEQB-IDDC        TO W-IDDC-B6                              
217200          PERFORM IMS-GU-WDB601                                           
217300       END-IF                                                             
217400*       --- CHECK IDLEVNR-SECURITY                                        
217500        MOVE SEQB-IDARTNR TO W-IDARTNR                                    
217600        PERFORM S1-SECURITY-CHECK-PARTNO                                  
217700     END-IF                                                               
217800                                                                          
217900     IF SEGMENT-FINNS                                                     
218000       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
218100     END-IF                                                               
218200                                                                          
218300     MOVE SEQB-IDARTNR          TO WS-SPARA-IDARTNR                       
218400     IF SEGMENT-FINNS                                                     
218500        MOVE SEQB-KDREFTYP      TO W-KDREFTYP-MIN                         
218600        MOVE SEQB-IDARTNR       TO W-IDARTNR-MIN                          
218700        MOVE WS-DC-NR (1)       TO W-IDDC-MIN                             
218800        PERFORM IMS-GU-WDE3B1-MIN-MAX                                     
218900**********                                                                
219000        PERFORM UNTIL SEGMENT-SAKNAS                                      
219100        OR NOT (WS-SPARA-IDARTNR = SEQB-IDARTNR                           
219200        AND SW-GILTIGT-DC-JA)                                             
219300          IF SEQB-KDREFTYP = WS-IDTYPE                                    
219400                                                                          
219500**********                                                                
219600            IF SEQB-IDDC = WS-DC-NR (1)                                   
219700              MOVE +1           TO IX-DC                                  
219800            END-IF                                                        
219900**********                                                                
220000            IF SEQB-IDDC = WS-DC-NR (2)                                   
220100              MOVE +2           TO IX-DC                                  
220200            END-IF                                                        
220300**********                                                                
220400            IF SEQB-IDDC = WS-DC-NR (3)                                   
220500              MOVE +3           TO IX-DC                                  
220600            END-IF                                                        
220700**********                                                                
220800            IF SEQB-IDDC = WS-DC-NR (4)                                   
220900              MOVE +4           TO IX-DC                                  
221000            END-IF                                                        
221100**********                                                                
221200            IF SEQB-IDDC = WS-DC-NR (5)                                   
221300              MOVE +5           TO IX-DC                                  
221400            END-IF                                                        
221500**********                                                                
221600            IF SEQB-IDDC = WS-DC-NR (6)                                   
221700              MOVE +6           TO IX-DC                                  
221800            END-IF                                                        
221900                                                                          
222000            MOVE SEQB-IDWDE301                                            
222100                                TO W-WDE301KY-X                           
222200            PERFORM IMS-GU-WDE301                                         
222300                                                                          
222400            PERFORM M-UPD-MOD-FAELT                                       
222500          END-IF                                                          
222600                                                                          
222700          PERFORM IMS-GN-WDE3B1-MIN-MAX                                   
222800          PERFORM S2-KOLLA-GILTIGT-DC                                     
222900                                                                          
223000          PERFORM UNTIL SEGMENT-SAKNAS                                    
223100          OR (WS-SPARA-IDARTNR = SEQB-IDARTNR                             
223200          AND SW-GILTIGT-DC-JA)                                           
223300            PERFORM IMS-GN-WDE3B1-MIN-MAX                                 
223400            PERFORM S2-KOLLA-GILTIGT-DC                                   
223500          END-PERFORM                                                     
223600                                                                          
223700        END-PERFORM                                                       
223800     END-IF                                                               
223900     IF SEGMENT-FINNS                                                     
224000       IF SEQB-IDDC NOT = DCS-IDDC                                        
224100          MOVE SEQB-IDDC          TO W-IDDC-B6                            
224200          PERFORM IMS-GU-WDB601                                           
224300       END-IF                                                             
224400     END-IF                                                               
224500     .                                                                    
224600     EJECT                                                                
224700 EB-SIMULERA SECTION.                                                     
224800     MOVE 'EB-SIMULERA         ' TO  WS-PGM-SEKTION                       
224900                                                                          
225000     MOVE NEJ                TO SW-TRAEFF                                 
225100     MOVE WS-MSGI-IDARTNR-ENTER                                           
225200                             TO W-IDARTNR                                 
225300                                W-IDARTNR-301                             
225400                                W-IDARTNR-MIN                             
225500     PERFORM IMS-GU-WDE3B1-MIN-MAX                                        
225600     PERFORM S2-KOLLA-GILTIGT-DC                                          
225700                                                                          
225800     PERFORM UNTIL SEGMENT-SAKNAS                                         
225900     OR (SEQB-IDARTNR = W-IDARTNR-MIN                                     
226000     AND SW-GILTIGT-DC-JA                                                 
226100     AND SEQB-KDREFTYP = WS-IDREFTYP)                                     
226200        PERFORM IMS-GN-WDE3B1-MIN-MAX                                     
226300        PERFORM S2-KOLLA-GILTIGT-DC                                       
226400                                                                          
226500     END-PERFORM                                                          
226600                                                                          
226700     IF SEGMENT-FINNS                                                     
226800       IF SEQB-IDDC NOT = DCS-IDDC                                        
226900          MOVE SEQB-IDDC        TO W-IDDC-B6                              
227000          PERFORM IMS-GU-WDB601                                           
227100       END-IF                                                             
227200*       --- CHECK IDLEVNR-SECURITY                                        
227300        MOVE SEQB-IDARTNR TO W-IDARTNR                                    
227400        PERFORM S1-SECURITY-CHECK-PARTNO                                  
227500     END-IF                                                               
227600                                                                          
227700     IF SEGMENT-FINNS                                                     
227800       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
227900     END-IF                                                               
228000                                                                          
228100     PERFORM UNTIL SEGMENT-SAKNAS                                         
228200     OR NOT (WS-SPARA-IDARTNR = SEQB-IDARTNR                              
228300     AND SW-GILTIGT-DC-JA)                                                
228400       IF SEQB-KDREFTYP = WS-IDREFTYP                                     
228500                                                                          
228600**********                                                                
228700         IF SEQB-IDDC = WS-DC-NR (1)                                      
228800           MOVE +1           TO IX-DC                                     
228900         END-IF                                                           
229000**********                                                                
229100         IF SEQB-IDDC = WS-DC-NR (2)                                      
229200           MOVE +2           TO IX-DC                                     
229300         END-IF                                                           
229400**********                                                                
229500         IF SEQB-IDDC = WS-DC-NR (3)                                      
229600           MOVE +3           TO IX-DC                                     
229700         END-IF                                                           
229800**********                                                                
229900         IF SEQB-IDDC = WS-DC-NR (4)                                      
230000           MOVE +4           TO IX-DC                                     
230100         END-IF                                                           
230200**********                                                                
230300         IF SEQB-IDDC = WS-DC-NR (5)                                      
230400           MOVE +5           TO IX-DC                                     
230500         END-IF                                                           
230600**********                                                                
230700         IF SEQB-IDDC = WS-DC-NR (6)                                      
230800           MOVE +6           TO IX-DC                                     
230900         END-IF                                                           
231000                                                                          
231100         MOVE SEQB-IDWDE301                                               
231200                             TO W-WDE301KY-X                              
231300         PERFORM IMS-GU-WDE301                                            
231400                                                                          
231500         MOVE REF-KDREFTXT   TO WS-REF-KDREFTXT (IX-DC)                   
232502         IF REF-KDREFORS = 'P'                                            
232503            MOVE 'PROPOSAL NOT REVIEWED'                                  
232504                             TO MOD-ORDERSTATUS                           
232505         ELSE                                                             
232506            MOVE 'REVIEWED'  TO MOD-ORDERSTATUS                           
232507         END-IF                                                           
232508                                                                          
232509         IF REF-KDREFTYP = 'B'                                            
232510           MOVE 'BOAT'       TO MOD-IDREFTYP-UT                           
232520         END-IF                                                           
232530         IF REF-KDREFTYP = 'A'                                            
232540           MOVE 'AIR'        TO MOD-IDREFTYP-UT                           
232550         END-IF                                                           
232560         IF REF-KDREFTYP = 'C'                                            
232570           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
232580         END-IF                                                           
232590         IF REF-KDREFTYP = 'L'                                            
232600           MOVE 'LOCAL'      TO MOD-IDREFTYP-UT                           
232700         END-IF                                                           
232800         IF REF-KDREFTYP = 'T'                                            
232900           MOVE 'TRANS'      TO MOD-IDREFTYP-UT                           
233000         END-IF                                                           
233100                                                                          
233200       END-IF                                                             
233300                                                                          
233400       PERFORM IMS-GN-WDE3B1-MIN-MAX                                      
233500       PERFORM S2-KOLLA-GILTIGT-DC                                        
233600                                                                          
233700       PERFORM UNTIL SEGMENT-SAKNAS                                       
233800       OR (WS-SPARA-IDARTNR = SEQB-IDARTNR                                
233900       AND SW-GILTIGT-DC-JA)                                              
234000         PERFORM IMS-GN-WDE3B1-MIN-MAX                                    
234100         PERFORM S2-KOLLA-GILTIGT-DC                                      
234200       END-PERFORM                                                        
234300                                                                          
234400     END-PERFORM                                                          
234500     IF SEGMENT-FINNS                                                     
234600       IF SEQB-IDDC NOT = DCS-IDDC                                        
234700          MOVE SEQB-IDDC          TO W-IDDC-B6                            
234800          PERFORM IMS-GU-WDB601                                           
234900       END-IF                                                             
235000     END-IF                                                               
235100     .                                                                    
235200     EJECT                                                                
235300                                                                          
235400 EC-SOEK-NY-ARTIKEL-TYP SECTION.                                          
235500     MOVE 'EC-SOEK-NY-ARTIKEL-TYP'                                        
235600                             TO WS-PGM-SEKTION                            
235700                                                                          
235800     MOVE NEJ                TO SW-TRAEFF                                 
235900*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
236000     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
236100     MOVE HIGH-VALUE         TO W-KDREFTYP-MAX                            
236200     MOVE W-IDARTNR          TO W-IDARTNR-MIN                             
236300     MOVE WS-DC-NR (1)       TO W-IDDC-MIN                                
236400     MOVE WS-DC-NR (WS-DC-MAX)                                            
236500                             TO W-IDDC-MAX                                
236600     IF WS-IDREFTYP = 'C'                                                 
236700        MOVE 'A'             TO W-KDREFTYP-MIN                            
236800     END-IF                                                               
236900                                                                          
237000     PERFORM IMS-GU-WDE3B1-MIN-MAX                                        
237100     PERFORM S2-KOLLA-GILTIGT-DC                                          
237200                                                                          
237300     PERFORM UNTIL SEGMENT-SAKNAS                                         
237400     OR (SEQB-IDARTNR = W-IDARTNR-MIN                                     
237500     AND SW-GILTIGT-DC-JA                                                 
237600     AND (SEQB-KDREFTYP = WS-IDREFTYP                                     
237700     OR  (SEQB-KDREFTYP = 'A'                                             
237800     AND  WS-IDREFTYP = 'C')                                              
237900     OR  (SEQB-KDREFTYP = 'C'                                             
238000     AND  WS-IDREFTYP = 'A')))                                            
238100                                                                          
238200       PERFORM IMS-GN-WDE3B1-MIN-MAX                                      
238300       PERFORM S2-KOLLA-GILTIGT-DC                                        
238400                                                                          
238500     END-PERFORM                                                          
238600                                                                          
238700     IF SEGMENT-FINNS                                                     
238800       IF SEQB-IDDC NOT = DCS-IDDC                                        
238900          MOVE SEQB-IDDC        TO W-IDDC-B6                              
239000          PERFORM IMS-GU-WDB601                                           
239100       END-IF                                                             
239200*       --- CHECK IDLEVNR-SECURITY                                        
239300        MOVE SEQB-IDARTNR TO W-IDARTNR                                    
239400        PERFORM S1-SECURITY-CHECK-PARTNO                                  
239500     END-IF                                                               
239600                                                                          
239700     IF SEGMENT-FINNS                                                     
239800       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
239900       MOVE SEQB-IDARTNR                                                  
240000                             TO W-IDARTNR                                 
240100                                W-IDARTNR-301                             
240200                                W-IDARTNR-MIN                             
240300*                               W-IDARTNR-MAX                             
240400                                                                          
240500       PERFORM UNTIL SEGMENT-SAKNAS                                       
240600       OR NOT (WS-SPARA-IDARTNR = SEQB-IDARTNR                            
240700       AND SW-GILTIGT-DC-JA)                                              
240800                                                                          
240900**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
241000**********      ARTNR FÖR DC MED RÄTT KDREFTYP                            
241100**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
241200         IF SEQB-KDREFTYP = WS-IDREFTYP                                   
241300         OR (SEQB-KDREFTYP = 'A'                                          
241400         AND WS-IDREFTYP = 'C')                                           
241500         OR (SEQB-KDREFTYP = 'C'                                          
241600         AND WS-IDREFTYP = 'A')                                           
241700**********                                                                
241800           IF SEQB-IDDC = WS-DC-NR (1)                                    
241900             MOVE +1         TO IX-DC                                     
242000           END-IF                                                         
242100**********                                                                
242200           IF SEQB-IDDC = WS-DC-NR (2)                                    
242300             MOVE +2         TO IX-DC                                     
242400           END-IF                                                         
242500**********                                                                
242600           IF SEQB-IDDC = WS-DC-NR (3)                                    
242700             MOVE +3         TO IX-DC                                     
242800           END-IF                                                         
242900**********                                                                
243000           IF SEQB-IDDC = WS-DC-NR (4)                                    
243100             MOVE +4         TO IX-DC                                     
243200           END-IF                                                         
243300**********                                                                
243400           IF SEQB-IDDC = WS-DC-NR (5)                                    
243500             MOVE +5         TO IX-DC                                     
243600           END-IF                                                         
243700**********                                                                
243800           IF SEQB-IDDC = WS-DC-NR (6)                                    
243900             MOVE +6         TO IX-DC                                     
244000           END-IF                                                         
244100                                                                          
244200           MOVE SEQB-IDWDE301                                             
244300                             TO W-WDE301KY-X                              
244400           PERFORM IMS-GU-WDE301                                          
244500                                                                          
244600           PERFORM M-UPD-MOD-FAELT                                        
244700         END-IF                                                           
244800                                                                          
244900         PERFORM IMS-GN-WDE3B1-MIN-MAX                                    
245000         PERFORM S2-KOLLA-GILTIGT-DC                                      
245100                                                                          
245200         PERFORM UNTIL SEGMENT-SAKNAS                                     
245300         OR (WS-SPARA-IDARTNR = SEQB-IDARTNR                              
245400         AND SW-GILTIGT-DC-JA)                                            
245500           PERFORM IMS-GN-WDE3B1-MIN-MAX                                  
245600           PERFORM S2-KOLLA-GILTIGT-DC                                    
245700         END-PERFORM                                                      
245800                                                                          
245900       END-PERFORM                                                        
246000       IF SEGMENT-FINNS                                                   
246100         IF SEQB-IDDC NOT = DCS-IDDC                                      
246200            MOVE SEQB-IDDC          TO W-IDDC-B6                          
246300            PERFORM IMS-GU-WDB601                                         
246400         END-IF                                                           
246500       END-IF                                                             
246600     END-IF                                                               
246700     .                                                                    
246800     EJECT                                                                
246900 ED-SOEK-NY-ARTIKEL SECTION.                                              
247000     MOVE 'ED-SOEK-NY-ARTIKEL          '                                  
247100                             TO WS-PGM-SEKTION                            
247200                                                                          
247300     MOVE NEJ                TO SW-TRAEFF                                 
247400*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
247500     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
247600     MOVE LOW-VALUE          TO W-WDE301KY-MIN-X                          
247700     MOVE HIGH-VALUE         TO W-WDE301KY-MAX-X                          
247800     MOVE WS-DC-NR (1)       TO W-IDDC-301-MIN                            
247900     MOVE WS-DC-NR (WS-DC-MAX)                                            
248000                             TO W-IDDC-301-MAX                            
248100     PERFORM IMS-GU-WDE301-MIN-MAX                                        
248200                                                                          
248300     PERFORM S3-KOLLA-GILTIGT-DC                                          
248400                                                                          
248500     MOVE 9                  TO WS-KTRL-PRIO                              
248600                                                                          
248700     PERFORM UNTIL SEGMENT-SAKNAS                                         
248800     OR WS-KTRL-PRIO = 1                                                  
248900       IF  REF-IDARTNR = W-IDARTNR                                        
249000       AND SW-GILTIGT-DC-JA                                               
249100         IF (REF-KDREFTYP = 'A' OR 'C')                                   
249200         AND REF-KDREFORS = 'O'                                           
249300         AND WS-KTRL-PRIO > 1                                             
249400*------    HÖGST PRIORITET SPARAS                                         
249500*------                                                                   
249600           MOVE REF-KDREFTYP TO WS-IDREFTYP                               
249700           MOVE 1            TO WS-KTRL-PRIO                              
249800                                                                          
249900         ELSE                                                             
250000           IF REF-KDREFTYP = 'B'                                          
250100           AND REF-KDREFORS = 'O'                                         
250200           AND WS-KTRL-PRIO > 2                                           
250300*------    HÖGST PRIORITET SPARAS                                         
250400*------                                                                   
250500             MOVE REF-KDREFTYP                                            
250600                             TO WS-IDREFTYP                               
250700             MOVE 2          TO WS-KTRL-PRIO                              
250800                                                                          
250900           ELSE                                                           
251000             IF REF-KDREFTYP = 'T'                                        
251100             AND REF-KDREFORS = 'O'                                       
251200             AND WS-KTRL-PRIO > 3                                         
251300*------    HÖGST PRIORITET SPARAS                                         
251400*------                                                                   
251500               MOVE REF-KDREFTYP                                          
251600                             TO WS-IDREFTYP                               
251700               MOVE 3        TO WS-KTRL-PRIO                              
251800                                                                          
251900             ELSE                                                         
252000               IF REF-KDREFTYP = 'L'                                      
252100               AND REF-KDREFORS = 'O'                                     
252200               AND WS-KTRL-PRIO > 4                                       
252300*------      HÖGST PRIORITET SPARAS                                       
252400*------                                                                   
252500                 MOVE REF-KDREFTYP                                        
252600                               TO WS-IDREFTYP                             
252700                 MOVE 4      TO WS-KTRL-PRIO                              
252800                                                                          
252900               ELSE                                                       
253000                 IF (REF-KDREFTYP = 'A' OR 'C')                           
253100                 AND REF-KDREFORS = 'P'                                   
253200                 AND WS-KTRL-PRIO > 5                                     
253300*------      HÖGST PRIORITET SPARAS                                       
253400*------                                                                   
253500                   MOVE REF-KDREFTYP                                      
253600                               TO WS-IDREFTYP                             
253700                   MOVE 5    TO WS-KTRL-PRIO                              
253800                                                                          
253900                 ELSE                                                     
254000                   IF REF-KDREFTYP = 'B'                                  
254100                   AND REF-KDREFORS = 'P'                                 
254200                   AND WS-KTRL-PRIO > 6                                   
254300*------      HÖGST PRIORITET SPARAS                                       
254400*------                                                                   
254500                     MOVE REF-KDREFTYP                                    
254600                               TO WS-IDREFTYP                             
254700                     MOVE 6  TO WS-KTRL-PRIO                              
254800                                                                          
254900                   ELSE                                                   
255000                     IF REF-KDREFTYP = 'T'                                
255100                     AND REF-KDREFORS = 'P'                               
255200                     AND WS-KTRL-PRIO > 7                                 
255300*------      HÖGST PRIORITET SPARAS                                       
255400*------                                                                   
255500                       MOVE REF-KDREFTYP                                  
255600                               TO WS-IDREFTYP                             
255700                       MOVE 7 TO WS-KTRL-PRIO                             
255800                                                                          
255900                     ELSE                                                 
256000                       IF REF-KDREFTYP = 'L'                              
256100                       AND REF-KDREFORS = 'P'                             
256200                       AND WS-KTRL-PRIO > 8                               
256300*------        HÖGST PRIORITET SPARAS                                     
256400*------                                                                   
256500                         MOVE REF-KDREFTYP                                
256600                                 TO WS-IDREFTYP                           
256700                         MOVE 8 TO WS-KTRL-PRIO                           
256800                                                                          
256900                       END-IF                                             
257000                     END-IF                                               
257100                   END-IF                                                 
257200                 END-IF                                                   
257300               END-IF                                                     
257400             END-IF                                                       
257500           END-IF                                                         
257600         END-IF                                                           
257700       END-IF                                                             
257800       PERFORM IMS-GN-WDE301-MIN-MAX                                      
257900       PERFORM S3-KOLLA-GILTIGT-DC                                        
258000                                                                          
258100     END-PERFORM                                                          
258200                                                                          
258300     IF SEGMENT-FINNS                                                     
258400       IF REF-IDDC NOT = DCS-IDDC                                         
258500          MOVE REF-IDDC      TO W-IDDC-B6                                 
258600          PERFORM IMS-GU-WDB601                                           
258700       END-IF                                                             
258800     END-IF                                                               
258900                                                                          
259000     IF WS-KTRL-PRIO NOT = 9                                              
259100*---    TRÄFF PÅ SÖKT ARTIKEL                                             
259200*---                                                                      
259300       MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                            
259400                                W-KDREFTYP-MAX                            
259500       MOVE W-IDARTNR        TO W-IDARTNR-MIN                             
259600                                W-IDARTNR-MAX                             
259700       MOVE WS-DC-NR (1)     TO W-IDDC-MIN                                
259800       MOVE WS-DC-NR (WS-DC-MAX)                                          
259900                             TO W-IDDC-MAX                                
260000                                                                          
260100       PERFORM IMS-GU-WDE3B1-MIN-MAX                                      
260200       PERFORM S2-KOLLA-GILTIGT-DC                                        
260300                                                                          
260400       PERFORM UNTIL SEGMENT-SAKNAS                                       
260500       OR (SEQB-IDARTNR = W-IDARTNR                                       
260600       AND SW-GILTIGT-DC-JA                                               
260700       AND SEQB-KDREFTYP = WS-IDREFTYP)                                   
260800*------    LÄS FRAM TILL SÖKT ARTIKEL/REFTYP                              
260900*------                                                                   
261000         PERFORM IMS-GN-WDE3B1-MIN-MAX                                    
261100         PERFORM S2-KOLLA-GILTIGT-DC                                      
261200                                                                          
261300       END-PERFORM                                                        
261400                                                                          
261500       IF SEGMENT-FINNS                                                   
261600         IF SEQB-IDDC NOT = DCS-IDDC                                      
261700            MOVE SEQB-IDDC     TO W-IDDC-B6                               
261800            PERFORM IMS-GU-WDB601                                         
261900         END-IF                                                           
262000*         --- CHECK IDLEVNR-SECURITY                                      
262100          MOVE REF-IDARTNR TO W-IDARTNR                                   
262200          PERFORM S1-SECURITY-CHECK-PARTNO                                
262300       END-IF                                                             
262400                                                                          
262500       IF SEGMENT-FINNS                                                   
262600         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
262700       ELSE                                                               
262800*------    DETTA FALL SKA INTE INTRÄFFA                                   
262900*------                                                                   
263000         MOVE 'GE'           TO STATUS-WS                                 
263100       END-IF                                                             
263200     ELSE                                                                 
263300       MOVE 'GE'             TO STATUS-WS                                 
263400     END-IF                                                               
263500                                                                          
263600     PERFORM UNTIL SEGMENT-SAKNAS                                         
263700     OR NOT (WS-SPARA-IDARTNR = SEQB-IDARTNR                              
263800     AND SW-GILTIGT-DC-JA                                                 
263900     AND SEQB-KDREFTYP = WS-IDREFTYP)                                     
264000                                                                          
264100**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
264200**********      ARTNR FÖR DC MED RÄTT KDREFTYP                            
264300**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
264400       IF  SEQB-KDREFTYP = WS-IDREFTYP                                    
264500**********                                                                
264600         IF SEQB-IDDC = WS-DC-NR (1)                                      
264700           MOVE +1           TO IX-DC                                     
264800         END-IF                                                           
264900**********                                                                
265000         IF SEQB-IDDC = WS-DC-NR (2)                                      
265100           MOVE +2           TO IX-DC                                     
265200         END-IF                                                           
265300**********                                                                
265400         IF SEQB-IDDC = WS-DC-NR (3)                                      
265500           MOVE +3           TO IX-DC                                     
265600         END-IF                                                           
265700**********                                                                
265800         IF SEQB-IDDC = WS-DC-NR (4)                                      
265900           MOVE +4           TO IX-DC                                     
266000         END-IF                                                           
266100**********                                                                
266200         IF SEQB-IDDC = WS-DC-NR (5)                                      
266300           MOVE +5           TO IX-DC                                     
266400         END-IF                                                           
266500**********                                                                
266600         IF SEQB-IDDC = WS-DC-NR (6)                                      
266700           MOVE +6           TO IX-DC                                     
266800         END-IF                                                           
266900                                                                          
267000         MOVE SEQB-IDWDE301                                               
267100                             TO W-WDE301KY-X                              
267200         PERFORM IMS-GU-WDE301                                            
267300                                                                          
267400         PERFORM M-UPD-MOD-FAELT                                          
267500       END-IF                                                             
267600                                                                          
267700       PERFORM IMS-GN-WDE3B1-MIN-MAX                                      
267800       PERFORM S2-KOLLA-GILTIGT-DC                                        
267900                                                                          
268000       PERFORM UNTIL SEGMENT-SAKNAS                                       
268100       OR (SEQB-IDARTNR = W-IDARTNR                                       
268200       AND SW-GILTIGT-DC-JA                                               
268300       AND SEQB-KDREFTYP = WS-IDREFTYP)                                   
268400*------    LÄS FRAM TILL SÖKT ARTIKEL/REFTYP                              
268500*------                                                                   
268600         PERFORM IMS-GN-WDE3B1-MIN-MAX                                    
268700         PERFORM S2-KOLLA-GILTIGT-DC                                      
268800                                                                          
268900       END-PERFORM                                                        
269000                                                                          
269100     END-PERFORM                                                          
269200                                                                          
269300     IF SEGMENT-FINNS                                                     
269400       IF SEQB-IDDC NOT = DCS-IDDC                                        
269500          MOVE SEQB-IDDC       TO W-IDDC-B6                               
269600          PERFORM IMS-GU-WDB601                                           
269700       END-IF                                                             
269800     END-IF                                                               
269900     .                                                                    
270000     EJECT                                                                
270100 EE-BERAKNA-REFPKT SECTION.                                               
270200     MOVE 'EE-BERAKNA-REFPKT           '                                  
270300                             TO WS-PGM-SEKTION                            
270400                                                                          
270500     MOVE +1                 TO IX-DC                                     
270600     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
270700                                                                          
270800       IF WS-KVPB-REF (IX-DC) > ZERO                                      
270900                                                                          
271000         MOVE WS-DC-NR (IX-DC)                                            
271100                             TO W-IDDC                                    
271200         PERFORM IMS-GU-WDK711                                            
271300         IF SEGMENT-FINNS                                                 
271400                                                                          
271500           MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                            
271600           MOVE DAGENS-DATUM    TO TMP2-YYMMDD                            
271700           PERFORM WY2000P1                                               
271800           IF TMP1-YYMMDD   >= TMP2-YYMMDD                                
271900                                                                          
272000*                                                                         
272100*---   KVREFPKT FRÅN BASEN GÄLLER PGA MANUELLT DATUM ÄR SATT              
272200             MOVE SLAG-KVREFPKT                                           
272300                             TO MOD-KVREFPKT (IX-DC)                      
272400             PERFORM IMS-GU-WDK727                                        
272500             IF SEGMENT-FINNS                                             
272600               IF PROG-KVPB-JUST(1) > ZERO                                
272700                 MOVE MFS-ADD-LYS-UPP-FAELT                               
272800                            TO MOD-KVREFPKT-ATTR (IX-DC)                  
272900               END-IF                                                     
273000             END-IF                                                       
273100             MOVE SLAG-KVREFBER                                           
273200                             TO MOD-KVREFBER (IX-DC)                      
273300           ELSE                                                           
273400***          THIS SIMULATES WHEN ENTER IS PRESSED                         
273500***          ALWAYS PASS YES TO FLSIM AND NO FOR UPDATES                  
273600*                                                                         
273700             MOVE JA         TO WS-FLSIM                                  
273800             MOVE SLAG-KVPBREOI  TO WS-KVPBREOI (IX-DC)                   
273900                                                                          
274000             PERFORM S90-CALL-W271REFL                                    
274100                                                                          
274200             MOVE W271-REFL-KVREFPKT                                      
274300                             TO MOD-KVREFPKT (IX-DC)                      
274400                                                                          
274500             PERFORM IMS-GU-WDK727                                        
274600             IF SEGMENT-FINNS                                             
274700               IF PROG-KVPB-JUST(1) > ZERO                                
274800                 MOVE MFS-ADD-LYS-UPP-FAELT                               
274900                            TO MOD-KVREFPKT-ATTR (IX-DC)                  
275000               END-IF                                                     
275100             END-IF                                                       
275200             MOVE W271-REFL-KVREFBER                                      
275300                             TO MOD-KVREFBER (IX-DC)                      
275400           END-IF                                                         
275500         ELSE                                                             
275600           MOVE ZERO         TO MOD-KVREFPKT (IX-DC)                      
275700           MOVE ZERO         TO MOD-KVREFBER (IX-DC)                      
275800         END-IF                                                           
275900       ELSE                                                               
276000         MOVE ZERO           TO MOD-KVREFPKT (IX-DC)                      
276100         MOVE ZERO           TO MOD-KVREFBER (IX-DC)                      
276200       END-IF                                                             
276300                                                                          
276400       ADD +1                TO IX-DC                                     
276500     END-PERFORM                                                          
276600     .                                                                    
276700     EJECT                                                                
276800 F-HAEMTA-INFO SECTION.                                                   
276900     MOVE 'F-HAEMTA-INFO               '                                  
277000                             TO WS-PGM-SEKTION                            
277100                                                                          
277200     PERFORM IMS-GU-WDK601                                                
277300     IF SEGMENT-FINNS                                                     
277400       MOVE ART-KDPRODSL     TO MOD-KDPRODSL                              
277500       MOVE ART-TIFINLV      TO MOD-TIFINLV                               
277600       MOVE ART-TIURPROD     TO MOD-TIURPROD                              
277700                                                                          
277800       PERFORM IMS-GNP-WDK611                                             
277900       IF SEGMENT-FINNS                                                   
278000         MOVE CLAG-KDERS     TO MOD-KDERS                                 
278100                                WS-KDERS                                  
278200         IF DCS-CHINA OR DCS-NDC-NA                                       
278300           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
278400           PERFORM IMS-GU-WDK712                                          
278500           IF SEGMENT-FINNS                                               
278600             MOVE LART-PRMATRL                                            
278700                             TO MOD-PRARTSTD                              
278800           END-IF                                                         
278900         ELSE                                                             
279000           MOVE CLAG-PRARTSTD                                             
279100                             TO MOD-PRARTSTD                              
279200         END-IF                                                           
279300                                                                          
279400         PERFORM IMS-GU-WDK901                                            
279500         IF SEGMENT-FINNS                                                 
279600           COMPUTE WS-KVOKS-TOT = WDK9-ART-KVOKS-BULK +                   
279700                                   WDK9-ART-KVOKS-DAG +                   
279800                                   WDK9-ART-KVOKS-VOR                     
279900         ELSE                                                             
280000           MOVE ZERO        TO WS-KVOKS-TOT                               
280100         END-IF                                                           
280200*                                                                         
280300         MOVE CLAG-KVQPACK-1 TO MOD-KVQPACK-1                             
280400         MOVE CLAG-KVQPACK-3 TO WS-KVQPACK-3                              
280500                                                                          
280600         MOVE WSA-IDDC       TO W-IDDC                                    
280700         MOVE SPACE          TO WS-IDDC                                   
280800         PERFORM IMS-GU-WDK711                                            
280900         IF SEGMENT-FINNS                                                 
281000           MOVE CLAG-VLARTNTO                                             
281100                             TO MOD-VLARTNTO                              
281200           MOVE SLAG-IDDC-REF TO W-IDDC                                   
281300                                 WS-IDDC                                  
281400           IF SLAG-IDDC-REF NOT = SPACE                                   
281500             MOVE SLAG-IDDC-REF TO W-IDDC-REF-B6                          
281600           ELSE                                                           
281700*************IF LOCALY SOURCED ON THE DC SHOW COUNTRY INFO FOR DC         
281800             MOVE SLAG-IDDC     TO W-IDDC-REF-B6                          
281900                                   WS-IDDC                                
282000                                   W-IDDC                                 
282100           END-IF                                                         
282200           PERFORM IMS-GU-WDB601-REF                                      
282300           IF SEGMENT-FINNS                                               
282400              MOVE B6-DCS-IDLANDX2                                        
282500                           TO W-IDLAND                                    
282600              PERFORM IMS-GU-WDK712                                       
282700              IF SEGMENT-FINNS                                            
282800                 IF LART-KVQPACK-3 > ZERO                                 
282900                    MOVE LART-KVQPACK-3                                   
283000                           TO WS-KVQPACK-3                                
283100                 END-IF                                                   
283200                 IF LART-VLARTNTO > ZERO                                  
283300                     MOVE LART-VLARTNTO                                   
283400                           TO MOD-VLARTNTO                                
283500                 END-IF                                                   
283600               END-IF                                                     
283700           END-IF                                                         
283800           PERFORM IMS-GU-WDK711                                          
283900         END-IF                                                           
284000         MOVE WS-KVQPACK-3      TO MOD-KVQPACK-3                          
284100                                                                          
284200         MOVE ZERO TO WS-KVPB                                             
284300         IF CDC OR (WS-IDDC = SPACE)                                      
284400           COMPUTE WS-AVAILABLE ROUNDED =                                 
284500                   CLAG-KVLS - CLAG-KVRESS - WS-KVOKS-TOT                 
284600           MOVE CLAG-KVROS   TO WS-KVROS                                  
284700***FD-SEC RÄKNAR TOTAL PB OM SÄNDANDE DC ÄR CDC FÖR DC:T SOM              
284800***FYLLT I I HUVUDET PÅ BILDEN                                            
284900           PERFORM FD-HAEMTA-KVPB                                         
285000         ELSE                                                             
285100           COMPUTE WS-AVAILABLE ROUNDED =                                 
285200                   SLAG-KVLS - SLAG-KVRESS - SLAG-KVOKS-BULK -            
285300                   SLAG-KVOKS-DAG                                         
285400           COMPUTE WS-KVROS ROUNDED =                                     
285500                   SLAG-KVROS-BULK + SLAG-KVROS-DAG                       
285600***summerar forecast för sändande dc til det dc som fyllts                
285700***i i huvudet på bilden                                                  
285800           ADD SLAG-KVPB-REF         TO WS-KVPB                           
285900           ADD SLAG-KVPBREOI         TO WS-KVPB                           
286000         END-IF                                                           
286100         MOVE WS-KVPB        TO MOD-KVPB-CDC                              
286200         MOVE WS-AVAILABLE   TO MOD-AVAIL                                 
286300         MOVE WS-KVROS       TO MOD-KVROS-CDC                             
286400         IF CLAG-KVUTRS > ZERO                                            
286500           MOVE CLAG-KVUTRS  TO WS-TEMF-UTRSALDO-NUM                      
286600           MOVE WS-TEMF-UTRSALDO-NUM                                      
286700                                 TO WS-TEMF-UTRSALDO                      
286800           MOVE WS-TEMF-RED-INVBAL                                        
286900                                 TO WS-TEMFSINF-TEXT                      
287000         END-IF                                                           
287100         MOVE MFS-RENSA-FAELT                                             
287200                             TO MOD-REPLACES                              
287300         IF ART-FLERS = JA                                                
287400            MOVE ART-IDARTNR TO W-IDARTNR-MIN7                            
287500                                W-IDARTNR-MAX7                            
287600            PERFORM IMS-GU-WDD7A1-MINMAX                                  
287700            IF SEGMENT-FINNS                                              
287800               IF WDD7A1-ERS-IDARTNR NOT = ZERO                           
287900                  MOVE WDD7A1-ERS-IDARTNR                                 
288000                             TO MOD-REPLACES                              
288100                  INSPECT MOD-REPLACES REPLACING                          
288200                                        LEADING ZERO BY SPACE             
288300                  PERFORM IMS-GN-WDD7A1-MINMAX                            
288400                  IF SEGMENT-FINNS                                        
288500                  AND WDD7A1-ERS-IDARTNR NOT = ZERO                       
288600                    MOVE 'VARIOUS'                                        
288700                             TO MOD-REPLACES                              
288800                  END-IF                                                  
288900               END-IF                                                     
289000            END-IF                                                        
289100         END-IF                                                           
289200                                                                          
289300         IF SW-HAEMTA-INPUT-FAELT-JA                                      
289400           PERFORM DB2-SELECT-TP5COMM                                     
289500           IF LINES-FOUND                                                 
289600             MOVE TP5COMM-TEARTNOT (1:20)                                 
289700                             TO MOD-COMMENT (1)                           
289800             MOVE TP5COMM-TEARTNOT (21:20)                                
289900                             TO MOD-COMMENT (2)                           
290000           ELSE                                                           
290100             MOVE SPACE      TO MOD-COMMENT (1)                           
290200                                MOD-COMMENT (2)                           
290300           END-IF                                                         
290400         END-IF                                                           
290500         MOVE MFS-ADD-LAES-IN-FAELT                                       
290600                             TO MOD-COMMENT-ATTR (1)                      
290700                                MOD-COMMENT-ATTR (2)                      
290800                                                                          
290900         IF WS-KDERS                    >  0                              
291000           IF WS-KDERS                  <  29                             
291100             IF MOD-TEMFSFEL = SPACE                                      
291200               MOVE ARTIKEL-ERSATT                                        
291300                             TO MED-IDMFSFEL                              
291400               CALL WMEDKONV USING MED-WMEDAREA                           
291500               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
291600             END-IF                                                       
291700           ELSE                                                           
291800             MOVE ARTIKEL-UTGANGEN                                        
291900                             TO MED-IDMFSFEL                              
292000             CALL WMEDKONV USING MED-WMEDAREA                             
292100             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
292200           END-IF                                                         
292300         END-IF                                                           
292400                                                                          
292500       ELSE                                                               
292600         MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                              
292700         CALL WMEDKONV USING MED-WMEDAREA                                 
292800         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
292900         PERFORM MFS-RENSA-FAELT-IN                                       
293000         PERFORM MFS-RENSA-FAELT-UT                                       
293100       END-IF                                                             
293200     ELSE                                                                 
293300       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
293400       CALL WMEDKONV USING MED-WMEDAREA                                   
293500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
293600       PERFORM MFS-RENSA-FAELT-IN                                         
293700       PERFORM MFS-RENSA-FAELT-UT                                         
293800     END-IF                                                               
293900                                                                          
294000     MOVE 1                  TO IX-DC                                     
294100     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
294200                                                                          
294300       MOVE WS-DC-NR (IX-DC) TO W-IDDC                                    
294400       PERFORM IMS-GU-WDK711                                              
294500       IF SEGMENT-FINNS                                                   
294600                                                                          
294700         PERFORM FB-BEHANDLA-DC                                           
294800       END-IF                                                             
294900*      IF MFS-UPDATE                                                      
295000*      OR SW-HAEMTA-INPUT-FAELT-NEJ                                       
295100*        CONTINUE                                                         
295200*      ELSE                                                               
295300*        MOVE WS-IDDC-FROM (IX-DC)                                        
295400*                            TO MOD-IDDC-FROM (IX-DC)                     
295500*      END-IF                                                             
295600       MOVE WS-IDDC-FROM (IX-DC)                                          
295700                             TO MOD-IDDC-FROM (IX-DC)                     
295800       ADD 1                 TO IX-DC                                     
295900     END-PERFORM                                                          
296000     PERFORM IMS-GU-WDL601                                                
296100     IF SEGMENT-FINNS                                                     
296200       PERFORM IMS-GNP-WDL611                                             
296300       MOVE ZERO                       TO WS-TIINLINL (1)                 
296400                                          WS-TIINLINL (2)                 
296500                                          WS-TIINLINL (3)                 
296600                                          WS-TIINLINL (4)                 
296700                                          WS-TIINLINL (5)                 
296800                                          WS-TIINLINL (6)                 
296900       PERFORM UNTIL SEGMENT-SAKNAS                                       
297000                                                                          
297100         MOVE 1              TO IX-DC                                     
297200         PERFORM UNTIL IX-DC > WS-DC-MAX                                  
297300         OR WS-DC-NR (IX-DC) = INL-IDDC                                   
297400           ADD 1             TO IX-DC                                     
297500         END-PERFORM                                                      
297600                                                                          
297700         IF IX-DC > WS-DC-MAX                                             
297800           CONTINUE                                                       
297900         ELSE                                                             
298000           MOVE INL-TIINLINL                                              
298100                         TO TMP1-YYMMDD                                   
298200           MOVE WS-TIINLINL (IX-DC)                                       
298300                         TO TMP2-YYMMDD                                   
298400           PERFORM WY2000P1                                               
298500           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
298600             MOVE INL-TIINLINL                                            
298700                         TO WS-TIINLINL (IX-DC)                           
298800           END-IF                                                         
298900         END-IF                                                           
299000         PERFORM IMS-GNP-WDL611                                           
299100       END-PERFORM                                                        
299200                                                                          
299300       MOVE 1                TO IX-DC                                     
299400       PERFORM UNTIL IX-DC > WS-DC-MAX                                    
299500         MOVE WS-TIINLINL (IX-DC)                                         
299600                             TO MOD-TIINLINL (IX-DC)                      
299700         ADD 1               TO IX-DC                                     
299800       END-PERFORM                                                        
299900     END-IF                                                               
300000                                                                          
300100     MOVE MFS-RENSA-FAELT    TO MOD-REPL-BY                               
300200     PERFORM IMS-GU-WDD701                                                
300300     IF SEGMENT-FINNS                                                     
300400        PERFORM IMS-GNP-WDD702                                            
300500        IF SEGMENT-FINNS                                                  
300600           MOVE WDD702-IDARTNR-TILLK                                      
300700                             TO MOD-REPL-BY                               
300800           INSPECT MOD-REPL-BY REPLACING LEADING ZERO BY SPACE            
300900           PERFORM IMS-GNP-WDD702                                         
301000           IF SEGMENT-FINNS                                               
301100             MOVE 'VARIOUS'  TO MOD-REPL-BY                               
301200           END-IF                                                         
301300        END-IF                                                            
301400     END-IF                                                               
301500                                                                          
301600                                                                          
301700     MOVE SPACE              TO MOD-BEART                                 
301800     PERFORM IMS-GU-WDD301-BSEQ                                           
301900     IF SEGMENT-FINNS                                                     
302000       MOVE 'GB'             TO W-IDSKYLT                                 
302100       PERFORM IMS-GNP-WDD311                                             
302200       IF SEGMENT-FINNS                                                   
302300         MOVE TEXT-BEART                                                  
302400                             TO MOD-BEART                                 
302500       END-IF                                                             
302600     END-IF                                                               
302700                                                                          
302800     MOVE 1                  TO IX-DC                                     
302900     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
303000                                                                          
303100       MOVE WS-DC-NR (IX-DC) TO W-IDDC                                    
303200       PERFORM IMS-GU-WDK711                                              
303300       IF SEGMENT-FINNS                                                   
303400                                                                          
303500         PERFORM FB-BEHANDLA-DC                                           
303600       END-IF                                                             
303700       ADD 1                 TO IX-DC                                     
303800     END-PERFORM                                                          
303900                                                                          
304000     MOVE 1                  TO IX-DC                                     
304100     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
304200                                                                          
304300       MOVE WS-DC-NR (IX-DC) TO W-IDDC                                    
304400       PERFORM IMS-GU-WDL711                                              
304500       IF SEGMENT-FINNS                                                   
304600         MOVE DC-TIREFEFT    TO MOD-TIREFEFT (IX-DC)                      
304700         PERFORM FA-BEHANDLA-ORDERINGGANG                                 
304800       END-IF                                                             
304900       ADD 1                 TO IX-DC                                     
305000     END-PERFORM                                                          
305100                                                                          
305200     MOVE +1                 TO IX                                        
305300     PERFORM IMS-GU-WDN601                                                
305400     IF SEGMENT-FINNS                                                     
305500       PERFORM IMS-GNP-WDN611                                             
305600     END-IF                                                               
305700     PERFORM UNTIL SEGMENT-SAKNAS                                         
305800     OR              IX > 3                                               
305900       MOVE KAT-BEMASTER (1:3)                                            
306000                             TO MOD-MODEL (IX)                            
306100       PERFORM IMS-GNP-WDN611                                             
306200       ADD +1                TO IX                                        
306300     END-PERFORM                                                          
306400*                                                                         
306500*    CHECK SOURCING MARKET FOR THE PART                                   
306600     PERFORM FF-CHECK-ART-SOURCE                                          
306700*                                                                         
306800     IF INDATA-OK                                                         
306900     AND NOT MFS-UPDATE                                                   
306910     AND NOT MFS-UPD-V                                                    
307000       MOVE WS-TEMFSINF      TO MOD-TEMFSINF                              
307100     END-IF                                                               
307200*                                                                         
307300*     MFS-ALFA-FAELT-FEL FLYTTAS TILL ATTRIBUTET MOD-PURCHQTY-ATTR        
307400*     ENBART FÖR ATT FÅ UPPLYST FÄLT PLUS CURSORPLACERING                 
307500*     INTE FÖR ATT DET ÄR NÅGOT FEL                                       
307600*                                                                         
307700     IF WS-PURCHQTY (6) > ZERO                                            
307800       MOVE MFS-ALFA-FAELT-FEL                                            
307900                             TO MOD-PURCHQTY-ATTR (6)                     
308000     END-IF                                                               
308100     IF WS-PURCHQTY (5) > ZERO                                            
308200       MOVE MFS-ALFA-FAELT-FEL                                            
308300                             TO MOD-PURCHQTY-ATTR (5)                     
308400     END-IF                                                               
308500     IF WS-PURCHQTY (4) > ZERO                                            
308600       MOVE MFS-ALFA-FAELT-FEL                                            
308700                             TO MOD-PURCHQTY-ATTR (4)                     
308800     END-IF                                                               
308900     IF WS-PURCHQTY (3) > ZERO                                            
309000       MOVE MFS-ALFA-FAELT-FEL                                            
309100                             TO MOD-PURCHQTY-ATTR (3)                     
309200     END-IF                                                               
309300     IF WS-PURCHQTY (2) > ZERO                                            
309400       MOVE MFS-ALFA-FAELT-FEL                                            
309500                             TO MOD-PURCHQTY-ATTR (2)                     
309600     END-IF                                                               
309700     IF WS-PURCHQTY (1) > ZERO                                            
309800       MOVE MFS-ALFA-FAELT-FEL                                            
309900                             TO MOD-PURCHQTY-ATTR (1)                     
310000     END-IF                                                               
310100                                                                          
310200     MOVE WS-DC-MAX          TO IX-DC                                     
310300     ADD 1                   TO IX-DC                                     
310400     PERFORM UNTIL IX-DC > DC-MAX-2382                                    
310500                                                                          
310600       PERFORM MFS-STANG-FALT                                             
310700       ADD 1                 TO IX-DC                                     
310800     END-PERFORM                                                          
310900                                                                          
311000*                                                                         
311100*     EFTER UPPDATERING SKA CURSOR STÅ PÅ FÄLTET IDARTNR-IN               
311200*                                                                         
311300     IF MFS-UPDATE OR                                                     
311310        MFS-UPD-V                                                         
311400       MOVE MFS-ADD-SAETT-CURSOR                                          
311500                             TO MOD-IDARTNR-IN-ATTR                       
311600     END-IF                                                               
311700     .                                                                    
311800     EJECT                                                                
311900 FA-BEHANDLA-ORDERINGGANG     SECTION.                                    
312000     MOVE 'FA-BEHANDLA-ORDERINGGANG    '                                  
312100                             TO WS-PGM-SEKTION                            
312200                                                                          
312300     PERFORM FAA-HAMTA-VV-I-PER                                           
312400                                                                          
312500                                                                          
312600*  --- FYLL PÅ TABELLEN MED OI                                            
312700                                                                          
312800     MOVE +1                 TO IX                                        
312900     PERFORM UNTIL IX        >  12                                        
313000       MOVE WS-FORSTA-V(IX)  TO IX-VV                                     
313100       PERFORM UNTIL IX-VV   >  WS-SISTA-V(IX)                            
313200         ADD DC-KVOI-RULL(IX-VV)                                          
313300                             TO WS-KVOI(IX)                               
313400         ADD DC-KVOI-REF-RULL(IX-VV)                                      
313500                             TO WS-KVOI(IX)                               
313600         ADD +1              TO IX-VV                                     
313700       END-PERFORM                                                        
313800       ADD +1                TO IX                                        
313900     END-PERFORM                                                          
314000                                                                          
314100*    --- FLYTTA UT TABELLEN TILL MOD:EN,                                  
314200*    --- ENDAST DE SENASTE FEM PERIODERNA VISAS                           
314300                                                                          
314400     MOVE +8                 TO IX                                        
314500     MOVE +1                 TO MOD-IX                                    
314600     PERFORM UNTIL IX > +12                                               
314700       MOVE WS-PER(IX)       TO MOD-TIPP(MOD-IX)                          
314800       INSPECT MOD-TIPP(MOD-IX) REPLACING LEADING ZERO BY SPACE           
314900       MOVE WS-FORSTA-V(IX)                                               
315000                             TO WS-FOM                                    
315100       MOVE WS-SISTA-V(IX)   TO WS-TOM                                    
315200       MOVE WS-FOM-TOM       TO MOD-TIVV-FOM-TOM(MOD-IX)                  
315300       MOVE WS-KVOI(IX)      TO MOD-KVOI-RULL(MOD-IX, IX-DC)              
315400       MOVE ZERO             TO WS-KVOI(IX)                               
315500       ADD +1                TO IX                                        
315600                                MOD-IX                                    
315700     END-PERFORM                                                          
315800                                                                          
315900*    --- LÄGG UT KVOI FÖR AKTUELL PERIOD                                  
316000*                                                                         
316100                                                                          
316200     MOVE +1                 TO IX                                        
316300     MOVE ZERO               TO WS-KVOI-SUM                               
316400     PERFORM UNTIL IX > +5                                                
316500       ADD DC-KVOI-INNEV(IX)    TO WS-KVOI-SUM                            
316600       ADD DC-KVOI-PP-INNEV(IX) TO WS-KVOI-SUM                            
316700       ADD DC-KVOI-REF-INNEV(IX)                                          
316800                                TO WS-KVOI-SUM                            
316900       ADD +1                   TO IX                                     
317000     END-PERFORM                                                          
317100     MOVE WS-ANTAL-VECKOR       TO MOD-VECKA                              
317200     MOVE WS-KVOI-SUM           TO MOD-KVOI-INNEV (IX-DC)                 
317300     .                                                                    
317400     EJECT                                                                
317500                                                                          
317600 FAA-HAMTA-VV-I-PER SECTION.                                              
317700     MOVE 'FAA-HAMTA-VV-I-PER          '                                  
317800                             TO WS-PGM-SEKTION                            
317900                                                                          
318000     MOVE +1                 TO IX                                        
318100     MOVE DAGENS-PER         TO WS-TIAAPER                                
318200     IF TIAA = 00                                                         
318300       MOVE 99 TO TIAA                                                    
318400     ELSE                                                                 
318500       SUBTRACT 1 FROM TIAA                                               
318600     END-IF                                                               
318700                                                                          
318800*    --- TA FRAM HUR MÅNGA VECKOR DET VAR FÖREGÅENDE ÅR                   
318900     MOVE TIAA               TO WS-AAR                                    
319000     MOVE 53                 TO WS-VV                                     
319100     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
319200     MOVE TIAAVV             TO DAT-I-TIDATUM                             
319300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
319400                         DAT-O-TIDATUM DAT-KDSVAR                         
319500     IF DAT-KDSVAR-OK                                                     
319600       MOVE 53               TO WS-ANT-VV                                 
319700     ELSE                                                                 
319800       MOVE 52               TO WS-ANT-VV                                 
319900     END-IF                                                               
320000                                                                          
320100*    --- FYLL I VECKONR FÖR PERIODERNA                                    
320200                                                                          
320300     MOVE 'AARP  '           TO DAT-KDDATFORM                             
320400     MOVE TIAAPER            TO DAT-I-TIDATUM                             
320500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
320600                         DAT-O-TIDATUM DAT-KDSVAR                         
320700     IF DAT-KDSVAR-OK                                                     
320800       IF PER                = 1                                          
320900         MOVE 1              TO WS-PER(IX)                                
321000                                WS-FORSTA-V(IX)                           
321100       ELSE                                                               
321200         MOVE PER            TO WS-PER(IX)                                
321300         MOVE DAT-TIVV       TO WS-FORSTA-V(IX)                           
321400       END-IF                                                             
321500     ELSE                                                                 
321600       MOVE 'FELAKTIGT DATUM - DATKONV2' TO FELTEXT                       
321700       CALL FELLOG                                                        
321800     END-IF                                                               
321900                                                                          
322000     PERFORM UNTIL IX        >  12                                        
322100       ADD +1                TO PER                                       
322200       IF PER                >  12                                        
322300         IF TIAA = 99                                                     
322400           MOVE ZERO         TO TIAA                                      
322500         ELSE                                                             
322600           ADD +1            TO TIAA                                      
322700         END-IF                                                           
322800         MOVE 01             TO PER                                       
322900       END-IF                                                             
323000                                                                          
323100       MOVE TIAAPER          TO DAT-I-TIDATUM                             
323200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
323300                           DAT-O-TIDATUM DAT-KDSVAR                       
323400       IF DAT-KDSVAR-OK                                                   
323500         IF PER              =  1                                         
323600           MOVE WS-ANT-VV    TO WS-SISTA-V(IX)                            
323700         ELSE                                                             
323800           COMPUTE WS-SISTA-V(IX) = DAT-TIVV - 1                          
323900         END-IF                                                           
324000         ADD +1              TO IX                                        
324100         IF IX               <= 12                                        
324200           MOVE PER          TO WS-PER(IX)                                
324300           IF PER            =  1                                         
324400             MOVE +1         TO WS-FORSTA-V(IX)                           
324500           ELSE                                                           
324600             MOVE DAT-TIVV   TO WS-FORSTA-V(IX)                           
324700           END-IF                                                         
324800         END-IF                                                           
324900       ELSE                                                               
325000         MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                     
325100         CALL FELLOG                                                      
325200       END-IF                                                             
325300     END-PERFORM                                                          
325400*    --- OM VECKOR I FÖRSTA OCH SISTA PERIODEN ÖVERLAPPAR,                
325500*    --- RÄTTA I FÖRSTA (DVS DEN ÄLDSTA) PERIODEN.                        
325600     IF WS-FORSTA-V(1)        = WS-SISTA-V(12)                            
325700     OR WS-FORSTA-V(1)        = WS-SISTA-V(12) - 1                        
325800       COMPUTE WS-FORSTA-V(1) = WS-SISTA-V(12) + 1                        
325900     END-IF                                                               
326000     .                                                                    
326100     EJECT                                                                
326200 FB-BEHANDLA-DC    SECTION.                                               
326300     MOVE 'FB-BEHANDLA-DC              '                                  
326400                             TO WS-PGM-SEKTION                            
326500                                                                          
326600*    MOVE SLAG-IDDC          TO MOD-IDDC-TO (IX-DC)                       
326700     MOVE 'WA'               TO WS-AREA (1:2)                             
326800     MOVE SLAG-ADLAGOMR      TO WS-ADLAGOMR                               
326900     MOVE WS-ADLAGOMR        TO WS-AREA (3:2)                             
327000     MOVE WS-AREA            TO MOD-AREA (IX-DC)                          
327100     IF SW-HAEMTA-INPUT-FAELT-JA                                          
327200       PERFORM FE-HAEMTA-KVPB-REF                                         
327300       MOVE WS-RED-KVPB-REF-NUM                                           
327400                             TO WS-RED-KVPB-REF                           
327500                                WS-KVPB-REF (IX-DC)                       
327600       MOVE WS-RED-KVPB-REF  TO MOD-KVPB-REF (IX-DC)                      
327700*      MOVE MFS-ADD-LAES-IN-FAELT                                         
327800*                            TO MOD-KVPB-REF-ATTR (IX-DC)                 
327900       IF SLAG-FLREFBEO = JA                                              
328000       AND MSGI-IDLAND-SPR = 'GB'                                         
328100         MOVE YES            TO MOD-FLREFBEO (IX-DC)                      
328200       ELSE                                                               
328300         MOVE SLAG-FLREFBEO  TO MOD-FLREFBEO (IX-DC)                      
328400       END-IF                                                             
328500       MOVE MFS-ADD-LAES-IN-FAELT                                         
328600                             TO MOD-FLREFBEO-ATTR (IX-DC)                 
328700       MOVE SLAG-KVREFBER    TO MOD-KVREFBER (IX-DC)                      
328800       MOVE SLAG-KVREFPKT    TO MOD-KVREFPKT (IX-DC)                      
328900       PERFORM IMS-GU-WDK727                                              
329000       IF SEGMENT-FINNS                                                   
329100         IF PROG-KVPB-JUST(1) > ZERO                                      
329200           MOVE MFS-ADD-LYS-UPP-FAELT                                     
329300                      TO MOD-KVREFPKT-ATTR (IX-DC)                        
329400         END-IF                                                           
329500       END-IF                                                             
329600     END-IF                                                               
329700                                                                          
329800     COMPUTE WS-BALANCE ROUNDED = SLAG-KVLS -                             
329900                 SLAG-KVROS-BULK - SLAG-KVROS-DAG -                       
330000                 SLAG-KVOKS-BULK - SLAG-KVOKS-DAG                         
330100     MOVE WS-BALANCE         TO MOD-BALANCE (IX-DC)                       
330200     COMPUTE WS-ORDERED ROUNDED = SLAG-KVBEART                            
330300                                + SLAG-KVAKS-PAV                          
330400                                + SLAG-KVAKS-SDC                          
330500     MOVE WS-ORDERED         TO MOD-IN-TRANS (IX-DC)                      
330600                                                                          
330700     MOVE +1                 TO IX                                        
330800     MOVE NEJ                TO SW-SEASON                                 
330900     PERFORM UNTIL IX > +12                                               
331000     OR SW-SEASON-JA                                                      
331100       IF SLAG-RESEASON (IX) NOT = +1.00                                  
331200         MOVE JA             TO SW-SEASON                                 
331300       END-IF                                                             
331400       ADD +1                TO IX                                        
331500     END-PERFORM                                                          
331600     IF SW-SEASON = JA                                                    
331700       MOVE 'Y'              TO MOD-SEASON (IX-DC)                        
331800     ELSE                                                                 
331900       MOVE 'N'              TO MOD-SEASON (IX-DC)                        
332000     END-IF                                                               
332100                                                                          
332200     IF SLAG-FLPB-FLYTT = JA                                              
332300       MOVE 'REPL'           TO WS-TEMFSINF-DC (IX-DC)                    
332400     END-IF                                                               
332500                                                                          
332600     IF WS-REF-KDREFTXT (IX-DC) > ZERO                                    
332700       MOVE +1               TO IX                                        
332800       PERFORM UNTIL IX > REF-TEXT-TABMAX                                 
332900       OR WS-REF-KDREFTXT (IX-DC) = REF-TEXT-KDREFTEXT (IX)               
333000         ADD +1              TO IX                                        
333100       END-PERFORM                                                        
333200       IF IX > REF-TEXT-TABMAX                                            
333300         CONTINUE                                                         
333400       ELSE                                                               
333500         IF REF-TEXT-KDREFTEXT (IX) = 70                                  
333600           CONTINUE                                                       
333700         ELSE                                                             
333800           MOVE REF-TEXT (IX)  TO WS-TEMFSINF-DC (IX-DC)                  
333900         END-IF                                                           
334000       END-IF                                                             
334100     END-IF                                                               
334200     .                                                                    
334300     EJECT                                                                
334400                                                                          
334500 FD-HAEMTA-KVPB SECTION.                                                  
334600     MOVE 'FD-HAEMTA-KVPB              '                                  
334700                             TO WS-PGM-SEKTION                            
334800                                                                          
334900     COMPUTE WS-KVPB ROUNDED =                                            
335000               (CLAG-KVPB-SATS + CLAG-KVPB-SEP + CLAG-KVPB-TPO)           
335100                                                                          
335200     PERFORM IMS-GN-WDB601                                                
335300     PERFORM UNTIL SEGMENT-SAKNAS                                         
335400       IF DCS-CDC OR DCS-DDC                                              
335500          CONTINUE                                                        
335600       ELSE                                                               
335700          MOVE DCS-IDDC         TO W-IDDC                                 
335800          PERFORM IMS-GU-WDK711                                           
335900          IF SEGMENT-FINNS                                                
336000*  ----  SLAG-KVPB ÄR PROGNOS FÖR EN MÅNADSPERIOD                         
336100             ADD SLAG-KVPB-REF   TO WS-KVPB                               
336200          END-IF                                                          
336300       END-IF                                                             
336400       PERFORM IMS-GN-WDB601                                              
336500     END-PERFORM                                                          
336600     .                                                                    
336700     EJECT                                                                
336800 FE-HAEMTA-KVPB-REF SECTION.                                              
336900     MOVE 'FE-HAEMTA-KVPB-REF          '                                  
337000                             TO WS-PGM-SEKTION                            
337100                                                                          
337200     MOVE SLAG-KVPB-REF      TO WS-RED-KVPB-REF-NUM                       
337300                                WS-KVPB-REF (IX-DC)                       
337400     MOVE MFS-ADD-LAES-IN-FAELT                                           
337500                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
337600                                                                          
337700     IF SLAG-KVPBREOI > ZERO                                              
337800       ADD SLAG-KVPBREOI     TO WS-RED-KVPB-REF-NUM                       
337900       MOVE MFS-CLOSE-FIELD  TO MOD-KVPB-REF-ATTR (IX-DC)                 
338000     ELSE                                                                 
338100       IF WS-IDDC-MED (IX-DC) > SPACES                                    
338200          MOVE MFS-CLOSE-FIELD                                            
338300                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
338400       END-IF                                                             
338500     END-IF                                                               
338600     .                                                                    
338700     EJECT                                                                
338800 FF-CHECK-ART-SOURCE SECTION.                                             
338900                                                                          
339000*  ----  CHECK MARKET WHERE PART IS SOURCED                               
339100*  ----  FOR CHECKING SOURCE MARKET CALL W271UTIL WITH KDCAL 001          
339200                                                                          
339300     INITIALIZE  UTIL-W271UTIL                                            
339400     MOVE 001                   TO UTIL-KDCALL                            
339500     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
339600                                                                          
339700     CALL W271UTIL USING UTIL-W271UTIL                                    
339800                         UTIL-WDK6-PCB                                    
339900                         UTIL-WDK7-PCB                                    
340000                         UTIL-WDB6-PCB                                    
340100                                                                          
340200     IF UTIL-KDSVAR-OK                                                    
340300        MOVE UTIL-TEXT          TO WS-TEMFSINF-SOURCE                     
340400     END-IF                                                               
340500     .                                                                    
340600     EJECT                                                                
340700 H-UPD-WDK6-WDK7 SECTION.                                                 
340800     MOVE 'H-UPD-DL1-DB2               '                                  
340900                             TO WS-PGM-SEKTION                            
341000                                                                          
341100     PERFORM IMS-GU-WDK611                                                
341200     MOVE +1                 TO IX-DC                                     
341300     MOVE NEJ                TO SW-KVPB-PLAN                              
341400*                                                                         
341500     MOVE WS-DC-NR (1)       TO W-IDDC                                    
341600                                WS-IDDC                                   
341700     PERFORM IMS-GHU-WDK711                                               
341800     IF SEGMENT-FINNS                                                     
341900       PERFORM HA-UPPDATERA-WDK7-WDL7                                     
342000       PERFORM IMS-REPL-WDK711                                            
342100     ELSE                                                                 
342200       IF WS-KVPB-REF (IX-DC) > ZERO                                      
342300         PERFORM HB-NY-WDK7                                               
342400         PERFORM IMS-GHU-WDK711                                           
342500         IF SEGMENT-FINNS                                                 
342600           PERFORM HA-UPPDATERA-WDK7-WDL7                                 
342700           PERFORM IMS-REPL-WDK711                                        
342800         END-IF                                                           
342900       END-IF                                                             
343000     END-IF                                                               
343100***  CALL SECTION BELOW TO UPDATE REFILLING PT & QTY TO WDK7              
343200*                                                                         
343300     PERFORM HD-UPD-REFL1-OUTPUT                                          
343400                                                                          
343500*                                                                         
343600     MOVE +2                 TO IX-DC                                     
343700     MOVE WS-DC-NR (2)       TO W-IDDC                                    
343800                                WS-IDDC                                   
343900     PERFORM IMS-GHU-WDK711                                               
344000     IF SEGMENT-FINNS                                                     
344100       PERFORM HA-UPPDATERA-WDK7-WDL7                                     
344200       PERFORM IMS-REPL-WDK711                                            
344300     ELSE                                                                 
344400       IF WS-KVPB-REF (IX-DC) > ZERO                                      
344500         PERFORM HB-NY-WDK7                                               
344600         PERFORM IMS-GHU-WDK711                                           
344700         IF SEGMENT-FINNS                                                 
344800           PERFORM HA-UPPDATERA-WDK7-WDL7                                 
344900           PERFORM IMS-REPL-WDK711                                        
345000         END-IF                                                           
345100       END-IF                                                             
345200     END-IF                                                               
345300     PERFORM HD-UPD-REFL1-OUTPUT                                          
345400                                                                          
345500*                                                                         
345600     MOVE +3                 TO IX-DC                                     
345700     MOVE WS-DC-NR (3)       TO W-IDDC                                    
345800                                WS-IDDC                                   
345900     PERFORM IMS-GHU-WDK711                                               
346000     IF SEGMENT-FINNS                                                     
346100       PERFORM HA-UPPDATERA-WDK7-WDL7                                     
346200       PERFORM IMS-REPL-WDK711                                            
346300     ELSE                                                                 
346400       IF WS-KVPB-REF (IX-DC) > ZERO                                      
346500         PERFORM HB-NY-WDK7                                               
346600         PERFORM IMS-GHU-WDK711                                           
346700         IF SEGMENT-FINNS                                                 
346800           PERFORM HA-UPPDATERA-WDK7-WDL7                                 
346900           PERFORM IMS-REPL-WDK711                                        
347000         END-IF                                                           
347100       END-IF                                                             
347200     END-IF                                                               
347300     PERFORM HD-UPD-REFL1-OUTPUT                                          
347400                                                                          
347500*                                                                         
347600     MOVE +4                 TO IX-DC                                     
347700     MOVE WS-DC-NR (4)       TO W-IDDC                                    
347800                                WS-IDDC                                   
347900     PERFORM IMS-GHU-WDK711                                               
348000     IF SEGMENT-FINNS                                                     
348100       PERFORM HA-UPPDATERA-WDK7-WDL7                                     
348200       PERFORM IMS-REPL-WDK711                                            
348300     ELSE                                                                 
348400       IF WS-KVPB-REF (IX-DC) > ZERO                                      
348500         PERFORM HB-NY-WDK7                                               
348600         PERFORM IMS-GHU-WDK711                                           
348700         IF SEGMENT-FINNS                                                 
348800           PERFORM HA-UPPDATERA-WDK7-WDL7                                 
348900           PERFORM IMS-REPL-WDK711                                        
349000         END-IF                                                           
349100       END-IF                                                             
349200     END-IF                                                               
349300     PERFORM HD-UPD-REFL1-OUTPUT                                          
349400                                                                          
349500*                                                                         
349600     MOVE +5                 TO IX-DC                                     
349700     MOVE WS-DC-NR (5)       TO W-IDDC                                    
349800                                WS-IDDC                                   
349900     PERFORM IMS-GHU-WDK711                                               
350000     IF SEGMENT-FINNS                                                     
350100       PERFORM HA-UPPDATERA-WDK7-WDL7                                     
350200       PERFORM IMS-REPL-WDK711                                            
350300     ELSE                                                                 
350400       IF WS-KVPB-REF (IX-DC) > ZERO                                      
350500         PERFORM HB-NY-WDK7                                               
350600         PERFORM IMS-GHU-WDK711                                           
350700         IF SEGMENT-FINNS                                                 
350800           PERFORM HA-UPPDATERA-WDK7-WDL7                                 
350900           PERFORM IMS-REPL-WDK711                                        
351000         END-IF                                                           
351100       END-IF                                                             
351200     END-IF                                                               
351300     PERFORM HD-UPD-REFL1-OUTPUT                                          
351400                                                                          
351500*                                                                         
351600     MOVE +6                 TO IX-DC                                     
351700     MOVE WS-DC-NR (6)       TO W-IDDC                                    
351800                                WS-IDDC                                   
351900     PERFORM IMS-GHU-WDK711                                               
352000     IF SEGMENT-FINNS                                                     
352100       PERFORM HA-UPPDATERA-WDK7-WDL7                                     
352200       PERFORM IMS-REPL-WDK711                                            
352300     ELSE                                                                 
352400       IF WS-KVPB-REF (IX-DC) > ZERO                                      
352500         PERFORM HB-NY-WDK7                                               
352600         PERFORM IMS-GHU-WDK711                                           
352700         IF SEGMENT-FINNS                                                 
352800           PERFORM HA-UPPDATERA-WDK7-WDL7                                 
352900           PERFORM IMS-REPL-WDK711                                        
353000         END-IF                                                           
353100       END-IF                                                             
353200     END-IF                                                               
353300     PERFORM HD-UPD-REFL1-OUTPUT                                          
353400*                                                                         
353500     IF KVPB-PLAN-UPD-JA                                                  
353600        PERFORM HC-RECALCULATE-PBPLAN                                     
353700     END-IF                                                               
353800                                                                          
353900     PERFORM DB2-SELECT-TP5COMM                                           
354000     IF LINES-FOUND                                                       
354100       MOVE MID-COMMENT (1)                                               
354200                             TO TP5COMM-TEARTNOT (1:20)                   
354300       MOVE MID-COMMENT (2)                                               
354400                             TO TP5COMM-TEARTNOT (21:20)                  
354500       PERFORM DB2-UPDATE-TP5COMM                                         
354600     ELSE                                                                 
354700       MOVE WS-IDLOPNR-DC    TO TP5COMM-IDLOPNR-DC                        
354800       MOVE W-IDARTNR        TO TP5COMM-IDARTNR                           
354900       MOVE MID-COMMENT (1)                                               
355000                             TO TP5COMM-TEARTNOT (1:20)                   
355100       MOVE MID-COMMENT (2)                                               
355200                             TO TP5COMM-TEARTNOT (21:20)                  
355300       PERFORM DB2-INSERT-TP5COMM                                         
355400     END-IF                                                               
355500     .                                                                    
355600     EJECT                                                                
355700 HA-UPPDATERA-WDK7-WDL7 SECTION.                                          
355800     MOVE 'HA-UPPDATERA-WDK7-WDL7      '                                  
355900                             TO WS-PGM-SEKTION                            
356000                                                                          
356100     IF WS-KVPB-REF (IX-DC) > SLAG-KVPB-REF                               
356200       MOVE NEJ           TO SLAG-FLREFNYO                                
356300     END-IF                                                               
356400                                                                          
356500* --                                                                      
356600     IF MID-FLREFBEO (IX-DC) = ALL '+' OR SPACE                           
356700        CONTINUE                                                          
356800     ELSE                                                                 
356900        IF MID-FLREFBEO (IX-DC) = YES                                     
357000          MOVE JA            TO SLAG-FLREFBEO                             
357100        ELSE                                                              
357200          MOVE MID-FLREFBEO (IX-DC)                                       
357300                                TO SLAG-FLREFBEO                          
357400        END-IF                                                            
357500     END-IF                                                               
357600     IF WS-KVPB-REF (IX-DC) = SLAG-KVPB-REF                               
357700       CONTINUE                                                           
357800     ELSE                                                                 
357900       IF SLAG-KVPBREOI = ZERO                                            
358000         MOVE DAGENS-DATUM   TO SLAG-TIREFMPB                             
358100         MOVE JA             TO SW-KVPB-SEP                               
358200       END-IF                                                             
358300     END-IF                                                               
358400                                                                          
358500* IF KVPBREOI > 0, KVPB-REF ON THE SCREEN WILL BE NON-EDITABLE.           
358600     IF SLAG-KVPBREOI > ZERO                                              
358700        CONTINUE                                                          
358800     ELSE                                                                 
358900        MOVE WS-KVPB-REF (IX-DC)                                          
359000                             TO SLAG-KVPB-REF                             
359100                                SLAG-KVPB-HIST                            
359200     END-IF                                                               
359300                                                                          
359400     IF SW-KVPB-SEP-JA                                                    
359500       IF SLAG-IDDC-REF = WS-CDC-11                                       
359600*****   IF REFILLED FROM CDC AND WE CHANGE FORECAST WE NEED TO            
359700*****   UDPATE CREF-KVPB-PLAN IF PART IS REFILLED TO CDC FROM             
359800*****   ANOTHER DC                                                        
359900         IF CLAG-IDDC-REF NOT = SPACE                                     
360000           MOVE JA          TO SW-KVPB-PLAN                               
360100         END-IF                                                           
360200       END-IF                                                             
360300     END-IF                                                               
360400                                                                          
360500     .                                                                    
360600     EJECT                                                                
360700                                                                          
360800 HC-RECALCULATE-PBPLAN SECTION.                                           
360900                                                                          
361000*--- THIS SECTION CALLS UTILITY PROGRAM W272UTUP TO UPDATE                
361100*--- KVPB-PLAN IN WDK6. ITS MANDATORY TO CALL THE UTILITY                 
361200*--- USING CORRECT KDCALL VALUE.                                          
361300                                                                          
361400     INITIALIZE W272-UTUP-W272UTUP                                        
361500     MOVE 1                     TO W272-UTUP-KDCALL                       
361600     MOVE W-IDARTNR             TO W272-UTUP-IDARTNR                      
361700     MOVE W-IDDC                TO W272-UTUP-IDDC                         
361800     IF CLAG-IDDC-REF = SPACE                                             
361900       CALL FELLOG                                                        
362000     ELSE                                                                 
362100       MOVE CLAG-IDDC-REF       TO W272-UTUP-IDDC-REF                     
362200     END-IF                                                               
362300                                                                          
362400     CALL W272UTUP USING W272-UTUP-W272UTUP                               
362500                         U2-WDK6-PCB                                      
362600                         U2-WDB6-PCB                                      
362700                         U2-PBTO-W222-WDK6-PCB                            
362800                         U2-PBTO-W222-WDK7-PCB                            
362900                         U2-PBTO-W222-ARTM-PCB                            
363000                         U2-PBTO-W222-REFL1-2501-PCB                      
363100                         U2-PBTO-W222-REFL1-WDB6R-PCB                     
363200                         U2-PBTO-W222-REFL1-WDK7R-PCB                     
363300                         U2-PBTO-W222-WDB6-PCB                            
363400                         U2-PBTO-W222-WDD7-PCB                            
363500                         U2-PBTO-W222-WDK7E-PCB                           
363600                         U2-PBTO-W222-REFL1-UTIL-K6-PCB                   
363700                         U2-PBTO-W222-REFL1-UTIL-K7-PCB                   
363800                         U2-PBTO-W222-REFL1-UTIL-B6-PCB                   
363900                         U2-PBTO-W222-UTUP1-WDK7-PCB                      
364000                         U2-PBTO-W222-UTUP1-WDB6-PCB                      
364100                         U2-PBTO-W222-UTUP1-UTIL-K6-PCB                   
364200                         U2-PBTO-W222-UTUP1-UTIL-K7-PCB                   
364300                         U2-PBTO-W222-UTUP1-UTIL-B6-PCB                   
364400                         U2-REFL2-2501-PCB                                
364500                         U2-REFL2-WDB6-PCB                                
364600                         U2-REFL2-UTIL-WDK6-PCB                           
364700                         U2-REFL2-UTIL-WDK7-PCB                           
364800                         U2-REFL2-UTIL-WDB6-PCB                           
364900                         U2-W222-WDK6-PCB                                 
365000                         U2-W222-WDK7-PCB                                 
365100                         U2-W222-ARTM-PCB                                 
365200                         U2-W222-2501-PCB                                 
365300                         U2-W222-WDB6R-PCB                                
365400                         U2-W222-WDK7R-PCB                                
365500                         U2-W222-WDB6-PCB                                 
365600                         U2-W222-WDD7-PCB                                 
365700                         U2-W222-WDK7E-PCB                                
365800                         U2-W222-UTIL-WDK6-PCB                            
365900                         U2-W222-UTIL-WDK7-PCB                            
366000                         U2-W222-UTIL-WDB6-PCB                            
366100                         U2-W222-UTUP1-WDK7-PCB                           
366200                         U2-W222-UTUP1-WDB6-PCB                           
366300                         U2-W222-UTUP1-UTIL-WDK6-PCB                      
366400                         U2-W222-UTUP1-UTIL-WDK7-PCB                      
366500                         U2-W222-UTUP1-UTIL-WDB6-PCB.                     
366600                                                                          
366700     IF W272-UTUP-KDSVAR-OK                                               
366800        CONTINUE                                                          
366900     ELSE                                                                 
367000        DISPLAY 'W272UTUP-ERROR :' W272-UTUP-TEXT                         
367100        CALL FELLOG                                                       
367200     END-IF                                                               
367300     .                                                                    
367400     EJECT                                                                
367500                                                                          
367600 HB-NY-WDK7 SECTION.                                                      
367700                                                                          
367800     MOVE ALL '+'      TO WDK7-W005WDK7                                   
367900     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
368000     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
368100     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
368200                          WDK7-IDDC                                       
368300     MOVE AKTIV        TO WDK7-KDREFSTA                                   
368400     IF MID-FLREFBEO (IX-DC) = ALL '+' OR SPACE                           
368500        CONTINUE                                                          
368600     ELSE                                                                 
368700       IF MID-FLREFBEO (IX-DC) = YES                                      
368800         MOVE JA       TO WDK7-FLREFBEO                                   
368900       ELSE                                                               
369000         MOVE MID-FLREFBEO (IX-DC)                                        
369100                         TO WDK7-FLREFBEO                                 
369200       END-IF                                                             
369300     END-IF                                                               
369400     MOVE DAGENS-DATUM TO WDK7-TIREFMPB                                   
369500                                                                          
369600     MOVE WS-KVPB-REF(IX-DC) TO WDK7-KVPB-REF                             
369700                                WDK7-KVPB-HIST                            
369800                                                                          
369900     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB WDK7I-PCB        
370000     IF WS-KVPB-REF(IX-DC) > ZERO                                         
370100       IF CLAG-IDDC-REF NOT = SPACE                                       
370200         MOVE JA            TO SW-KVPB-PLAN                               
370300       END-IF                                                             
370400     END-IF                                                               
370500     PERFORM HBA-NYA-DATA-TILL-MOD                                        
370600     .                                                                    
370700     EJECT                                                                
370800                                                                          
370900 HBA-NYA-DATA-TILL-MOD   SECTION.                                         
371000                                                                          
371100     PERFORM IMS-GU-WDK711                                                
371200     IF SEGMENT-FINNS                                                     
371300        MOVE SLAG-IDDC-REF TO WS-IDDC-FROM  (IX-DC)                       
371400                              MOD-IDDC-FROM (IX-DC)                       
371500        MOVE SPACE         TO WS-IDDC-MED   (IX-DC)                       
371600     END-IF                                                               
371700                                                                          
371800     MOVE ZERO             TO MOD-KVOI-RULL (1, IX-DC)                    
371900                              MOD-KVOI-RULL (2, IX-DC)                    
372000                              MOD-KVOI-RULL (3, IX-DC)                    
372100                              MOD-KVOI-RULL (4, IX-DC)                    
372200                              MOD-KVOI-RULL (5, IX-DC)                    
372300     .                                                                    
372400     EJECT                                                                
372500                                                                          
372600 HD-UPD-REFL1-OUTPUT SECTION.                                             
372700                                                                          
372800***  THIS SECTION CALLS W271REFL TO GET THE CALULATED                     
372900***  REFILLING POINT AND REFILLING QUANTITY                               
373000***  CAN BE USED FOR UPDATE ON WDK7 OR JUST SIMULATION                    
373100***                                                                       
373200     PERFORM IMS-GHU-WDK711                                               
373300     IF SEGMENT-FINNS                                                     
373400***     FOR UPDATES SIMULATION FLAG FLSIM SHOULD BE NO                    
373500        MOVE NEJ             TO WS-FLSIM                                  
373600        PERFORM S90-CALL-W271REFL                                         
373700*                                                                         
373800        MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                               
373900        MOVE DAGENS-DATUM    TO TMP2-YYMMDD                               
374000        PERFORM WY2000P1                                                  
374100        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
374200                                                                          
374300*                                                                         
374400*--- INGEN UPPDATERING AV KVREFPKT PGA MANUELLT DATUM ÄR SATT             
374500          CONTINUE                                                        
374600        ELSE                                                              
374700          MOVE W271-REFL-KVREFPKT TO SLAG-KVREFPKT                        
374800        END-IF                                                            
374900                                                                          
375000        MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                               
375100        MOVE DAGENS-DATUM    TO TMP2-YYMMDD                               
375200        PERFORM WY2000P1                                                  
375300        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
375400*                                                                         
375500*--- INGEN UPPDATERING AV KVREFBER PGA MANUELLT DATUM ÄR SATT             
375600          CONTINUE                                                        
375700        ELSE                                                              
375800          MOVE W271-REFL-KVREFBER TO SLAG-KVREFBER                        
375900        END-IF                                                            
376000                                                                          
376100        MOVE W271-REFL-KVREFOVL  TO SLAG-KVREFOVL                         
376200                                                                          
376300        IF  SLAG-KDREFSTA        = PASSIV                                 
376400        AND SLAG-KVPB-REF        > ZERO                                   
376500          MOVE AKTIV             TO SLAG-KDREFSTA                         
376600          MOVE DAGENS-DATUM      TO SLAG-TIREFSTA                         
376700        END-IF                                                            
376800*                                                                         
376900        PERFORM IMS-REPL-WDK711                                           
377000     END-IF                                                               
377100     .                                                                    
377200     EJECT                                                                
377300                                                                          
377400 I-KOLLA-INPUT SECTION.                                                   
377500     MOVE 'I-KOLLA-INPUT               '                                  
377600                             TO WS-PGM-SEKTION                            
377700                                                                          
377800     MOVE +1                 TO IX-DC                                     
377900     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
378000       MOVE ZERO TO WS-PURCHQTY-SUM (IX-DC)                               
378100       ADD +1               TO IX-DC                                      
378200     END-PERFORM                                                          
378300                                                                          
378400     PERFORM IMS-GU-WDK611                                                
378500     IF SEGMENT-FINNS                                                     
378600       MOVE NEJ              TO SW-KTRL-ERS                               
378700                                ERSATT-SW                                 
378800       MOVE +1               TO IX-DC                                     
378900       PERFORM UNTIL IX-DC > WS-DC-MAX                                    
379000*      OR SW-KTRL-ERS-JA                                                  
379100         IF MID-PURCHQTY (IX-DC) = ALL '+'                                
379200           CONTINUE                                                       
379300         ELSE                                                             
379400           INSPECT MID-PURCHQTY (IX-DC)                                   
379500                          REPLACING LEADING SPACE BY ZERO                 
379600           IF  MID-PURCHQTY (IX-DC) NUMERIC                               
379700           AND MID-PURCHQTY (IX-DC) > ZERO                                
379800             MOVE JA         TO SW-KTRL-ERS                               
379900             EVALUATE TRUE                                                
380000               WHEN MID-IDDC-FROM (IX-DC) = WS-DC-NR (1)                  
380100                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
380200                 ADD W-PURCHQTY (IX-DC) TO WS-PURCHQTY-SUM (1)            
380300               WHEN MID-IDDC-FROM (IX-DC) = WS-DC-NR (2)                  
380400                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
380500                 ADD W-PURCHQTY (IX-DC) TO WS-PURCHQTY-SUM (2)            
380600               WHEN MID-IDDC-FROM (IX-DC) = WS-DC-NR (3)                  
380700                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
380800                 ADD W-PURCHQTY (IX-DC) TO WS-PURCHQTY-SUM (3)            
380900               WHEN MID-IDDC-FROM (IX-DC) = WS-DC-NR (4)                  
381000                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
381100                 ADD W-PURCHQTY (IX-DC) TO WS-PURCHQTY-SUM (4)            
381200               WHEN MID-IDDC-FROM (IX-DC) = WS-DC-NR (5)                  
381300                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
381400                 ADD W-PURCHQTY (IX-DC) TO WS-PURCHQTY-SUM (5)            
381500               WHEN MID-IDDC-FROM (IX-DC) = WS-DC-NR (6)                  
381600                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
381700                 ADD W-PURCHQTY (IX-DC) TO WS-PURCHQTY-SUM (6)            
381800             END-EVALUATE                                                 
381900           END-IF                                                         
382000         END-IF                                                           
382100         ADD +1              TO IX-DC                                     
382200       END-PERFORM                                                        
382300       IF SW-KTRL-ERS-JA                                                  
382400        IF CLAG-KDERS               < 10                                  
382500          IF CLAG-PRARTSTD = ZERO                                         
382600            MOVE PRIS-SAKNAS                                              
382700                           TO MED-IDMFSFEL                                
382800            CALL WMEDKONV USING MED-WMEDAREA                              
382900            MOVE MED-TEMFSFEL                                             
383000                           TO MOD-TEMFSFEL                                
383100            MOVE NEJ          TO INDATA-SW                                
383200          END-IF                                                          
383300        ELSE                                                              
383400          MOVE JA               TO ERSATT-SW                              
383500        END-IF                                                            
383600       END-IF                                                             
383700     ELSE                                                                 
383800       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
383900       CALL WMEDKONV USING MED-WMEDAREA                                   
384000       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
384100       MOVE NEJ              TO INDATA-SW                                 
384200     END-IF                                                               
384300                                                                          
384400*    FOR INTERNAL REFILL CHINA FOR SUPER SEEDED PART                      
384500     IF ERSATT-PART                                                       
384600        PERFORM IA-CHECK-KDERS-CHINA                                      
384700     END-IF                                                               
384800*                                                                         
384900     MOVE +1                 TO IX-DC                                     
385000     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
385100       IF MID-KVPB-REF (IX-DC) = ALL '+'                                  
385200         MOVE MFS-ADD-LAES-IN-FAELT                                       
385300                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
385400       ELSE                                                               
385500         MOVE MID-KVPB-REF (IX-DC)                                        
385600                             TO DEC-IDFRIDATA                             
385700                                MOD-KVPB-REF (IX-DC)                      
385800         MOVE 6              TO DEC-KVHELTAL                              
385900         MOVE 1              TO DEC-KVDECIMAL                             
386000         CALL WDECEDIT USING DEC-WDECAREA                                 
386100         IF DEC-KDSVAR-OK                                                 
386200           MOVE MFS-ADD-LAES-IN-FAELT                                     
386300                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
386400           MOVE DEC-IDEDITDATA                                            
386500                             TO WS-RED-KVPB-REF                           
386600                                WS-KVPB-REF (IX-DC)                       
386700           MOVE WS-RED-KVPB-REF                                           
386800                             TO MOD-KVPB-REF (IX-DC)                      
386900         ELSE                                                             
387000           MOVE MED-3        TO MOD-TEMFSFEL                              
387100           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
387200                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
387300           MOVE NEJ          TO INDATA-SW                                 
387400           MOVE MID-KVPB-REF (IX-DC)                                      
387500                             TO MOD-KVPB-REF (IX-DC)                      
387600         END-IF                                                           
387700       END-IF                                                             
387800                                                                          
387900       IF WS-KVPB-REF (IX-DC) > ZERO                                      
388000         MOVE WS-DC-NR (IX-DC)                                            
388100                             TO W-IDDC                                    
388200                                WS-IDDC                                   
388300         PERFORM IMS-GU-WDK711                                            
388400         IF SEGMENT-FINNS                                                 
388500           MOVE MFS-ADD-LAES-IN-FAELT                                     
388600                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
388700         ELSE                                                             
388800           IF NDC-CN                                                      
388900              IF CLAG-PRARTSTD  = ZERO                                    
389000                 MOVE PRIS-SAKNAS    TO MED-IDMFSFEL                      
389100                 CALL WMEDKONV    USING MED-WMEDAREA                      
389200                 MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                      
389300                 MOVE NEJ            TO INDATA-SW                         
389400              END-IF                                                      
389500           END-IF                                                         
389600         END-IF                                                           
389700       END-IF                                                             
389800                                                                          
389900       IF MID-PURCHQTY (IX-DC) = ALL '+'                                  
390000         MOVE ZERO           TO WS-RED-PURCHQTY                           
390100                                WS-PURCHQTY (IX-DC)                       
390200         MOVE WS-RED-PURCHQTY                                             
390300                             TO MOD-PURCHQTY (IX-DC)                      
390400       ELSE                                                               
390500         INSPECT MID-PURCHQTY (IX-DC)                                     
390600                          REPLACING LEADING SPACE BY ZERO                 
390700         IF MID-PURCHQTY (IX-DC) NUMERIC                                  
390800           MOVE MID-PURCHQTY (IX-DC)                                      
390900                             TO WS-RED-PURCHQTY                           
391000                                WS-PURCHQTY (IX-DC)                       
391100           MOVE WS-RED-PURCHQTY                                           
391200                             TO MOD-PURCHQTY (IX-DC)                      
391300                                                                          
391400           IF MID-PURCHQTY (IX-DC) > ZERO                                 
391500             MOVE WS-DC-NR (IX-DC)                                        
391600                               TO W-IDDC                                  
391700                                  WS-IDDC                                 
391800             PERFORM IMS-GU-WDK711                                        
391900             IF SEGMENT-FINNS                                             
391910               IF MFS-UPDATE AND (WS-IDREFTYP = 'A' OR                    
391920                                  WS-IDREFTYP = 'C')                      
391921                  IF SLAG-FLFLYG = 'S'                                    
391922                    MOVE MED-17 TO MOD-TEMFSFEL                           
391923                    MOVE NEJ  TO INDATA-SW                                
391924                    MOVE MFS-ROER-EJ-FAELT                                
391925                                 TO MOD-PURCHQTY-ATTR (IX-DC)             
391930                  ELSE                                                    
391931                    PERFORM IB-CHECK-AIR-COST                             
391932                  END-IF                                                  
391940               END-IF                                                     
392000               IF DCS-IDDC NOT = WS-IDDC-FROM (IX-DC)                     
392100                  MOVE WS-IDDC-FROM (IX-DC) TO W-IDDC-B6                  
392200                  PERFORM IMS-GU-WDB601                                   
392300                  IF SEGMENT-SAKNAS                                       
392400                     MOVE SPACE TO DCS-KDDC                               
392500                  END-IF                                                  
392600               END-IF                                                     
392700               IF ((WS-IDREFTYP = 'A'                                     
392800               OR   WS-IDREFTYP = 'C'                                     
392900               OR   WS-IDREFTYP = 'B'                                     
393000               OR   WS-IDREFTYP = 'T')                                    
393100               AND (DCS-CDC OR DCS-NDC))                                  
393200               OR  (WS-IDREFTYP = 'L'                                     
393300               AND NOT (DCS-CDC OR DCS-NDC-CN OR DCS-USA))                
393400                 MOVE MFS-ADD-LAES-IN-FAELT                               
393500                               TO MOD-PURCHQTY-ATTR (IX-DC)               
393600               ELSE                                                       
393700                 MOVE MED-8  TO MOD-TEMFSFEL                              
393800                 MOVE NEJ    TO INDATA-SW                                 
393900                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
394000                               TO MOD-PURCHQTY-ATTR (IX-DC)               
394100               END-IF                                                     
394200*-----                                                                    
394300*-----     FOR SUPERSEDED PARTS ALLOW INTERNAL REFILL IN CHINA            
394400*-----                                                                    
394500               IF  NDC AND SLAG-IDDC-REF NOT = '11'                       
394600               AND ERSATT-PART                                            
394700                 IF CLAG-KDERS NOT = +52                                  
394800                     PERFORM S5-CHK-KVDISP-SEND-DC                        
394900                     IF WS-KVDISP-SEND-DC > 0                             
395000                         CONTINUE                                         
395100                     ELSE                                                 
395200                         MOVE ARTIKEL-ERSATT                              
395300                                          TO MED-IDMFSFEL                 
395400                         CALL WMEDKONV USING MED-WMEDAREA                 
395500                         MOVE MED-TEMFSFEL                                
395600                                          TO MOD-TEMFSFEL                 
395700                         MOVE NEJ         TO INDATA-SW                    
395800                     END-IF                                               
395900                 ELSE                                                     
396000                     MOVE ARTIKEL-UTGANGEN                                
396100                                          TO MED-IDMFSFEL                 
396200                     CALL WMEDKONV USING MED-WMEDAREA                     
396300                     MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                 
396400                     MOVE NEJ             TO INDATA-SW                    
396500                 END-IF                                                   
396600               ELSE                                                       
396700                 IF ERSATT-PART                                           
396800                    MOVE NEJ           TO  INDATA-SW                      
396900                 END-IF                                                   
397000               END-IF                                                     
397100*                                                                         
397200               IF WS-IDREFTYP = 'L'                                       
397300*-----                                                                    
397400*-----   FÖR LOKALA LEVERANSER MÅSTE                                      
397500*-----   DET FINNAS ETT GODKÄNT PRIS                                      
397600*-----                                                                    
397700                 PERFORM IMS-GU-WDK601                                    
397800                 IF SEGMENT-FINNS                                         
397900                   COMPUTE W-DAPRLIST-21 =                                
398000                            99999999 - DAGENS-DATUM-SEKEL                 
398100                   MOVE SLAG-IDLEVNR   TO W-IDLEVNR-21                    
398200                   PERFORM IMS-GNP-WDK621                                 
398300                   PERFORM UNTIL SEGMENT-SAKNAS OR                        
398400                       PRL-KDSTATUS-PR = 1                                
398500                     PERFORM IMS-GNP-WDK621                               
398600                   END-PERFORM                                            
398700                 END-IF                                                   
398800***    LOKALA ORDER FÅR INTE GÖRAS FÖR KINA                               
398900                 IF DCS-CHINA                                             
399000                    MOVE MED-8  TO MOD-TEMFSFEL                           
399100                    MOVE NEJ    TO INDATA-SW                              
399200                 END-IF                                                   
399300                                                                          
399400                 IF SEGMENT-FINNS                                         
399500***    GODKÄNT PRIS ***************                                       
399600                   CONTINUE                                               
399700                 ELSE                                                     
399800                   MOVE NEJ  TO INDATA-SW                                 
399900                   MOVE MED-9 TO MOD-TEMFSFEL                             
400000                   MOVE MFS-ADD-LAES-IN-FAELT-HI                          
400100                               TO MOD-PURCHQTY-ATTR (IX-DC)               
400200                 END-IF                                                   
400300               ELSE                                                       
400400                 IF CLAG-KVQPACK-1 > ZERO                                 
400500***      KONTROLLERAR ATT KÖPET ÄR EN JÄMN                                
400600***      MULTIPEL AV Q1                                                   
400700                    DIVIDE WS-PURCHQTY (IX-DC)                            
400800                              BY CLAG-KVQPACK-1                           
400900                              GIVING WS-SLASK                             
401000                              REMAINDER WS-REST                           
401100                 END-IF                                                   
401200                 IF WS-REST > ZERO                                        
401300                    MOVE NEJ  tO INDATA-SW                                
401400                    MOVE MED-10                                           
401500                              TO MOD-TEMFSFEL                             
401600                    MOVE MFS-ADD-LAES-IN-FAELT-HI                         
401700                              TO MOD-PURCHQTY-ATTR (IX-DC)                
401800                 END-IF                                                   
401900               END-IF                                                     
402000             ELSE                                                         
402100               MOVE ARTIKEL-SAKNAS                                        
402200                             TO MED-IDMFSFEL                              
402300               CALL WMEDKONV USING MED-WMEDAREA                           
402400               MOVE MED-TEMFSFEL                                          
402500                             TO MOD-TEMFSFEL                              
402600               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
402700                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
402800               MOVE NEJ      TO INDATA-SW                                 
402900             END-IF                                                       
403000           ELSE                                                           
403100             MOVE MFS-ADD-LAES-IN-FAELT                                   
403200                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
403300           END-IF                                                         
403400         ELSE                                                             
403500           MOVE MED-4        TO MOD-TEMFSFEL                              
403600           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
403700                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
403800           MOVE MID-PURCHQTY (IX-DC)                                      
403900                             TO MOD-PURCHQTY (IX-DC)                      
404000           MOVE NEJ          TO INDATA-SW                                 
404100         END-IF                                                           
404200       END-IF                                                             
404300                                                                          
404400       IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                        
404500           MID-IDDC-FROM (IX-DC) NOT = '11')                              
404600       AND MID-PURCHQTY (IX-DC) NUMERIC                                   
404700       AND MID-PURCHQTY (IX-DC) > ZERO                                    
404800         IF WS-IDREFTYP = 'T'                                             
404900           MOVE MID-IDDC-FROM (IX-DC)                                     
405000                             TO WS-IDDC-SEND                              
405100                                MOD-IDDC-FROM (IX-DC)                     
405200                                WS-IDDC-FROM (IX-DC)                      
405300                                W-IDDC                                    
405400           MOVE WS-DC-NR (IX-DC)                                          
405500                             TO WS-IDDC-REC                               
405600                                                                          
405700           IF WS-IDDC-SEND = WS-CDC-11                                    
405800             MOVE MED-11     TO MOD-TEMFSFEL                              
405900             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
406000                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
406100             MOVE NEJ TO INDATA-SW                                        
406200           ELSE                                                           
406300             PERFORM DB2-SELECT-TP4TRAN                                   
406400             IF LINES-FOUND                                               
406500               MOVE TP4TRAN-IDDISTR  TO WS-IDDISTR(IX-DC)                 
406600               MOVE TP4TRAN-IDKUNDNR TO WS-IDKUNDNR(IX-DC)                
406700               MOVE MFS-NUM-FAELT-RAETT                                   
406800                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
406900               PERFORM IMS-GU-WDK711-TRANS                                
407000               IF SEGMENT-FINNS                                           
407100                COMPUTE WS-TRANSF-BALANCE ROUNDED =                       
407200                                     TRANS-SLAG-KVLS                      
407300                                   - TRANS-SLAG-KVROS-BULK                
407400                                   - TRANS-SLAG-KVROS-DAG                 
407500                                   - TRANS-SLAG-KVOKS-BULK                
407600                                   - TRANS-SLAG-KVOKS-DAG                 
407700                 IF ((MID-IDDC-FROM (IX-DC) = WS-DC-NR (1)                
407800                 AND WS-PURCHQTY-SUM (1) > WS-TRANSF-BALANCE))            
407900                 OR ((MID-IDDC-FROM (IX-DC) = WS-DC-NR (2)                
408000                 AND WS-PURCHQTY-SUM (2) > WS-TRANSF-BALANCE))            
408100                 OR ((MID-IDDC-FROM (IX-DC) = WS-DC-NR (3)                
408200                 AND WS-PURCHQTY-SUM (3) > WS-TRANSF-BALANCE))            
408300                 OR ((MID-IDDC-FROM (IX-DC) = WS-DC-NR (4)                
408400                 AND WS-PURCHQTY-SUM (4) > WS-TRANSF-BALANCE))            
408500                 OR ((MID-IDDC-FROM (IX-DC) = WS-DC-NR (5)                
408600                 AND WS-PURCHQTY-SUM (5) > WS-TRANSF-BALANCE))            
408700                 OR ((MID-IDDC-FROM (IX-DC) = WS-DC-NR (6)                
408800                 AND WS-PURCHQTY-SUM (6) > WS-TRANSF-BALANCE))            
408900                   MOVE MED-12 TO MOD-TEMFSFEL                            
409000                   MOVE NEJ TO INDATA-SW                                  
409100                   MOVE MFS-ADD-LAES-IN-FAELT-HI                          
409200                               TO MOD-PURCHQTY-ATTR (IX-DC)               
409300                 END-IF                                                   
409400               ELSE                                                       
409500                 MOVE MED-12   TO MOD-TEMFSFEL                            
409600                 MOVE NEJ TO INDATA-SW                                    
409700                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
409800                               TO MOD-PURCHQTY-ATTR (IX-DC)               
409900               END-IF                                                     
410000             ELSE                                                         
410100               MOVE MED-11   TO MOD-TEMFSFEL                              
410200               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
410300                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
410400               MOVE NEJ TO INDATA-SW                                      
410500             END-IF                                                       
410600           END-IF                                                         
410700         ELSE                                                             
410800           MOVE WS-DC-NR (IX-DC)  TO W-IDDC-B6                            
410900*          PERFORM IMS-GU-WDB601                                          
411000           IF MID-IDDC-FROM (IX-DC) = SLAG-IDDC-REF                       
411100             MOVE MFS-NUM-FAELT-RAETT                                     
411200                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
411300           ELSE                                                           
411400             MOVE MED-11     TO MOD-TEMFSFEL                              
411500             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
411600                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
411700             MOVE NEJ TO INDATA-SW                                        
411800           END-IF                                                         
411900         END-IF                                                           
412000       ELSE                                                               
412100***    KONTROLL NÄR MAN VILL NOLLA TRANSFERORDER                          
412200         MOVE MID-IDDC-FROM (IX-DC)   TO REFILL-WS-IDDC                   
412300         IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                      
412400             MID-IDDC-FROM (IX-DC) NOT = SPACE   AND                      
412500             NOT (REFILL-CDC-SE OR REFILL-NDC-CN))                        
412600         AND MID-PURCHQTY (IX-DC) NUMERIC                                 
412700         AND MID-PURCHQTY (IX-DC) = ZERO                                  
412800           IF WS-IDREFTYP = 'T'                                           
412900             MOVE MID-IDDC-FROM (IX-DC)                                   
413000                               TO WS-IDDC-SEND                            
413100                                  MOD-IDDC-FROM (IX-DC)                   
413200                                  WS-IDDC-FROM (IX-DC)                    
413300                                  W-IDDC                                  
413400*            MOVE WS-DC-NR (IX-DC)                                        
413500*                              TO WS-IDDC-REC                             
413600             MOVE WS-DC-NR (IX-DC)   TO W-IDDC-301                        
413700                                        WS-IDDC-REC                       
413800                                        W-IDDC                            
413900             MOVE 'T'          TO W-KDREFTYP                              
414000             IF WS-IDDC-SEND = WS-CDC-11                                  
414100               MOVE MED-11     TO MOD-TEMFSFEL                            
414200               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
414300                               TO MOD-IDDC-FROM-ATTR (IX-DC)              
414400               MOVE NEJ TO INDATA-SW                                      
414500             ELSE                                                         
414600               PERFORM DB2-SELECT-TP4TRAN                                 
414700               IF LINES-FOUND                                             
414800                 MOVE MFS-NUM-FAELT-RAETT                                 
414900                               TO MOD-IDDC-FROM-ATTR (IX-DC)              
415000                 MOVE TP4TRAN-IDDISTR TO W-IDDISTR                        
415100               ELSE                                                       
415200                 MOVE MED-11   TO MOD-TEMFSFEL                            
415300                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
415400                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
415500                 MOVE NEJ          TO INDATA-SW                           
415600               END-IF                                                     
415700               PERFORM IMS-GU-WDK711                                      
415800               IF SEGMENT-FINNS                                           
415900                 MOVE SLAG-IDPERSON-BUY  TO W-IDPERSON-BUY                
416000                 PERFORM IMS-GU-WDE3-TRAN                                 
416100               END-IF                                                     
416200                                                                          
416300               IF SEGMENT-FINNS                                           
416400                 CONTINUE                                                 
416500               ELSE                                                       
416600                 MOVE MED-12 TO MOD-TEMFSFEL                              
416700                 MOVE NEJ TO INDATA-SW                                    
416800                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
416900                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
417000                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
417100                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
417200               END-IF                                                     
417300             END-IF                                                       
417400           END-IF                                                         
417500         ELSE                                                             
417600           IF WS-IDREFTYP = 'T'                                           
417700           AND MID-IDDC-FROM (IX-DC) = SLAG-IDDC-REF                      
417800           AND MID-PURCHQTY (IX-DC) NOT = ZERO                            
417900              MOVE MED-13    TO MOD-TEMFSFEL                              
418000              MOVE NEJ TO INDATA-SW                                       
418100              MOVE MFS-ADD-LAES-IN-FAELT-HI                               
418200                          TO MOD-PURCHQTY-ATTR (IX-DC)                    
418300              MOVE MFS-ADD-LAES-IN-FAELT-HI                               
418400                                TO MOD-IDDC-FROM-ATTR (IX-DC)             
418500           END-IF                                                         
418600         END-IF                                                           
418700       END-IF                                                             
418800                                                                          
418900       IF WS-IDREFTYP = 'T'                                               
419000         IF ((MID-IDDC-FROM (1) = ALL '+' OR                              
419100              MID-IDDC-FROM (1) = SPACE)                                  
419200         AND WS-PURCHQTY (1) NOT = ZERO)                                  
419300         OR  ((MID-IDDC-FROM (2) = ALL '+' OR                             
419400              MID-IDDC-FROM (2) = SPACE)                                  
419500         AND WS-PURCHQTY (2) NOT = ZERO)                                  
419600         OR  ((MID-IDDC-FROM (3) = ALL '+' OR                             
419700              MID-IDDC-FROM (3) = SPACE)                                  
419800         AND WS-PURCHQTY (3) NOT = ZERO)                                  
419900         OR  ((MID-IDDC-FROM (4) = ALL '+' OR                             
420000              MID-IDDC-FROM (4) = SPACE)                                  
420100         AND WS-PURCHQTY (4) NOT = ZERO)                                  
420200         OR  ((MID-IDDC-FROM (5) = ALL '+' OR                             
420300              MID-IDDC-FROM (5) = SPACE)                                  
420400         AND WS-PURCHQTY (5) NOT = ZERO)                                  
420500         OR  ((MID-IDDC-FROM (6) = ALL '+' OR                             
420600              MID-IDDC-FROM (6) = SPACE)                                  
420700         AND WS-PURCHQTY (6) NOT = ZERO)                                  
420800           MOVE MED-14 TO MOD-TEMFSFEL                                    
420900           MOVE NEJ TO INDATA-SW                                          
421000           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
421100                       TO MOD-PURCHQTY-ATTR (IX-DC)                       
421200           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
421300                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
421400         END-IF                                                           
421500       END-IF                                                             
421600                                                                          
421700       IF MID-FLREFBEO (IX-DC) = ALL '+' OR SPACE                         
421800         MOVE MFS-RENSA-FAELT                                             
421900                             TO MOD-FLREFBEO (IX-DC)                      
422000       ELSE                                                               
422100         MOVE MID-FLREFBEO (IX-DC)                                        
422200                             TO MOD-FLREFBEO (IX-DC)                      
422300         MOVE WS-DC-NR (IX-DC)                                            
422400                           TO W-IDDC                                      
422500         PERFORM IMS-GU-WDK711                                            
422600         IF SEGMENT-FINNS                                                 
422700           MOVE SLAG-IDDC-REF      TO REFILL-WS-IDDC                      
422800           IF REFILL-CDC-SE                                               
422900           OR REFILL-NDC                                                  
423000*----                                                                     
423100*---- EJ LOKAL ARTIKEL                                                    
423200*----                                                                     
423300             IF MID-FLREFBEO (IX-DC) = JA                                 
423400             OR MID-FLREFBEO (IX-DC) = YES                                
423500             OR MID-FLREFBEO (IX-DC) = NEJ                                
423600             OR MID-FLREFBEO (IX-DC) = 'S'                                
423700               MOVE MFS-ADD-LAES-IN-FAELT                                 
423800                               TO MOD-FLREFBEO-ATTR (IX-DC)               
423900             ELSE                                                         
424000               MOVE MED-5    TO MOD-TEMFSFEL                              
424100               MOVE NEJ      TO INDATA-SW                                 
424200               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
424300                               TO MOD-FLREFBEO-ATTR (IX-DC)               
424400             END-IF                                                       
424500           ELSE                                                           
424600*----                                                                     
424700*---- LOKAL ARTIKEL                                                       
424800*----                                                                     
424900             IF MID-FLREFBEO (IX-DC) = NEJ                                
425000               MOVE MFS-ADD-LAES-IN-FAELT                                 
425100                               TO MOD-FLREFBEO-ATTR (IX-DC)               
425200             ELSE                                                         
425300               MOVE MED-5    TO MOD-TEMFSFEL                              
425400               MOVE NEJ      TO INDATA-SW                                 
425500               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
425600                               TO MOD-FLREFBEO-ATTR (IX-DC)               
425700             END-IF                                                       
425800           END-IF                                                         
425900*        ELSE                                                             
426000*----      INGEN KONTROLL SKA GÖRAS                                       
426100*----                                                                     
426200*          MOVE MED-5        TO MOD-TEMFSFEL                              
426300*          MOVE NEJ          TO INDATA-SW                                 
426400*          MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
426500*                          TO MOD-FLREFBEO-ATTR (IX-DC)                   
426600         END-IF                                                           
426700       END-IF                                                             
426800                                                                          
426900       ADD +1                TO IX-DC                                     
427000     END-PERFORM                                                          
427100                                                                          
427200     IF MID-COMMENT (1) = ALL '+'                                         
427300       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT (1)                           
427400                                MID-COMMENT (1)                           
427500     ELSE                                                                 
427600       MOVE MID-COMMENT (1)  TO MOD-COMMENT (1)                           
427700     END-IF                                                               
427800     MOVE MFS-ADD-LAES-IN-FAELT                                           
427900                             TO MOD-COMMENT-ATTR (1)                      
428000                                                                          
428100     IF MID-COMMENT (2) = ALL '+'                                         
428200       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT (2)                           
428300                                MID-COMMENT (2)                           
428400     ELSE                                                                 
428500       MOVE MID-COMMENT (2)  TO MOD-COMMENT (2)                           
428600     END-IF                                                               
428700     MOVE MFS-ADD-LAES-IN-FAELT                                           
428800                             TO MOD-COMMENT-ATTR (2)                      
428900     .                                                                    
429000     EJECT                                                                
429100 IA-CHECK-KDERS-CHINA SECTION.                                            
429200                                                                          
429300     MOVE 'CN'                  TO W-IDLAND                               
429400     PERFORM IMS-GU-WDK712                                                
429500     IF SEGMENT-FINNS                                                     
429600        MOVE LART-TIERSDAT-VIPS TO WS-TIERSDAT-VIPS                       
429700     END-IF                                                               
429800                                                                          
429900     MOVE  CLAG-KDERS           TO WS-KDERS                               
430000                                                                          
430100     .                                                                    
430200     EJECT                                                                
430210******************************************************************        
430211*1.GET WEIGHT/VOLUME OF THE PART IN WDK712 OR WDK611                      
430212*2.GET AIR FREIGHT COST FACTOR & AIRCOST FROM WDB616                      
430220******************************************************************        
430300 IB-CHECK-AIR-COST   SECTION.                                             
430400     MOVE 'IB-CHECK-AIR-COST           '                                  
430500                                TO WS-PGM-SEKTION                         
430501                                                                          
430502     MOVE CLAG-VKART            TO WS-VKART                               
430503     MOVE CLAG-VLARTNTO         TO WS-VLARTNTO                            
430504                                                                          
430510     IF SLAG-IDDC-REF NOT = SPACE                                         
430520       MOVE SLAG-IDDC-REF       TO W-IDDC-REF-B6                          
430530     ELSE                                                                 
430540       MOVE SLAG-IDDC           TO W-IDDC-REF-B6                          
430550     END-IF                                                               
430551                                                                          
430560     PERFORM IMS-GU-WDB601-REF                                            
430570     IF SEGMENT-FINNS                                                     
430580        MOVE B6-DCS-IDLANDX2   TO W-IDLAND                                
430591        PERFORM IMS-GU-WDK712                                             
430600        IF SEGMENT-FINNS                                                  
430601           IF LART-VKART          > ZERO                                  
430602             MOVE LART-VKART    TO WS-VKART                               
430605           END-IF                                                         
430606           IF LART-VLARTNTO       > ZERO                                  
430607             MOVE LART-VLARTNTO TO WS-VLARTNTO                            
430610           END-IF                                                         
430614        END-IF                                                            
430615     END-IF                                                               
430616*                                                                         
430617     MOVE ZEROES                TO WS-REAIRCO                             
430618                                   WS-PRFRAKT                             
430619                                   WS-AIR-COST-SEK                        
430620     MOVE W-IDDC                TO W-IDDC-B6-X                            
430621     MOVE SLAG-IDDC-REF         TO W-IDDC-B616-X                          
430622     PERFORM IMS-GU-WDB616                                                
430623     IF SEGMENT-FINNS                                                     
430624        MOVE B6-REF-REAIRCO        TO WS-REAIRCO                          
430625        MOVE B6-REF-PRFRAKT        TO WS-PRFRAKT                          
430626     END-IF                                                               
430627                                                                          
430628     COMPUTE WS-KR-VIKT  ROUNDED                                          
430629                                 = WS-VKART * WS-REAIRCO / 1000           
430630     COMPUTE WS-KR-VOLYM ROUNDED =                                        
430631             WS-VLARTNTO * WS-REAIRCO * 167 / 1000000                     
430632                                                                          
430633     COMPUTE WS-KR-VIKT-RED  ROUNDED = WS-KR-VIKT * 1                     
430634     COMPUTE WS-KR-VOLYM-RED ROUNDED = WS-KR-VOLYM * 1                    
430635                                                                          
430636     IF WS-KR-VIKT-RED > WS-KR-VOLYM-RED                                  
430637        MOVE WS-KR-VIKT-RED     TO WS-AIR-COST-SEK                        
430638     ELSE                                                                 
430639        MOVE WS-KR-VOLYM-RED    TO WS-AIR-COST-SEK                        
430640     END-IF                                                               
430641     COMPUTE WS-AIR-COST-SEK = WS-AIR-COST-SEK *                          
430642                               WS-PURCHQTY (IX-DC)                        
430643                                                                          
430644     IF WS-AIR-COST-SEK > WS-PRFRAKT                                      
430645        MOVE ERR-HIGH-AIR-COST  TO MED-IDMFSFEL                           
430646        CALL WMEDKONV           USING MED-WMEDAREA                        
430647        MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                           
430648        MOVE NEJ                TO INDATA-SW                              
430649     END-IF                                                               
430650     .                                                                    
430651     EJECT                                                                
430652 J-FYLL-I-ANT-REVIEW SECTION.                                             
430653     MOVE 'J-FYLL-I-ANT-REVIEW         '                                  
430654                             TO WS-PGM-SEKTION                            
430660                                                                          
430700     MOVE ZERO               TO WS-ANT-REVIEW                             
430800                                W-IDARTNR-MIN                             
430900     MOVE +999999999         TO W-IDARTNR-MAX                             
431000     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
431100                                W-KDREFTYP-MAX                            
431200                                                                          
431300     PERFORM IMS-GU-WDE3B1-MIN-MAX                                        
431400     PERFORM S2-KOLLA-GILTIGT-DC                                          
431500     PERFORM UNTIL SEGMENT-SAKNAS                                         
431600                                                                          
431700         IF  SEQB-KDREFTYP          = WS-IDTYPE                           
431800         AND SW-GILTIGT-DC-JA                                             
431900         AND SEQB-KDREFORS          = 'P'                                 
432000            ADD +1           TO WS-ANT-REVIEW                             
432100            MOVE SEQB-IDARTNR                                             
432200                             TO WS-SPARA-IDARTNR                          
432300                                                                          
432400            PERFORM UNTIL SEGMENT-SAKNAS                                  
432500            OR WS-SPARA-IDARTNR NOT = SEQB-IDARTNR                        
432600                                                                          
432700               PERFORM IMS-GN-WDE3B1-MIN-MAX                              
432800               PERFORM S2-KOLLA-GILTIGT-DC                                
432900                                                                          
433000            END-PERFORM                                                   
433100                                                                          
433200         ELSE                                                             
433300            PERFORM IMS-GN-WDE3B1-MIN-MAX                                 
433400            PERFORM S2-KOLLA-GILTIGT-DC                                   
433500                                                                          
433600         END-IF                                                           
433700                                                                          
433800     END-PERFORM                                                          
433900                                                                          
434000     IF SEGMENT-FINNS                                                     
434100       IF SEQB-IDDC NOT = DCS-IDDC                                        
434200          MOVE SEQB-IDDC     TO W-IDDC-B6                                 
434300          PERFORM IMS-GU-WDB601                                           
434400       END-IF                                                             
434500     END-IF                                                               
434600     MOVE WS-ANT-REVIEW      TO MOD-ANT-REVIEW                            
434700     .                                                                    
434800     EJECT                                                                
434900 K-FYLL-I-NYCKEL-FAELT SECTION.                                           
435000     MOVE 'K-FYLL-I-NYCKEL-FAELT       '                                  
435100                             TO WS-PGM-SEKTION                            
435200                                                                          
435300     MOVE JA                 TO SW-TRAEFF                                 
435400     MOVE SEQB-IDARTNR       TO WS-SPARA-IDARTNR                          
435500                                W-IDARTNR                                 
435600     MOVE SEQB-IDARTNR       TO MOD-IDARTNR-UT                            
435700     IF SEQB-KDREFTYP = 'A'                                               
435800        MOVE 'AIR'           TO MOD-IDTYPE-UT                             
435900                                WS-IDTYPE                                 
436000     END-IF                                                               
436100     IF SEQB-KDREFTYP = 'C'                                               
436200        MOVE 'AIRCR'         TO MOD-IDTYPE-UT                             
436300        MOVE 'C'             TO WS-IDTYPE                                 
436400     END-IF                                                               
436500     IF SEQB-KDREFTYP = 'B'                                               
436600        MOVE 'BOAT'          TO MOD-IDTYPE-UT                             
436700                                WS-IDTYPE                                 
436800     END-IF                                                               
436900     IF SEQB-KDREFTYP = 'L'                                               
437000        MOVE 'LOCAL'         TO MOD-IDTYPE-UT                             
437100                                WS-IDTYPE                                 
437200     END-IF                                                               
437300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
437400     .                                                                    
437500     EJECT                                                                
437600 L-KOLLA-INPUT-NYCKLAR SECTION.                                           
437700     MOVE 'L-KOLLA-INPUT-NYCKLAR       '                                  
437800                             TO WS-PGM-SEKTION                            
437900                                                                          
438000     IF NOT (MID-IDARTNR-IN       = ALL '+'                               
438100        AND  MID-IDDC-IN          = ALL '+'                               
438200        AND  MID-IDTYPE-IN        = ALL '+'                               
438300        AND  MID-IDSTATUS-IN      = ALL '+')                              
438400                                                                          
438500       MOVE NEJ              TO INDATA-SW                                 
438600     END-IF                                                               
438700     .                                                                    
438800     EJECT                                                                
438900 M-UPD-MOD-FAELT SECTION.                                                 
439000     MOVE 'M-UPD-MOD-FAELT             '                                  
439100                             TO WS-PGM-SEKTION                            
439200     MOVE REF-KDREFTXT       TO WS-REF-KDREFTXT (IX-DC)                   
442702                                                                          
442703     IF REF-KDREFORS = 'P'                                                
442704        MOVE 'PROPOSAL NOT REVIEWED'                                      
442705                             TO MOD-ORDERSTATUS                           
442706        MOVE 'NOT REVIEWED'                                               
442707                             TO MOD-IDSTATUS-UT                           
442708        MOVE 'N'             TO WS-STATUS                                 
442709     ELSE                                                                 
442710        MOVE 'REVIEWED'      TO MOD-ORDERSTATUS                           
442711*                               MOD-IDSTATUS-UT                           
442712*       MOVE 'R'             TO WS-STATUS                                 
442713     END-IF                                                               
442714                                                                          
442715     IF REF-KDREFTYP = 'B'                                                
442716       MOVE 'BOAT'           TO MOD-IDREFTYP-UT                           
442717     END-IF                                                               
442718     IF REF-KDREFTYP = 'A'                                                
442719       MOVE 'AIR'            TO MOD-IDREFTYP-UT                           
442720     END-IF                                                               
442730     IF REF-KDREFTYP = 'C'                                                
442740       MOVE 'AIRCR'          TO MOD-IDREFTYP-UT                           
442750     END-IF                                                               
442760     IF REF-KDREFTYP = 'L'                                                
442770       MOVE 'LOCAL'          TO MOD-IDREFTYP-UT                           
442780     END-IF                                                               
442790     IF REF-KDREFTYP = 'T'                                                
442800       MOVE 'TRANS'          TO MOD-IDREFTYP-UT                           
442900       PERFORM DB2-SELECT-TP4TRAN-2                                       
443000       IF LINES-FOUND                                                     
443100         MOVE TP4TRAN-IDDC-SEND                                           
443200                             TO MOD-IDDC-FROM (IX-DC)                     
443300                                WS-IDDC-FROM (IX-DC)                      
443400       END-IF                                                             
443500     END-IF                                                               
443600                                                                          
443700     MOVE REF-KVBEART        TO WS-RED-PURCHQTY                           
443800                                WS-PURCHQTY (IX-DC)                       
443900     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY (IX-DC)                      
444000     MOVE MFS-ADD-LAES-IN-FAELT                                           
444100                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
444200     .                                                                    
444300     EJECT                                                                
444400 N-UPD-WDE3-WDK7 SECTION.                                                 
444500     MOVE 'N-UPD-WDE3-WDK7             '                                  
444600                             TO WS-PGM-SEKTION                            
444700                                                                          
444800     MOVE +1                 TO IX-DC                                     
444900     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
445000                                                                          
445100       MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                            
445200                                W-KDREFTYP-MAX                            
445300       MOVE W-IDARTNR        TO W-IDARTNR-MIN                             
445400                                W-IDARTNR-MAX                             
445500                                                                          
445600       PERFORM IMS-GU-WDE3B1-MIN-MAX                                      
445700                                                                          
445800       PERFORM UNTIL SEGMENT-SAKNAS                                       
445900       OR (SEQB-KDREFTYP          = W-KDREFTYP-MIN                        
446000       AND SEQB-IDARTNR           = W-IDARTNR-MIN                         
446100       AND SEQB-IDDC              = WS-DC-NR (IX-DC))                     
446200                                                                          
446300           PERFORM IMS-GN-WDE3B1-MIN-MAX                                  
446400       END-PERFORM                                                        
446500                                                                          
446600       IF SEGMENT-FINNS                                                   
446700                                                                          
446800         MOVE SEQB-IDWDE301                                               
446900                             TO W-WDE301KY-X                              
447000         PERFORM IMS-GHU-WDE301                                           
447100                                                                          
447200         MOVE WS-DC-NR (IX-DC)                                            
447300                             TO W-IDDC                                    
447400         PERFORM IMS-GHU-WDK711                                           
447500         IF REF-KDREFORS = 'O'                                            
447600           IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                    
447700               MID-IDDC-FROM (IX-DC) NOT = SPACE)                         
447800             SUBTRACT REF-KVBEART                                         
447900                               FROM SLAG-KVBEART                          
448000             PERFORM NB-EV-CROSS-DOCKING                                  
448100           ELSE                                                           
448200             IF WS-IDREFTYP NOT = 'T'                                     
448300               SUBTRACT REF-KVBEART                                       
448400                                 FROM SLAG-KVBEART                        
448500               PERFORM NB-EV-CROSS-DOCKING                                
448600             END-IF                                                       
448700           END-IF                                                         
448800         END-IF                                                           
448900*        IF REF-KDREFORS = 'O'                                            
449000*          SUBTRACT REF-KVBEART                                           
449100*                            FROM SLAG-KVBEART                            
449200*          PERFORM NB-EV-CROSS-DOCKING                                    
449300*        END-IF                                                           
449400         IF WS-PURCHQTY (IX-DC) > ZERO                                    
449500           ADD WS-PURCHQTY (IX-DC)                                        
449600                             TO SLAG-KVBEART                              
449700           MOVE DAGENS-DATUM TO SLAG-TIORDREG                             
449800         END-IF                                                           
449900         PERFORM IMS-REPL-WDK711                                          
450000                                                                          
450100         MOVE WS-IDREFTYP    TO REF-KDREFTYP                              
450200         IF WS-PURCHQTY (IX-DC) > ZERO                                    
450300           MOVE WS-PURCHQTY (IX-DC)                                       
450400                             TO REF-KVBEART                               
450500           MOVE 'O '         TO REF-KDREFORS                              
450600***jn      MOVE SLAG-IDLEVNR TO REF-IDLEVNR                               
450700                                                                          
450800           PERFORM NA-EV-CROSS-DOCKING                                    
450900           PERFORM IMS-REPL-WDE301                                        
451000         ELSE                                                             
451100           IF WS-IDREFTYP = 'T'                                           
451200             MOVE MID-IDDC-FROM (IX-DC)   TO REFILL-WS-IDDC               
451300             IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                  
451400                 MID-IDDC-FROM (IX-DC) NOT = SPACE   AND                  
451500                 NOT REFILL-CDC-SE)                                       
451600               PERFORM IMS-DLET-WDE301                                    
451700*     --   OM MAN NOLLAT KÖPFÖRSLAGET SÄTTS FLREFNYO TILL JA              
451800               MOVE SLAG-IDDC-REF         TO REFILL-WS-IDDC               
451900               IF (REFILL-CDC-SE                                          
452000               OR  REFILL-NDC-CN )                                        
452300                 MOVE WS-DC-NR (IX-DC) TO W-IDDC                          
452400                 PERFORM IMS-GHU-WDK711                                   
452500                 IF SEGMENT-FINNS                                         
452600                   MOVE JA             TO SLAG-FLREFNYO                   
452700                   PERFORM IMS-REPL-WDK711                                
452800                 END-IF                                                   
452900               END-IF                                                     
453000             END-IF                                                       
453100           ELSE                                                           
453200             PERFORM IMS-DLET-WDE301                                      
453300*   --     OM MAN NOLLAT KÖPFÖRSLAGET SÄTTS FLREFNYO TILL JA              
453400             MOVE SLAG-IDDC-REF         TO REFILL-WS-IDDC                 
453500             IF (REFILL-CDC-SE                                            
453600             OR  REFILL-NDC-CN )                                          
453900               MOVE WS-DC-NR (IX-DC) TO W-IDDC                            
454000               PERFORM IMS-GHU-WDK711                                     
454100               IF SEGMENT-FINNS                                           
454200                  MOVE JA            TO SLAG-FLREFNYO                     
454300                  PERFORM IMS-REPL-WDK711                                 
454400               END-IF                                                     
454500             END-IF                                                       
454600           END-IF                                                         
454700         END-IF                                                           
454800       ELSE                                                               
454900         IF WS-PURCHQTY (IX-DC) > ZERO                                    
455000                                                                          
455100           MOVE WS-IDREFTYP  TO REF-KDREFTYP                              
455200           MOVE WS-DC-NR (IX-DC)                                          
455300                             TO REF-IDDC                                  
455400                                W-IDDC                                    
455500           MOVE W-IDARTNR    TO REF-IDARTNR                               
455600           PERFORM IMS-GHU-WDK711                                         
455700           ADD WS-PURCHQTY (IX-DC)                                        
455800                             TO SLAG-KVBEART                              
455900           MOVE DAGENS-DATUM TO SLAG-TIORDREG                             
456000           PERFORM IMS-REPL-WDK711                                        
456100           MOVE SLAG-ADART   TO REF-ADART-SDC                             
456200           MOVE CLAG-ADART   TO REF-ADART-CDC                             
456300           MOVE WS-PURCHQTY (IX-DC)                                       
456400                             TO REF-KVBEART                               
456500           MOVE 'O'          TO REF-KDREFORS                              
456600           MOVE SLAG-IDLEVNR TO REF-IDLEVNR                               
456700           MOVE SLAG-IDPERSON-BUY                                         
456800                             TO REF-IDPERSON-BUY                          
456900           MOVE ZERO         TO REF-KDREFTXT                              
457000                                REF-KDFRAKT                               
457100                                REF-KVBEART-CD                            
457200                                REF-ADLAGOMR-CD                           
457300                                REF-ADGANG-CD                             
457400                                REF-ADPLATS-CD                            
457500**jn                                                                      
457600           MOVE MID-IDDC-FROM (IX-DC)    TO REFILL-WS-IDDC                
457700           IF WS-IDREFTYP = 'T'                                           
457800           AND (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                   
457900                MID-IDDC-FROM (IX-DC) NOT = SPACE   AND                   
458000                NOT REFILL-CDC-SE )                                       
458100             MOVE WS-IDDISTR (IX-DC)                                      
458200                             TO REF-IDDISTR                               
458300             MOVE WS-IDKUNDNR (IX-DC)                                     
458400                             TO REF-IDKUNDNR                              
458500             MOVE MID-IDDC-FROM (IX-DC)    TO W-IDDC-B6                   
458510                                              REF-IDDC-REF                
458600             PERFORM IMS-GU-WDB601                                        
458700             IF SEGMENT-FINNS                                             
458800               MOVE DCS-IDLEVNR-DC   TO REF-IDLEVNR                       
458900             END-IF                                                       
459000           ELSE                                                           
459100             MOVE WS-IDDISTR (IX-DC)                                      
459200                             TO REF-IDDISTR                               
459300             MOVE ZERO       TO REF-IDKUNDNR                              
459400           END-IF                                                         
459410           MOVE SLAG-IDDC-REF                                             
459420                           TO REF-IDDC-REF                                
459500           PERFORM NA-EV-CROSS-DOCKING                                    
459600           PERFORM IMS-ISRT-WDE301                                        
459700         END-IF                                                           
459800       END-IF                                                             
459900       ADD +1                TO IX-DC                                     
460000     END-PERFORM                                                          
460100     MOVE 'REVIEWED'         TO MOD-ORDERSTATUS                           
460200     .                                                                    
460300     EJECT                                                                
460400                                                                          
460500 NA-EV-CROSS-DOCKING  SECTION.                                            
460600     MOVE 'NA-EV-CROSS-DOCKING         '                                  
460700                             TO WS-PGM-SEKTION                            
460800                                                                          
460900     IF SLAG-ADLAGOMR-CD > ZERO                                           
461000                                                                          
461100       MOVE 1                TO IX-CD                                     
461200       PERFORM UNTIL IX-CD > 4                                            
461300       OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX-CD)                     
461400         ADD 1               TO IX-CD                                     
461500       END-PERFORM                                                        
461600                                                                          
461700       IF IX-CD > 4                                                       
461800*    SKA INTE KUNNA INTRÄFFA                                              
461900         CONTINUE                                                         
462000       ELSE                                                               
462100                                                                          
462200         IF (CLAG-KVLS-CD (IX-CD) - CLAG-KVRESS-CD (IX-CD))               
462300                             < CLAG-KVQPACK-3                             
462400           CONTINUE                                                       
462500         ELSE                                                             
462600           DIVIDE REF-KVBEART BY CLAG-KVQPACK-3                           
462700                                 GIVING WS-HELTAL-BEST                    
462800           COMPUTE WS-SALDO =                                             
462900                   CLAG-KVLS-CD (IX-CD) - CLAG-KVRESS-CD (IX-CD)          
463000           DIVIDE WS-SALDO       BY CLAG-KVQPACK-3                        
463100                                 GIVING WS-HELTAL-SALDO                   
463200           IF WS-HELTAL-BEST > WS-HELTAL-SALDO                            
463300             COMPUTE REF-KVBEART-CD =                                     
463400                             WS-HELTAL-SALDO * CLAG-KVQPACK-3             
463500           ELSE                                                           
463600             COMPUTE REF-KVBEART-CD =                                     
463700                             WS-HELTAL-BEST * CLAG-KVQPACK-3              
463800           END-IF                                                         
463900           MOVE CLAG-ADLAGOMR-CD (IX-CD)                                  
464000                             TO REF-ADLAGOMR-CD                           
464100           MOVE CLAG-ADGANG-CD (IX-CD)                                    
464200                             TO REF-ADGANG-CD                             
464300           MOVE CLAG-ADPLATS-CD (IX-CD)                                   
464400                             TO REF-ADPLATS-CD                            
464500                                                                          
464600           PERFORM IMS-GHU-WDK611                                         
464700           COMPUTE CLAG-KVRESS-CD (IX-CD) =                               
464800                   CLAG-KVRESS-CD (IX-CD) + REF-KVBEART-CD                
464900           PERFORM IMS-REPL-WDK6                                          
465000         END-IF                                                           
465100       END-IF                                                             
465200     END-IF                                                               
465300     .                                                                    
465400     EJECT                                                                
465500                                                                          
465600                                                                          
465700 NB-EV-CROSS-DOCKING  SECTION.                                            
465800     MOVE 'NB-EV-CROSS-DOCKING         '                                  
465900                             TO WS-PGM-SEKTION                            
466000                                                                          
466100     IF SLAG-ADLAGOMR-CD > ZERO                                           
466200                                                                          
466300       MOVE 1                TO IX-CD                                     
466400       PERFORM UNTIL IX-CD > 4                                            
466500       OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX-CD)                     
466600         ADD 1               TO IX-CD                                     
466700       END-PERFORM                                                        
466800                                                                          
466900       IF IX-CD > 4                                                       
467000*    SKA INTE KUNNA INTRÄFFA                                              
467100         CONTINUE                                                         
467200       ELSE                                                               
467300                                                                          
467400         PERFORM IMS-GHU-WDK611                                           
467500         COMPUTE CLAG-KVRESS-CD (IX-CD) =                                 
467600                 CLAG-KVRESS-CD (IX-CD) - REF-KVBEART-CD                  
467700         PERFORM IMS-REPL-WDK6                                            
467800       END-IF                                                             
467900     END-IF                                                               
468000     .                                                                    
468100     EJECT                                                                
468200 Z-JUSTERA-MOD SECTION.                                                   
468300     MOVE 'Z-JUSTERA-MOD               '                                  
468400                             TO WS-PGM-SEKTION                            
468500                                                                          
468600     MOVE MOD-TEMFSINF           TO WS-TEMFSINF                           
468700     MOVE 1 TO IX                                                         
468800     PERFORM UNTIL IX > 6                                                 
468900        IF WS-TEMFSINF-DC(IX) = SPACE                                     
469000           MOVE WS-IDDC-MED(IX)  TO WS-TEMFSINF-DC(IX)                    
469100        END-IF                                                            
469200        ADD 1 TO IX                                                       
469300     END-PERFORM                                                          
469400     MOVE WS-TEMFSINF            TO MOD-TEMFSINF                          
469500     .                                                                    
469600                                                                          
469700 S1-SECURITY-CHECK-PARTNO SECTION.                                        
469800     MOVE 'S1-SECURITY-CHECK-PARTNO    '                                  
469900                             TO WS-PGM-SEKTION                            
470000     SKIP2                                                                
470100*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
470200     PERFORM IMS-GU-WDK601                                                
470300     IF  SEGMENT-FINNS                                                    
470400       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
470500       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
470600       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
470700*        --- USER GRANTED                                                 
470800         SET PASSED-SECURITY-CHECK TO TRUE                                
470900       ELSE                                                               
471000         SET BLOCKED-SECURITY-CHECK TO TRUE                               
471100       END-IF                                                             
471200     END-IF                                                               
471300     .                                                                    
471400     EJECT                                                                
471500 S2-KOLLA-GILTIGT-DC SECTION.                                             
471600     MOVE 'S2-KOLLA-GILTIGT-DC         '                                  
471700                             TO WS-PGM-SEKTION                            
471800                                                                          
471900     MOVE 1                  TO IX-DC                                     
472000     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
472100     OR SEQB-IDDC = WS-DC-NR (IX-DC)                                      
472200       ADD 1                 TO IX-DC                                     
472300     END-PERFORM                                                          
472400                                                                          
472500     IF IX-DC > WS-DC-MAX                                                 
472600       MOVE NEJ              TO SW-GILTIGT-DC                             
472700     ELSE                                                                 
472800       MOVE JA               TO SW-GILTIGT-DC                             
472900     END-IF                                                               
473000     .                                                                    
473100     EJECT                                                                
473200 S3-KOLLA-GILTIGT-DC SECTION.                                             
473300     MOVE 'S3-KOLLA-GILTIGT-DC         '                                  
473400                             TO WS-PGM-SEKTION                            
473500                                                                          
473600     MOVE 1                  TO IX-DC                                     
473700     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
473800     OR REF-IDDC = WS-DC-NR (IX-DC)                                       
473900       ADD 1                 TO IX-DC                                     
474000     END-PERFORM                                                          
474100                                                                          
474200     IF IX-DC > WS-DC-MAX                                                 
474300       MOVE NEJ              TO SW-GILTIGT-DC                             
474400     ELSE                                                                 
474500       MOVE JA               TO SW-GILTIGT-DC                             
474600     END-IF                                                               
474700     .                                                                    
474800     EJECT                                                                
474900                                                                          
475000 S4-GET-BESPRIS SECTION.                                                  
475100     MOVE 'S4-GET-BESPRIS              '                                  
475200                             TO WS-PGM-SEKTION                            
475300     IF W-IDDC NOT = DCS-IDDC                                             
475400        MOVE W-IDDC          TO W-IDDC-B6                                 
475500        PERFORM IMS-GU-WDB601                                             
475600     END-IF                                                               
475700     IF DCS-CHINA OR DCS-NDC-NA                                           
475800       PERFORM S4A-GET-BESPRIS-OTHER                                      
475900     ELSE                                                                 
476000       PERFORM S4B-GET-BESPRIS-CDC                                        
476100     END-IF                                                               
476200     .                                                                    
476300     EJECT                                                                
476400                                                                          
476500 S4A-GET-BESPRIS-OTHER SECTION.                                           
476600     MOVE 'S4A-GET-BESPRIS-OTHER       '                                  
476700                             TO WS-PGM-SEKTION                            
476800*    -- WDK712                                                            
476900     MOVE DCS-IDLANDX2       TO W-IDLAND                                  
477000     PERFORM IMS-GU-WDK712                                                
477100     IF SEGMENT-FINNS                                                     
477200       MOVE LART-PRMATRL     TO WS-PRARTBES                               
477300     ELSE                                                                 
477400       MOVE ZERO             TO WS-PRARTBES                               
477500     END-IF                                                               
477600     .                                                                    
477700* GET WDK711 DETAILS FOR SENDING DC                                       
477800* FIRST GU-WDK711 CALL IS TO GET SENDING DC DETAILS                       
477900* SECOND GU-WDK711 CALL IS TO RESET THE WDK711 TO REFILL DC               
478000 S5-CHK-KVDISP-SEND-DC SECTION.                                           
478100     MOVE 'S5-CHK-KVDISP-SEND-DC       '                                  
478200                             TO WS-PGM-SEKTION                            
478300                                                                          
478400     MOVE W-IDDC             TO W-RECEIVING-IDDC                          
478500     MOVE SLAG-IDDC-REF      TO W-IDDC                                    
478600     MOVE ZEROES             TO WS-KVDISP-SEND-DC                         
478700                                                                          
478800     PERFORM IMS-GU-WDK711                                                
478900     IF SEGMENT-FINNS                                                     
479000        COMPUTE WS-KVDISP-SEND-DC = SLAG-KVLS         +                   
479100                                    SLAG-KVBEART      +                   
479200                                    SLAG-KVAKS-SDC    +                   
479300                                    SLAG-KVAKS-PAV    -                   
479400                                    SLAG-KVOKS-DAG    -                   
479500                                    SLAG-KVOKS-BULK   -                   
479600                                    SLAG-KVROS-DAG    -                   
479700                                    SLAG-KVROS-BULK   -                   
479800                                    SLAG-KVSPARR-KVAL -                   
479900                                    SLAG-KVRESS                           
480000     END-IF                                                               
480100                                                                          
480200     MOVE W-RECEIVING-IDDC   TO W-IDDC                                    
480300     PERFORM IMS-GU-WDK711                                                
480400     .                                                                    
480500     EJECT                                                                
480600                                                                          
480700 S4B-GET-BESPRIS-CDC SECTION.                                             
480800                                                                          
480900     MOVE CLAG-PRARTSTD         TO WS-PRARTBES                            
481000     MOVE 'S4B-GET-BESPRIS-CDC         '                                  
481100                             TO WS-PGM-SEKTION                            
481200*    COMPUTE W-DAPRLIST-21 = 99999999 - DAGENS-DATUM-SEKEL                
481300*    MOVE SLAG-IDLEVNR   TO W-IDLEVNR-21                                  
481400*    PERFORM IMS-GNP-WDK621-FIRST                                         
481500*    IF SEGMENT-SAKNAS                                                    
481600*      MOVE CLAG-PRARTSTD       TO WS-PRARTBES                            
481700*    ELSE                                                                 
481800*      MOVE NEJ                 TO FL-PRARTBES                            
481900*      PERFORM UNTIL  SEGMENT-SAKNAS                                      
482000*        IF PRL-SUINLEV-PR > ZERO                                         
482100*          MOVE PRL-PRARTBES-PR  TO WS-PRARTBES                           
482200*          SET SEGMENT-SAKNAS TO TRUE                                     
482300*        ELSE                                                             
482400*          IF FL-PRARTBES = NEJ                                           
482500*            MOVE PRL-PRARTBES-PR TO WS-PRARTBES                          
482600*            MOVE JA              TO FL-PRARTBES                          
482700*          END-IF                                                         
482800*          PERFORM IMS-GNP-WDK621                                         
482900*        END-IF                                                           
483000*      END-PERFORM                                                        
483100*    END-IF                                                               
483200     .                                                                    
483300     EJECT                                                                
483530 S90-CALL-W271REFL SECTION.                                               
483540                                                                          
483600     INITIALIZE  W271-REFL-W271REFL                                       
483700                                                                          
483800     MOVE ZERO               TO W271-REFL-NDC-KVDAGAR-TBT-DC              
483900     MOVE W-IDDC             TO W271-REFL-IDDC                            
484000     MOVE W-IDARTNR          TO W271-REFL-IDARTNR                         
484100     MOVE SLAG-IDDC-REF      TO W271-REFL-IDDC-REF                        
484200     MOVE SLAG-IDREFTAB      TO W271-REFL-IDREFTAB                        
484300     MOVE SLAG-FLWILSON      TO W271-REFL-FLWILSON                        
484400                                                                          
484500     PERFORM S4-GET-BESPRIS                                               
484600     MOVE WS-PRARTBES        TO W271-REFL-PRARTBES                        
484700                                                                          
484800     MOVE 1 TO IX                                                         
484900     PERFORM UNTIL IX > 12                                                
485000        MOVE SLAG-RESEASON(IX) TO W271-REFL-RESEASON(IX)                  
485100        ADD 1 TO IX                                                       
485200     END-PERFORM                                                          
485300                                                                          
485400     MOVE SLAG-IDLEVNR        TO W271-REFL-IN-IDLEVNR-DC                  
485500     MOVE SLAG-IDLEVNR        TO W271-REFL-IN-IDLEVNR-DC                  
485600     MOVE SLAG-FLFLYG         TO W271-REFL-FLFLYG                         
485700                                                                          
485800     MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                                  
485900     MOVE DAGENS-DATUM    TO TMP2-YYMMDD                                  
486000     PERFORM WY2000P1                                                     
486100     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
486200       MOVE SLAG-KVREFPKT     TO W271-REFL-IN-KVREFPKT                    
486300     ELSE                                                                 
486400       MOVE ZERO              TO W271-REFL-IN-KVREFPKT                    
486500     END-IF                                                               
486600                                                                          
486700     MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                                  
486800     MOVE DAGENS-DATUM    TO TMP2-YYMMDD                                  
486900     PERFORM WY2000P1                                                     
487000     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
487100       MOVE SLAG-KVREFBER     TO W271-REFL-IN-KVREFBER                    
487200     ELSE                                                                 
487300       MOVE ZERO              TO W271-REFL-IN-KVREFBER                    
487400     END-IF                                                               
487500                                                                          
487600     MOVE ZERO            TO W271-REFL-NDC-KVDAGAR-TBT-DC                 
487700*                                                                         
487800***  BELOW CHECK ONLY IF SIMUNATION IS REQUESTED AND NOT FOR UPD          
487900*                                                                         
488000     IF WS-FLSIM = JA                                                     
488100        MOVE JA                  TO W271-REFL-IN-FLSIM                    
488200        MOVE WS-KVPB-REF (IX-DC) TO W271-REFL-IN-KVPB-REF                 
488300        MOVE WS-KVPBREOI (IX-DC) TO W271-REFL-IN-KVPBREOI                 
488400     END-IF                                                               
488500                                                                          
488600     PERFORM S100-CALL-W271UTUP                                           
488700                                                                          
488800     CALL W271REFL USING W271-REFL-W271REFL                               
488900                         REFL1-2501-PCB                                   
489000                         REFL1-WDB6-PCB                                   
489100                         REFL1-WDK7-PCB                                   
489200                         REFL1-UTIL-WDK6-PCB                              
489300                         REFL1-UTIL-WDK7-PCB                              
489400                         REFL1-UTIL-WDB6-PCB                              
489500                                                                          
489600     .                                                                    
489700     EJECT                                                                
489800                                                                          
489900 S100-CALL-W271UTUP SECTION.                                              
490000                                                                          
490100***  THIS SECTION CALLS W271UTUP TO GET LEAD TIME                         
490200***  ADJUSTED DEMAND - ( LEAD TIME ADJUSTED FROM CURRENT WEEK)            
490300*                                                                         
490400     INITIALIZE W271-UTUP-W271UTUP                                        
490500     MOVE W-IDARTNR                 TO W271-UTUP-IDARTNR                  
490600     MOVE W-IDDC                    TO W271-UTUP-IDDC                     
490700     MOVE SLAG-IDDC-REF             TO W271-UTUP-IDDC-REF                 
490800     MOVE DAGENS-DATUM              TO W271-UTUP-TIAAMMDD                 
490900     MOVE 004                       TO W271-UTUP-KDCALL                   
491000*                                                                         
491100***  BELOW CHECK ONLY IF SIMUNATION IS REQUESTED AND NOT FOR UPD          
491200*                                                                         
491300     IF WS-FLSIM = JA                                                     
491400        MOVE JA                     TO W271-UTUP-FLSIM                    
491500        MOVE WS-KVPB-REF (IX-DC)    TO W271-UTUP-KVPB-REF                 
491600        MOVE WS-KVPBREOI (IX-DC)    TO W271-UTUP-KVPBREOI                 
491700     END-IF                                                               
491800*                                                                         
491900     CALL W271UTUP USING W271-UTUP-W271UTUP                               
492000                         UTUP1-WDK7-PCB                                   
492100                         UTUP1-WDB6-PCB                                   
492200                         UTUP1-UTIL-WDK6-PCB                              
492300                         UTUP1-UTIL-WDK7-PCB                              
492400                         UTUP1-UTIL-WDB6-PCB                              
492500     IF W271-UTUP-KDSVAR-OK                                               
492600        MOVE W271-UTUP-LEADTID-BEHOV TO W271-REFL-IN-LEADTID-BEHOV        
492700     ELSE                                                                 
492800        MOVE 'FEL FRÅN W20352 S100-'                                      
492900                                    TO FELTEXT                            
493000        CALL FELLOG                                                       
493100     END-IF                                                               
493200     .                                                                    
493300     EJECT                                                                
493400                                                                          
493500 MFS-RENSA-FAELT-IN SECTION.                                              
493600                                                                          
493700*    --- ALLA INDATA-FÄLT                                                 
493800     MOVE MFS-RENSA-FAELT    TO MOD-COMMENT (1)                           
493900                                MOD-COMMENT (2)                           
494000                                                                          
494100     MOVE 1                  TO IX                                        
494200     PERFORM UNTIL IX > DC-MAX-2382                                       
494300       MOVE MFS-RENSA-FAELT  TO MOD-KVPB-REF (IX)                         
494400                                MOD-FLREFBEO (IX)                         
494500                                MOD-PURCHQTY (IX)                         
494600                                MOD-IDDC-FROM (IX)                        
494700       ADD 1                  TO IX                                       
494800     END-PERFORM                                                          
494900                                                                          
495000     MOVE ZERO               TO WS-RED-PURCHQTY                           
495100     MOVE 1                  TO IX-DC                                     
495200     PERFORM UNTIL IX-DC > WS-DC-MAX                                      
495300                                                                          
495400       MOVE WS-RED-PURCHQTY                                               
495500                       TO MOD-PURCHQTY (IX-DC)                            
495600       ADD 1                 TO IX-DC                                     
495700     END-PERFORM                                                          
495800     .                                                                    
495900     EJECT                                                                
496000 S101-GET-REFILLDC-OCH-DISTRIKT SECTION.                                  
496100     MOVE 'S101-GET-REFDC-O-DISTR' TO WS-PGM-SEKTION                      
496200                                                                          
496300     PERFORM DB2-SELECT-TP5IDDC                                           
496400     IF LINES-FOUND                                                       
496500       MOVE TP5IDDC-IDLOPNR-DC                                            
496600                             TO WS-IDLOPNR-DC                             
496700                                                                          
496800       PERFORM DB2-OPEN-TP5IDDC-CRS                                       
496900       IF LINES-FOUND                                                     
497000         PERFORM DB2-FETCH-TP5IDDC-CRS                                    
497100       END-IF                                                             
497200                                                                          
497300       MOVE 1                TO IX-DC                                     
497400                                IX-TILL                                   
497500       MOVE ZERO             TO WS-DC-MAX                                 
497600       PERFORM UNTIL SQLCODE > ZERO                                       
497700       OR IX-DC > DC-MAX-2382                                             
497800         MOVE TP5IDDC-IDDC    TO WS-DC-NR (IX-TILL)                       
497900                                 W-IDDC-B6                                
498000                                 W-IDDC                                   
498100                                 MOD-IDDC-TO (IX-TILL)                    
498200         MOVE IX-TILL         TO WS-DC-MAX                                
498300         PERFORM IMS-GU-WDB601                                            
498400         MOVE DCS-IDDISTR-REFILL                                          
498500                              TO WS-IDDISTR (IX-TILL)                     
498600         PERFORM IMS-GU-WDK711                                            
498700         IF SEGMENT-FINNS                                                 
498800            IF MFS-UPDATE OR                                              
498810               MFS-UPD-V                                                  
498900               IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                  
499000                     PERFORM IMS-GU-WDB601                                
499100                     MOVE DCS-IDDISTR-REFILL                              
499200                                 TO WS-IDDISTR(IX-TILL)                   
499300               ELSE                                                       
499400                  IF SLAG-IDDC-REF NOT = SPACE                            
499500                     MOVE SLAG-IDDC-REF                                   
499600                                    TO W-IDDC-B616                        
499700                     PERFORM IMS-GU-WDB616                                
499800                     IF SEGMENT-FINNS                                     
499900                     MOVE B6-REF-IDDISTR-REFILL                           
500000                                    TO WS-IDDISTR(IX-TILL)                
500100                     ELSE                                                 
500200                        MOVE NEJ TO NYCKLAR-SW                            
500300                        MOVE MED-15 TO MOD-TEMFSINF                       
500400                     END-IF                                               
500500                  END-IF                                                  
500600               END-IF                                                     
500700            END-IF                                                        
500800            MOVE SLAG-IDDC-REF TO WS-IDDC-FROM (IX-TILL)                  
500900                                    MOD-IDDC-FROM (IX-TILL)               
501000            IF (SLAG-IDDC-REF = SPACE) AND DCS-CHINA                      
501100               MOVE '  PURCH'  TO WS-IDDC-MED (IX-TILL)                   
501200            ELSE                                                          
501300               MOVE SPACE      TO WS-IDDC-MED (IX-TILL)                   
501400            END-IF                                                        
501500         ELSE                                                             
501600            MOVE SPACE         TO WS-IDDC-FROM (IX-TILL)                  
501700                                  MOD-IDDC-FROM (IX-TILL)                 
501800            MOVE SPACE         TO WS-IDDC-MED (IX-TILL)                   
501900         END-IF                                                           
502000         ADD 1                TO IX-TILL                                  
502100         ADD 1                TO IX-DC                                    
502200                                                                          
502300         PERFORM DB2-FETCH-TP5IDDC-CRS                                    
502400       END-PERFORM                                                        
502500       PERFORM DB2-CLOSE-TP5IDDC-CRS                                      
502600     ELSE                                                                 
502700       MOVE NEJ               TO NYCKLAR-SW                               
502800     END-IF                                                               
502900     .                                                                    
503000                                                                          
503100                                                                          
503200 MFS-RENSA-FAELT-UT SECTION.                                              
503300                                                                          
503400*    --- ALLA UTDATA-FÄLT                                                 
503500     MOVE MFS-RENSA-FAELT    TO MOD-BEART                                 
503600                                MOD-ANT-REVIEW                            
503700                                MOD-VECKA                                 
503800                                MOD-TIFINLV                               
503900                                MOD-TIURPROD                              
504000                                MOD-REPLACES                              
504100                                MOD-REPL-BY                               
504200                                MOD-KDERS                                 
504300                                MOD-KDPRODSL                              
504400                                MOD-VLARTNTO                              
504500                                MOD-PRARTSTD                              
504600                                MOD-KVPB-CDC                              
504700                                MOD-AVAIL                                 
504800                                MOD-KVROS-CDC                             
504900                                MOD-KVQPACK-1                             
505000                                MOD-KVQPACK-3                             
505100                                                                          
505200     MOVE 1                  TO IX                                        
505300     PERFORM UNTIL IX > 3                                                 
505400       MOVE MFS-RENSA-FAELT  TO MOD-MODEL (IX)                            
505500       ADD 1                 TO IX                                        
505600     END-PERFORM                                                          
505700                                                                          
505800     MOVE 1                  TO IX                                        
505900     PERFORM UNTIL IX > DC-MAX-2382                                       
506000       MOVE MFS-RENSA-FAELT  TO MOD-KVOI-INNEV (IX)                       
506100                                MOD-BALANCE (IX)                          
506200                                MOD-IN-TRANS (IX)                         
506300                                MOD-KVREFPKT (IX)                         
506400                                MOD-KVREFBER (IX)                         
506500                                MOD-TIREFEFT (IX)                         
506600                                MOD-TIINLINL (IX)                         
506700                                MOD-SEASON (IX)                           
506800       ADD 1                 TO IX                                        
506900     END-PERFORM                                                          
507000                                                                          
507100     MOVE 1                  TO IX                                        
507200     PERFORM UNTIL IX > 5                                                 
507300       MOVE MFS-RENSA-FAELT  TO MOD-TIPP (IX)                             
507400                                MOD-TIVV-FOM-TOM (IX)                     
507500       MOVE 1                TO IX2                                       
507600       PERFORM UNTIL IX2 > DC-MAX-2382                                    
507700         MOVE MFS-RENSA-FAELT                                             
507800                             TO MOD-KVOI-RULL (IX, IX2)                   
507900         ADD 1               TO IX2                                       
508000       END-PERFORM                                                        
508100       ADD 1                 TO IX                                        
508200     END-PERFORM                                                          
508300     .                                                                    
508400     EJECT                                                                
508500 MFS-RENSA-OBEHOERIGA-FAELT-IN SECTION.                                   
508600     SKIP2                                                                
508700     MOVE MFS-RENSA-FAELT      TO MOD-PURCHQTY(1)                         
508800                                  MOD-PURCHQTY(2)                         
508900                                  MOD-PURCHQTY(3)                         
509000                                  MOD-PURCHQTY(4)                         
509100                                  MOD-PURCHQTY(5)                         
509200                                  MOD-PURCHQTY(6)                         
509300                                  MOD-KVPB-REF(1)                         
509400                                  MOD-KVPB-REF(2)                         
509500                                  MOD-KVPB-REF(3)                         
509600                                  MOD-KVPB-REF(4)                         
509700                                  MOD-KVPB-REF(5)                         
509800                                  MOD-KVPB-REF(6)                         
509900                                  MOD-FLREFBEO(1)                         
510000                                  MOD-FLREFBEO(2)                         
510100                                  MOD-FLREFBEO(3)                         
510200                                  MOD-FLREFBEO(4)                         
510300                                  MOD-FLREFBEO(5)                         
510400                                  MOD-FLREFBEO(6)                         
510500                                  MOD-IDDC-FROM(1)                        
510600                                  MOD-IDDC-FROM(2)                        
510700                                  MOD-IDDC-FROM(3)                        
510800                                  MOD-IDDC-FROM(4)                        
510900                                  MOD-IDDC-FROM(5)                        
511000                                  MOD-IDDC-FROM(6)                        
511100                                  MOD-COMMENT (1)                         
511200                                  MOD-COMMENT (2)                         
511300     .                                                                    
511400     EJECT                                                                
511500 MFS-STAENG-OBEHOERIGA-FAELT-IN  SECTION.                                 
511600     SKIP2                                                                
511700     MOVE MFS-STAENG-FAELT TO MOD-PURCHQTY-ATTR(1)                        
511800                              MOD-PURCHQTY-ATTR(2)                        
511900                              MOD-PURCHQTY-ATTR(3)                        
512000                              MOD-PURCHQTY-ATTR(4)                        
512100                              MOD-PURCHQTY-ATTR(5)                        
512200                              MOD-PURCHQTY-ATTR(6)                        
512300                              MOD-KVPB-REF-ATTR(1)                        
512400                              MOD-KVPB-REF-ATTR(2)                        
512500                              MOD-KVPB-REF-ATTR(3)                        
512600                              MOD-KVPB-REF-ATTR(4)                        
512700                              MOD-KVPB-REF-ATTR(5)                        
512800                              MOD-KVPB-REF-ATTR(6)                        
512900                              MOD-FLREFBEO-ATTR(1)                        
513000                              MOD-FLREFBEO-ATTR(2)                        
513100                              MOD-FLREFBEO-ATTR(3)                        
513200                              MOD-FLREFBEO-ATTR(4)                        
513300                              MOD-FLREFBEO-ATTR(5)                        
513400                              MOD-FLREFBEO-ATTR(6)                        
513500                              MOD-IDDC-FROM-ATTR(1)                       
513600                              MOD-IDDC-FROM-ATTR(2)                       
513700                              MOD-IDDC-FROM-ATTR(3)                       
513800                              MOD-IDDC-FROM-ATTR(4)                       
513900                              MOD-IDDC-FROM-ATTR(5)                       
514000                              MOD-IDDC-FROM-ATTR(6)                       
514100                              MOD-COMMENT-ATTR (1)                        
514200                              MOD-COMMENT-ATTR (2)                        
514300     .                                                                    
514400     EJECT                                                                
514500 MFS-STANG-FALT SECTION.                                                  
514600                                                                          
514700*    --- ALLA INDATA-FÄLT                                                 
514800                                                                          
514900                                                                          
515000     MOVE MFS-STAENG-FAELT   TO MOD-KVPB-REF-ATTR (IX-DC)                 
515100                                MOD-PURCHQTY-ATTR (IX-DC)                 
515200                                MOD-FLREFBEO-ATTR (IX-DC)                 
515300                                MOD-IDDC-FROM-ATTR (IX-DC)                
515400     MOVE MFS-RENSA-FAELT    TO MOD-KVPB-REF (IX-DC)                      
515500                                MOD-PURCHQTY (IX-DC)                      
515600                                MOD-FLREFBEO (IX-DC)                      
515700                                MOD-IDDC-FROM (IX-DC)                     
515800     .                                                                    
515900     EJECT                                                                
516000 MFS-LAES-IN-IGEN SECTION.                                                
516100                                                                          
516200*    --- ALLA INDATA-FÄLT                                                 
516300                                                                          
516400     MOVE MFS-ADD-LAES-IN-FAELT                                           
516500                             TO MOD-IDARTNR-IN-ATTR                       
516600                                MOD-COMMENT-ATTR (1)                      
516700                                MOD-COMMENT-ATTR (2)                      
516800     MOVE 1                  TO IX                                        
516900     PERFORM UNTIL IX > DC-MAX-2382                                       
517000       MOVE MFS-ADD-LAES-IN-FAELT                                         
517100                             TO MOD-KVPB-REF-ATTR (IX)                    
517200                                MOD-PURCHQTY-ATTR (IX)                    
517300                                MOD-FLREFBEO-ATTR (IX)                    
517400                                MOD-IDDC-FROM-ATTR (IX)                   
517500       ADD 1                 TO IX                                        
517600     END-PERFORM                                                          
517700     .                                                                    
517800     EJECT                                                                
517900* --- IMS SEKTIONER ---                                                   
518000     SKIP3                                                                
518100 IMS-GET-MSG SECTION.                                                     
518200                                                                          
518300     MOVE '  QC' TO GODK-STATUSKODER                                      
518400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
518500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
518600     PERFORM IMS-STATUSKONTROLL                                           
518700     .                                                                    
518800     SKIP3                                                                
518900 IMS-INSERT-MSG SECTION.                                                  
519000                                                                          
519100     IF ENGLISH-TEXT                                                      
519200       MOVE 'N' TO MFS-KDHUVOMR                                           
519300     END-IF                                                               
519400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
519500     MOVE SPACE TO GODK-STATUSKODER                                       
519600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
519700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
519800     PERFORM IMS-STATUSKONTROLL                                           
519900     .                                                                    
520000     EJECT                                                                
520100 IMS-GU-WDK711 SECTION.                                                   
520200     MOVE 'IMS-GU-WDK711       ' TO  WS-IMS-SEKTION                       
520300                                                                          
520400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
520500          DELIMITED BY SIZE INTO SSA1                                     
520600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
520700          DELIMITED BY SIZE INTO SSA2                                     
520800     MOVE '  GE' TO GODK-STATUSKODER                                      
520900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
521000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
521100     PERFORM IMS-STATUSKONTROLL                                           
521200     .                                                                    
521300     SKIP3                                                                
521400 IMS-GU-WDK711-TRANS SECTION.                                             
521500     MOVE 'IMS-GU-WDK711-TRANS       ' TO  WS-IMS-SEKTION                 
521600                                                                          
521700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
521800          DELIMITED BY SIZE INTO SSA1                                     
521900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
522000          DELIMITED BY SIZE INTO SSA2                                     
522100     MOVE '  GE' TO GODK-STATUSKODER                                      
522200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711-TRANS              
522300                                    SSA1 SSA2                             
522400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
522500     PERFORM IMS-STATUSKONTROLL                                           
522600     .                                                                    
522700     SKIP3                                                                
522800 IMS-GHU-WDK711 SECTION.                                                  
522900     MOVE 'IMS-GHU-WDK711      ' TO  WS-IMS-SEKTION                       
523000                                                                          
523100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
523200          DELIMITED BY SIZE INTO SSA1                                     
523300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
523400          DELIMITED BY SIZE INTO SSA2                                     
523500     MOVE '  GE' TO GODK-STATUSKODER                                      
523600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2         
523700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
523800     PERFORM IMS-STATUSKONTROLL                                           
523900     .                                                                    
524000     SKIP3                                                                
524100 IMS-REPL-WDK711 SECTION.                                                 
524200                                                                          
524300     MOVE '  ' TO GODK-STATUSKODER                                        
524400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
524500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
524600     PERFORM IMS-STATUSKONTROLL                                           
524700     .                                                                    
524800     EJECT                                                                
524900 IMS-GU-WDK712 SECTION.                                                   
525000     MOVE 'IMS-GU-WDK712         '                                        
525100                                 TO WS-IMS-SEKTION                        
525200                                                                          
525300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
525400          DELIMITED BY SIZE INTO SSA1                                     
525500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
525600          DELIMITED BY SIZE INTO SSA2                                     
525700     MOVE '  GE'              TO GODK-STATUSKODER                         
525800     CALL CBLTDLI USING GU WDK7I-PCB DLI-IO-AREA-WDK712 SSA1 SSA2         
525900     MOVE WDK7I-STATUS-CODE    TO STATUS-WS                               
526000     PERFORM IMS-STATUSKONTROLL                                           
526100     .                                                                    
526200 IMS-GU-WDK727 SECTION.                                                   
526300     MOVE 'IMS-GU-WDK727 '  TO WS-IMS-SEKTION                             
526400                                                                          
526500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
526600          DELIMITED BY SIZE INTO SSA1                                     
526700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
526800          DELIMITED BY SIZE INTO SSA2                                     
526900     STRING 'WDK727  (KDSEGKEY =1)'                                       
527000          DELIMITED BY SIZE INTO SSA3                                     
527100     MOVE '  GE'            TO GODK-STATUSKODER                           
527200     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-AREA-WDK727                   
527300                        SSA1 SSA2 SSA3                                    
527400     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
527500     PERFORM IMS-STATUSKONTROLL                                           
527600     .                                                                    
527700     EJECT                                                                
527800                                                                          
527900 IMS-GU-WDD301-BSEQ SECTION.                                              
528000     MOVE 'IMS-GU-WDD301-BSEQ    '                                        
528100                                 TO WS-IMS-SEKTION                        
528200                                                                          
528300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
528400          DELIMITED BY SIZE INTO SSA1                                     
528500     MOVE '  GE' TO GODK-STATUSKODER                                      
528600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD301 SSA1               
528700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
528800     PERFORM IMS-STATUSKONTROLL                                           
528900     .                                                                    
529000     SKIP3                                                                
529100 IMS-GNP-WDD311 SECTION.                                                  
529200     MOVE 'IMS-GNP-WDD311        '                                        
529300                                 TO WS-IMS-SEKTION                        
529400                                                                          
529500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
529600          DELIMITED BY SIZE INTO SSA1                                     
529700     MOVE '  GE' TO GODK-STATUSKODER                                      
529800     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-WDD311 SSA1              
529900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
530000     PERFORM IMS-STATUSKONTROLL                                           
530100     .                                                                    
530200     EJECT                                                                
530300 IMS-GHU-WDE301 SECTION.                                                  
530400     MOVE 'IMS-GHU-WDE301        '                                        
530500                                 TO WS-IMS-SEKTION                        
530600                                                                          
530700     STRING 'WDE301  (WDE301KY =' W-WDE301KY-X ')'                        
530800          DELIMITED BY SIZE INTO SSA1                                     
530900     MOVE '  ' TO GODK-STATUSKODER                                        
531000     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-AREA-WDE301 SSA1              
531100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
531200     PERFORM IMS-STATUSKONTROLL                                           
531300     .                                                                    
531400     SKIP3                                                                
531500 IMS-GU-WDE301 SECTION.                                                   
531600     MOVE 'IMS-GU-WDE301        '                                         
531700                                 TO WS-IMS-SEKTION                        
531800                                                                          
531900     STRING 'WDE301  (WDE301KY =' W-WDE301KY-X ')'                        
532000          DELIMITED BY SIZE INTO SSA1                                     
532100     MOVE '  ' TO GODK-STATUSKODER                                        
532200     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
532300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
532400     PERFORM IMS-STATUSKONTROLL                                           
532500     .                                                                    
532600     SKIP3                                                                
532700 IMS-GU-WDE3-TRAN SECTION.                                                
532800     MOVE 'IMS-GU-WDE3-TRAN   ' TO  WS-IMS-SEKTION                        
532900                                                                          
533000     STRING 'WDE301  (WDE301KY =' W-WDE301KY-X ')'                        
533100          DELIMITED BY SIZE INTO SSA1                                     
533200     MOVE '  GE' TO GODK-STATUSKODER                                      
533300     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
533400     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
533500     PERFORM IMS-STATUSKONTROLL                                           
533600     .                                                                    
533700     SKIP3                                                                
533800 IMS-GU-WDE3B1-MIN-MAX SECTION.                                           
533900     MOVE 'IMS-GU-WDE3B1-MIN-MAX '                                        
534000                                 TO WS-IMS-SEKTION                        
534100                                                                          
534200     STRING 'WDE3B1  (WDE3B1KY>=' W-WDE3B1KY-MIN-X                        
534300                    '&WDE3B1KY<=' W-WDE3B1KY-MAX-X ')'                    
534400          DELIMITED BY SIZE INTO SSA1                                     
534500     MOVE '  GE' TO GODK-STATUSKODER                                      
534600     CALL CBLTDLI USING GU WDE3B-PCB DLI-IO-AREA-WDE3B1 SSA1              
534700     MOVE WDE3B-STATUS-CODE TO STATUS-WS                                  
534800     PERFORM IMS-STATUSKONTROLL                                           
534900     .                                                                    
535000     SKIP3                                                                
535100 IMS-GN-WDE3B1-MIN-MAX SECTION.                                           
535200     MOVE 'IMS-GN-WDE3B1-MIN-MAX '                                        
535300                                 TO WS-IMS-SEKTION                        
535400                                                                          
535500     STRING 'WDE3B1  (WDE3B1KY>=' W-WDE3B1KY-MIN-X                        
535600                    '&WDE3B1KY<=' W-WDE3B1KY-MAX-X ')'                    
535700          DELIMITED BY SIZE INTO SSA1                                     
535800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
535900     CALL CBLTDLI USING GN WDE3B-PCB DLI-IO-AREA-WDE3B1 SSA1              
536000     MOVE WDE3B-STATUS-CODE TO STATUS-WS                                  
536100     PERFORM IMS-STATUSKONTROLL                                           
536200     .                                                                    
536300     SKIP3                                                                
536400 IMS-GU-WDE301-MIN-MAX SECTION.                                           
536500     MOVE 'IMS-GU-WDE301-MIN-MAX '                                        
536600                                 TO WS-IMS-SEKTION                        
536700                                                                          
536800     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
536900                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
537000          DELIMITED BY SIZE INTO SSA1                                     
537100     MOVE '  GE' TO GODK-STATUSKODER                                      
537200     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
537300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
537400     PERFORM IMS-STATUSKONTROLL                                           
537500     .                                                                    
537600     SKIP3                                                                
537700 IMS-GN-WDE301-MIN-MAX SECTION.                                           
537800     MOVE 'IMS-GN-WDE301-MIN-MAX '                                        
537900                                 TO WS-IMS-SEKTION                        
538000                                                                          
538100     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
538200                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
538300          DELIMITED BY SIZE INTO SSA1                                     
538400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
538500     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
538600     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
538700     PERFORM IMS-STATUSKONTROLL                                           
538800     .                                                                    
538900     SKIP3                                                                
539000 IMS-GU-WDE3B1-PF8 SECTION.                                               
539100     MOVE 'IMS-GU-WDE3B1-PF8     '                                        
539200                                 TO WS-IMS-SEKTION                        
539300                                                                          
539400     STRING 'WDE3B1  (WDE3B1KY >' W-WDE3B1KY-MIN-X                        
539500                    '&WDE3B1KY<=' W-WDE3B1KY-MAX-X ')'                    
539600          DELIMITED BY SIZE INTO SSA1                                     
539700     MOVE '  GE' TO GODK-STATUSKODER                                      
539800     CALL CBLTDLI USING GU WDE3B-PCB DLI-IO-AREA-WDE3B1 SSA1              
539900     MOVE WDE3B-STATUS-CODE TO STATUS-WS                                  
540000     PERFORM IMS-STATUSKONTROLL                                           
540100     .                                                                    
540200     SKIP3                                                                
540300 IMS-GN-WDE3B1-PF8 SECTION.                                               
540400     MOVE 'IMS-GN-WDE3B1-PF8     '                                        
540500                                 TO WS-IMS-SEKTION                        
540600                                                                          
540700     STRING 'WDE3B1  (WDE3B1KY >' W-WDE3B1KY-MIN-X                        
540800                    '&WDE3B1KY<=' W-WDE3B1KY-MAX-X ')'                    
540900          DELIMITED BY SIZE INTO SSA1                                     
541000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
541100     CALL CBLTDLI USING GN WDE3B-PCB DLI-IO-AREA-WDE3B1 SSA1              
541200     MOVE WDE3B-STATUS-CODE TO STATUS-WS                                  
541300     PERFORM IMS-STATUSKONTROLL                                           
541400     .                                                                    
541500     SKIP3                                                                
541600 IMS-ISRT-WDE301 SECTION.                                                 
541700     MOVE 'IMS-ISRT-WDE301       '                                        
541800                                 TO WS-IMS-SEKTION                        
541900                                                                          
542000     MOVE 'WDE301   ' TO SSA1                                             
542100     MOVE '  ' TO GODK-STATUSKODER                                        
542200     CALL CBLTDLI USING ISRT WDE3-PCB DLI-IO-AREA-WDE301 SSA1             
542300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
542400     PERFORM IMS-STATUSKONTROLL                                           
542500     .                                                                    
542600     SKIP3                                                                
542700 IMS-REPL-WDE301 SECTION.                                                 
542800     MOVE 'IMS-REPL-WDE301       '                                        
542900                                 TO WS-IMS-SEKTION                        
543000                                                                          
543100     MOVE '  ' TO GODK-STATUSKODER                                        
543200     CALL CBLTDLI USING REPL WDE3-PCB DLI-IO-AREA-WDE301                  
543300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
543400     PERFORM IMS-STATUSKONTROLL                                           
543500     .                                                                    
543600     EJECT                                                                
543700 IMS-DLET-WDE301 SECTION.                                                 
543800     MOVE 'IMS-DLET-WDE301       '                                        
543900                                 TO WS-IMS-SEKTION                        
544000                                                                          
544100     MOVE '  ' TO GODK-STATUSKODER                                        
544200     CALL CBLTDLI USING DLET WDE3-PCB DLI-IO-AREA-WDE301                  
544300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
544400     PERFORM IMS-STATUSKONTROLL                                           
544500     .                                                                    
544600     SKIP3                                                                
544700 IMS-GU-WDK601 SECTION.                                                   
544800     MOVE 'IMS-GU-WDK601         '                                        
544900                                 TO WS-IMS-SEKTION                        
545000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
545100          DELIMITED BY SIZE INTO SSA1                                     
545200     MOVE '  GE' TO GODK-STATUSKODER                                      
545300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
545400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
545500     PERFORM IMS-STATUSKONTROLL                                           
545600     .                                                                    
545700     SKIP3                                                                
545800 IMS-GU-WDK611 SECTION.                                                   
545900     MOVE 'IMS-GU-WDK611         '                                        
546000                                 TO WS-IMS-SEKTION                        
546100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
546200          DELIMITED BY SIZE INTO SSA1                                     
546300     MOVE 'WDK611  '       TO SSA2                                        
546400     MOVE '  GE' TO GODK-STATUSKODER                                      
546500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
546600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
546700     PERFORM IMS-STATUSKONTROLL                                           
546800     .                                                                    
546900     SKIP3                                                                
547000 IMS-GNP-WDK611 SECTION.                                                  
547100     MOVE 'IMS-GNP-WDK611        '                                        
547200                                 TO WS-IMS-SEKTION                        
547300     MOVE 'WDK611  '       TO SSA1                                        
547400     MOVE '  GE' TO GODK-STATUSKODER                                      
547500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
547600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
547700     PERFORM IMS-STATUSKONTROLL                                           
547800     .                                                                    
547900     EJECT                                                                
548000 IMS-GNP-WDK621-FIRST SECTION.                                            
548100     MOVE 'IMS-GNP-WDK621        '                                        
548200                                 TO WS-IMS-SEKTION                        
548300     STRING 'WDK621  *F(DAPRLIST >' W-DAPRLIST-21-N                       
548400                      '&IDLEVNR  =' W-IDLEVNR-21-X ')'                    
548500          DELIMITED BY SIZE INTO SSA1                                     
548600     MOVE '  GE' TO GODK-STATUSKODER                                      
548700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
548800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
548900     PERFORM IMS-STATUSKONTROLL                                           
549000     SKIP3                                                                
549100     .                                                                    
549200     EJECT                                                                
549300 IMS-GNP-WDK621 SECTION.                                                  
549400     STRING 'WDK621  (DAPRLIST >' W-DAPRLIST-21-N                         
549500                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
549600          DELIMITED BY SIZE INTO SSA1                                     
549700     MOVE '  GE' TO GODK-STATUSKODER                                      
549800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
549900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
550000     PERFORM IMS-STATUSKONTROLL                                           
550100     SKIP3                                                                
550200     .                                                                    
550300     EJECT                                                                
550400 IMS-GU-WDN601 SECTION.                                                   
550500     MOVE 'IMS-GU-WDN601         '                                        
550600                                 TO WS-IMS-SEKTION                        
550700     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
550800            DELIMITED BY SIZE INTO SSA1                                   
550900     MOVE '  GE' TO GODK-STATUSKODER                                      
551000     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-AREA-WDN601 SSA1               
551100     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
551200     PERFORM IMS-STATUSKONTROLL                                           
551300     .                                                                    
551400     SKIP3                                                                
551500 IMS-GNP-WDN611 SECTION.                                                  
551600     MOVE 'IMS-GNP-WDN611        '                                        
551700                                 TO WS-IMS-SEKTION                        
551800     MOVE 'WDN611   ' TO SSA1                                             
551900     MOVE '  GE' TO GODK-STATUSKODER                                      
552000     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-AREA-WDN611 SSA1              
552100     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
552200     PERFORM IMS-STATUSKONTROLL                                           
552300     .                                                                    
552400     EJECT                                                                
552500 IMS-GU-WDD701 SECTION.                                                   
552600     MOVE 'IMS-GU-WDD701         '                                        
552700                                 TO WS-IMS-SEKTION                        
552800     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
552900            DELIMITED BY SIZE INTO SSA1                                   
553000     MOVE '  GE' TO GODK-STATUSKODER                                      
553100     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-AREA-WDD701 SSA1               
553200     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
553300     PERFORM IMS-STATUSKONTROLL                                           
553400     .                                                                    
553500     SKIP2                                                                
553600 IMS-GNP-WDD702 SECTION.                                                  
553700     MOVE 'IMS-GNP-WDD702        '                                        
553800                                 TO WS-IMS-SEKTION                        
553900     STRING 'WDD702  (FLTEXT   =N)'                                       
554000            DELIMITED BY SIZE INTO SSA1                                   
554100     MOVE '  GE' TO GODK-STATUSKODER                                      
554200     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-AREA-WDD702 SSA1              
554300     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
554400     PERFORM IMS-STATUSKONTROLL                                           
554500     .                                                                    
554600     SKIP2                                                                
554700 IMS-GU-WDD7A1-MINMAX SECTION.                                            
554800     MOVE 'IMS-GU-WDD7A1-MINMAX  '                                        
554900                                 TO WS-IMS-SEKTION                        
555000     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
555100                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
555200            DELIMITED BY SIZE INTO SSA1                                   
555300     MOVE '  GE' TO GODK-STATUSKODER                                      
555400     CALL CBLTDLI USING GU WDD7A1-PCB DLI-IO-AREA-WDD7A1 SSA1             
555500     MOVE WDD7A1-STATUS-CODE TO STATUS-WS                                 
555600     PERFORM IMS-STATUSKONTROLL                                           
555700     .                                                                    
555800     EJECT                                                                
555900 IMS-GN-WDD7A1-MINMAX SECTION.                                            
556000     MOVE 'IMS-GN-WDD7A1-MINMAX  '                                        
556100                                 TO WS-IMS-SEKTION                        
556200     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
556300                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
556400            DELIMITED BY SIZE INTO SSA1                                   
556500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
556600     CALL CBLTDLI USING GN WDD7A1-PCB DLI-IO-AREA-WDD7A1 SSA1             
556700     MOVE WDD7A1-STATUS-CODE TO STATUS-WS                                 
556800     PERFORM IMS-STATUSKONTROLL                                           
556900     .                                                                    
557000     EJECT                                                                
557100 IMS-GU-WDL711          SECTION.                                          
557200     MOVE 'IMS-GU-WDL711         '                                        
557300                                 TO WS-IMS-SEKTION                        
557400     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
557500          DELIMITED BY SIZE INTO SSA1                                     
557600     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
557700          DELIMITED BY SIZE INTO SSA2                                     
557800     MOVE '  GE' TO GODK-STATUSKODER                                      
557900     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-WDL711 SSA1 SSA2          
558000     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
558100     PERFORM IMS-STATUSKONTROLL                                           
558200     .                                                                    
558300     SKIP3                                                                
558400 IMS-GHU-WDL711 SECTION.                                                  
558500     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
558600          DELIMITED BY SIZE INTO SSA1                                     
558700     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
558800          DELIMITED BY SIZE INTO SSA2                                     
558900     MOVE '  GE' TO GODK-STATUSKODER                                      
559000     CALL CBLTDLI USING GHU WDL7-PCB DLI-IO-AREA-WDL711 SSA1 SSA2         
559100     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
559200     PERFORM IMS-STATUSKONTROLL                                           
559300     .                                                                    
559400     SKIP3                                                                
559500 IMS-REPL-WDL711 SECTION.                                                 
559600     MOVE 'IMS-REPL-WDL711       '                                        
559700                                 TO WS-IMS-SEKTION                        
559800                                                                          
559900     MOVE '  ' TO GODK-STATUSKODER                                        
560000     CALL CBLTDLI USING REPL WDL7-PCB DLI-IO-AREA-WDL711                  
560100     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
560200     PERFORM IMS-STATUSKONTROLL                                           
560300     .                                                                    
560400     EJECT                                                                
560500 IMS-GU-WDK901         SECTION.                                           
560600     MOVE 'IMS-GU-WDK901         '                                        
560700                                 TO WS-IMS-SEKTION                        
560800     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
560900            DELIMITED BY SIZE INTO SSA1                                   
561000     MOVE '  GE' TO GODK-STATUSKODER                                      
561100     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-WDK901 SSA1               
561200     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
561300     PERFORM IMS-STATUSKONTROLL                                           
561400     .                                                                    
561500     SKIP3                                                                
561600                                                                          
561700 IMS-GHU-WDK611 SECTION.                                                  
561800     MOVE 'IMS-GHU-WDK611        '                                        
561900                                 TO WS-IMS-SEKTION                        
562000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
562100          DELIMITED BY SIZE INTO SSA1                                     
562200     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
562300     MOVE '  GE' TO GODK-STATUSKODER                                      
562400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
562500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
562600     PERFORM IMS-STATUSKONTROLL                                           
562700     .                                                                    
562800     EJECT                                                                
562900                                                                          
563000 IMS-REPL-WDK6 SECTION.                                                   
563100     MOVE 'IMS-REPL-WDK611       '                                        
563200                                 TO WS-IMS-SEKTION                        
563300                                                                          
563400     MOVE '  ' TO GODK-STATUSKODER                                        
563500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
563600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
563700     PERFORM IMS-STATUSKONTROLL                                           
563800     .                                                                    
563900     EJECT                                                                
564000                                                                          
564100 IMS-GU-WDB601-REF SECTION.                                               
564200     MOVE 'IMS-GU-WDB601-REF     '                                        
564300                                 TO WS-IMS-SEKTION                        
564400     STRING 'WDB601  (IDDC     =' W-IDDC-REF-B6-X ')'                     
564500          DELIMITED BY SIZE INTO SSA1                                     
564600     MOVE '  GE' TO GODK-STATUSKODER                                      
564700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-2-B601 SSA1               
564800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
564900     PERFORM IMS-STATUSKONTROLL                                           
565000     .                                                                    
565100     EJECT                                                                
565200                                                                          
565300 IMS-GU-WDB601    SECTION.                                                
565400     MOVE 'IMS-GU-WDB601         '                                        
565500                                 TO WS-IMS-SEKTION                        
565600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
565700          DELIMITED BY SIZE INTO SSA1                                     
565800     MOVE '  GE' TO GODK-STATUSKODER                                      
565900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
566000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
566100     PERFORM IMS-STATUSKONTROLL                                           
566200     .                                                                    
566300     EJECT                                                                
566400                                                                          
566500 IMS-GN-WDB601    SECTION.                                                
566600     MOVE 'IMS-GN-WDB601         '                                        
566700                                 TO WS-IMS-SEKTION                        
566800     MOVE 'WDB601 ' TO SSA1                                               
566900     MOVE '  GB' TO GODK-STATUSKODER                                      
567000     CALL CBLTDLI USING GN WDB6-GN-PCB DLI-IO-AREA-B601 SSA1              
567100     MOVE WDB6-GN-STATUS-CODE    TO STATUS-WS                             
567200     PERFORM IMS-STATUSKONTROLL                                           
567300     .                                                                    
567400     EJECT                                                                
567500 IMS-GU-WDB616    SECTION.                                                
567600     MOVE 'IMS-GU-WDB616         '                                        
567700                                 TO WS-IMS-SEKTION                        
567800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
567900          DELIMITED BY SIZE INTO SSA1                                     
568000     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
568100          DELIMITED BY SIZE INTO SSA2                                     
568200     MOVE '  GE' TO GODK-STATUSKODER                                      
568300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
568400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
568500     PERFORM IMS-STATUSKONTROLL                                           
568600     .                                                                    
568700     SKIP2                                                                
568800 IMS-GU-WDL601 SECTION.                                                   
568900     MOVE 'IMS-GU-WDL601         '                                        
569000                                 TO WS-IMS-SEKTION                        
569100                                                                          
569200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
569300          DELIMITED BY SIZE INTO SSA1                                     
569400     MOVE '  GE' TO GODK-STATUSKODER                                      
569500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
569600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
569700     PERFORM IMS-STATUSKONTROLL                                           
569800     .                                                                    
569900     SKIP2                                                                
570000 IMS-GNP-WDL611 SECTION.                                                  
570100     MOVE 'IMS-GNP-WDL611        '                                        
570200                                 TO WS-IMS-SEKTION                        
570300                                                                          
570400     STRING 'WDL611     '                                                 
570500          DELIMITED BY SIZE INTO SSA1                                     
570600     MOVE '  GE' TO GODK-STATUSKODER                                      
570700     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
570800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
570900     PERFORM IMS-STATUSKONTROLL                                           
571000     .                                                                    
571100     SKIP2                                                                
571200 IMS-STATUSKONTROLL SECTION.                                              
571300                                                                          
571400     SET STATUS-IX TO 1                                                   
571500     SEARCH GODK-STATUS                                                   
571600       AT END                                                             
571700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
571800         DELIMITED BY SIZE INTO FELTEXT                                   
571900         CALL FELLOG                                                      
572000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
572100         CONTINUE                                                         
572200     END-SEARCH                                                           
572300     .                                                                    
572400     EJECT                                                                
572500                                                                          
572600 DB2-OPEN-TP5IDDC-CRS SECTION.                                            
572700     MOVE 'DB2-OPEN-TP5IDDC-CRS' TO  WS-DB2-SEKTION                       
572800                                                                          
572900     EXEC SQL DECLARE TP5IDDC-CRS CURSOR FOR                              
573000         SELECT  IDLOPNR_DC                                               
573100                ,IDDC                                                     
573200                                                                          
573300         FROM    TP5IDDC                                                  
573400                                                                          
573500         WHERE   IDLOPNR_DC = :WS-IDLOPNR-DC                              
573600                                                                          
573700         ORDER BY IDDC                                                    
573800                                                                          
573900     END-EXEC                                                             
574000                                                                          
574100     MOVE SQLCODE TO SQLCODE-WS                                           
574200     MOVE 000     TO GOOD-SQLCODECODES                                    
574300     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
574400     PERFORM DB2-STATUS-CHECK                                             
574500     .                                                                    
574600     EJECT                                                                
574700 DB2-FETCH-TP5IDDC-CRS SECTION.                                           
574800     MOVE 'DB2-FETCH-TP5IDDC-CRS' TO  WS-DB2-SEKTION                      
574900     MOVE 000100  TO GOOD-SQLCODECODES                                    
575000     EXEC SQL FETCH TP5IDDC-CRS INTO                                      
575100                :TP5IDDC-IDLOPNR-DC                                       
575200               ,:TP5IDDC-IDDC                                             
575300                                                                          
575400     END-EXEC                                                             
575500                                                                          
575600     MOVE SQLCODE TO SQLCODE-WS                                           
575700     PERFORM DB2-STATUS-CHECK                                             
575800     .                                                                    
575900     SKIP3                                                                
576000                                                                          
576100 DB2-CLOSE-TP5IDDC-CRS SECTION.                                           
576200     MOVE 'DB2-CLOSE-TP5IDDC-CRS' TO  WS-DB2-SEKTION                      
576300                                                                          
576400     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
576500     .                                                                    
576600     EJECT                                                                
576700                                                                          
576800 DB2-SELECT-TP5IDDC SECTION.                                              
576900     MOVE 'DB2-SELECT-TP5IDDC' TO  WS-DB2-SEKTION                         
577000     MOVE 000100  TO GOOD-SQLCODECODES                                    
577100                                                                          
577200     EXEC SQL                                                             
577300           SELECT IDLOPNR_DC                                              
577400                 ,IDDC                                                    
577500                                                                          
577600           INTO :TP5IDDC-IDLOPNR-DC                                       
577700               ,:TP5IDDC-IDDC                                             
577800                                                                          
577900           FROM   TP5IDDC                                                 
578000                                                                          
578100           WHERE IDDC = :WSA-IDDC                                         
578200     END-EXEC                                                             
578300     MOVE SQLCODE TO SQLCODE-WS                                           
578400     PERFORM DB2-STATUS-CHECK                                             
578500     .                                                                    
578600     EJECT                                                                
578700                                                                          
578800 DB2-SELECT-TP5COMM SECTION.                                              
578900     MOVE 'DB2-SELECT-TP5COMM' TO  WS-DB2-SEKTION                         
579000     MOVE 000100  TO GOOD-SQLCODECODES                                    
579100                                                                          
579200     EXEC SQL                                                             
579300           SELECT IDLOPNR_DC                                              
579400                 ,TEARTNOT                                                
579500                                                                          
579600           INTO :TP5COMM-IDLOPNR-DC                                       
579700               ,:TP5COMM-TEARTNOT                                         
579800                                                                          
579900           FROM   TP5COMM                                                 
580000                                                                          
580100           WHERE IDLOPNR_DC = :WS-IDLOPNR-DC                              
580200           AND   IDARTNR    = :W-IDARTNR                                  
580300     END-EXEC                                                             
580400     MOVE SQLCODE TO SQLCODE-WS                                           
580500     PERFORM DB2-STATUS-CHECK                                             
580600     .                                                                    
580700     EJECT                                                                
580800 DB2-INSERT-TP5COMM  SECTION.                                             
580900     MOVE 'DB2-INSERT-TP5COMM   ' TO  WS-DB2-SEKTION                      
581000     SKIP2                                                                
581100     MOVE 000     TO GOOD-SQLCODECODES                                    
581200     EXEC SQL                                                             
581300         INSERT INTO TP5COMM                                              
581400            (IDLOPNR_DC,IDARTNR,TEARTNOT)                                 
581500         VALUES                                                           
581600            (:TP5COMM-IDLOPNR-DC                                          
581700            ,:TP5COMM-IDARTNR                                             
581800            ,:TP5COMM-TEARTNOT)                                           
581900     END-EXEC                                                             
582000                                                                          
582100     MOVE SQLCODE TO SQLCODE-WS                                           
582200     PERFORM DB2-STATUS-CHECK                                             
582300     .                                                                    
582400     EJECT                                                                
582500 DB2-UPDATE-TP5COMM  SECTION.                                             
582600     MOVE 'DB2-UPDATE-TP5COMM   ' TO  WS-DB2-SEKTION                      
582700                                                                          
582800     MOVE 000     TO GOOD-SQLCODECODES                                    
582900     EXEC SQL                                                             
583000         UPDATE TP5COMM                                                   
583100             SET IDLOPNR_DC = :TP5COMM-IDLOPNR-DC                         
583200               , TEARTNOT   = :TP5COMM-TEARTNOT                           
583300                                                                          
583400         WHERE IDLOPNR_DC = :WS-IDLOPNR-DC                                
583500         AND   IDARTNR    = :W-IDARTNR                                    
583600     END-EXEC                                                             
583700                                                                          
583800     MOVE SQLCODE TO SQLCODE-WS                                           
583900     PERFORM DB2-STATUS-CHECK                                             
584000     .                                                                    
584100     EJECT                                                                
584200 DB2-SELECT-TP4TRAN     SECTION.                                          
584300     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
584400                                                                          
584500     MOVE 000100  TO GOOD-SQLCODECODES                                    
584600                                                                          
584700     EXEC SQL                                                             
584800           SELECT  KDARBTYP                                               
584900                  ,IDDC_SEND                                              
585000                  ,IDDC_REC                                               
585100                  ,IDDISTR                                                
585200                  ,IDKUNDNR                                               
585300                                                                          
585400           INTO   :TP4TRAN-KDARBTYP                                       
585500                 ,:TP4TRAN-IDDC-SEND                                      
585600                 ,:TP4TRAN-IDDC-REC                                       
585700                 ,:TP4TRAN-IDDISTR                                        
585800                 ,:TP4TRAN-IDKUNDNR                                       
585900                                                                          
586000           FROM    TP4TRAN                                                
586100                                                                          
586200           WHERE KDARBTYP  = :WS-KDARBTYP-X3                              
586300           AND   IDDC_SEND = :WS-IDDC-SEND                                
586400           AND   IDDC_REC  = :WS-IDDC-REC                                 
586500     END-EXEC                                                             
586600                                                                          
586700     MOVE SQLCODE TO SQLCODE-WS                                           
586800     PERFORM DB2-STATUS-CHECK                                             
586900     .                                                                    
587000     EJECT                                                                
587100 DB2-SELECT-TP4TRAN-2   SECTION.                                          
587200     MOVE 'DB2-SELECT-TP4TRAN-2 ' TO  WS-DB2-SEKTION                      
587300                                                                          
587400     MOVE 000100  TO GOOD-SQLCODECODES                                    
587500                                                                          
587600     EXEC SQL                                                             
587700           SELECT  KDARBTYP                                               
587800                  ,IDDC_SEND                                              
587900                  ,IDDC_REC                                               
588000                  ,IDDISTR                                                
588100                  ,IDKUNDNR                                               
588200                                                                          
588300           INTO   :TP4TRAN-KDARBTYP                                       
588400                 ,:TP4TRAN-IDDC-SEND                                      
588500                 ,:TP4TRAN-IDDC-REC                                       
588600                 ,:TP4TRAN-IDDISTR                                        
588700                 ,:TP4TRAN-IDKUNDNR                                       
588800                                                                          
588900           FROM    TP4TRAN                                                
589000                                                                          
589100           WHERE KDARBTYP  = :WS-KDARBTYP-X3                              
589200           AND   IDDC_REC  = :REF-IDDC                                    
589300           AND   IDDISTR   = :REF-IDDISTR                                 
589400           AND   IDKUNDNR  = :REF-IDKUNDNR                                
589500     END-EXEC                                                             
589600                                                                          
589700     MOVE SQLCODE TO SQLCODE-WS                                           
589800     PERFORM DB2-STATUS-CHECK                                             
589900     .                                                                    
590000     EJECT                                                                
590100 DB2-STATUS-CHECK  SECTION.                                               
590200                                                                          
590300     SET SQLCODE-IX TO 1                                                  
590400     SEARCH GOOD-SQLCODE                                                  
590500       AT END                                                             
590600*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
590700*         DELIMITED BY SIZE INTO ERROR-TEXT                               
590800          CALL ABEND USING RKOD-ABEND-DB2                                 
590900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
591000     END-SEARCH                                                           
591100     .                                                                    
591200     EJECT                                                                
591300*    -COPY WY2000P1                                                       
