000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2721000.                                                
000400 AUTHOR.         JOHAN NIHLBLAD.                                          
000500 DATE-WRITTEN.   NOV-15.                                                  
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*        PROGRAMMET KÖRS VID OLIKA TIDPUNKTER FÖR OLIKA DC'ER             
001300*                                                                         
001400*        LÄSER WDK7 FÖR AKTUELLT DC.                                      
001500*        SAMLAR DATA FRÅN WDL7 WDK6 WDK9 WDQ4B WDQ2 WDE4                  
001600*        BEDÖMMER OM REFILL BEHÖVS FÖR VARJE ARTIKEL                      
001700*        I AKTUELLT DC                                                    
001800*        BERÄKNAR REFILLKVANTITET                                         
001900*        SKAPAR REFILLORDER VID BRIST I DC                                
002000*                                                                         
002100*                                                                         
002200*        PROGRAMMET LÄSER      WLARTC (WDE4)                              
002300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002400*        PROGRAMMET LÄSER      WLOIGA (WDL7)                              
002500*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
002600*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
002700*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
002800*                                                                         
002900*    TEST-TIP: WHEN TESTING, BE SURE TO RUN ROUTINE W222V2                
003000*              OR W222D1 (WHICH CONTAIN PROGRAMS W22232 AND               
003100*              W22233), BEFORE RE-RUNNING THIS PROGRAM AND                
003200*              ROUTINE. OTHERWISE CLAG-KVTILLG-TOT WON'T BE               
003300*              UPDATED WHICH MAY CAUSE PROPOSALS TO BE                    
003400*              CALCULATED INCORRECTLY.                                    
003500*                                                                         
003600*    ABENDKODER:                                                          
003700*        U0016 -  . . . .                                                 
003800*        U1000 -  . . . .                                                 
003900*                                                                         
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP2                                                                
004400 INPUT-OUTPUT SECTION.                                                    
004500                                                                          
004600 FILE-CONTROL.                                                            
004700     SKIP2                                                                
004800*         ---DC ATT KONTROLLERA I DENNA KÖRNING                           
004900     SELECT W272DC           ASSIGN W27210D1.                             
005000                                                                          
005100*          --- REFILLORDER / REFILLFÖRSLAG                                
005200     SELECT W27210                     ASSIGN TO W27210D2.                
005300                                                                          
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600                                                                          
005700 FILE SECTION.                                                            
005800 FD  W272DC                                                               
005900     LABEL RECORD STANDARD                                                
006000     RECORDING F                                                          
006100     BLOCK CONTAINS 0.                                                    
006200 01  FILLER                  PIC X(80).                                   
006300                                                                          
006400 FD  W27210                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700*01  POST -COPY W27111 -PRE  W27110-  -L.                                 
006800                                                                          
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100*    -COPY WY2000W2                                                       
007200     SKIP3                                                                
007300*    -COPY WY2000W1                                                       
007400     SKIP3                                                                
007500 01  FILLER                    PIC X(24)  VALUE 'WORKING STORAGE'.        
007600 77  IDPGM                       PIC X(8)    VALUE 'W2721000'.            
007700 77  JA                          PIC X       VALUE 'J'.                   
007800 77  NEJ                         PIC X       VALUE 'N'.                   
007900                                                                          
008000 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR'.              
008100                                                                          
008200 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-INDEX'.        
008300 01  ARBETSAREOR-INDEX.                                                   
008400                                                                          
008500                                                                          
008600     03 IX                      PIC S9(9)   VALUE ZERO COMP-3.            
008700     03 IX-2                    PIC S9(4)   VALUE ZERO COMP-3.            
008800     03 IX-3                    PIC S9(4)   VALUE ZERO COMP-3.            
008900     03 SUB                     PIC 9(1)    VALUE ZERO.                   
009000     03 VECKO-IX                PIC S9(2)   VALUE ZERO COMP-3.            
009100                                                                          
009200     03 DC-IX                   PIC 9(2)    VALUE ZERO.                   
009300     03 GRP-IX                  PIC 9(5)    VALUE ZERO.                   
009400     03 IDLOPNR-IX PIC 9(5) VALUE ZERO.                                   
009500     03 IDLOPNR-MAX-GRP         PIC 9(3)    VALUE 30.                     
009600     03 DC-MAX-GRP              PIC 9(3)    VALUE 6.                      
009700                                                                          
009800                                                                          
009900 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-TIDER'.        
010000 01  ARBETSAREOR-TIDER.                                                   
010100                                                                          
010200     03 W-DAGENS-DATUM           PIC 9(8)       VALUE ZERO.               
010300                                                                          
010400     03 DAGENS-TIAAVVD           PIC 9(5)       VALUE ZERO.               
010500     03 DAGENS-TIAAVVD-GRP       REDEFINES DAGENS-TIAAVVD.                
010600        05 DAGENS-TIAAVV         PIC 9(4).                                
010700        05 DAGENS-TID            PIC 9(1).                                
010800                                                                          
010900     03 DD-LT-FRAMAT-TIAAVVD      PIC 9(5)       VALUE ZERO.              
011000     03 DD-LT-FRAMAT-TIAAVVD-GRP REDEFINES DD-LT-FRAMAT-TIAAVVD.          
011100        05 DD-LT-FRAMAT-TIAAVV     PIC 9(4).                              
011200        05 DD-LT-FRAMAT-TID        PIC 9(1).                              
011300                                                                          
011400     03 W26-V-FRAMAT-TIAAVVD      PIC 9(5)       VALUE ZERO.              
011500     03 W26-V-FRAMAT-TIAAVVD-GRP REDEFINES W26-V-FRAMAT-TIAAVVD.          
011600        05 W26-V-FRAMAT-TIAAVV     PIC 9(4).                              
011700        05 W26-V-FRAMAT-TID        PIC 9(1).                              
011800                                                                          
011900     03 WS-TIAAVVD               PIC 9(5)       VALUE ZERO.               
012000     03 WS-TIAAVVD-GRP           REDEFINES WS-TIAAVVD.                    
012100        05 WS-TIAAVV             PIC 9(4).                                
012200        05 WS-TID                PIC 9(1).                                
012300                                                                          
012400     03 WS-INNEV-TIAARP          PIC 9(4)       VALUE ZERO.               
012500     03 WS-INNEV-TIAARP-DELAR    REDEFINES WS-INNEV-TIAARP.               
012600        05 WS-INNEV-TIAA         PIC 9(2).                                
012700        05 WS-INNEV-TIRP         PIC 9(2).                                
012800                                                                          
012900     03 WS-NASTA-TIAARP          PIC 9(4)       VALUE ZERO.               
013000     03 WS-NASTA-TIAARP-DELAR    REDEFINES WS-NASTA-TIAARP.               
013100        05 WS-NASTA-TIAA         PIC 9(2).                                
013200        05 WS-NASTA-TIRP         PIC 9(2).                                
013300                                                                          
013400     03 WS-NASTA-PER-TIAAMMDD    PIC 9(6)   VALUE ZERO.                   
013500     03 DAGENS-VECKA             PIC 9(2)   VALUE ZERO.                   
013600     03 WS-KVVIPER               PIC 9(1)   VALUE ZERO.                   
013700     03 WS-DAG-I-VECKA           PIC 9(1)   VALUE ZERO.                   
013800     03 WS-INNEV-VECKA           PIC 9(2)   VALUE ZERO.                   
013900     03 WS-FIRST-VV-RPER         PIC 9(2)   VALUE ZERO.                   
014000     03 WS-VECKA                 PIC 9(2)   VALUE ZERO.                   
014100     03 WS-WEEKBY2-QUOTIENT      PIC 9(2)   VALUE ZERO.                   
014200     03 WS-WEEKBY2-REMAINDER     PIC 9(2)   VALUE ZERO.                   
014300     03 WS-ERSDAT-AND-LT         PIC 9(5)   VALUE ZERO.                   
014400     03 WS-PREL-ERSDAT           PIC 9(5)   VALUE ZERO.                   
014500     03 WS-AAMMDD-ERSDAT         PIC 9(6)   VALUE ZERO.                   
014600     03 WS-DAT-AIRREQ            PIC 9(6)   VALUE ZERO.                   
014700     03 WS-DAPUBL-AAR            PIC 9(4)   VALUE ZERO.                   
014800     03 WS-DAGENS-AAR            PIC 9(4)   VALUE ZERO.                   
014900     03 WS-DAGENS-AAR-MINUS1     PIC 9(4)   VALUE ZERO.                   
015000     03 PERIOD-VECKA             PIC 9(1)   VALUE ZERO.                   
015100     03 WS-RESEASON-PLAN         PIC S9V9(2)     VALUE ZERO               
015200                                   OCCURS 12.                             
015300                                                                          
015400     03 WS-FLYGT-MAX-TIAAMMDD    PIC 9(6)   VALUE ZERO.                   
015500     03 WS-FLYGBEHOV-MAX-TIAARP  PIC 9(4)   VALUE ZERO.                   
015600     03 WS-FIRST-TIBERANK        PIC 9(6)  VALUE ZERO.                    
015700     03 WS-FLYGBEHOV-MAX-TIAAMMDD                                         
015800                                 PIC 9(6) VALUE ZERO.                     
015900     03 WS-TIFINLV               PIC S9(5) VALUE ZERO COMP-3.             
016000     03 WS-DAPUBL                PIC 9(8)  VALUE ZERO.                    
016100     03 WS-TIERSDAT-PREL-C1      PIC 9(5)  VALUE ZERO.                    
016200                                                                          
016300     03 WS-KDERS                 PIC 9(3)    VALUE ZEROES.                
016400                                                                          
016500     03 WS-LTID-A                PIC S9(3) VALUE ZERO COMP-3.             
016600     03 WS-LTID-B                PIC S9(3) VALUE ZERO COMP-3.             
016700     03 WS-LT-WEEKS              PIC S9(3) VALUE ZERO COMP-3.             
016800     03 WS-REST-DAYS             PIC  9(2) VALUE ZERO.                    
016900                                                                          
017000 01 WS-JMFR2-AAAAMMDD      PIC 9(8).                                      
017100 01 WS-JMFR-AAAAMMDD       PIC 9(8).                                      
017200 01 FILLER REDEFINES WS-JMFR-AAAAMMDD.                                    
017300     03 FILLER                PIC 9(2).                                   
017400     03 WS-JMFR-AA            PIC 9(2).                                   
017500     03 FILLER                PIC 9(4).                                   
017600                                                                          
017700 01  WS-DATUM                    PIC 9(8)    VALUE ZERO.                  
017800 01  FILLER   REDEFINES WS-DATUM.                                         
017900     03 WS-DATUM-SEKEL           PIC 9(2).                                
018000     03 WS-DATUM-AAR             PIC 9(2).                                
018100     03 WS-DATUM-MAN             PIC 9(2).                                
018200     03 WS-DATUM-DAG             PIC 9(2).                                
018300                                                                          
018400 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-ALLM'.         
018500 01  ARBETSAREOR-ALLM.                                                    
018600                                                                          
018700     03 WS-SPAR-IDARTNR          PIC S9(9) VALUE ZERO COMP-3.             
018800     03 WS-SPAR-IDARTNR-NUM      PIC  9(9) VALUE ZERO.                    
018900     03 WS-IDDISTR               PIC 9(5)  VALUE ZERO.                    
019000     03 WS-NEXT-WORKDAY          PIC 9(6)       VALUE ZERO.               
019100     03 WS-RA-TILLGANG          PIC S9(7)   VALUE ZERO COMP-3.            
019200     03 WS-KVSPARR-KVAL         PIC S9(7)   VALUE ZERO COMP-3.            
019300     03 WS-KVQPACK-3            PIC S9(5)   VALUE ZERO COMP-3.            
019400     03 WS-KVBEART-Q-RUP        PIC S9(7)   VALUE ZERO COMP-3.            
019500                                                                          
019600     03 DC-PARAMETER-DELAR.                                               
019700        05 FILLER                PIC X(2)  VALUE SPACE.                   
019800        05 DC-PARAMETER-LAGER    PIC X(2)  VALUE SPACE.                   
019900                                                                          
020000     03 FILLER               PIC X(8)      VALUE 'KVREFPKT'.              
020100     03 WS-KVREFPKT-DC       PIC S9(7)     VALUE ZERO COMP-3.             
020200                                                                          
020300     03 WS-PRIS              PIC S9(7)V9(2) VALUE ZERO COMP-3.            
020400     03 FILLER                   PIC X(16)   VALUE                        
020500                                             'WS-DB2-SEKTION'.            
020600     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
020700     03 WS-MEDD-ERS.                                                      
020800       05 WS-MEDD-ERSKOD         PIC 9(2)    VALUE ZERO.                  
020900       05 FILLER                 PIC X(1)    VALUE SPACE.                 
021000       05 WS-MEDD-TEXT           PIC X(23)   VALUE SPACE.                 
021100     03 WS-FLERSATT-X2X3         PIC X(1)    VALUE SPACE.                 
021200                                                                          
021300                                                                          
021400 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-ANTAL'.        
021500 01  ARBETSAREOR-ANTAL.                                                   
021600                                                                          
021700     03 FIX-ANTAL              PIC S9(7) VALUE ZERO COMP-3.               
021800     03 WS-ANTAL-GODK-E3       PIC S9(7) VALUE ZERO COMP-3.               
021900     03 WS-ANTAL-RO-RESS-KL1-CDC PIC S9(7) VALUE ZERO COMP-3.             
022000     03 WS-ANTAL-BER           PIC S9(7) VALUE ZERO COMP-3.               
022100     03 WS-ANTAL-VID-ERS       PIC S9(7) VALUE ZERO COMP-3.               
022200     03 WS-ANTAL-QX            PIC S9(7) VALUE ZERO COMP-3.               
022300     03 WS-ANTAL-SLUT          PIC S9(7) VALUE ZERO COMP-3.               
022400     03 WS-KVOKS-DAG           PIC S9(7) VALUE ZERO COMP-3.               
022500     03 WS-KVOKS-BULK          PIC S9(7) VALUE ZERO COMP-3.               
022600     03 WS-KVOKS-VOR           PIC S9(7) VALUE ZERO COMP-3.               
022700     03 WS-BALANCE-CDC         PIC S9(7) VALUE ZERO COMP-3.               
022800                                                                          
022900     03 WS-ANTAL-LEDT-FLYG     PIC S9(7)V9(1) VALUE ZERO COMP-3.          
023000     03 WS-ANTAL-FLYG          PIC S9(7)V9(1) VALUE ZERO COMP-3.          
023100                                                                          
023200     03 WS-ANTAL-FLYG-AVRUND       PIC  9(7)V9(1) VALUE ZERO.             
023300     03 WS-ANTAL-FLYG-AVRUND-DELAR                                        
023400                         REDEFINES WS-ANTAL-FLYG-AVRUND.                  
023500        05 WS-ANTAL-FLYG-AVRUND-HELTAL PIC 9(7).                          
023600        05 WS-ANTAL-FLYG-AVRUND-DECTAL PIC 9(1).                          
023700                                                                          
023800     03 WS-ANTAL-TILL-ERS      PIC S9(7)V9(1) VALUE ZERO COMP-3.          
023900                                                                          
024000     03 WS-ANTAL-TILL-ERS-AVRUND   PIC  9(7)V9(1) VALUE ZERO.             
024100     03 WS-ANTAL-TILL-ERS-AVRUND-DELAR                                    
024200                         REDEFINES WS-ANTAL-TILL-ERS-AVRUND.              
024300        05 WS-ANTAL-TILL-ERS-AVRUND-HEL PIC 9(7).                         
024400        05 WS-ANTAL-TILL-ERS-AVRUND-DEC PIC 9(1).                         
024500                                                                          
024600     03 WS-SUANTAL-ETA         PIC S9(7) VALUE ZERO COMP-3.               
024700     03 WS-KVDAGAR-FLYG-INNEV  PIC S9(2) VALUE ZERO COMP-3.               
024800     03 WS-KVDAGAR-FLYG-NASTA  PIC S9(3) VALUE ZERO COMP-3.               
024900                                                                          
025000     03 WS-1V-BEHOV-CDC      PIC  9(7)V9(2) VALUE ZERO.                   
025100     03 WS-1V-BEHOV-CDC-DELAR  REDEFINES WS-1V-BEHOV-CDC.                 
025200        05 WS-1V-BEHOV-CDC-HELTAL PIC 9(7).                               
025300        05 WS-1V-BEHOV-CDC-DECTAL PIC 9(2).                               
025400                                                                          
025500                                                                          
025600 01  FILLER                  PIC X(24) VALUE 'ARBETSAREOR-TILLG'.         
025700 01  ARBETSAREOR-TILLG.                                                   
025800                                                                          
025900     03 WS-KVANT-QX          PIC  9(5)V9(2) VALUE ZERO.                   
026000     03 WS-KVANT-QX-DELAR    REDEFINES WS-KVANT-QX.                       
026100        05 WS-KVANT-QX-HELTAL PIC 9(5).                                   
026200        05 WS-KVANT-QX-DECTAL PIC 9(2).                                   
026300                                                                          
026400                                                                          
026500     03 FILLER              PIC X(8)        VALUE 'KVDISP'.               
026600     03 WS-KVDISP-DC        PIC S9(7)       VALUE ZERO COMP-3.            
026700     03 WS-KVDISP-FLYG      PIC S9(7)V9(1) VALUE ZERO COMP-3.             
026800     03 WS-TILLG-CDC        PIC S9(7)      VALUE ZERO COMP-3.             
026900     03 WS-BALANCE-ERSATT   PIC S9(7)      VALUE ZERO COMP-3.             
027000     03 FILLER              PIC X(8)       VALUE 'BALANCE'.               
027100     03 WS-KVKUNDRETUR      PIC S9(7)      VALUE ZERO COMP-3.             
027200     03 WS-QX-BRYTNING      PIC  9(2)      VALUE ZERO.                    
027300     03 WS-RADPRIS          PIC S9(7)V9(2) VALUE ZERO COMP-3.             
027400     03 WS-HELTAL-BEST      PIC S9(7)      VALUE ZERO COMP-3.             
027500     03 WS-HELTAL-SALDO     PIC S9(7)      VALUE ZERO COMP-3.             
027600     03 WS-SALDO            PIC S9(7)      VALUE ZERO COMP-3.             
027700     03 WS-BEHOV-VID-ERS    PIC S9(7)      VALUE ZERO COMP-3.             
027800     03 WS-ANTAL-SLUT-ERS   PIC S9(7)      VALUE ZERO COMP-3.             
027900     03 WS-SUM-KVAVROP      PIC S9(7)      VALUE ZERO COMP-3.             
028000     03 WS-ARBETSDAGAR      PIC S9         VALUE ZERO COMP-3.             
028100                                                                          
028200                                                                          
028300 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-BEHOV'.        
028400 01  ARBETSAREOR-BEHOV.                                                   
028500                                                                          
028600     03 WS-CDC-DAGSBEHOV     PIC S9(6)V9(2) VALUE ZERO COMP-3.            
028700     03 WS-4DAG-BEHOV-CDC    PIC S9(7)V9(3) VALUE ZERO COMP-3.            
028800                                                                          
028900     03 WS-KVPB-ANTV-DC      PIC S9(6)V9(3) VALUE ZERO COMP-3.            
029000     03 WS-KVPB-DAG-INNEV-RP PIC S9(6)V9(5) VALUE ZERO COMP-3.            
029100     03 WS-KVPB-DAG-NASTA-RP PIC S9(6)V9(5) VALUE ZERO COMP-3.            
029200     03 WS-KVPB-INNEV-RP     PIC S9(6)V9(3) VALUE ZERO COMP-3.            
029300     03 WS-KVPB-NASTA-RP     PIC S9(6)V9(3) VALUE ZERO COMP-3.            
029400     03 WS-KVPB-FLYG         PIC S9(7)V9(1) VALUE ZERO COMP-3.            
029500                                                                          
029600     03  WS-KVPBREOI-DAY         PIC S9(6)V9(5) VALUE ZERO                
029700                                                COMP-3.                   
029800     03  WS-KVPB-REF-DAY-PER-I   PIC S9(6)V9(5) VALUE ZERO                
029900                                                COMP-3.                   
030000     03  WS-KVPB-REF-DAY-PER-II  PIC S9(6)V9(5) VALUE ZERO                
030100                                                COMP-3.                   
030200     03  WS-KVPB-REF-DAY-PER-III PIC S9(6)V9(5) VALUE ZERO                
030300                                                COMP-3.                   
030400     03  WS-KVPB-REF-DAY-PER-IV  PIC S9(6)V9(5) VALUE ZERO                
030500                                                COMP-3.                   
030600     03  WS-KVPB-REF-DAY-PER-V   PIC S9(6)V9(5) VALUE ZERO                
030700                                                COMP-3.                   
030800     03  WS-KVPB-REF-DAY-PER-VI  PIC S9(6)V9(5) VALUE ZERO                
030900                                                COMP-3.                   
031000     03 WS-KVPB-ANTV-DC-NUM PIC  9(6)V9(2)  VALUE ZERO.                   
031100     03 WS-KVPB-ANTV-DC-NUM-DELAR REDEFINES WS-KVPB-ANTV-DC-NUM.          
031200        05 WS-KVPB-ANTV-DC-HELTAL-NUM PIC 9(6).                           
031300        05 WS-KVPB-ANTV-DC-DECTAL-NUM PIC 9(2).                           
031400                                                                          
031500                                                                          
031600     EJECT                                                                
031700                                                                          
031800 01  FILLER                     PIC X(24)   VALUE 'SWITCHAR'.             
031900                                                                          
032000 77  KONTROLL-SW                    PIC X   VALUE 'N'.                    
032100     88  KONTROLL                           VALUE 'J'.                    
032200                                                                          
032300 77  RELEASEDAY-SW                  PIC X   VALUE 'N'.                    
032400     88  RELEASEDAY                         VALUE 'J'.                    
032500                                                                          
032600 77  BEORDRA-SW                     PIC X   VALUE 'N'.                    
032700     88  BEORDRA                            VALUE 'J'.                    
032800                                                                          
032900 77  BRIST-I-DC-SW                  PIC X   VALUE 'N'.                    
033000     88  BRIST-I-DC                         VALUE 'J'.                    
033100     88  EJ-BRIST-I-DC                      VALUE 'N'.                    
033200                                                                          
033300 77  FLYGFORSLAG-SW                 PIC X   VALUE 'N'.                    
033400     88  FLYGFORSLAG                        VALUE 'J'.                    
033500                                                                          
033600 77  FORSLAG-SW                     PIC X   VALUE 'N'.                    
033700     88  FORSLAG                            VALUE 'J'.                    
033800                                                                          
033900 77  TRAEFF-SW                      PIC X   VALUE 'N'.                    
034000     88  TRAEFF                             VALUE 'J'.                    
034100                                                                          
034200 77  ARTIKEL-UTGANGEN-K6-SW         PIC X   VALUE 'N'.                    
034300     88  ARTIKEL-UTGANGEN-K6                VALUE 'J'.                    
034400                                                                          
034500 77  MASKINELT-PB-SW                PIC X   VALUE 'J'.                    
034600     88  MASKINELT-PB-FINNS                 VALUE 'J'.                    
034700     88  MANUELLT-PB-FINNS                  VALUE 'N'.                    
034800                                                                          
034900 77  NORMAL-REF-SW                  PIC X   VALUE 'N'.                    
035000     88  NORMAL-REF                         VALUE 'J'.                    
035100                                                                          
035200 77  SW-REFILL-KANSKE               PIC X   VALUE 'N'.                    
035300     88  ART-SKALL-KANSKE-REFILLAS          VALUE 'J'.                    
035400                                                                          
035500 77  INTERN-REFILL-SW               PIC X   VALUE 'N'.                    
035600     88  INTERN-REFILL-OK                   VALUE 'J'.                    
035700                                                                          
035800 77  WS-WORKDAY-STATUS              PIC X   VALUE 'J'.                    
035900     88  RATT-FRAN-WORKDAY                  VALUE 'J'.                    
036000     88  FEL-FRAN-WORKDAY                   VALUE 'N'.                    
036100                                                                          
036200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
036300     88  NYCKLAR-OK                          VALUE 'J'.                   
036400     88  NYCKLAR-FEL                         VALUE 'N'.                   
036500                                                                          
036600 77  KDFARLIG-SW                    PIC X   VALUE 'N'.                    
036700     88  DANGEROUSGOODS                     VALUE 'J'.                    
036800     88  NOTDANGEROUS                       VALUE 'N'.                    
036900                                                                          
037000 77  WEEK-SW                        PIC X   VALUE ' '.                    
037100     88  WEEK-EVEN                          VALUE 'J'.                    
037200     88  WEEK-ODD                           VALUE 'N'.                    
037300                                                                          
037400     EJECT                                                                
037500                                                                          
037600 01  FILLER                    PIC X(24)  VALUE 'KONSTANTER'.             
037700                                                                          
037800*01  -COPY W271RTXT                                                       
037900     EJECT                                                                
038000                                                                          
038100*    --- COPYTEXT FÖR ATT KUNNA UR DISTR FÅ MOTTAGANDE IDDC               
038200*01 -COPY WWDIST35                                                        
038300     EJECT                                                                
038400                                                                          
038500 01  DC-POST.                                                             
038600     03  DC-PARAMETER-TIME    PIC X(2).                                   
038700     03  FILLER               PIC X(78).                                  
038800                                                                          
038900     03  LEVNR-KOLL.                                                      
039000       05  LEVNR-KONTROLL    PIC X(5).                                    
039100           88 CDC-LEVERANTOR VALUE '1441 '.                               
039200                                                                          
039300 01  FELTEXT.                                                             
039400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
039500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
039600     EJECT                                                                
039700                                                                          
039800                                                                          
039900     EJECT                                                                
040000                                                                          
040100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
040200 01  FILLER REDEFINES DAGENS-DATUM.                                       
040300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
040400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
040500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
040600                                                                          
040700     EJECT                                                                
040800                                                                          
040900*      --- VALID IDDC CODES                                               
041000*                                                                         
041100*01    -COPY WWDC99                                                       
041200*01    -COPY WWDCKONS                                                     
041300       EJECT                                                              
041400*                                                                         
041500*01    -COPY WWBYT03                                                      
041600       EJECT                                                              
041700                                                                          
041800 01  FILLER                    PIC X(24)  VALUE 'SUBPROGRAM'.             
041900                                                                          
042000 01  DYNAMISKA-SUBPROGRAM.                                                
042100*                                                                         
042200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
042300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
042400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
042500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
042600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
042700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
042800     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
042900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
043000     03  W271LTPB                PIC X(8)    VALUE 'W271LTPB'.            
043100     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
043200     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
043300     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
043400     EJECT                                                                
043500*    --- PARAMETRAR TILL ABEND                                            
043600                                                                          
043700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
043800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
043900     SKIP2                                                                
044000*    --- PARAMETRAR TILL POSTSUM                                          
044100*                                                                         
044200*01  -COPY W0005   -PRE  POSTSUM-                                         
044300     EJECT                                                                
044400*    --- PARAMETRAR TILL WORKDAY                                          
044500*                                                                         
044600*01  -COPY WORKAREA                                                       
044700     EJECT                                                                
044800*    --- PARAMETRAR TILL DATKONV                                          
044900*                                                                         
045000*01  -COPY WDATAREA                                                       
045100     EJECT                                                                
045200                                                                          
045300 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
045400*   -COPY WZ20DAYS                                                        
045500     EJECT                                                                
045600                                                                          
045700*    --- PARAMETRAR TILL DAGKONV                                          
045800*                                                                         
045900*01  -COPY WDAGAREA                                                       
046000     EJECT                                                                
046100                                                                          
046200*    --- PARAMETRAR TILL VECKOADD                                         
046300                                                                          
046400 01  W009VADD-AREA.                                                       
046500     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
046600     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
046700                                                                          
046800                                                                          
046900*    --- PARAMETRAR TILL W271LTPB                                         
047000                                                                          
047100*01  -COPY W271LTPB                                                       
047200                                                                          
047300*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
047400 01  FILLER                     PIC X(16) VALUE 'W218LETA START'.         
047500*01  -COPY W218LETA  -PRE ETA-                                            
047600                                                                          
047700 01  FILLER                     PIC X(12) VALUE 'DUMMY ARTC'.             
047800 01  ETA-ARTC-PCB               PIC X(1).                                 
047900 01  FILLER                     PIC X(12) VALUE 'DUMMY ARTS'.             
048000 01  ETA-ARTS-PCB               PIC X(1).                                 
048100 01  FILLER                     PIC X(12) VALUE 'DUMMY INLC'.             
048200 01  ETA-INLC-PCB               PIC X(1).                                 
048300 01  FILLER                     PIC X(12) VALUE 'DUMMY LEVA'.             
048400 01  ETA-LEVA-PCB               PIC X(1).                                 
048500                                                                          
048600     EJECT                                                                
048700*    --- PARAMETRAR TILL W271UTIL                                         
048800*01 -COPY W271UTIL                                                        
048900     EJECT                                                                
049000*    --- INITIALIZED     W271UTIL                                         
049100*01 -COPY W271UTIL         -PRE INIT-                                     
049200     EJECT                                                                
049300                                                                          
049400                                                                          
049500 01  W27210-AREA-START           PIC X(24)   VALUE                        
049600                                             'W27210-AREA-START'.         
049700     SKIP2                                                                
049800                                                                          
049900*01  AREA -COPY W27111     -PRE W27210-                                   
050000*                                                                         
050100     EJECT                                                                
050200*  KEYS                                                                   
050300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
050400     SKIP3                                                                
050500 01  NYCKLAR-TILL-DLI.                                                    
050600     03  W-IDARTNR-X.                                                     
050700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
050800                                                                          
050900     03  W-IDARTNR-INT-X.                                                 
051000         05  W-IDARTNR-INT       PIC S9(9)   VALUE ZERO COMP-3.           
051100                                                                          
051200     03  W-IDARTNR-ERS-X.                                                 
051300         05  W-IDARTNR-ERS       PIC S9(9)   VALUE ZERO COMP-3.           
051400                                                                          
051500     03  W-IDLEVNR-X.                                                     
051600         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
051700                                                                          
051800     03  W-IDDC-X.                                                        
051900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
052000                                                                          
052100     03  W-IDLAND-X.                                                      
052200         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
052300                                                                          
052400     03  W-IDDC-B6-X.                                                     
052500         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
052600                                                                          
052700     03  W-IDDC-B616-X.                                                   
052800         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
052900                                                                          
053000     03  W-IDDC-ERS-X.                                                    
053100         05  W-IDDC-ERS          PIC X(2)    VALUE SPACE.                 
053200                                                                          
053300     03  W-IDDC-INT-X.                                                    
053400         05  W-IDDC-INT          PIC X(2)    VALUE SPACE.                 
053500                                                                          
053600     03  W-KDSEGKEY-X.                                                    
053700         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
053800                                                                          
053900     03 W-WDE301KY-MIN-X.                                                 
054000         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
054100         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
054200         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
054300         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
054400         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
054500                                                                          
054600     03 W-WDE301KY-MAX-X.                                                 
054700         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
054800         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE ZERO COMP-3.             
054900         05  W-KDREFTYP-MAX      PIC X     VALUE SPACE.                   
055000         05  W-IDARTNR-MAX       PIC S9(9)                                
055100                                         VALUE +999999999 COMP-3.         
055200         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
055300                                                                          
055400     03  W-WDD7A1KY-MIN-X.                                                
055500       05  W-IDARTNR-TILLK-MIN     PIC S9(9)   COMP-3  VALUE ZERO.        
055600       05  W-IDARTNR-ERS-MIN       PIC S9(9)   COMP-3  VALUE ZERO.        
055700       05  W-IDKORTNR-MIN          PIC S9(3)   COMP-3  VALUE ZERO.        
055800                                                                          
055900     03  W-WDD7A1KY-MAX-X.                                                
056000       05  W-IDARTNR-TILLK-MAX     PIC S9(9)   COMP-3                     
056100                                              VALUE +999999999.           
056200       05  W-IDARTNR-ERS-MAX       PIC S9(9)   COMP-3                     
056300                                              VALUE +999999999.           
056400       05  W-IDKORTNR-MAX          PIC S9(3)   COMP-3  VALUE +999.        
056500                                                                          
056600     03  W-WDA5D1KY-MIN-X.                                                
056700       05  W-IDDISTR-A5D-MIN   PIC S9(5)  VALUE ZERO COMP-3.              
056800       05  W-IDKUNDNR-A5D-MIN  PIC S9(7)  VALUE ZERO COMP-3.              
056900       05  W-IDARTNR-A5D-MIN   PIC S9(9)  VALUE ZERO COMP-3.              
057000       05  W-IDKUNDREF-A5D-MIN PIC X(10)  VALUE LOW-VALUE.                
057100       05  W-IDLOPNR-A5D-MIN   PIC S9(3)  VALUE ZERO COMP-3.              
057200     03  W-WDA5D1KY-MAX-X.                                                
057300       05  W-IDDISTR-A5D-MAX   PIC S9(5)  VALUE +99999 COMP-3.            
057400       05  W-IDKUNDNR-A5D-MAX  PIC S9(7)  VALUE +9999999                  
057500                                                 COMP-3.                  
057600       05  W-IDARTNR-A5D-MAX   PIC S9(9)  VALUE +999999999                
057700                                                 COMP-3.                  
057800       05  W-IDKUNDREF-A5D-MAX PIC X(10)  VALUE HIGH-VALUE.               
057900       05  W-IDLOPNR-A5D-MAX   PIC S9(3)  VALUE +999 COMP-3.              
058000                                                                          
058100     03  W-WDD901KY-X.                                                    
058200         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
058300         05  W-IDDC-D9           PIC X(2)    VALUE '11'.                  
058400                                                                          
058500     03  W-WDA501KY-X.                                                    
058600         05  W-WDA501KY          PIC X(24)   VALUE SPACE.                 
058700                                                                          
058800     03  W-WDE4CSEQ-MIN-X.                                                
058900         05  W-WDE4C-IDARTNR-MIN PIC S9(9) VALUE ZERO COMP-3.             
059000                                                                          
059100     03  W-WDE4CSEQ-MAX-X.                                                
059200         05  W-WDE4C-IDARTNR-MAX PIC S9(9) VALUE ZERO COMP-3.             
059300                                                                          
059400     03  W-IDPRODNR-X.                                                    
059500         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
059600     03  W-IDKOLLI-X.                                                     
059700         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
059800                                                                          
059900     03  W-WDQ4BKY-FOM.                                                   
060000         05  W-Q4-IDARTNR-MIN    PIC  S9(9)    COMP-3.                    
060100         05  FILLER              PIC  X(32)    VALUE LOW-VALUE.           
060200                                                                          
060300     03  W-WDQ4BKY-TOM.                                                   
060400         05  W-Q4-IDARTNR-MAX    PIC  S9(9)    COMP-3.                    
060500         05  FILLER              PIC  X(32)    VALUE HIGH-VALUE.          
060600                                                                          
060700     03  W-Q2-IDORDER-X.                                                  
060800         05  W-Q2-IDORDER        PIC S9(7)     COMP-3.                    
060900                                                                          
061000     03  W-Q2-IDDC-X.                                                     
061100         05  W-Q2-IDDC           PIC X(2)       VALUE SPACE.              
061200                                                                          
061300     03  W-Q4-IDDC-X.                                                     
061400         05  W-Q4-IDDC           PIC X(2)       VALUE SPACE.              
061500                                                                          
061600     SKIP2                                                                
061700 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
061800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
061900                                                                          
062000 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
062100 01  DB2-WS.                                                              
062200     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
062300         88  CURSOR-OK                      VALUE 000.                    
062400         88  LINES-FOUND                    VALUE 000.                    
062500         88  LINES-MISSING                  VALUE 100.                    
062600         88  RESOURCE-WRONG                 VALUE 904.                    
062700     03  GOOD-SQLCODECODES.                                               
062800         05  GOOD-SQLCODE OCCURS 5                                        
062900             INDEXED BY SQLCODE-IX PIC 9(3).                              
063000     SKIP2                                                                
063100*    --- STATUS-KOD FRÅN IMS                                              
063200 01  STATUS-WS                   PIC XX.                                  
063300     88  SEGMENT-FINNS                       VALUE '  '.                  
063400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
063500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
063600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
063700     88  IMS-EJ-OK                           VALUE 'XD'.                  
063800     SKIP2                                                                
063900 01  GODK-STATUSKODER.                                                    
064000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
064100     SKIP3                                                                
064200 01  SSA1                        PIC X(128).                              
064300 01  SSA2                        PIC X(128).                              
064400     EJECT                                                                
064500*    --- IMS FUNKTIONSKODER                                               
064600*01  -COPY W0003                                                          
064700     EJECT                                                                
064800                                                                          
064900*    ---  DLI INPUT-OUTPUT AREA                                           
065000                                                                          
065100                                                                          
065200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
065300                                                                          
065400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
065500 01  DLI-IO-WDK601.                                                       
065600*    03  -COPY WDK601                                                     
065700     EJECT                                                                
065800                                                                          
065900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
066000 01  DLI-IO-WDK611.                                                       
066100*    03  -COPY WDK611                                                     
066200     EJECT                                                                
066300                                                                          
066400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
066500 01  DLI-IO-WDK629.                                                       
066600*    03  -COPY WDK629                                                     
066700     EJECT                                                                
066800                                                                          
066900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK901'.                      
067000 01  DLI-IO-WDK901.                                                       
067100*    03  -COPY WDK901   -PRE WDK9-                                        
067200     EJECT                                                                
067300                                                                          
067400                                                                          
067500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD704'.             
067600 01  DLI-IO-WDD704.                                                       
067700*    03  -COPY WDD704   -PRE WDD704-                                      
067800     EJECT                                                                
067900                                                                          
068000 01  FILLER         PIC X(24)   VALUE 'DLI-IO-WDE301'.                    
068100 01  DLI-IO-WDE301.                                                       
068200*     03  -COPY WDE301                                                    
068300                                                                          
068400     EJECT                                                                
068500                                                                          
068600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB01'.                      
068700 01  DLI-IO-INLB01.                                                       
068800*    03  -COPY WDD901    -PRE WDD901-                                     
068900     EJECT                                                                
069000                                                                          
069100 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB11'.                      
069200 01  DLI-IO-INLB11.                                                       
069300*    03  -COPY WDD902    -PRE WDD902-                                     
069400     EJECT                                                                
069500                                                                          
069600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB23'.                      
069700 01  DLI-IO-INLB23.                                                       
069800*    03  -COPY WDD905    -PRE WDD905-                                     
069900     EJECT                                                                
070000                                                                          
070100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-INTERN'.               
070200 01  DLI-IO-WDK711-INTERN.                                                
070300*    03  -COPY WDK711   -PRE  INT-                                        
070400     EJECT                                                                
070500                                                                          
070600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK722-INTERN'.               
070700 01  DLI-IO-WDK722-INTERN.                                                
070800*    03  -COPY WDK722   -PRE  INT-                                        
070900     EJECT                                                                
071000                                                                          
071100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712-LART'.                 
071200 01  DLI-IO-WDK712-LART.                                                  
071300*    03  -COPY WDK712                                                     
071400     EJECT                                                                
071500                                                                          
071600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLC01'.                      
071700 01  DLI-IO-INLC01.                                                       
071800*    03  -COPY WDL601   -PRE  WDL6-                                       
071900     EJECT                                                                
072000                                                                          
072100 01  FILLER         PIC X(24)   VALUE 'DLI-IO-INLC11'.                    
072200 01  DLI-IO-INLC11.                                                       
072300*     03  -COPY WDL611                                                    
072400     EJECT                                                                
072500                                                                          
072600 01  FILLER                PIC X(16)   VALUE 'DLI-IO-ORDT01'.             
072700 01  DLI-IO-ORDT01.                                                       
072800*     03  -COPY WDA5D1                                                    
072900                                                                          
073000     EJECT                                                                
073100                                                                          
073200 01  FILLER                PIC X(16)   VALUE 'DLI-IO-ORDP01'.             
073300 01  DLI-IO-ORDP01.                                                       
073400*     03  -COPY WDA501                                                    
073500                                                                          
073600     EJECT                                                                
073700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
073800 01   DLI-IO-AREA-B601.                                                   
073900*     03  -COPY WDB601                                                    
074000     EJECT                                                                
074100 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
074200 01   DLI-IO-AREA-B616.                                                   
074300*     03  -COPY WDB616       -PRE B6-                                     
074400     EJECT                                                                
074500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE401'.                      
074600 01  DLI-IO-WDE401.                                                       
074700*    03  -COPY WDE401                                                     
074800     EJECT                                                                
074900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE411'.                      
075000 01  DLI-IO-WDE411.                                                       
075100*    03  -COPY WDE411                                                     
075200     EJECT                                                                
075300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
075400 01  DLI-IO-WDE421.                                                       
075500*    03  -COPY WDE421                                                     
075600     EJECT                                                                
075700 01  FILLER         PIC X(24) VALUE 'DLI-IO-E601'.                        
075800 01  DLI-IO-E601.                                                         
075900*    03  -COPY WDE601                                                     
076000     EJECT                                                                
076100 01  FILLER         PIC X(24) VALUE 'DLI-IO-E611'.                        
076200 01  DLI-IO-E611.                                                         
076300*    03  -COPY WDE611                                                     
076400     EJECT                                                                
076500 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDQ4B1'.              
076600 01      DLI-IO-WDQ4B1.                                                   
076700*     03  -COPY WDQ4B1                                                    
076800     EJECT                                                                
076900 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDQ201'.              
077000 01      DLI-IO-WDQ201.                                                   
077100*     03  -COPY WDQ201                                                    
077200     EJECT                                                                
077300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDQ212'.              
077400 01      DLI-IO-WDQ212.                                                   
077500*     03  -COPY WDQ212                                                    
077600     EJECT                                                                
077700*****DB2 AREOR*******************                                         
077800 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
077900                                                                          
078000*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
078100     EJECT                                                                
078200 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
078300                                                                          
078400*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
078500     EJECT                                                                
078600     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
078700     EJECT                                                                
078800     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
078900     EJECT                                                                
079000                                                                          
079100 LINKAGE SECTION.                                                         
079200*01  -COPY W0008  -PRE WDK6-                                              
079300     05  FILLER                  PIC X.                                   
079400     EJECT                                                                
079500*01  -COPY W0008  -PRE WDK9-                                              
079600     05  FILLER                  PIC X.                                   
079700     EJECT                                                                
079800*01  -COPY W0008  -PRE WDE3-                                              
079900     05  FILLER                  PIC X.                                   
080000     EJECT                                                                
080100*01  -COPY W0008  -PRE WDD7-                                              
080200     05  FILLER                  PIC X.                                   
080300     EJECT                                                                
080400*01  -COPY W0008  -PRE INLB-                                              
080500     05  FILLER                  PIC X.                                   
080600     EJECT                                                                
080700*01  -COPY W0008  -PRE WDK74-                                             
080800     05  FILLER                  PIC X.                                   
080900     EJECT                                                                
081000*01  -COPY W0008  -PRE INLC-                                              
081100     05  FILLER                  PIC X.                                   
081200     EJECT                                                                
081300*01  -COPY W0008  -PRE ORDT-                                              
081400     05  FILLER                  PIC X.                                   
081500     EJECT                                                                
081600*01  -COPY W0008  -PRE ORDP-                                              
081700     05  FILLER                  PIC X.                                   
081800     EJECT                                                                
081900*01  -COPY W0008  -PRE WDB6-                                              
082000     05  FILLER                  PIC X.                                   
082100     EJECT                                                                
082200*01  -COPY W0008  -PRE WDK7-2-                                            
082300     05  FILLER                  PIC X.                                   
082400     EJECT                                                                
082500 01  ETA-WDB6-PCB                PIC X.                                   
082600     EJECT                                                                
082700*01  -COPY W0008      -PRE UTIL-WDK6-                                     
082800     05  FILLER                  PIC X.                                   
082900     EJECT                                                                
083000*01  -COPY W0008      -PRE UTIL-WDK7-                                     
083100     05  FILLER                  PIC X.                                   
083200     EJECT                                                                
083300*01  -COPY W0008      -PRE UTIL-WDB6-                                     
083400     05  FILLER                  PIC X.                                   
083500     EJECT                                                                
083600*01  -COPY W0008  -PRE WDE4C-                                             
083700     05  FILLER                  PIC X.                                   
083800     EJECT                                                                
083900*01  -COPY W0008  -PRE WDE6-                                              
084000     05  FILLER                  PIC X.                                   
084100     EJECT                                                                
084200*01  -COPY W0008  -PRE WDQ4B-                                             
084300     05  FILLER                  PIC X.                                   
084400     EJECT                                                                
084500*01  -COPY W0008  -PRE WDQ2-                                              
084600     05  FILLER                  PIC X.                                   
084700     EJECT                                                                
084800 PROCEDURE DIVISION  USING                                                
084900                            WDK6-PCB WDK9-PCB WDE3-PCB WDD7-PCB           
085000                            INLB-PCB WDK74-PCB INLC-PCB                   
085100                            ORDT-PCB ORDP-PCB WDB6-PCB                    
085200                            WDK7-2-PCB ETA-WDB6-PCB                       
085300                            UTIL-WDK6-PCB UTIL-WDK7-PCB                   
085400                            UTIL-WDB6-PCB                                 
085500                            WDE4C-PCB                                     
085600                            WDE6-PCB  WDQ4B-PCB WDQ2-PCB.                 
085700                                                                          
085800     ENTRY 'DLITCBL' USING                                                
085900                            WDK6-PCB WDK9-PCB WDE3-PCB WDD7-PCB           
086000                            INLB-PCB WDK74-PCB INLC-PCB                   
086100                            ORDT-PCB ORDP-PCB WDB6-PCB                    
086200                            WDK7-2-PCB ETA-WDB6-PCB                       
086300                            UTIL-WDK6-PCB UTIL-WDK7-PCB                   
086400                            UTIL-WDB6-PCB                                 
086500                            WDE4C-PCB                                     
086600                            WDE6-PCB  WDQ4B-PCB WDQ2-PCB.                 
086700                                                                          
086800     PERFORM A-INIT                                                       
086900     PERFORM IMS-GN-WDK629                                                
087000     PERFORM UNTIL SEGMENT-SLUT                                           
087100        PERFORM IMS-GNP-WDK611                                            
087200        PERFORM IMS-GNP-WDK601                                            
087300        MOVE ART-IDARTNR      TO WS-SPAR-IDARTNR                          
087400                                 WS-SPAR-IDARTNR-NUM                      
087500                                 W-IDARTNR                                
087600                                 W-IDARTNR-D9                             
087700                                                                          
087800        IF CREF-IDDC-REF = SPACE                                          
087900        OR CLAG-REDIRLEV = 1.0                                            
088000          CONTINUE                                                        
088100        ELSE                                                              
088200          PERFORM B-NOLLSTALL                                             
088300          IF CREF-IDDC-REF NOT = W-IDDC-B616                              
088400            MOVE '11'                      TO W-IDDC-B6                   
088500            PERFORM IMS-GU-WDB601                                         
088600            MOVE CREF-IDDC-REF             TO W-IDDC-B616                 
088700            PERFORM IMS-GU-WDB616                                         
088800            IF SEGMENT-FINNS                                              
088900               MOVE B6-REF-KVDLTID-AIRETA  TO WS-LTID-A                   
089000               MOVE B6-REF-KVDLTID-TOT     TO WS-LTID-B                   
089100               PERFORM S80-CURR-WEEK-PLUS-LT                              
089200            END-IF                                                        
089300          END-IF                                                          
089400****************W271V2 DC-PARAMETER-TIME=99                    **         
089500****************W271D2 DC-PARAMETER-TIME=02 (RUNS AROUND 02.00)**         
089600***PARAMETER(TIREFBAT IS SET ON 4408 SCREEN TO TELL WHAT BATCH **         
089700          IF (DC-PARAMETER-TIME = '99')                                   
089800          OR (DC-PARAMETER-TIME = B6-REF-TIREFBAT)                        
089900             MOVE '11'               TO WS-IDDC                           
090000             MOVE CREF-IDDC-REF TO W-IDDC-INT                             
090100             MOVE WS-SPAR-IDARTNR TO W-IDARTNR-INT                        
090200             PERFORM IMS-GU-WDK711-INTERN                                 
090300             IF SEGMENT-FINNS                                             
090400               MOVE INT-SLAG-KVSPARR-KVAL  TO WS-KVSPARR-KVAL             
090500               PERFORM D-CHECK-STOP-DATE                                  
090600               IF INTERN-REFILL-OK                                        
090700                  PERFORM C-BEHANDLA-ARTIKEL                              
090800               END-IF                                                     
090900             END-IF                                                       
091000          END-IF                                                          
091100        END-IF                                                            
091200        PERFORM IMS-GN-WDK629                                             
091300     END-PERFORM                                                          
091400                                                                          
091500     PERFORM Z-FINIT                                                      
091600                                                                          
091700     MOVE ZERO TO RETURN-CODE                                             
091800     GOBACK                                                               
091900     .                                                                    
092000     EJECT                                                                
092100                                                                          
092200                                                                          
092300 A-INIT SECTION.                                                          
092400     SKIP2                                                                
092500                                                                          
092600                                                                          
092700***  DATA TILL DATUMFÄLT                                                  
092800                                                                          
092900     OPEN INPUT  W272DC                                                   
093000     OPEN OUTPUT W27210                                                   
093100                                                                          
093200     MOVE ZERO TO ART-IDARTNR                                             
093300     PERFORM S21-LAES-W272DC                                              
093400     DISPLAY 'DC-PARAMETER-TIME : ' DC-PARAMETER-TIME                     
093500                                                                          
093600     ACCEPT DAGENS-DATUM FROM DATE                                        
093700     DISPLAY 'DAGENS-DATUM : ' DAGENS-DATUM                               
093800                                                                          
093900*    HÄMTA INNEVARANDE VECKA                                              
094000*                      ÅR-VECKA-DAG                                       
094100*                      R-PERIOD                                           
094200*                      ANTAL VECKOR I PERIODEN                            
094300*                                                                         
094400*                                                                         
094500     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
094600     CALL WDATKONV USING DAT-KDDATFORM                                    
094700                         DAT-I-TIDATUM                                    
094800                         DAT-O-TIDATUM                                    
094900                         DAT-KDSVAR                                       
095000                                                                          
095100     IF DAT-KDSVAR-OK                                                     
095200        MOVE DAT-TIVV    TO DAGENS-VECKA                                  
095300        MOVE DAT-TIAAVVD TO DAGENS-TIAAVVD                                
095400        MOVE DAT-TIAARP  TO WS-INNEV-TIAARP                               
095500        MOVE DAT-KVVIPER TO WS-KVVIPER                                    
095600        MOVE DAT-TID     TO WS-DAG-I-VECKA                                
095700                            DD-LT-FRAMAT-TID                              
095800                            W26-V-FRAMAT-TID                              
095900        DISPLAY 'WS-DAG-I-VECKA : ' WS-DAG-I-VECKA                        
096000     ELSE                                                                 
096100        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W27210' TO          
096200                                    FELTEXT-STR                           
096300        DISPLAY FELTEXT                                                   
096400        PERFORM S99-ABEND                                                 
096500     END-IF                                                               
096600                                                                          
096700*RÄKNA FRAM NÄSTA PERIOD                                                  
096800                                                                          
096900     MOVE WS-INNEV-TIAARP TO WS-NASTA-TIAARP                              
097000     IF WS-INNEV-TIAARP = 9912                                            
097100        MOVE 0001  TO WS-NASTA-TIAARP                                     
097200     ELSE                                                                 
097300        IF WS-INNEV-TIRP = 12                                             
097400           MOVE 01 TO WS-NASTA-TIRP                                       
097500           ADD 1 TO WS-NASTA-TIAA                                         
097600        ELSE                                                              
097700           ADD 1 TO WS-NASTA-TIRP                                         
097800        END-IF                                                            
097900     END-IF                                                               
098000                                                                          
098100*HÄMTA FÖRSTA VECKAN I INNEVARANDE R-PERIOD                               
098200                                                                          
098300     MOVE 'AARP'           TO DAT-KDDATFORM                               
098400     MOVE WS-INNEV-TIAARP  TO DAT-I-TIDATUM                               
098500     CALL WDATKONV USING   DAT-KDDATFORM                                  
098600                           DAT-I-TIDATUM                                  
098700                           DAT-O-TIDATUM                                  
098800                           DAT-KDSVAR                                     
098900                                                                          
099000     IF DAT-KDSVAR-OK                                                     
099100        MOVE DAT-TIVV    TO WS-FIRST-VV-RPER                              
099200     ELSE                                                                 
099300        MOVE 'FEL FRÅN WDATKONV 2  I A-INIT SECTION I W27210' TO          
099400                                    FELTEXT-STR                           
099500        DISPLAY FELTEXT                                                   
099600        PERFORM S99-ABEND                                                 
099700     END-IF                                                               
099800                                                                          
099900*LETA UPP INNEVARANDE VECKAS RELATIVA PLACERING I INNEVARANDE             
100000*PERIOD                                                                   
100100                                                                          
100200     IF WS-INNEV-TIRP = 1 AND WS-FIRST-VV-RPER = 52                       
100300        MOVE 1 TO WS-FIRST-VV-RPER                                        
100400        COMPUTE WS-KVVIPER = WS-KVVIPER - 1                               
100500     END-IF                                                               
100600                                                                          
100700     MOVE DAGENS-VECKA TO WS-INNEV-VECKA                                  
100800     DIVIDE 2 INTO DAGENS-VECKA  GIVING WS-WEEKBY2-QUOTIENT               
100900                              REMAINDER WS-WEEKBY2-REMAINDER              
101000     IF DAGENS-VECKA = 1 OR WS-WEEKBY2-REMAINDER > 0                      
101100       SET WEEK-ODD        TO TRUE                                        
101200     ELSE                                                                 
101300       SET WEEK-EVEN       TO TRUE                                        
101400     END-IF                                                               
101500     MOVE WS-FIRST-VV-RPER TO WS-VECKA                                    
101600     MOVE +1 TO VECKO-IX                                                  
101700     PERFORM UNTIL VECKO-IX > WS-KVVIPER                                  
101800        IF WS-VECKA = WS-INNEV-VECKA                                      
101900           MOVE VECKO-IX TO PERIOD-VECKA                                  
102000        END-IF                                                            
102100        ADD +1 TO VECKO-IX                                                
102200        ADD +1 TO WS-VECKA                                                
102300     END-PERFORM                                                          
102400                                                                          
102500                                                                          
102600*    BERÄKNA DATUM 26  VECKOR FRAMMÅT                                     
102700                                                                          
102800     MOVE DAGENS-TIAAVV TO VADD-DATUM-AAVV                                
102900     MOVE +26           TO VADD-ANTAL                                     
103000                                                                          
103100     CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                       
103200                                                                          
103300     MOVE VADD-DATUM-AAVV   TO W26-V-FRAMAT-TIAAVV                        
103400                                                                          
103500                                                                          
103600                                                                          
103700     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-DATUM                    
103800                                                                          
103900*    BERÄKNA DAGENS SEKELDATUM                                            
104000     MOVE 'IDAG'           TO DAT-KDDATFORM                               
104100     CALL WDATKONV USING   DAT-KDDATFORM                                  
104200                           DAT-I-TIDATUM                                  
104300                           DAT-O-TIDATUM                                  
104400                           DAT-KDSVAR                                     
104500                                                                          
104600     IF DAT-KDSVAR-OK                                                     
104700        INITIALIZE           INIT-UTIL-W271UTIL                           
104800        MOVE DAT-TIAAVVD  TO INIT-UTIL-TIAAVVD                            
104900     ELSE                                                                 
105000        MOVE 'FEL FRÅN WDATKONV 3  I A-INIT SECTION I W27210' TO          
105100                                    FELTEXT-STR                           
105200        DISPLAY FELTEXT                                                   
105300        PERFORM S99-ABEND                                                 
105400     END-IF                                                               
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800 B-NOLLSTALL SECTION.                                                     
105900                                                                          
106000     MOVE ZERO       TO WS-IDDISTR                                        
106100                        WS-KVPB-ANTV-DC                                   
106200                        WS-KVPB-DAG-INNEV-RP                              
106300                        WS-KVPB-DAG-NASTA-RP                              
106400                        WS-KVPB-INNEV-RP                                  
106500                        WS-KVPB-NASTA-RP                                  
106600                        WS-KVPB-FLYG                                      
106700                        WS-SUM-KVAVROP                                    
106800                        WS-KVREFPKT-DC                                    
106900                        WS-SUANTAL-ETA                                    
107000                        WS-FLYGT-MAX-TIAAMMDD                             
107100                        WS-FLYGBEHOV-MAX-TIAARP                           
107200                        WS-FIRST-TIBERANK                                 
107300                        WS-KVDISP-FLYG                                    
107400                        WS-BALANCE-ERSATT                                 
107500                        WS-KVDAGAR-FLYG-INNEV                             
107600                        WS-KVDAGAR-FLYG-NASTA                             
107700                        WS-TILLG-CDC                                      
107800                        WS-NASTA-PER-TIAAMMDD                             
107900                        WS-PRIS                                           
108000                        WS-PREL-ERSDAT                                    
108100                        WS-ERSDAT-AND-LT                                  
108200                        WS-ANTAL-SLUT-ERS                                 
108300                        WS-AAMMDD-ERSDAT                                  
108400                                                                          
108500                        WS-ANTAL-GODK-E3                                  
108600                        WS-ANTAL-RO-RESS-KL1-CDC                          
108700                        WS-ANTAL-LEDT-FLYG                                
108800                        WS-ANTAL-FLYG                                     
108900                        WS-ANTAL-BER                                      
109000                        WS-ANTAL-QX                                       
109100                        WS-ANTAL-SLUT                                     
109200                        WS-KVANT-QX                                       
109300                        WS-KVOKS-DAG                                      
109400                        WS-KVOKS-BULK                                     
109500                        WS-KVOKS-VOR                                      
109600                        WS-DAPUBL                                         
109700                        WS-ANTAL-TILL-ERS                                 
109800                        WS-TIERSDAT-PREL-C1                               
109900                                                                          
110000                        WS-KVPBREOI-DAY                                   
110100                        WS-KVPB-REF-DAY-PER-I                             
110200                        WS-KVPB-REF-DAY-PER-II                            
110300                        WS-KVPB-REF-DAY-PER-III                           
110400                        WS-KVPB-REF-DAY-PER-IV                            
110500                        WS-KVPB-REF-DAY-PER-V                             
110600                        WS-KVPB-REF-DAY-PER-VI                            
110700                        WS-KVPB-ANTV-DC-NUM                               
110800                        WS-BEHOV-VID-ERS                                  
110900                        WS-KVSPARR-KVAL                                   
111000                        WS-KVQPACK-3                                      
111100                        WS-KDERS                                          
111200                                                                          
111300                        IX                                                
111400                                                                          
111500     MOVE NEJ        TO KONTROLL-SW                                       
111600                        BEORDRA-SW                                        
111700                        BRIST-I-DC-SW                                     
111800                        FLYGFORSLAG-SW                                    
111900                        FORSLAG-SW                                        
112000                        ARTIKEL-UTGANGEN-K6-SW                            
112100                        INTERN-REFILL-SW                                  
112200                        TRAEFF-SW                                         
112300                                                                          
112400     INITIALIZE         W271LTPB-W271LTPB                                 
112500                        W27210-AREA                                       
112600     MOVE ZERO       TO UTIL-KVPB-TOT                                     
112700                                                                          
112800     .                                                                    
112900     EJECT                                                                
113000                                                                          
113100                                                                          
113200 C-BEHANDLA-ARTIKEL SECTION.                                              
113300                                                                          
113400     IF ART-KDERS-UTG > 0                                                 
113500        CONTINUE                                                          
113600     ELSE                                                                 
113700        MOVE NEJ TO KDFARLIG-SW                                           
113800        IF CLAG-KDFARLIG = 4                                              
113900          MOVE JA TO KDFARLIG-SW                                          
114000        END-IF                                                            
114100        MOVE CLAG-PRARTSTD       TO WS-PRIS                               
114200        IF CLAG-KDERS < 7                                                 
114300           PERFORM CG-KOLLA-KVPB-PLAN                                     
114400           IF CLAG-KDERS = 0                                              
114500              PERFORM S34B-BER-BALANCE-CDC                                
114600              MOVE CREF-KVREFPKT TO WS-KVREFPKT-DC                        
114700              IF WS-BALANCE-CDC < WS-KVREFPKT-DC                          
114800                 PERFORM CB-VALJA-ARTIKLAR                                
114900              END-IF                                                      
115000           ELSE                                                           
115100*             CDC MED ERSKOD 03 OCH 06                                    
115200**************LÄS WDD7 FÖR ATT FÅ FRAM PREL ERS DAT                       
115300              IF CLAG-KDERS = 03 OR 06                                    
115400                PERFORM IMS-GU-WDD704                                     
115500                IF SEGMENT-FINNS                                          
115600                  MOVE WDD704-TIERSDAT-PREL-C1 TO                         
115700                              WS-TIERSDAT-PREL-C1                         
115800                ELSE                                                      
115900                  MOVE ZERO                  TO                           
116000                              WS-TIERSDAT-PREL-C1                         
116100                END-IF                                                    
116200              ELSE                                                        
116300*             CDC MED ERSKOD 01, 02, 04 OCH 05                            
116400                MOVE CLAG-TISTOREF TO WS-TIERSDAT-PREL-C1                 
116500              END-IF                                                      
116600                                                                          
116700              IF WS-TIERSDAT-PREL-C1 = 0                                  
116800****OM PREL ERSDAT = 0 BEHANDLA SOM KDERS = 0                             
116900                MOVE CREF-KVREFPKT TO WS-KVREFPKT-DC                      
117000                IF CLAG-KVTILLG-TOT < WS-KVREFPKT-DC                      
117100                   PERFORM CB-VALJA-ARTIKLAR                              
117200                END-IF                                                    
117300              ELSE                                                        
117400                IF WS-TIERSDAT-PREL-C1 > 50000                            
117500***FÖR   ATT TA HAND OM DATUM FÖRE 2000-01-01                             
117600                  MOVE 01011  TO WS-TIERSDAT-PREL-C1                      
117700                END-IF                                                    
117800***OM   PREL ERSDAT LÄNGRE FRAM ÄN 26 VECKOR BEHANDLA SOM VANLIGT         
117900***FÖR   ERSKOD 03 OCH 06                                                 
118000***OM   PREL ERSDAT + LEDTID LÄNGRE FRAM ÄN 26 VECKOR BEHANDLA SOM        
118100***VANLIGT   FÖR ERSKOD 01 ,02 ,04 OCH 05                                 
118200                IF CLAG-KDERS = 03 OR 06                                  
118300                  MOVE WS-TIERSDAT-PREL-C1 TO WS-PREL-ERSDAT              
118400                ELSE                                                      
118500                  PERFORM S88-SLUT-DATUM-ERS-LEDT                         
118600                  MOVE WS-ERSDAT-AND-LT TO WS-PREL-ERSDAT                 
118700                END-IF                                                    
118800                IF WS-PREL-ERSDAT > W26-V-FRAMAT-TIAAVVD                  
118900                  MOVE JA    TO NORMAL-REF-SW                             
119000                  MOVE CREF-KVREFPKT TO WS-KVREFPKT-DC                    
119100                  IF CLAG-KVTILLG-TOT < WS-KVREFPKT-DC                    
119200                     PERFORM CB-VALJA-ARTIKLAR                            
119300                  END-IF                                                  
119400                ELSE                                                      
119500                  IF WS-TIERSDAT-PREL-C1 < DAGENS-TIAAVVD                 
119600****OM   PREL ERSDAT ÄR PASSERAT INGET REFILLFÖRSLAG/ORDER                
119700                    CONTINUE                                              
119800                  ELSE                                                    
119900****BEHANDLAR     DAGAR FRAM TILL TIERSDAT + TOT LEDTID FRÅN 4408         
120000                    PERFORM CH-BEHOV-TILL-ERSDAT                          
120100                    IF CLAG-KVTILLG-TOT > WS-BEHOV-VID-ERS                
120200                      CONTINUE                                            
120300                    ELSE                                                  
120400                      COMPUTE WS-ANTAL-SLUT-ERS =                         
120500                              WS-BEHOV-VID-ERS - CLAG-KVTILLG-TOT         
120600                      PERFORM CB-VALJA-ARTIKLAR                           
120700                    END-IF                                                
120800                  END-IF                                                  
120900                END-IF                                                    
121000              END-IF                                                      
121100           END-IF                                                         
121200        END-IF                                                            
121300*                                                                         
121301**250225 FUNCTION TO STOP WHEN MI = YES AND ALWAYS AIR = S                
121310        IF CREF-FLREFNYO = JA                                             
121320        AND CREF-FLFLYG = 'S'                                             
121330          MOVE NEJ TO KONTROLL-SW                                         
121340        END-IF                                                            
121400        PERFORM S8-CHECK-REFILL-PART-K7                                   
121500*                                                                         
121600        IF KONTROLL                                                       
121700           PERFORM CC-KONTROLERA-DC                                       
121800           IF BRIST-I-DC                                                  
121900              PERFORM CD-BERAKNA-PAFYLLNING                               
122000           END-IF                                                         
122100        END-IF                                                            
122200                                                                          
122300*                                                                         
122400        IF BEORDRA                                                        
122500           IF WS-ANTAL-SLUT > ZERO                                        
122600              PERFORM CE-SKAPA-ORDER                                      
122700**HÄR STOPPAR MAN AUTOMATGODKÄNDA ORDRAR MEDANS MAN I                     
122800**W2711200 STOPPAR MANUELLT GODKÄNDA ORDRAR                               
122900**W27210-KDREFTYP = 'O' ÄR AUTOMATGODKÄND , AUTOREFILL = J                
123000**DC-PARAMETER-TIME='99' = VECKOKÖRNINGEN W271V2                          
123100**WS-DAG-I-VECKA NOT = 2(TISDAG)                                          
123200**WS-DAG-I-VECKA NOT = 3(ONSDAG)                                          
123300              IF W27210-KDREFTYP = 'O'                                    
123400                MOVE NEJ           TO RELEASEDAY-SW                       
123500                PERFORM CI-CHECK-RELEASEDAY                               
123600              END-IF                                                      
123700              IF  W27210-KDREFTYP NOT = 'O'                               
123800              OR (W27210-KDREFTYP = 'O' AND                               
123900                  RELEASEDAY)                                             
124000                PERFORM S11-SKRIV-W27210                                  
124100                PERFORM S70-SUMMERA-ORDRAR                                
124200              END-IF                                                      
124300           END-IF                                                         
124400        END-IF                                                            
124500     END-IF                                                               
124600                                                                          
124700     .                                                                    
124800     EJECT                                                                
124900                                                                          
125000 CB-VALJA-ARTIKLAR SECTION.                                               
125100                                                                          
125200                                                                          
125300***  VÄLJER UT DE ARTTIKLAR DÄR REFILLBEHOV SKALL KONTROLERAS             
125400***  SÅDANA ARTIKLAR SKALL VARA AKTIVA - REFILLSTATUS = A                 
125500***  DE FÅR INTE VARA REFILLSTOPPPADE ELLER HA REFILLSTOP-                
125600***  DATUM FRAMÅT I TIDEN, ARTIKLARNA FÅR INTE VÄNTA                      
125700***  PÅ NY ORDERINGÅNG.                                                   
125800                                                                          
125900     MOVE CLAG-KVQPACK-3  TO WS-KVQPACK-3                                 
126000                                                                          
126100     MOVE WS-SPAR-IDARTNR TO BYT03-IDARTNR                                
126200     IF BYT03-OBJEKT OR                                                   
126300        ART-KDSORT = 'SW' OR                                              
126400        CLAG-FLLSRDEL = NEJ                                               
126500        MOVE NEJ TO SW-REFILL-KANSKE                                      
126600     ELSE                                                                 
126700        MOVE JA  TO SW-REFILL-KANSKE                                      
126800     END-IF                                                               
126900                                                                          
127000     IF (CLAG-FLREFILL = JA) OR (ART-FLIART = JA                          
127100                             AND CLAG-FLREFILL = NEJ)                     
127200       IF (CREF-FLREFILL = JA                                             
127300       AND WS-PRIS > ZERO)                                                
127400       OR ((CLAG-KVROS > ZERO)                                            
127500       AND WS-PRIS > ZERO)                                                
127600         MOVE CREF-TIREFSTO          TO TMP1-YYMMDD                       
127700         MOVE DAGENS-DATUM           TO TMP2-YYMMDD                       
127800         PERFORM WY2000P1                                                 
127900         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
128000*********                                                                 
128100           IF CREF-FLREFNYO = NEJ                                         
128200           OR CLAG-KVROS > 0                                              
128300           OR WS-KVOKS-VOR > 0                                            
128400             MOVE ART-TIFINLV            TO TMP1-YYWWD                    
128500             IF CREF-IDDC-REF (1:1) = '4'                                 
128600               MOVE 'US'            TO W-IDLAND                           
128700             END-IF                                                       
128800             IF CREF-IDDC-REF (1:1) = '7'                                 
128900               MOVE 'CN'            TO W-IDLAND                           
129000             END-IF                                                       
129100             PERFORM IMS-GU-WDK712-LART                                   
129200             IF SEGMENT-FINNS                                             
129300               MOVE LART-DAPUBL TO WS-DAPUBL                              
129400                 IF LART-DAPUBL > 0                                       
129500                   MOVE 'AAMMDD'    TO DAT-KDDATFORM                      
129600                   MOVE LART-DAPUBL TO DAT-I-TIDATUM                      
129700                   CALL WDATKONV USING DAT-KDDATFORM                      
129800                                         DAT-I-TIDATUM                    
129900                                         DAT-O-TIDATUM                    
130000                                         DAT-KDSVAR                       
130100                                                                          
130200                   IF DAT-KDSVAR-OK                                       
130300                      MOVE DAT-TIAAVVD TO TMP1-YYWWD                      
130400                   ELSE                                                   
130500                      MOVE 'FEL WDATKONV CB-VALJA-ARTIKLAR' TO            
130600                                            FELTEXT-STR                   
130700                      DISPLAY FELTEXT                                     
130800                      PERFORM S99-ABEND                                   
130900                   END-IF                                                 
131000                 END-IF                                                   
131100                 IF LART-KVQPACK-3 > 0                                    
131200                   MOVE LART-KVQPACK-3 TO WS-KVQPACK-3                    
131300                 END-IF                                                   
131400             ELSE                                                         
131500               MOVE ZERO               TO WS-DAPUBL                       
131600             END-IF                                                       
131700             MOVE DD-LT-FRAMAT-TIAAVVD        TO TMP2-YYWWD               
131800             PERFORM WY2000P2                                             
131900             IF TMP1-YYWWD <= TMP2-YYWWD                                  
132000                                                                          
132100                                                                          
132200               MOVE WS-DAPUBL(1:4)         TO WS-DAPUBL-AAR               
132300               MOVE W-DAGENS-DATUM(1:4)    TO WS-DAGENS-AAR               
132400               COMPUTE WS-DAGENS-AAR-MINUS1 =                             
132500                       WS-DAGENS-AAR - 1                                  
132600               IF    (WS-DAPUBL > ZERO)                                   
132700               AND (WS-DAPUBL-AAR >= WS-DAGENS-AAR-MINUS1)                
132800                                                                          
132900                 MOVE B6-REF-KVDLTID-TOT                                  
133000                           TO DAG-KVKALDAG                                
133100                 MOVE WS-DAPUBL (3:6)                                     
133200                           TO DAG-TIAAMMDD-TOM                            
133300                 MOVE 003    TO DAG-KDCALL                                
133400                 CALL WDAGKONV USING DAG-KDCALL                           
133500                                     DAG-DATUM-AREA                       
133600                                     DAG-KDSVAR                           
133700                 IF DAG-KDSVAR = SPACE                                    
133800                   CONTINUE                                               
133900                 ELSE                                                     
134000                   MOVE 'FEL FRÅN WDAGKONV, I CB-SECTION'                 
134100                               TO     FELTEXT-STR                         
134200                    DISPLAY FELTEXT                                       
134300                    PERFORM S99-ABEND                                     
134400                 END-IF                                                   
134500                                                                          
134600                 MOVE DAG-TIAAMMDD-FOM                                    
134700                           TO TMP1-YYMMDD                                 
134800                 MOVE DAGENS-DATUM                                        
134900                           TO TMP2-YYMMDD                                 
135000               ELSE                                                       
135100                 MOVE ZERO                                                
135200                           TO TMP1-YYMMDD                                 
135300                              TMP2-YYMMDD                                 
135400                              WS-DAPUBL                                   
135500               END-IF                                                     
135600               PERFORM WY2000P1                                           
135700                                                                          
135800               IF WS-DAPUBL = ZERO                                        
135900               OR TMP1-YYMMDD < TMP2-YYMMDD                               
136000                 MOVE JA TO KONTROLL-SW                                   
136100               END-IF                                                     
136200             END-IF                                                       
136300           END-IF                                                         
136400         END-IF                                                           
136500       END-IF                                                             
136600     END-IF                                                               
136700     .                                                                    
136800     EJECT                                                                
136900                                                                          
137000 CC-KONTROLERA-DC SECTION.                                                
137100                                                                          
137200****TOG BORT KONTROLL 28/6 I TESTER KINA EXPORT                           
137300*    IF (CREF-FLFLYG = NEJ                                                
137400*    OR  CREF-FLFLYG = SPACE)                                             
137500       MOVE CREF-KVREFPKT TO WS-KVREFPKT-DC                               
137600       PERFORM S51-L-STOCK-KOLL-CDC--NDC                                  
137700       IF (WS-KVREFPKT-DC = ZERO                                          
137800       OR  (UTIL-KVPB-TOT <= 0.5))                                        
137900       AND (WS-KVDISP-FLYG > ZERO                                         
138000       OR       WS-KVDISP-FLYG = ZERO)                                    
138100*                                                                         
138200*   OM ORDERPUNKT ÄR NOLL OCH RESTORDERN ÄR TÄCKT                         
138300*   ELLER LÅGFREKVENT ARTIKEL OCH RESTORDERN ÄR TÄCKT                     
138400*   SKA DET INTE BLI NÅGOT FÖRSLAG                                        
138500*                                                                         
138600         CONTINUE                                                         
138700       ELSE                                                               
138800         IF WS-KVDISP-FLYG < WS-KVPB-FLYG                                 
138900**********LOW STOCK WARNING POTENTIALLY, **********                       
139000**********NEED TO CHECK PRODSTATUS ALSO  **********                       
139100            PERFORM S51B-SUM-OF-PROD-STATUS-R-U-P                         
139200            ADD WS-KVBEART-Q-RUP     TO WS-KVDISP-FLYG                    
139300                                                                          
139400            IF (WS-KVREFPKT-DC = ZERO                                     
139500            OR  (UTIL-KVPB-TOT <= 0.5))                                   
139600            AND (WS-KVDISP-FLYG > ZERO                                    
139700            OR       WS-KVDISP-FLYG = ZERO)                               
139800*                                                                         
139900*      OM ORDERPUNKT ÄR NOLL OCH RESTORDERN ÄR TÄCKT                      
140000*      NÄR PRODSTATUSES R U OCH P INKLUDERAS                              
140100*      ELLER LÅGFREKVENT ARTIKEL OCH RESTORDERN ÄR TÄCKT                  
140200*      NÄR PRODSTATUSES R U OCH P INKLUDERAS                              
140300*      SKA DET INTE BLI NÅGOT FÖRSLAG                                     
140400*                                                                         
140500               CONTINUE                                                   
140600            ELSE                                                          
140700               IF WS-KVDISP-FLYG < WS-KVPB-FLYG                           
140800****************LOW STOCK WARNING DEFINITELY ****                         
140900                 IF CREF-FLFLYG = 'S'                                     
140910                    MOVE NEJ TO FLYGFORSLAG-SW                            
140912                    IF CLAG-KVROS > ZERO                                  
140914                       MOVE JA TO BRIST-I-DC-SW                           
140915                    END-IF                                                
140920                 ELSE                                                     
141000                    MOVE JA TO FLYGFORSLAG-SW                             
141100                 END-IF                                                   
141110               END-IF                                                     
141200            END-IF                                                        
141300         END-IF                                                           
141400       END-IF                                                             
141500*    END-IF                                                               
141600                                                                          
141700     IF FLYGFORSLAG                                                       
141800        PERFORM S52-L-STOCK-ANTAL-CDC                                     
141900        IF WS-ANTAL-BER > ZERO                                            
142000           MOVE JA TO BRIST-I-DC-SW                                       
142100        ELSE                                                              
142200           MOVE NEJ TO FLYGFORSLAG-SW                                     
142300        END-IF                                                            
142400     END-IF                                                               
142500                                                                          
142600     IF EJ-BRIST-I-DC                                                     
142700        IF CREF-KDREFSTA = 'A'                                            
142800        OR (CREF-KDREFSTA = 'P'                                           
142900        AND (CLAG-KVROS   > ZERO                                          
143000        OR   CLAG-KVVORKO > ZERO))                                        
143100           IF WS-KVREFPKT-DC > +0                                         
143200           OR CREF-FLFLYG = JA                                            
143300           OR DCS-FLEXCP2-REFBER = JA                                     
143400              IF CLAG-KVTILLG-TOT < WS-KVREFPKT-DC                        
143500                 MOVE JA TO BRIST-I-DC-SW                                 
143600              END-IF                                                      
143700           END-IF                                                         
143800        END-IF                                                            
143900     END-IF                                                               
144000     .                                                                    
144100     EJECT                                                                
144200                                                                          
144300                                                                          
144400 CD-BERAKNA-PAFYLLNING SECTION.                                           
144500                                                                          
144600     IF FLYGFORSLAG                                                       
144700        PERFORM S47-BER-KVPB-ANTV-DC                                      
144800        IF CLAG-KVQPACK-1 > ZERO                                          
144900           PERFORM S56C-SATT-QX-PROCENT-BRYTNING                          
145000           PERFORM S56B-KVQPACK-1                                         
145100           MOVE WS-ANTAL-QX TO WS-ANTAL-SLUT                              
145200        ELSE                                                              
145300           MOVE WS-ANTAL-BER TO WS-ANTAL-SLUT                             
145400        END-IF                                                            
145500        MOVE JA TO BEORDRA-SW                                             
145600     ELSE                                                                 
145700        IF CREF-KVREFPKT = 1                                              
145800        AND CLAG-KVQ = 1                                                  
145900           MOVE 1 TO WS-ANTAL-BER                                         
146000        ELSE                                                              
146100           IF CLAG-KVUTRS = 0                                             
146200             IF CREF-KVREFPKT > ZERO                                      
146300               PERFORM S35-BER-ANTAL-V-STORRE-BRIST                       
146400             ELSE                                                         
146500               IF  (CREF-FLFLYG = (JA OR 'S'))                            
146600               AND CLAG-KVROS  > ZERO                                     
146700                   MOVE CLAG-KVROS                                        
146800                     TO WS-ANTAL-BER                                      
146900               END-IF                                                     
147000             END-IF                                                       
147100           ELSE                                                           
147200             PERFORM S37-BER-ANTAL-V-UTRSALDO                             
147300           END-IF                                                         
147400        END-IF                                                            
147500                                                                          
147600        PERFORM CDA-ANPASSA-ANTAL                                         
147700                                                                          
147800     END-IF                                                               
147900                                                                          
148000     PERFORM CDE-KONTROLLERA-FORSLAG                                      
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400 CDA-ANPASSA-ANTAL SECTION.                                               
148500                                                                          
148520     IF CLAG-KVROS > 0   AND                                              
148530        CREF-FLFLYG = 'S'                                                 
148540       IF WS-ANTAL-BER > ZERO                                             
148550         MOVE WS-ANTAL-BER            TO WS-ANTAL-SLUT                    
148560       END-IF                                                             
148570     ELSE                                                                 
148600       IF CREF-KVREFPKT = +1 AND                                          
148700          CLAG-KVQ = +1 AND                                               
148800          CLAG-KVQPACK-1 < 2    AND                                       
148900          WS-ANTAL-BER = +1                                               
149000             MOVE +1 TO WS-ANTAL-SLUT                                     
149100       ELSE                                                               
149200          PERFORM S47-BER-KVPB-ANTV-DC                                    
149300          PERFORM S56C-SATT-QX-PROCENT-BRYTNING                           
149400          PERFORM S56-Q-ANPASSA                                           
149500          IF WS-ANTAL-SLUT > WS-KVPB-ANTV-DC                              
149600          AND WS-KVPB-ANTV-DC > ZERO                                      
149700*   REFILLA MAX X ANTAL VECKOR DOCK MINST UPP TILL REFILLPUNKTEN          
149800             IF (CLAG-KVTILLG-TOT + WS-KVPB-ANTV-DC) <                    
149900                CREF-KVREFPKT                                             
150000                COMPUTE WS-KVPB-ANTV-DC =                                 
150100                CREF-KVREFPKT - CLAG-KVTILLG-TOT                          
150200             END-IF                                                       
150300             IF CLAG-KVQPACK-1 > 1                                        
150400                MOVE WS-KVPB-ANTV-DC                                      
150500                               TO WS-ANTAL-BER                            
150600                PERFORM S56B-KVQPACK-1                                    
150700                MOVE WS-ANTAL-QX                                          
150800                               TO WS-ANTAL-SLUT                           
150900             ELSE                                                         
151000                MOVE WS-KVPB-ANTV-DC                                      
151100                               TO WS-ANTAL-SLUT                           
151200             END-IF                                                       
151300          END-IF                                                          
151400       END-IF                                                             
151410     END-IF                                                               
151500     MOVE JA TO BEORDRA-SW                                                
151600     .                                                                    
151700     EJECT                                                                
151800                                                                          
151900 CDE-KONTROLLERA-FORSLAG SECTION.                                         
152000                                                                          
152100     IF WS-KVSPARR-KVAL     > 0      OR                                   
152200        INT-SLAG-KDLEVSP   > 0       OR                                   
152300        CLAG-KVUTRS        > 0       OR                                   
152400        CREF-FLREFBEO = NEJ          OR                                   
152500        CREF-FLREFBEO = 'S'          OR                                   
152510*                                                                         
152600       (CREF-FLREFBEO = JA           AND                                  
152700        CREF-FLFLYG        = JA)     OR                                   
152710*                                                                         
152720       (CREF-FLREFBEO = JA           AND                                  
152730        CREF-FLFLYG        = 'S'     AND                                  
152740        CLAG-KVROS > ZERO)                                                
152800          MOVE JA TO FORSLAG-SW                                           
152900     END-IF                                                               
153000     .                                                                    
153100     EJECT                                                                
153200                                                                          
153300                                                                          
153400 CE-SKAPA-ORDER SECTION.                                                  
153500                                                                          
153600     IF FLYGFORSLAG                                                       
153700        PERFORM CED-SKAPA-FLYGFORSLAG                                     
153800     ELSE                                                                 
153900        IF CREF-FLREFBEO = NEJ                                            
154000        OR CREF-FLREFBEO = 'S'                                            
154100             MOVE JA TO FORSLAG-SW                                        
154200        END-IF                                                            
154300        IF FORSLAG                                                        
154400           PERFORM CEA-SKAPA-REFILLFORSLAG                                
154500        ELSE                                                              
154600           PERFORM CEB-SKAPA-REFILLORDER                                  
154700        END-IF                                                            
154800     END-IF                                                               
154900     .                                                                    
155000     EJECT                                                                
155100                                                                          
155200                                                                          
155300 CEA-SKAPA-REFILLFORSLAG SECTION.                                         
155400                                                                          
155500     IF CREF-FLFLYG = JA                                                  
155600       MOVE 'A'                  TO W27210-KDREFTYP                       
155700     ELSE                                                                 
155800       MOVE 'B'                  TO W27210-KDREFTYP                       
155900     END-IF                                                               
156000     MOVE '11'                   TO W27210-IDDC                           
156100     MOVE WS-SPAR-IDARTNR        TO W27210-IDARTNR                        
156200     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
156300     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
156400     MOVE WS-IDDISTR             TO W27210-IDDISTR                        
156500     MOVE ZERO                   TO W27210-IDKUNDNR                       
156600     MOVE 'P'                    TO W27210-KDREFORS                       
156700     MOVE ART-IDLEVNR            TO W27210-IDLEVNR                        
156800     MOVE '99'                   TO W27210-KDREFTXT                       
156900     MOVE ZERO                   TO W27210-KDFRAKT                        
157000     MOVE CLAG-IDDC-REF          TO W27210-IDDC-REF                       
157100     MOVE CREF-IDPERSON-BUY      TO W27210-IDPERSON-BUY                   
157200     IF CLAG-KDERS = ZERO                                                 
157300       MOVE WS-ANTAL-SLUT        TO W27210-KVBEART                        
157400     ELSE                                                                 
157500       IF WS-TIERSDAT-PREL-C1 = ZERO                                      
157600       OR NORMAL-REF                                                      
157700         MOVE WS-ANTAL-SLUT      TO W27210-KVBEART                        
157800       ELSE                                                               
157900         IF WS-ANTAL-SLUT-ERS < WS-ANTAL-SLUT                             
158000           MOVE WS-ANTAL-SLUT-ERS TO W27210-KVBEART                       
158100         END-IF                                                           
158200       END-IF                                                             
158300     END-IF                                                               
158400                                                                          
158500     PERFORM S57-RANGORDNA-REFTEXT                                        
158600     .                                                                    
158700     EJECT                                                                
158800                                                                          
158900                                                                          
159000 CEB-SKAPA-REFILLORDER SECTION.                                           
159100                                                                          
159200     MOVE 'O'                    TO W27210-KDREFTYP                       
159300     MOVE SPACE                  TO W27210-KDREFORS                       
159400     MOVE ZERO                   TO W27210-KDFRAKT                        
159500     MOVE '11'                   TO W27210-IDDC                           
159600     MOVE WS-SPAR-IDARTNR        TO W27210-IDARTNR                        
159700     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
159800     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
159900     MOVE WS-IDDISTR             TO W27210-IDDISTR                        
160000     MOVE ZERO                   TO W27210-IDKUNDNR                       
160100     MOVE ART-IDLEVNR            TO W27210-IDLEVNR                        
160200     MOVE ZERO                   TO W27210-KDREFTXT                       
160300     MOVE CLAG-IDDC-REF          TO W27210-IDDC-REF                       
160400     MOVE CREF-IDPERSON-BUY      TO W27210-IDPERSON-BUY                   
160500     IF CLAG-KDERS = ZERO                                                 
160600       MOVE WS-ANTAL-SLUT        TO W27210-KVBEART                        
160700     ELSE                                                                 
160800       IF WS-TIERSDAT-PREL-C1 = ZERO                                      
160900       OR NORMAL-REF                                                      
161000         MOVE WS-ANTAL-SLUT      TO W27210-KVBEART                        
161100       ELSE                                                               
161200         IF WS-ANTAL-SLUT-ERS < WS-ANTAL-SLUT                             
161300           MOVE WS-ANTAL-SLUT-ERS TO W27210-KVBEART                       
161400         END-IF                                                           
161500       END-IF                                                             
161600     END-IF                                                               
161700     .                                                                    
161800     EJECT                                                                
161900                                                                          
162000 CED-SKAPA-FLYGFORSLAG SECTION.                                           
162100                                                                          
162200     IF ((CLAG-KVROS > ZERO OR CLAG-KVVORKO > ZERO)                       
162300     AND  WS-KVDISP-FLYG < ZERO)                                          
162400       MOVE 'C'                     TO W27210-KDREFTYP                    
162500     ELSE                                                                 
162600       MOVE 'A'                     TO W27210-KDREFTYP                    
162700     END-IF                                                               
162800                                                                          
162900     MOVE '11'                      TO W27210-IDDC                        
163000     MOVE WS-SPAR-IDARTNR           TO W27210-IDARTNR                     
163100     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
163200     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
163300     MOVE WS-IDDISTR                TO W27210-IDDISTR                     
163400     MOVE ZERO                      TO W27210-IDKUNDNR                    
163500     MOVE 'P'                       TO W27210-KDREFORS                    
163600     MOVE ART-IDLEVNR               TO W27210-IDLEVNR                     
163700     MOVE '99'                      TO W27210-KDREFTXT                    
163800     MOVE CLAG-IDDC-REF          TO W27210-IDDC-REF                       
163900     MOVE CREF-IDPERSON-BUY      TO W27210-IDPERSON-BUY                   
164000     MOVE ZERO                   TO W27210-KDFRAKT                        
164100     IF CLAG-KDERS = ZERO                                                 
164200       MOVE WS-ANTAL-SLUT        TO W27210-KVBEART                        
164300     ELSE                                                                 
164400       IF WS-TIERSDAT-PREL-C1 = ZERO                                      
164500       OR NORMAL-REF                                                      
164600         MOVE WS-ANTAL-SLUT      TO W27210-KVBEART                        
164700       ELSE                                                               
164800         IF WS-ANTAL-SLUT-ERS < WS-ANTAL-SLUT                             
164900           MOVE WS-ANTAL-SLUT-ERS TO W27210-KVBEART                       
165000         END-IF                                                           
165100       END-IF                                                             
165200     END-IF                                                               
165300                                                                          
165400     IF FORSLAG                                                           
165500        PERFORM S57-RANGORDNA-REFTEXT                                     
165600     END-IF                                                               
165700     .                                                                    
165800     EJECT                                                                
165900                                                                          
166000 CG-KOLLA-KVPB-PLAN SECTION.                                              
166100                                                                          
166200     PERFORM CGA-CALC-PROGNOS-REFILLED-XDC                                
166300                                                                          
166400     IF (CLAG-DASEASON >= W-DAGENS-DATUM)                                 
166500     AND (CLAG-DAPBPLAN < 500000)                                         
166600*****OM MANUELL SÄSONG FINNS ANVÄND VÄRDE FRÅN WDK611                     
166700       MOVE +1  TO IX-2                                                   
166800       PERFORM UNTIL IX-2 > 12                                            
166900         MOVE CLAG-RESEASON-PLAN (IX-2) TO WS-RESEASON-PLAN (IX-2)        
167000         ADD +1 TO IX-2                                                   
167100       END-PERFORM                                                        
167200     ELSE                                                                 
167300*****MASKINELLT FRÅN WDK629                                               
167400       MOVE +1  TO IX-2                                                   
167500       PERFORM UNTIL IX-2 > 12                                            
167600         MOVE 1.00                      TO WS-RESEASON-PLAN (IX-2)        
167700         ADD +1 TO IX-2                                                   
167800       END-PERFORM                                                        
167900     END-IF                                                               
168000     .                                                                    
168100     EJECT                                                                
168200                                                                          
168300 CGA-CALC-PROGNOS-REFILLED-XDC SECTION.                                   
168400*  ----  CALC TOTAL FORECAST OF DCS REFILLED FROM CDC                     
168500*  ----  FOR FORECAST OF ALL DC CALL W271UTIL WITH KDCAL 002              
168600                                                                          
168700     MOVE INIT-UTIL-W271UTIL    TO UTIL-W271UTIL                          
168800     MOVE 003                   TO UTIL-KDCALL                            
168900     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
169000*---- UTIL-TIAAVVD IS SET IN SECTION A-INIT                               
169100                                                                          
169200     CALL W271UTIL USING UTIL-W271UTIL                                    
169300                         UTIL-WDK6-PCB                                    
169400                         UTIL-WDK7-PCB                                    
169500                         UTIL-WDB6-PCB                                    
169600                                                                          
169700     IF UTIL-KDSVAR-OK                                                    
169800        CONTINUE                                                          
169900****     VALUE IS IN UTIL-KVPB-TOT                                        
170000     ELSE                                                                 
170100        MOVE 'FEL FRÅN W271UTIL '                                         
170200                                TO FELTEXT-STR                            
170300        DISPLAY FELTEXT                                                   
170400        PERFORM S99-ABEND                                                 
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800                                                                          
170900                                                                          
171000 CH-BEHOV-TILL-ERSDAT SECTION.                                            
171100                                                                          
171200     MOVE 'AAVVD '                  TO DAT-KDDATFORM                      
171300     MOVE WS-PREL-ERSDAT            TO DAT-I-TIDATUM                      
171400     CALL WDATKONV USING DAT-KDDATFORM                                    
171500                           DAT-I-TIDATUM                                  
171600                           DAT-O-TIDATUM                                  
171700                           DAT-KDSVAR                                     
171800                                                                          
171900     IF DAT-KDSVAR-OK                                                     
172000        MOVE DAT-TIAAMMDD TO WS-AAMMDD-ERSDAT                             
172100     ELSE                                                                 
172200        MOVE 'FEL WDATKONV CH-BEHOV-TILL-ERS' TO                          
172300                              FELTEXT-STR                                 
172400        DISPLAY FELTEXT                                                   
172500        PERFORM S99-ABEND                                                 
172600     END-IF                                                               
172700     INITIALIZE W271LTPB-W271LTPB                                         
172800*       ANTAL BEHOV DAGAR FRAM TILL ERSÄTTNINGSDATUM +                    
172900*       EV LEDTID(KDERS = 01,02,04,05)                                    
173000     MOVE WS-AAMMDD-ERSDAT   TO W271LTPB-BINNDAY-TIAAMMDD                 
173100     MOVE '11'               TO W271LTPB-IDDC                             
173200     MOVE CREF-IDDC-REF      TO W271LTPB-IDDC-REF                         
173300     MOVE ZERO               TO W271LTPB-START-DATUM                      
173400                                                                          
173500     CALL W271LTPB USING                                                  
173600                       W271LTPB-W271LTPB                                  
173700                                                                          
173800*      SUMMERA BEHOV FÖR DE ARBETSDAGAR SOM LIGGER INOM                   
173900*      PREL ERSDAT + LEDTID                                               
174000                                                                          
174100     COMPUTE WS-KVPB-REF-DAY-PER-I =                                      
174200       (((UTIL-KVPB-TOT *                                                 
174300       WS-RESEASON-PLAN(W271LTPB-PER-I-TIRP)) / 4.33) / 7)                
174400                                                                          
174500     COMPUTE WS-KVPB-REF-DAY-PER-II =                                     
174600        (((UTIL-KVPB-TOT *                                                
174700        WS-RESEASON-PLAN(W271LTPB-PER-II-TIRP)) / 4.33) / 7)              
174800                                                                          
174900     COMPUTE WS-KVPB-REF-DAY-PER-III =                                    
175000         (((UTIL-KVPB-TOT *                                               
175100        WS-RESEASON-PLAN(W271LTPB-PER-III-TIRP)) / 4.33) / 7)             
175200                                                                          
175300     COMPUTE WS-KVPB-REF-DAY-PER-IV =                                     
175400         (((UTIL-KVPB-TOT *                                               
175500        WS-RESEASON-PLAN(W271LTPB-PER-IV-TIRP)) / 4.33) / 7)              
175600                                                                          
175700     COMPUTE WS-KVPB-REF-DAY-PER-V =                                      
175800         (((UTIL-KVPB-TOT *                                               
175900        WS-RESEASON-PLAN(W271LTPB-PER-V-TIRP)) / 4.33) / 7)               
176000                                                                          
176100     COMPUTE WS-KVPB-REF-DAY-PER-VI =                                     
176200         (((UTIL-KVPB-TOT *                                               
176300        WS-RESEASON-PLAN(W271LTPB-PER-VI-TIRP)) / 4.33) / 7)              
176400                                                                          
176500     COMPUTE WS-ANTAL-TILL-ERS  ROUNDED =                                 
176600        (W271LTPB-KVDAGAR-PER-I * WS-KVPB-REF-DAY-PER-I)     +            
176700        (W271LTPB-KVDAGAR-PER-II * WS-KVPB-REF-DAY-PER-II)   +            
176800        (W271LTPB-KVDAGAR-PER-III * WS-KVPB-REF-DAY-PER-III) +            
176900        (W271LTPB-KVDAGAR-PER-IV  * WS-KVPB-REF-DAY-PER-IV)  +            
177000        (W271LTPB-KVDAGAR-PER-V   * WS-KVPB-REF-DAY-PER-V)   +            
177100        (W271LTPB-KVDAGAR-PER-VI  * WS-KVPB-REF-DAY-PER-VI)               
177200                                                                          
177300     IF WS-ANTAL-TILL-ERS < ZERO                                          
177400         MOVE ZERO          TO WS-BEHOV-VID-ERS                           
177500     ELSE                                                                 
177600        MOVE WS-ANTAL-TILL-ERS TO WS-ANTAL-TILL-ERS-AVRUND                
177700        IF WS-ANTAL-TILL-ERS-AVRUND-HEL = ZERO                            
177800           IF WS-ANTAL-TILL-ERS-AVRUND-DEC < 4                            
177900              CONTINUE                                                    
178000           ELSE                                                           
178100              MOVE 1 TO WS-ANTAL-TILL-ERS-AVRUND-HEL                      
178200           END-IF                                                         
178300        ELSE                                                              
178400           IF WS-ANTAL-TILL-ERS-AVRUND-DEC < 3                            
178500              CONTINUE                                                    
178600           ELSE                                                           
178700              ADD 1 TO WS-ANTAL-TILL-ERS-AVRUND-HEL                       
178800           END-IF                                                         
178900        END-IF                                                            
179000        MOVE WS-ANTAL-TILL-ERS-AVRUND-HEL TO WS-BEHOV-VID-ERS             
179100     END-IF                                                               
179200     .                                                                    
179300     EJECT                                                                
179400                                                                          
179500 CI-CHECK-RELEASEDAY SECTION.                                             
179600                                                                          
179700     MOVE WS-DAG-I-VECKA      TO SUB                                      
179800                                                                          
179900     EVALUATE TRUE                                                        
180000     WHEN NOTDANGEROUS                                                    
180100      AND B6-REF-FLREFBLK(SUB) = 'J'                                      
180200       SET RELEASEDAY         TO TRUE                                     
180300     WHEN DANGEROUSGOODS                                                  
180400      AND (  (B6-REF-KDREFDG(SUB) = 'J')                                  
180500          OR (B6-REF-KDREFDG(SUB) = 'E' AND WEEK-EVEN)                    
180600          OR (B6-REF-KDREFDG(SUB) = 'O' AND WEEK-ODD)  )                  
180700       SET RELEASEDAY         TO TRUE                                     
180800     END-EVALUATE                                                         
180900                                                                          
181000     .                                                                    
181100     EJECT                                                                
181200                                                                          
181300 D-CHECK-STOP-DATE SECTION.                                               
181400                                                                          
181500     PERFORM IMS-GNP-WDK722-INTERN                                        
181600     IF SEGMENT-FINNS                                                     
181700        MOVE INT-XLAG-TIREFSTO-LOC TO TMP1-YYMMDD                         
181800        MOVE DAGENS-DATUM      TO TMP2-YYMMDD                             
181900        PERFORM WY2000Q1                                                  
182000* WHEN WDK722 EXISTS - IF THE STOP DATE IS NOT 0 AND IT IS                
182100* LESS THAN THE CURRENT DATE, REFILL SHOULD BE DONE                       
182200        IF ((TMP1-YYMMDD > +0)                                            
182300        AND (TMP1-YYMMDD < TMP2-YYMMDD))                                  
182400            MOVE JA            TO INTERN-REFILL-SW                        
182500        END-IF                                                            
182600* IF THE STOP DATE IS 0, REFILL SHOULD BE DONE                            
182700        IF TMP1-YYMMDD = +0                                               
182800            MOVE JA            TO INTERN-REFILL-SW                        
182900        END-IF                                                            
183000     ELSE                                                                 
183100* IF WDK722 DOES NOT EXIST, REFILL SHOULD BE DONE                         
183200        MOVE JA                TO INTERN-REFILL-SW                        
183300     END-IF                                                               
183400     .                                                                    
183500     EJECT                                                                
183600                                                                          
183700                                                                          
183800 Z-FINIT SECTION.                                                         
183900                                                                          
184000                                                                          
184100     CLOSE W272DC                                                         
184200           W27210                                                         
184300     SKIP2                                                                
184400     MOVE 'S' TO POSTSUM-OPKOD                                            
184500     CALL POSTSUM USING POSTSUM-PARM                                      
184600     .                                                                    
184700     EJECT                                                                
184800                                                                          
184900 S8-CHECK-REFILL-PART-K7 SECTION.                                         
185000                                                                          
185100***   CHECK IF SUPPLIER FOR NDC EQUALS 1441                               
185200***   IF TRUE THE REFILL OF THE PART FROM NDC TO CDC                      
185300***   IS NOT POSSIBLE.                                                    
185400     MOVE CLAG-IDDC-REF          TO W-IDDC-INT                            
185500                                    WS-IDDC                               
185600     MOVE JA                     TO NYCKLAR-SW                            
185700     PERFORM IMS-GU-WDK711-INTERN                                         
185800     IF SEGMENT-FINNS                                                     
185900        IF  INT-SLAG-IDLEVNR     = '1441'                                 
186000        AND (NDC-CN OR NDC-US)                                            
186100            MOVE NEJ      TO KONTROLL-SW                                  
186200                             NYCKLAR-SW                                   
186300        END-IF                                                            
186400     ELSE                                                                 
186500        MOVE NEJ                 TO KONTROLL-SW                           
186600                                    NYCKLAR-SW                            
186700     END-IF                                                               
186800     .                                                                    
186900     EJECT                                                                
187000                                                                          
187100                                                                          
187200 S11-SKRIV-W27210 SECTION.                                                
187300     SKIP2                                                                
187400     WRITE W27110-POST FROM W27210-AREA                                   
187500                                                                          
187600     MOVE 'W27210 ' TO POSTSUM-FDNAMN                                     
187700     MOVE 'W27210D2' TO POSTSUM-DDNAMN2                                   
187800     CALL POSTSUM USING POSTSUM-PARM                                      
187900     .                                                                    
188000     EJECT                                                                
188100                                                                          
188200                                                                          
188300 S21-LAES-W272DC   SECTION.                                               
188400                                                                          
188500      READ W272DC             INTO DC-POST                                
188600     .                                                                    
188700     SKIP3                                                                
188800                                                                          
188900 S34B-BER-BALANCE-CDC SECTION.                                            
189000                                                                          
189100     PERFORM IMS-GU-WDK901                                                
189200     IF SEGMENT-FINNS                                                     
189300       MOVE WDK9-ART-KVOKS-DAG    TO WS-KVOKS-DAG                         
189400       MOVE WDK9-ART-KVOKS-BULK   TO WS-KVOKS-BULK                        
189500       MOVE WDK9-ART-KVOKS-VOR    TO WS-KVOKS-VOR                         
189600     ELSE                                                                 
189700       MOVE ZERO             TO WS-KVOKS-DAG                              
189800                                WS-KVOKS-BULK                             
189900                                WS-KVOKS-VOR                              
190000     END-IF                                                               
190100     COMPUTE WS-BALANCE-CDC = CLAG-KVLS         -                         
190200                              WS-KVOKS-DAG      -                         
190300                              WS-KVOKS-BULK     -                         
190400                              WS-KVOKS-VOR      -                         
190500                              CLAG-KVROS        -                         
190600                              CLAG-KVRESS       -                         
190700                              CLAG-KVSPARR-KVAL                           
190800     .                                                                    
190900     EJECT                                                                
191000                                                                          
191100 S35-BER-ANTAL-V-STORRE-BRIST SECTION.                                    
191200                                                                          
191300     COMPUTE WS-ANTAL-BER =                                               
191400        (WS-KVREFPKT-DC - CLAG-KVTILLG-TOT) + CLAG-KVQ                    
191500     .                                                                    
191600     EJECT                                                                
191700                                                                          
191800                                                                          
191900 S37-BER-ANTAL-V-UTRSALDO SECTION.                                        
192000                                                                          
192100      MOVE ZERO   TO WS-1V-BEHOV-CDC                                      
192200      COMPUTE WS-1V-BEHOV-CDC = (UTIL-KVPB-TOT *                          
192300                                 WS-RESEASON-PLAN(WS-INNEV-TIRP))         
192400                                 / 4.33                                   
192500      COMPUTE WS-ANTAL-BER =                                              
192600         (WS-KVREFPKT-DC - CLAG-KVTILLG-TOT                               
192700          + WS-1V-BEHOV-CDC-HELTAL)                                       
192800     .                                                                    
192900     EJECT                                                                
193000                                                                          
193100 S45-FLYTTA-ARTIKELADRESS SECTION.                                        
193200                                                                          
193300     MOVE INT-SLAG-ADLAGOMR      TO W27210-ADLAGOMR-CDC                   
193400     MOVE INT-SLAG-ADGANG        TO W27210-ADGANG-CDC                     
193500     MOVE INT-SLAG-ADPLATS       TO W27210-ADPLATS-CDC                    
193600     MOVE CLAG-ADLAGOMR          TO W27210-ADLAGOMR-SDC                   
193700     MOVE CLAG-ADGANG            TO W27210-ADGANG-SDC                     
193800     MOVE CLAG-ADPLATS           TO W27210-ADPLATS-SDC                    
193900     .                                                                    
194000     EJECT                                                                
194100                                                                          
194200 S47-BER-KVPB-ANTV-DC SECTION.                                            
194300                                                                          
194400*                                                                         
194500*      52 VECKORS BEHOV (MAX PÅFYLLNAD)                                   
194600*                                                                         
194700     COMPUTE WS-KVPB-ANTV-DC = UTIL-KVPB-TOT * 12                         
194800     .                                                                    
194900     EJECT                                                                
195000                                                                          
195100 S48-HAMTA-REFILLDISTRIKT SECTION.                                        
195200                                                                          
195300     MOVE B6-REF-IDDISTR-REFILL TO WS-IDDISTR                             
195400                                   DIST35-IDDISTR                         
195500                                                                          
195600     .                                                                    
195700     EJECT                                                                
195800                                                                          
195900 S51-L-STOCK-KOLL-CDC--NDC SECTION.                                       
196000                                                                          
196100     PERFORM S55-GODK-E3--RO-CDC--TRANSFER                                
196200     PERFORM S51A-BER-ANK-UNDER-ETA-TID                                   
196300                                                                          
196400*-- NOTE:                                                                 
196500*--  DUE TO PERFORMANCE PROBLEMS                                          
196600*--  WHEN PROCESSING ALL PARTS AND DCS,                                   
196700*--  SECTION S51B- IS ONLY CALLED IF AN                                   
196800*--  AIR-PROPOSAL IS ABOUT TO BE CREATED                                  
196900*--  BEFORE CONSIDERING PRODSTATUSES R U AND P.                           
197000*--  THAT IS, SECTION S51B-                                               
197100*--  IS CALLED DIRECTLY FROM SECTION CC-                                  
197200*--  WHEN PRODSTATUS R U AND P SHOULD BE CHECKED                          
197300*--  BEFORE DECIDING IF AN AIR-PROPOSAL IS ACTUALLY NEEDED                
197400**** PERFORM S51B-SUM-OF-PROD-STATUS-R-U-P                                
197500                                                                          
197600     PERFORM S51C-BER-DISPONIBELT-FLYG                                    
197700     PERFORM S51D-BER-BEHOV-FLYG                                          
197800     .                                                                    
197900     EJECT                                                                
198000                                                                          
198100                                                                          
198200 S51A-BER-ANK-UNDER-ETA-TID SECTION.                                      
198300                                                                          
198400*    BERÄKANA ANTAL SOM ÄR FRAMME FÖRE DEN FLYGORDER SOM                  
198500*    SKICKAS IVÄG IDAG.                                                   
198600                                                                          
198700     MOVE 601                TO ETA-KDCALL                                
198800     MOVE CREF-IDDC-REF      TO ETA-IDDC-SEND                             
198900     MOVE WC-CDC-SE          TO ETA-IDDC-REC                              
199000     MOVE ZERO               TO ETA-IDARTNR                               
199100                                ETA-KDFRAKT                               
199200     MOVE SPACE              TO ETA-IDLEVNR                               
199300     MOVE ZERO               TO ETA-KDFRAKT                               
199400     MOVE DAGENS-DATUM       TO ETA-TIAAMMDD-ANROP                        
199500     IF DAGENS-DATUM-AAR < 50                                             
199600       MOVE 20               TO ETA-TISEKEL-ANROP                         
199700     ELSE                                                                 
199800       MOVE 19               TO ETA-TISEKEL-ANROP                         
199900     END-IF                                                               
200000                                                                          
200100     CALL W218ETA USING ETA-W218LETA ETA-ARTC-PCB ETA-ARTS-PCB            
200200                                     ETA-INLC-PCB ETA-LEVA-PCB            
200300                                     ETA-WDB6-PCB                         
200400     IF ETA-SVAR-OK = SPACE OR JA                                         
200500        MOVE ETA-TIAAMMDD-SVAR                                            
200600                            TO WS-FLYGT-MAX-TIAAMMDD                      
200700        PERFORM S48-HAMTA-REFILLDISTRIKT                                  
200800        PERFORM IMS-GU-INLC01                                             
200900        IF SEGMENT-FINNS                                                  
201000           PERFORM IMS-GNP-INLC11                                         
201100           PERFORM UNTIL SEGMENT-SAKNAS                                   
201200              IF INL-IDPTYP = 'R30'                                       
201300              OR INL-IDPTYP = 'R31'                                       
201400              OR INL-IDPTYP = '310'                                       
201500                 IF INL-IDDC = '11'                                       
201600                   MOVE INL-TIBERANK             TO TMP1-YYMMDD           
201700                   MOVE WS-FLYGT-MAX-TIAAMMDD    TO TMP2-YYMMDD           
201800                   MOVE DAGENS-DATUM             TO TMP3-YYMMDD           
201900                   PERFORM WY2000Q1                                       
202000                   IF TMP1-YYMMDD <= TMP2-YYMMDD                          
202010                   OR (INL-KDFRAKT = 17 OR 18 OR 19)                      
202100                     IF INL-TIINLINL = ZERO                               
202200                        COMPUTE WS-SUANTAL-ETA =                          
202300                                WS-SUANTAL-ETA + INL-KVAVIS               
202400                     END-IF                                               
202500                   END-IF                                                 
202600                   MOVE INL-TIBERANK             TO TMP1-YYMMDD           
202700                   MOVE WS-FLYGT-MAX-TIAAMMDD    TO TMP2-YYMMDD           
202800                   MOVE WS-FIRST-TIBERANK        TO TMP3-YYMMDD           
202900                   PERFORM WY2000Q1                                       
203000                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
203100                     IF INL-TIINLINL = ZERO                               
203200                        IF WS-FIRST-TIBERANK = ZERO                       
203300                           MOVE INL-TIBERANK TO                           
203400                                         WS-FIRST-TIBERANK                
203500                        ELSE                                              
203600                           IF TMP1-YYMMDD < TMP3-YYMMDD                   
203700                              MOVE INL-TIBERANK TO                        
203800                                            WS-FIRST-TIBERANK             
203900                           END-IF                                         
204000                        END-IF                                            
204100                     END-IF                                               
204200                   END-IF                                                 
204300                 END-IF                                                   
204400              END-IF                                                      
204500              PERFORM IMS-GNP-INLC11                                      
204600           END-PERFORM                                                    
204700        END-IF                                                            
204800     ELSE                                                                 
204900        MOVE ' FEL I SUBPGM W218ETA   '                                   
205000                                 TO   FELTEXT-STR                         
205100        DISPLAY FELTEXT                                                   
205200        PERFORM S99-ABEND                                                 
205300     END-IF                                                               
205400     .                                                                    
205500     EJECT                                                                
205600                                                                          
205700                                                                          
205800 S51B-SUM-OF-PROD-STATUS-R-U-P SECTION.                                   
205900                                                                          
206000     MOVE WS-SPAR-IDARTNR   TO W-Q4-IDARTNR-MIN                           
206100                               W-Q4-IDARTNR-MAX                           
206200                                                                          
206300     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
206400                                                                          
206500*** ADD PRODSTATUS R QUANTITIES FROM WDQ2/WDQ4                            
206600     MOVE ZERO              TO WS-KVBEART-Q-RUP                           
206700     MOVE CREF-IDDC-REF     TO W-Q4-IDDC                                  
206800     PERFORM IMS-GU-WDQ4B1                                                
206900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
207000        IF  SEQB-IDDISTR  = WS-IDDISTR                                    
207100           MOVE SEQB-IDORDER    TO W-Q2-IDORDER                           
207200           PERFORM IMS-GU-WDQ201                                          
207300           IF  OHUV-KDORDKL    = 1                                        
207400           AND OHUV-FLKLAR     = JA                                       
207500              MOVE SEQB-IDDC   TO W-Q2-IDDC                               
207600              PERFORM IMS-GNP-WDQ212                                      
207700              IF ARB-KDTRPKAT = 'A'                                       
207800                 ADD SEQB-KVBEART-Q  TO WS-KVBEART-Q-RUP                  
207900              END-IF                                                      
208000           END-IF                                                         
208100        END-IF                                                            
208200        PERFORM IMS-GN-WDQ4B1                                             
208300     END-PERFORM                                                          
208400                                                                          
208500*** ADD PRODSTATUS U AND P QUANTITIES FROM WDE4                           
208600     MOVE LOW-VALUE         TO W-WDE4CSEQ-MIN-X                           
208700     MOVE HIGH-VALUE        TO W-WDE4CSEQ-MAX-X                           
208800     MOVE WS-SPAR-IDARTNR   TO W-WDE4C-IDARTNR-MIN                        
208900                               W-WDE4C-IDARTNR-MAX                        
209000     PERFORM IMS-GU-WDE4CSEQ                                              
209100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
209200        IF     ORAD-KDRADSTA < +4                                         
209300*------------  PRODSTATUS U                                               
209400                                                                          
209500        OR (   ORAD-KDRADSTA > +3                                         
209600           AND ORAD-KVAVBART > ZERO)                                      
209700*------------  PRODSTATUS P                                               
209800                                                                          
209900           PERFORM IMS-GNP-WDE401                                         
210000           IF  KORD-IDDISTR = WS-IDDISTR                                  
210100           AND KORD-IDDC    = CREF-IDDC-REF                               
210200           AND KORD-KDORDKL = 1                                           
210300              IF ORAD-KDRADSTA = +4                                       
210400                 PERFORM IMS-GNP-WDE421                                   
210500                 IF SEGMENT-FINNS                                         
210600                    MOVE KKOLLI-IDPRODNR    TO W-IDPRODNR                 
210700                    MOVE KKOLLI-IDKOLLI     TO W-IDKOLLI                  
210800                    PERFORM IMS-GU-WDE611                                 
210900                    IF SEGMENT-FINNS                                      
211000                       IF KOLLI-KDKOLSTA < 8                              
211100                          ADD ORAD-KVAVBART TO WS-KVBEART-Q-RUP           
211200                       END-IF                                             
211300                    ELSE                                                  
211400                       ADD ORAD-KVAVBART TO WS-KVBEART-Q-RUP              
211500                    END-IF                                                
211600                 ELSE                                                     
211700                    ADD ORAD-KVAVBART TO WS-KVBEART-Q-RUP                 
211800                 END-IF                                                   
211900              ELSE                                                        
212000                 ADD ORAD-KVBEART TO WS-KVBEART-Q-RUP                     
212100              END-IF                                                      
212200                                                                          
212300           END-IF                                                         
212400        END-IF                                                            
212500                                                                          
212600        PERFORM IMS-GN-WDE4CSEQ                                           
212700     END-PERFORM                                                          
212800     .                                                                    
212900     EJECT                                                                
213000                                                                          
213100                                                                          
213200 S51C-BER-DISPONIBELT-FLYG SECTION.                                       
213300                                                                          
213400     PERFORM IMS-GU-WDK901                                                
213500     IF SEGMENT-FINNS                                                     
213600       MOVE WDK9-ART-KVOKS-DAG    TO WS-KVOKS-DAG                         
213700       MOVE WDK9-ART-KVOKS-BULK   TO WS-KVOKS-BULK                        
213800       MOVE WDK9-ART-KVOKS-VOR    TO WS-KVOKS-VOR                         
213900     ELSE                                                                 
214000       MOVE ZERO             TO WS-KVOKS-DAG                              
214100                                WS-KVOKS-BULK                             
214200                                WS-KVOKS-VOR                              
214300     END-IF                                                               
214400                                                                          
214500     COMPUTE WS-KVDISP-FLYG =                                             
214600          ((CLAG-KVLS + WS-SUANTAL-ETA +                                  
214700            WS-ANTAL-RO-RESS-KL1-CDC + WS-ANTAL-GODK-E3 +                 
214800            CLAG-KVAKS-CDC + WS-SUM-KVAVROP       ) -                     
214900                                                                          
215000          (WS-KVOKS-DAG + WS-KVOKS-BULK +                                 
215100           WS-KVOKS-VOR + CLAG-KVUTRS +                                   
215200           CLAG-KVROS + CLAG-KVRESS))                                     
215300     .                                                                    
215400     EJECT                                                                
215500                                                                          
215600                                                                          
215700 S51D-BER-BEHOV-FLYG SECTION.                                             
215800                                                                          
215900*    HUR MYCKET BEHÖVS UNDER FLYGTIDEN                                    
216000     MOVE B6-REF-KVDLTID-AIRREQ     TO DAG-KVKALDAG                       
216100     MOVE DAGENS-DATUM           TO DAG-TIAAMMDD-FOM                      
216200     MOVE 002                    TO DAG-KDCALL                            
216300     CALL WDAGKONV USING DAG-KDCALL                                       
216400                         DAG-DATUM-AREA                                   
216500                         DAG-KDSVAR                                       
216600     IF DAG-KDSVAR = SPACE                                                
216700        MOVE DAG-TIAAMMDD-TOM    TO WS-FLYGBEHOV-MAX-TIAAMMDD             
216800     ELSE                                                                 
216900        MOVE 'FEL FRÅN WDAGKONV, I S51D-SECTION I W27210'                 
217000                                 TO   FELTEXT-STR                         
217100        DISPLAY FELTEXT                                                   
217200        PERFORM S99-ABEND                                                 
217300     END-IF                                                               
217400                                                                          
217500*    I VILKEN PERIOD ÄR GODSET FRAMME                                     
217600     MOVE WS-FLYGBEHOV-MAX-TIAAMMDD TO DAT-I-TIDATUM                      
217700     MOVE 'AAMMDD'                  TO DAT-KDDATFORM                      
217800     CALL WDATKONV USING               DAT-KDDATFORM                      
217900                                       DAT-I-TIDATUM                      
218000                                       DAT-O-TIDATUM                      
218100                                       DAT-KDSVAR                         
218200                                                                          
218300     IF DAT-KDSVAR-OK                                                     
218400        MOVE DAT-TIAARP   TO WS-FLYGBEHOV-MAX-TIAARP                      
218500        IF WS-INNEV-TIAARP = WS-FLYGBEHOV-MAX-TIAARP                      
218600*          SAMMA SOM INNEVARANDE                                          
218700           PERFORM S51D1-BEHOV-FLYGT-INNEV-PER                            
218800        ELSE                                                              
218900*          ELLER NÄSTA                                                    
219000           PERFORM S51D2-BEHOV-FLYGT-NASTA-PER                            
219100        END-IF                                                            
219200     ELSE                                                                 
219300        MOVE 'FEL FRÅN WDATKONV  I S51D SECTION I W27210'                 
219400                              TO   FELTEXT-STR                            
219500        DISPLAY FELTEXT                                                   
219600        PERFORM S99-ABEND                                                 
219700     END-IF                                                               
219800                                                                          
219900     .                                                                    
220000     EJECT                                                                
220100                                                                          
220200                                                                          
220300 S51D1-BEHOV-FLYGT-INNEV-PER SECTION.                                     
220400                                                                          
220500     COMPUTE WS-KVPB-DAG-INNEV-RP =                                       
220600              (((UTIL-KVPB-TOT *                                          
220700              WS-RESEASON-PLAN(WS-INNEV-TIRP)) / 4.33) / 5)               
220800                                                                          
220900                                                                          
221000     COMPUTE   WS-KVPB-FLYG ROUNDED =                                     
221100               WS-KVPB-DAG-INNEV-RP *                                     
221200               B6-REF-KVDLTID-AIRREQ                                      
221300                                                                          
221400     .                                                                    
221500     EJECT                                                                
221600                                                                          
221700                                                                          
221800 S51D2-BEHOV-FLYGT-NASTA-PER SECTION.                                     
221900                                                                          
222000*    GODSET ANKOMMER I NÄSTA PERIOD                                       
222100*       VILKET DATUM BÖRJAR NÄSTA PERIOD                                  
222200     MOVE WS-FLYGBEHOV-MAX-TIAARP TO DAT-I-TIDATUM                        
222300     MOVE 'AARP  '                TO DAT-KDDATFORM                        
222400     CALL WDATKONV USING             DAT-KDDATFORM                        
222500                                     DAT-I-TIDATUM                        
222600                                     DAT-O-TIDATUM                        
222700                                     DAT-KDSVAR                           
222800                                                                          
222900     IF DAT-KDSVAR-OK                                                     
223000        MOVE DAT-TIAAMMDD     TO WS-NASTA-PER-TIAAMMDD                    
223100     ELSE                                                                 
223200        MOVE 'FEL FRÅN WDATKONV, I S51D2-SECTION I W27210'                
223300                                 TO   FELTEXT-STR                         
223400        DISPLAY FELTEXT                                                   
223500        PERFORM S99-ABEND                                                 
223600     END-IF                                                               
223700                                                                          
223800*    HUR MÅNGA KALENDERDAGAR ÄR DET TILLS NÄSTA PERIOD BÖRJAR             
223900     MOVE DAGENS-DATUM          TO DAG-TIAAMMDD-FOM                       
224000     MOVE WS-NASTA-PER-TIAAMMDD TO DAG-TIAAMMDD-TOM                       
224100     MOVE 001                   TO DAG-KDCALL                             
224200     CALL WDAGKONV USING DAG-KDCALL                                       
224300                         DAG-DATUM-AREA                                   
224400                         DAG-KDSVAR                                       
224500     IF DAG-KDSVAR = SPACE                                                
224600        MOVE DAG-KVKALDAG       TO WS-KVDAGAR-FLYG-INNEV                  
224700                                                                          
224800        COMPUTE WS-KVDAGAR-FLYG-NASTA =                                   
224900                B6-REF-KVDLTID-AIRREQ    -                                
225000                WS-KVDAGAR-FLYG-INNEV                                     
225100                                                                          
225200        COMPUTE WS-KVPB-DAG-INNEV-RP =                                    
225300                (((UTIL-KVPB-TOT *                                        
225400                WS-RESEASON-PLAN(WS-INNEV-TIRP)) / 4.33) / 5)             
225500                                                                          
225600        COMPUTE WS-KVPB-DAG-NASTA-RP =                                    
225700                (((UTIL-KVPB-TOT *                                        
225800                WS-RESEASON-PLAN(WS-NASTA-TIRP)) / 4.33) / 5)             
225900                                                                          
226000        COMPUTE WS-KVPB-INNEV-RP =                                        
226100                WS-KVDAGAR-FLYG-INNEV * WS-KVPB-DAG-INNEV-RP              
226200                                                                          
226300        COMPUTE WS-KVPB-NASTA-RP =                                        
226400                WS-KVDAGAR-FLYG-NASTA * WS-KVPB-DAG-NASTA-RP              
226500                                                                          
226600        COMPUTE WS-KVPB-FLYG ROUNDED =                                    
226700                WS-KVPB-INNEV-RP + WS-KVPB-NASTA-RP                       
226800     ELSE                                                                 
226900        MOVE 'FEL FRÅN WDATKONV 1  I S51D2 SECTION I W27210'              
227000                                TO  FELTEXT-STR                           
227100        DISPLAY FELTEXT                                                   
227200        PERFORM S99-ABEND                                                 
227300     END-IF                                                               
227400     .                                                                    
227500     EJECT                                                                
227600                                                                          
227700                                                                          
227800 S52-L-STOCK-ANTAL-CDC SECTION.                                           
227900                                                                          
228000     INITIALIZE W271LTPB-W271LTPB                                         
228100     MOVE ZERO TO       WS-KVPB-REF-DAY-PER-I                             
228200                        WS-KVPB-REF-DAY-PER-II                            
228300                        WS-KVPB-REF-DAY-PER-III                           
228400                        WS-KVPB-REF-DAY-PER-IV                            
228500                        WS-KVPB-REF-DAY-PER-V                             
228600                        WS-KVPB-REF-DAY-PER-VI                            
228700     IF WS-FIRST-TIBERANK > ZERO                                          
228800*       ANTAL BEHOV DAGAR FRAM TILL FÖRSTA INLEVERANS                     
228900        MOVE WS-FIRST-TIBERANK   TO W271LTPB-BINNDAY-TIAAMMDD             
229000        MOVE '11'            TO W271LTPB-IDDC                             
229100        MOVE CREF-IDDC-REF   TO W271LTPB-IDDC-REF                         
229200        MOVE ZERO            TO W271LTPB-START-DATUM                      
229300     ELSE                                                                 
229400*       BEHOV UNDER BÅTLEDTID                                             
229500        MOVE '11'            TO W271LTPB-IDDC                             
229600        MOVE CREF-IDDC-REF    TO W271LTPB-IDDC-REF                        
229700        MOVE B6-REF-KVDLTID-TOT TO  W271LTPB-KVDLTID-TOT                  
229800        MOVE B6-REF-KVDLTID-BOATPAC                                       
229900                             TO  W271LTPB-KVDLTID-BOATPAC                 
230000        MOVE B6-REF-KVDLTID-BOATTRP                                       
230100                             TO  W271LTPB-KVDLTID-BOATTRP                 
230200        MOVE B6-REF-KVDLTID-BOAT2DC                                       
230300                             TO  W271LTPB-KVDLTID-BOAT2DC                 
230400        MOVE B6-REF-KVDLTID-BOATINS                                       
230500                             TO  W271LTPB-KVDLTID-BOATINS                 
230600        MOVE B6-REF-KVDLTID-AIRREQ                                        
230700                             TO  W271LTPB-KVDLTID-AIRREQ                  
230800        MOVE B6-REF-KVDLTID-AIRETA                                        
230900                             TO  W271LTPB-KVDLTID-AIRETA                  
231000        MOVE ZERO            TO W271LTPB-START-DATUM                      
231100     END-IF                                                               
231200                                                                          
231300     CALL W271LTPB USING                                                  
231400                       W271LTPB-W271LTPB                                  
231500                                                                          
231600*      SUMMERA BEHOV FÖR DE ARBETSDAGAR SOM LIGGER INOM                   
231700*      LEDTIDEN                                                           
231800                                                                          
231900     COMPUTE WS-KVPB-REF-DAY-PER-I =                                      
232000       (((UTIL-KVPB-TOT *                                                 
232100       WS-RESEASON-PLAN(W271LTPB-PER-I-TIRP)) / 4.33) / 7)                
232200                                                                          
232300     COMPUTE WS-KVPB-REF-DAY-PER-II =                                     
232400        (((UTIL-KVPB-TOT *                                                
232500        WS-RESEASON-PLAN(W271LTPB-PER-II-TIRP)) / 4.33) / 7)              
232600                                                                          
232700     COMPUTE WS-KVPB-REF-DAY-PER-III =                                    
232800         (((UTIL-KVPB-TOT *                                               
232900        WS-RESEASON-PLAN(W271LTPB-PER-III-TIRP)) / 4.33) / 7)             
233000                                                                          
233100     COMPUTE WS-KVPB-REF-DAY-PER-IV =                                     
233200         (((UTIL-KVPB-TOT *                                               
233300        WS-RESEASON-PLAN(W271LTPB-PER-IV-TIRP)) / 4.33) / 7)              
233400                                                                          
233500     COMPUTE WS-KVPB-REF-DAY-PER-V =                                      
233600         (((UTIL-KVPB-TOT *                                               
233700        WS-RESEASON-PLAN(W271LTPB-PER-V-TIRP)) / 4.33) / 7)               
233800                                                                          
233900     COMPUTE WS-KVPB-REF-DAY-PER-VI =                                     
234000         (((UTIL-KVPB-TOT *                                               
234100        WS-RESEASON-PLAN(W271LTPB-PER-VI-TIRP)) / 4.33) / 7)              
234200                                                                          
234300     COMPUTE WS-ANTAL-LEDT-FLYG ROUNDED =                                 
234400        (W271LTPB-KVDAGAR-PER-I * WS-KVPB-REF-DAY-PER-I)     +            
234500        (W271LTPB-KVDAGAR-PER-II * WS-KVPB-REF-DAY-PER-II)   +            
234600        (W271LTPB-KVDAGAR-PER-III * WS-KVPB-REF-DAY-PER-III) +            
234700        (W271LTPB-KVDAGAR-PER-IV  * WS-KVPB-REF-DAY-PER-IV)  +            
234800        (W271LTPB-KVDAGAR-PER-V   * WS-KVPB-REF-DAY-PER-V)   +            
234900        (W271LTPB-KVDAGAR-PER-VI  * WS-KVPB-REF-DAY-PER-VI)               
235000                                                                          
235100     COMPUTE WS-ANTAL-FLYG               =                                
235200                WS-ANTAL-LEDT-FLYG       +                                
235300                WS-KVOKS-DAG             +                                
235400                WS-KVOKS-BULK            +                                
235500                WS-KVOKS-VOR             +                                
235600                CLAG-KVROS               +                                
235700                CLAG-KVRESS              -                                
235800                WS-ANTAL-RO-RESS-KL1-CDC -                                
235900                CLAG-KVLS                -                                
236000                CLAG-KVAKS-CDC           -                                
236100                WS-SUANTAL-ETA           -                                
236200                WS-ANTAL-GODK-E3         -                                
236300                WS-KVBEART-Q-RUP                                          
236400                                                                          
236500     IF WS-ANTAL-FLYG < ZERO                                              
236600         MOVE ZERO          TO WS-ANTAL-BER                               
236700     ELSE                                                                 
236800        MOVE WS-ANTAL-FLYG    TO WS-ANTAL-FLYG-AVRUND                     
236900        IF WS-ANTAL-FLYG-AVRUND-HELTAL = ZERO                             
237000           IF WS-ANTAL-FLYG-AVRUND-DECTAL < 4                             
237100              CONTINUE                                                    
237200           ELSE                                                           
237300              MOVE 1 TO WS-ANTAL-FLYG-AVRUND-HELTAL                       
237400           END-IF                                                         
237500        ELSE                                                              
237600           IF WS-ANTAL-FLYG-AVRUND-DECTAL < 3                             
237700              CONTINUE                                                    
237800           ELSE                                                           
237900              ADD 1 TO WS-ANTAL-FLYG-AVRUND-HELTAL                        
238000           END-IF                                                         
238100        END-IF                                                            
238200        MOVE WS-ANTAL-FLYG-AVRUND-HELTAL  TO WS-ANTAL-BER                 
238300     END-IF                                                               
238400     .                                                                    
238500     EJECT                                                                
238600                                                                          
238700                                                                          
238800                                                                          
238900 S55-GODK-E3--RO-CDC--TRANSFER SECTION.                                   
239000                                                                          
239100*       GODKÄNDA FLYGFÖRSLAG                                              
239200                                                                          
239300     MOVE ZERO TO WS-ANTAL-GODK-E3                                        
239400     MOVE '11'                   TO W-IDDC                                
239500                                    W-IDDC-MIN                            
239600                                    W-IDDC-MAX                            
239700     MOVE ZERO                   TO W-IDPERSON-BUY-MIN                    
239800     MOVE 999                    TO W-IDPERSON-BUY-MAX                    
239900                                                                          
240000     MOVE 'A'                    TO W-KDREFTYP-MIN                        
240100     MOVE 'C'                    TO W-KDREFTYP-MAX                        
240200     MOVE WS-SPAR-IDARTNR        TO W-IDARTNR                             
240300                                                                          
240400     PERFORM IMS-GU-WDE301                                                
240500     PERFORM UNTIL SEGMENT-SAKNAS                                         
240600        IF REF-KDREFTYP = 'A'                                             
240700        OR REF-KDREFTYP = 'C'                                             
240800           IF REF-KDREFORS = 'O'                                          
240900              COMPUTE WS-ANTAL-GODK-E3 =                                  
241000                      WS-ANTAL-GODK-E3 + REF-KVBEART                      
241100           END-IF                                                         
241200        END-IF                                                            
241300        IF REF-KDREFTYP = 'O'                                             
241400           COMPUTE WS-ANTAL-GODK-E3 =                                     
241500                   WS-ANTAL-GODK-E3 + REF-KVBEART                         
241600        END-IF                                                            
241700        PERFORM IMS-GN-WDE301                                             
241800     END-PERFORM                                                          
241900                                                                          
242000*                                                                         
242100     IF (INT-SLAG-KVROS-BULK > 0 OR INT-SLAG-KVROS-DAG > 0 OR             
242200         INT-SLAG-KVRESS > 0)                                             
242300        PERFORM S48-HAMTA-REFILLDISTRIKT                                  
242400        MOVE WS-IDDISTR TO W-IDDISTR-A5D-MIN                              
242500                           W-IDDISTR-A5D-MAX                              
242600        PERFORM IMS-GU-ORDT01                                             
242700        PERFORM UNTIL SEGMENT-SAKNAS                                      
242800           IF SEQD-KDORDKL = 1                                            
242900              IF SEQD-KDSTARAD = '2' OR '3'                               
243000                 MOVE SEQD-IDWDA501 TO W-WDA501KY                         
243100                 PERFORM IMS-GU-ORDP01                                    
243200                 IF SEGMENT-FINNS                                         
243300                    COMPUTE WS-ANTAL-RO-RESS-KL1-CDC =                    
243400                            WS-ANTAL-RO-RESS-KL1-CDC +                    
243500                            RAD-KVRO                                      
243600                 END-IF                                                   
243700              END-IF                                                      
243800           END-IF                                                         
243900           PERFORM IMS-GN-ORDT01                                          
244000        END-PERFORM                                                       
244100     END-IF                                                               
244200***ANTAL AVROP IN TIDEN FÖR AIR PROPOSAL(4408)                            
244300     MOVE ZERO                    TO WS-DAT-AIRREQ                        
244400     MOVE DAGENS-DATUM            TO DAYS-TIDATE1                         
244500     MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                       
244600     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
244700     MOVE B6-REF-KVDLTID-AIRREQ   TO DAYS-KVDAYS                          
244800     MOVE SPACE                   TO DAYS-TIDATE2                         
244900                                     DAYS-IDCALEND                        
245000     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
245100*                                                                         
245200     IF DAYS-KDRC = 8                                                     
245300       MOVE 'FEL2 VID ANROP TILL WZ20DAYS' TO FELTEXT                     
245400       CALL ABEND USING FELLOG                                            
245500     ELSE                                                                 
245600       MOVE DAYS-TIDATE2(1:6) TO WS-DAT-AIRREQ                            
245700     END-IF                                                               
245800                                                                          
245900     PERFORM IMS-GU-ART-WDD9                                              
246000     IF SEGMENT-FINNS                                                     
246100        PERFORM IMS-GNP-WDD902                                            
246200        PERFORM UNTIL SEGMENT-SAKNAS                                      
246300           PERFORM IMS-GNP-WDD905                                         
246400           PERFORM UNTIL SEGMENT-SAKNAS                                   
246500              IF WDD905-KDAVROP = 2                                       
246600                IF WDD905-TIAVRDAT-DISP < WS-DAT-AIRREQ                   
246700                  COMPUTE WS-SUM-KVAVROP =                                
246800                          WS-SUM-KVAVROP + WDD905-KVAVROP                 
246900                END-IF                                                    
247000              END-IF                                                      
247100              PERFORM IMS-GNP-WDD905                                      
247200           END-PERFORM                                                    
247300           PERFORM IMS-GNP-WDD902                                         
247400        END-PERFORM                                                       
247500     END-IF                                                               
247600     .                                                                    
247700     EJECT                                                                
247800                                                                          
247900                                                                          
248000 S56-Q-ANPASSA SECTION.                                                   
248100                                                                          
248200     MOVE ZERO               TO WS-KVANT-QX                               
248300     MOVE WS-ANTAL-BER       TO WS-ANTAL-QX                               
248400                                                                          
248500     IF CLAG-KVQPACK-4 > 1                                                
248600     OR WS-KVQPACK-3   > 1                                                
248700     OR CLAG-KVQPACK-2 > 1                                                
248800     OR CLAG-KVQPACK-1 > 1                                                
248900     OR CLAG-KVQPACK-0 > 1                                                
249000                                                                          
249100*KVQPACK-4                                                                
249200       IF CLAG-KVQPACK-4 > 1                                              
249300         COMPUTE WS-KVANT-QX =                                            
249400                 WS-ANTAL-BER / CLAG-KVQPACK-4                            
249500         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
249600           IF WS-KVANT-QX-HELTAL > ZERO                                   
249700             COMPUTE WS-ANTAL-QX =                                        
249800                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                  
249900           END-IF                                                         
250000         ELSE                                                             
250100           COMPUTE WS-KVANT-QX-HELTAL =                                   
250200                   WS-KVANT-QX-HELTAL + 1                                 
250300           COMPUTE WS-ANTAL-QX =                                          
250400                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                    
250500         END-IF                                                           
250600       END-IF                                                             
250700                                                                          
250800*KVQPACK-3                                                                
250900       IF WS-KVQPACK-3 > 1  AND                                           
251000          WS-KVANT-QX-HELTAL = ZERO                                       
251100         COMPUTE WS-KVANT-QX =                                            
251200                 WS-ANTAL-BER / WS-KVQPACK-3                              
251300         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
251400           IF WS-KVANT-QX-HELTAL > ZERO                                   
251500             COMPUTE WS-ANTAL-QX =                                        
251600                     WS-KVANT-QX-HELTAL * WS-KVQPACK-3                    
251700           END-IF                                                         
251800         ELSE                                                             
251900           COMPUTE WS-KVANT-QX-HELTAL =                                   
252000                   WS-KVANT-QX-HELTAL + 1                                 
252100           COMPUTE WS-ANTAL-QX =                                          
252200                   WS-KVANT-QX-HELTAL * WS-KVQPACK-3                      
252300         END-IF                                                           
252400       END-IF                                                             
252500                                                                          
252600*KVQPACK-2                                                                
252700       IF CLAG-KVQPACK-2 > 1  AND                                         
252800          WS-KVANT-QX-HELTAL = ZERO                                       
252900         COMPUTE WS-KVANT-QX =                                            
253000                 WS-ANTAL-BER / CLAG-KVQPACK-2                            
253100         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
253200           IF WS-KVANT-QX-HELTAL > ZERO                                   
253300             COMPUTE WS-ANTAL-QX =                                        
253400                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                  
253500           END-IF                                                         
253600         ELSE                                                             
253700           COMPUTE WS-KVANT-QX-HELTAL =                                   
253800                   WS-KVANT-QX-HELTAL + 1                                 
253900           COMPUTE WS-ANTAL-QX =                                          
254000                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                    
254100         END-IF                                                           
254200       END-IF                                                             
254300                                                                          
254400*KVQPACK-0                                                                
254500       IF CLAG-KVQPACK-0 > 1   AND                                        
254600          WS-KVANT-QX-HELTAL = ZERO                                       
254700         COMPUTE WS-KVANT-QX =                                            
254800                 WS-ANTAL-BER / CLAG-KVQPACK-0                            
254900         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
255000           IF WS-KVANT-QX-HELTAL < 1                                      
255100             IF CLAG-KVQPACK-1 > 0                                        
255200               PERFORM S56B-KVQPACK-1                                     
255300             ELSE                                                         
255400               MOVE WS-ANTAL-BER TO WS-ANTAL-QX                           
255500             END-IF                                                       
255600           ELSE                                                           
255700             COMPUTE WS-ANTAL-QX =                                        
255800                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                  
255900           END-IF                                                         
256000         ELSE                                                             
256100           COMPUTE WS-KVANT-QX-HELTAL =                                   
256200                   WS-KVANT-QX-HELTAL + 1                                 
256300           COMPUTE WS-ANTAL-QX =                                          
256400                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                    
256500         END-IF                                                           
256600       END-IF                                                             
256700                                                                          
256800*KVQPACK-1                                                                
256900       IF CLAG-KVQPACK-1 > 1                                              
257000          IF WS-KVANT-QX-HELTAL > ZERO                                    
257100             MOVE WS-ANTAL-QX TO WS-ANTAL-BER                             
257200          END-IF                                                          
257300          PERFORM S56B-KVQPACK-1                                          
257400       END-IF                                                             
257500     END-IF                                                               
257600     MOVE WS-ANTAL-QX TO WS-ANTAL-SLUT                                    
257700                                                                          
257800     .                                                                    
257900     EJECT                                                                
258000                                                                          
258100 S56B-KVQPACK-1 SECTION.                                                  
258200                                                                          
258300     COMPUTE WS-KVANT-QX =                                                
258400             WS-ANTAL-BER / CLAG-KVQPACK-1                                
258500     IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                               
258600       IF WS-KVANT-QX-HELTAL < 1                                          
258700         COMPUTE WS-ANTAL-QX = 1 * CLAG-KVQPACK-1                         
258800       ELSE                                                               
258900         COMPUTE WS-ANTAL-QX =                                            
259000                 WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                      
259100       END-IF                                                             
259200     ELSE                                                                 
259300       COMPUTE WS-KVANT-QX-HELTAL =                                       
259400               WS-KVANT-QX-HELTAL + 1                                     
259500       COMPUTE WS-ANTAL-QX =                                              
259600               WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                        
259700     END-IF                                                               
259800     .                                                                    
259900     EJECT                                                                
260000                                                                          
260100 S56C-SATT-QX-PROCENT-BRYTNING SECTION.                                   
260200                                                                          
260300     MOVE DCS-REQXBRYT       TO WS-QX-BRYTNING                            
260400     .                                                                    
260500     EJECT                                                                
260600                                                                          
260700                                                                          
260800 S57-RANGORDNA-REFTEXT SECTION.                                           
260900                                                                          
261000     IF CLAG-KVUTRS > 0                                                   
261100        MOVE '50'                   TO W27210-KDREFTXT                    
261200     END-IF                                                               
261300                                                                          
261400     IF ART-FLERS = JA                                                    
261500        MOVE '15'                   TO W27210-KDREFTXT                    
261600     END-IF                                                               
261700                                                                          
261800     IF WS-KVSPARR-KVAL > 0                                               
261900        MOVE '03'                   TO W27210-KDREFTXT                    
262000     END-IF                                                               
262100                                                                          
262200     IF CLAG-KVPB-SATS > 0                                                
262300        MOVE '71'                   TO W27210-KDREFTXT                    
262400     END-IF                                                               
262500                                                                          
262600     IF CLAG-KVPB-TPO > 0                                                 
262700        MOVE '72'                   TO W27210-KDREFTXT                    
262800     END-IF                                                               
262900                                                                          
263000     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
263100     IF SQLCODE-WS = ZERO                                                 
263200       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
263300     END-IF                                                               
263400                                                                          
263500     PERFORM UNTIL SQLCODE-WS > ZERO OR TRAEFF                            
263600       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
263700         MOVE TP1KAMP-TISTODAT-KAMP                                       
263800                             TO WS-JMFR-AAAAMMDD                          
263900       ELSE                                                               
264000         MOVE TP1KAMP-TISTADAT-KAMP                                       
264100                             TO WS-JMFR-AAAAMMDD                          
264200       END-IF                                                             
264300       IF WS-JMFR-AA > 50                                                 
264400         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
264500       ELSE                                                               
264600         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
264700       END-IF                                                             
264800       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
264900* LÄGG TILL 5 ÅR                                                          
265000         ADD 50000           TO WS-JMFR-AAAAMMDD                          
265100       END-IF                                                             
265200       MOVE W-DAGENS-DATUM     TO WS-DATUM                                
265300                                                                          
265400       IF WS-DATUM-MAN > 06                                               
265500         SUBTRACT 600 FROM WS-DATUM                                       
265600       ELSE                                                               
265700         ADD 600 TO WS-DATUM                                              
265800         SUBTRACT 10000 FROM WS-DATUM                                     
265900       END-IF                                                             
266000       MOVE WS-DATUM TO WS-JMFR2-AAAAMMDD                                 
266100       IF WS-JMFR-AAAAMMDD >= WS-JMFR2-AAAAMMDD                           
266200         MOVE '74'                 TO W27210-KDREFTXT                     
266300         MOVE JA                   TO TRAEFF-SW                           
266400       END-IF                                                             
266500       IF NOT TRAEFF                                                      
266600         PERFORM DB2-FETCH-TP1ARTK-CRS                                    
266700       END-IF                                                             
266800     END-PERFORM                                                          
266900     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
267000     .                                                                    
267100     EJECT                                                                
267200                                                                          
267300 S70-SUMMERA-ORDRAR SECTION.                                              
267400                                                                          
267500     MOVE 'W27210'               TO POSTSUM-FDNAMN                        
267600     MOVE 'DC'                   TO POSTSUM-TRANSTYP (1:2)                
267700     MOVE W27210-IDDC            TO POSTSUM-TRANSTYP (3:2)                
267800     EVALUATE W27210-KDREFTYP                                             
267900        WHEN 'A'                                                          
268000              MOVE 'FLYG '       TO POSTSUM-DDNAMN2                       
268100        WHEN 'C'                                                          
268200              MOVE 'FLYG '       TO POSTSUM-DDNAMN2                       
268300        WHEN 'B'                                                          
268400              MOVE 'BÅT '        TO POSTSUM-DDNAMN2                       
268500        WHEN 'O'                                                          
268600              MOVE 'REFILL  '    TO POSTSUM-DDNAMN2                       
268700     END-EVALUATE                                                         
268800     .                                                                    
268900     EJECT                                                                
269000                                                                          
269100 S80-CURR-WEEK-PLUS-LT SECTION.                                           
269200                                                                          
269300*    CALCULATE DATUM TVA IN WEEK FORMAT                                   
269400*    ADD LEATTIME. MINIMUM 2 WEEKS                                        
269500                                                                          
269600     MOVE ZERO             TO WS-LT-WEEKS                                 
269700                              WS-REST-DAYS                                
269800                                                                          
269900     IF CREF-FLFLYG = JA                                                  
270000        DIVIDE  WS-LTID-A  BY 7                                           
270100                       GIVING WS-LT-WEEKS                                 
270200                    REMAINDER WS-REST-DAYS                                
270300     ELSE                                                                 
270400        DIVIDE  WS-LTID-B  BY 7                                           
270500                       GIVING WS-LT-WEEKS                                 
270600                    REMAINDER WS-REST-DAYS                                
270700     END-IF                                                               
270800*                                                                         
270900     IF WS-REST-DAYS > ZERO                                               
271000        ADD +1             TO WS-LT-WEEKS                                 
271100     END-IF                                                               
271200*                                                                         
271300     IF WS-LT-WEEKS < +2                                                  
271400        MOVE +2            TO WS-LT-WEEKS                                 
271500     END-IF                                                               
271600*                                                                         
271700     MOVE DAGENS-TIAAVV    TO VADD-DATUM-AAVV                             
271800     MOVE WS-LT-WEEKS      TO VADD-ANTAL                                  
271900                                                                          
272000     CALL W009VADD      USING VADD-DATUM-AAVV VADD-ANTAL                  
272100                                                                          
272200     MOVE VADD-DATUM-AAVV  TO DD-LT-FRAMAT-TIAAVV                         
272300     .                                                                    
272400     EJECT                                                                
272500                                                                          
272600                                                                          
272700 S88-SLUT-DATUM-ERS-LEDT SECTION.                                         
272800*WZ20DAYS                                                                 
272900     MOVE WS-TIERSDAT-PREL-C1            TO DAYS-TIDATE1                  
273000     MOVE 'YYWWD'                TO DAYS-KDDATFMT1                        
273100     MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                       
273200     IF CREF-FLFLYG = JA                                                  
273300       MOVE B6-REF-KVDLTID-AIRETA TO DAYS-KVDAYS                          
273400     ELSE                                                                 
273500       MOVE B6-REF-KVDLTID-TOT    TO DAYS-KVDAYS                          
273600     END-IF                                                               
273700     MOVE SPACE                   TO DAYS-TIDATE2                         
273800                                     DAYS-IDCALEND                        
273900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
274000*                                                                         
274100     IF DAYS-KDRC = 8                                                     
274200       MOVE 'FEL VID ANROP TILL WZ20DAYS' TO FELTEXT-STR                  
274300       CALL ABEND USING FELLOG                                            
274400     ELSE                                                                 
274500       MOVE DAYS-TIDATE2(1:5) TO WS-ERSDAT-AND-LT                         
274600     END-IF                                                               
274700     .                                                                    
274800     EJECT                                                                
274900                                                                          
275000 S99-ABEND SECTION.                                                       
275100                                                                          
275200     SKIP2                                                                
275300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
275400     .                                                                    
275500     EJECT                                                                
275600                                                                          
275700                                                                          
275800                                                                          
275900* --- IMS SEKTIONER ---                                                   
276000*     IMSSEKT                                                             
276100                                                                          
276200 IMS-GN-WDK629 SECTION.                                                   
276300     MOVE 'WDK629 '        TO SSA1                                        
276400     MOVE '  GEGB'           TO GODK-STATUSKODER                          
276500     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK629 SSA1                    
276600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
276700     PERFORM IMS-STATUSKONTROLL                                           
276800     SKIP3                                                                
276900     .                                                                    
277000     EJECT                                                                
277100 IMS-GNP-WDK611 SECTION.                                                  
277200     MOVE 'WDK611 '        TO SSA1                                        
277300     MOVE '  '             TO GODK-STATUSKODER                            
277400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
277500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
277600     PERFORM IMS-STATUSKONTROLL                                           
277700     SKIP3                                                                
277800     .                                                                    
277900     EJECT                                                                
278000 IMS-GNP-WDK601 SECTION.                                                  
278100     MOVE 'WDK601 '        TO SSA1                                        
278200     MOVE '  '             TO GODK-STATUSKODER                            
278300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK601 SSA1                   
278400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
278500     PERFORM IMS-STATUSKONTROLL                                           
278600     SKIP3                                                                
278700     .                                                                    
278800     EJECT                                                                
278900                                                                          
279000 IMS-GU-ART-WDD9 SECTION.                                                 
279100                                                                          
279200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
279300          DELIMITED BY SIZE INTO SSA1                                     
279400     MOVE '  GE' TO GODK-STATUSKODER                                      
279500     CALL CBLTDLI USING GU INLB-PCB DLI-IO-INLB01 SSA1                    
279600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
279700     PERFORM IMS-STATUSKONTROLL                                           
279800     .                                                                    
279900     EJECT                                                                
280000                                                                          
280100                                                                          
280200 IMS-GNP-WDD902 SECTION.                                                  
280300                                                                          
280400     MOVE 'WLINLB11 ' TO SSA1                                             
280500     MOVE '  GE' TO GODK-STATUSKODER                                      
280600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB11 SSA1                   
280700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
280800     PERFORM IMS-STATUSKONTROLL                                           
280900     .                                                                    
281000     EJECT                                                                
281100                                                                          
281200 IMS-GNP-WDD905 SECTION.                                                  
281300                                                                          
281400     MOVE 'WLINLB23 ' TO SSA1                                             
281500     MOVE '  GE' TO GODK-STATUSKODER                                      
281600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB23 SSA1                   
281700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
281800     PERFORM IMS-STATUSKONTROLL                                           
281900     .                                                                    
282000     EJECT                                                                
282100                                                                          
282200                                                                          
282300 IMS-GN-WDE301 SECTION.                                                   
282400                                                                          
282500     STRING 'WDE301  (WDE301KY=>' W-WDE301KY-MIN-X                        
282600                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
282700                    '&IDARTNR  =' W-IDARTNR-X ')'                         
282800          DELIMITED BY SIZE INTO SSA1                                     
282900     MOVE '  GE' TO GODK-STATUSKODER                                      
283000     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-WDE301 SSA1                    
283100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
283200     PERFORM IMS-STATUSKONTROLL                                           
283300     .                                                                    
283400     EJECT                                                                
283500                                                                          
283600                                                                          
283700 IMS-GU-WDE301 SECTION.                                                   
283800                                                                          
283900     STRING 'WDE301  (WDE301KY=>' W-WDE301KY-MIN-X                        
284000                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
284100                    '&IDARTNR  =' W-IDARTNR-X ')'                         
284200          DELIMITED BY SIZE INTO SSA1                                     
284300     MOVE '  GE' TO GODK-STATUSKODER                                      
284400     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-WDE301 SSA1                    
284500     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
284600     PERFORM IMS-STATUSKONTROLL                                           
284700     .                                                                    
284800     EJECT                                                                
284900                                                                          
285000                                                                          
285100 IMS-GU-WDE4CSEQ SECTION.                                                 
285200                                                                          
285300     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
285400                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
285500          DELIMITED BY SIZE INTO SSA1                                     
285600     MOVE '  GE'            TO GODK-STATUSKODER                           
285700     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE411 SSA1                   
285800     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
285900     PERFORM IMS-STATUSKONTROLL                                           
286000     .                                                                    
286100                                                                          
286200 IMS-GN-WDE4CSEQ SECTION.                                                 
286300                                                                          
286400     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
286500                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
286600          DELIMITED BY SIZE INTO SSA1                                     
286700     MOVE '  GEGB'          TO GODK-STATUSKODER                           
286800     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE411 SSA1                   
286900     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
287000     PERFORM IMS-STATUSKONTROLL                                           
287100     .                                                                    
287200                                                                          
287300 IMS-GNP-WDE401 SECTION.                                                  
287400                                                                          
287500     MOVE 'WDE401 '         TO SSA1                                       
287600     MOVE '  '              TO GODK-STATUSKODER                           
287700     CALL CBLTDLI USING GNP WDE4C-PCB DLI-IO-WDE401 SSA1                  
287800     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
287900     PERFORM IMS-STATUSKONTROLL                                           
288000     .                                                                    
288100     EJECT                                                                
288200                                                                          
288300 IMS-GNP-WDE421  SECTION.                                                 
288400                                                                          
288500     MOVE 'WDE421 '         TO SSA1                                       
288600     MOVE '  GE' TO GODK-STATUSKODER                                      
288700     CALL CBLTDLI USING GNP WDE4C-PCB DLI-IO-WDE421 SSA1                  
288800     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
288900     PERFORM IMS-STATUSKONTROLL                                           
289000     .                                                                    
289100     SKIP3                                                                
289200                                                                          
289300 IMS-GU-WDE611 SECTION.                                                   
289400                                                                          
289500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
289600          DELIMITED BY SIZE INTO SSA1                                     
289700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
289800          DELIMITED BY SIZE INTO SSA2                                     
289900     MOVE '  GE' TO GODK-STATUSKODER                                      
290000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
290100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
290200     PERFORM IMS-STATUSKONTROLL                                           
290300     .                                                                    
290400     EJECT                                                                
290500                                                                          
290600                                                                          
290700 IMS-GU-WDK712-LART   SECTION.                                            
290800                                                                          
290900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
291000          DELIMITED BY SIZE INTO SSA1                                     
291100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
291200          DELIMITED BY SIZE INTO SSA2                                     
291300     MOVE '  GE' TO GODK-STATUSKODER                                      
291400     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK712-LART                  
291500          SSA1 SSA2                                                       
291600     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
291700     PERFORM IMS-STATUSKONTROLL                                           
291800     .                                                                    
291900     EJECT                                                                
292000                                                                          
292100 IMS-GU-WDK711-INTERN SECTION.                                            
292200                                                                          
292300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-INT-X ')'                     
292400          DELIMITED BY SIZE INTO SSA1                                     
292500     STRING 'WDK711  (IDDC     =' W-IDDC-INT-X ')'                        
292600          DELIMITED BY SIZE INTO SSA2                                     
292700     MOVE '  GE' TO GODK-STATUSKODER                                      
292800     CALL CBLTDLI USING GU WDK74-PCB DLI-IO-WDK711-INTERN                 
292900          SSA1 SSA2                                                       
293000     MOVE WDK74-STATUS-CODE TO STATUS-WS                                  
293100     PERFORM IMS-STATUSKONTROLL                                           
293200     .                                                                    
293300     EJECT                                                                
293400                                                                          
293500 IMS-GN-WDK711-INTERN SECTION.                                            
293600                                                                          
293700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-INT-X ')'                     
293800          DELIMITED BY SIZE INTO SSA1                                     
293900     MOVE 'WDK711  '       TO SSA2                                        
294000     MOVE '  GE' TO GODK-STATUSKODER                                      
294100     CALL CBLTDLI USING GN WDK74-PCB DLI-IO-WDK711-INTERN                 
294200          SSA1 SSA2                                                       
294300     MOVE WDK74-STATUS-CODE TO STATUS-WS                                  
294400     PERFORM IMS-STATUSKONTROLL                                           
294500     .                                                                    
294600     EJECT                                                                
294700                                                                          
294800 IMS-GNP-WDK722-INTERN SECTION.                                           
294900                                                                          
295000     MOVE 'WDK722  '          TO SSA1                                     
295100     MOVE '  GE'              TO GODK-STATUSKODER                         
295200     CALL CBLTDLI USING GNP WDK74-PCB DLI-IO-WDK722-INTERN                
295300          SSA1                                                            
295400     MOVE WDK74-STATUS-CODE   TO STATUS-WS                                
295500     PERFORM IMS-STATUSKONTROLL                                           
295600     .                                                                    
295700     EJECT                                                                
295800                                                                          
295900                                                                          
296000 IMS-GU-WDQ4B1 SECTION.                                                   
296100                                                                          
296200     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4BKY-FOM                           
296300                    '&WDQ4B1KY<=' W-WDQ4BKY-TOM                           
296400                    '&IDDC     =' W-Q4-IDDC-X   ')'                       
296500            DELIMITED BY SIZE INTO SSA1                                   
296600     MOVE '  GE'              TO GODK-STATUSKODER                         
296700     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
296800     MOVE WDQ4B-STATUS-CODE   TO STATUS-WS                                
296900     PERFORM IMS-STATUSKONTROLL                                           
297000     .                                                                    
297100     EJECT                                                                
297200                                                                          
297300 IMS-GN-WDQ4B1 SECTION.                                                   
297400                                                                          
297500     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4BKY-FOM                           
297600                    '&WDQ4B1KY<=' W-WDQ4BKY-TOM                           
297700                    '&IDDC     =' W-Q4-IDDC-X   ')'                       
297800            DELIMITED BY SIZE INTO SSA1                                   
297900     MOVE '  GEGB'            TO GODK-STATUSKODER                         
298000     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
298100     MOVE WDQ4B-STATUS-CODE   TO STATUS-WS                                
298200     PERFORM IMS-STATUSKONTROLL                                           
298300     .                                                                    
298400     EJECT                                                                
298500                                                                          
298600 IMS-GU-WDQ201      SECTION.                                              
298700                                                                          
298800     STRING 'WDQ201  (IDORDER  =' W-Q2-IDORDER-X ')'                      
298900            DELIMITED BY SIZE INTO SSA1                                   
299000     MOVE  '    '            TO GODK-STATUSKODER                          
299100     CALL CBLTDLI USING GU   WDQ2-PCB DLI-IO-WDQ201 SSA1                  
299200     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
299300     PERFORM IMS-STATUSKONTROLL                                           
299400     .                                                                    
299500                                                                          
299600 IMS-GNP-WDQ212 SECTION.                                                  
299700                                                                          
299800     STRING 'WDQ212  (IDDC     =' W-Q2-IDDC-X ')'                         
299900            DELIMITED BY SIZE INTO SSA1                                   
300000     MOVE  '    '            TO GODK-STATUSKODER                          
300100     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-WDQ212 SSA1                  
300200     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
300300     PERFORM IMS-STATUSKONTROLL                                           
300400     .                                                                    
300500     EJECT                                                                
300600                                                                          
300700                                                                          
300800 IMS-GU-INLC01 SECTION.                                                   
300900                                                                          
301000     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
301100          DELIMITED BY SIZE INTO SSA1                                     
301200     MOVE '  GE' TO GODK-STATUSKODER                                      
301300     CALL CBLTDLI USING GU INLC-PCB DLI-IO-INLC01 SSA1                    
301400     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
301500     PERFORM IMS-STATUSKONTROLL                                           
301600     .                                                                    
301700     EJECT                                                                
301800                                                                          
301900                                                                          
302000 IMS-GNP-INLC11 SECTION.                                                  
302100                                                                          
302200     STRING 'WLINLC11(IDDC     =' W-IDDC-X ')'                            
302300          DELIMITED BY SIZE INTO SSA1                                     
302400     MOVE '  GE' TO GODK-STATUSKODER                                      
302500     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-INLC11 SSA1                   
302600     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
302700     PERFORM IMS-STATUSKONTROLL                                           
302800     .                                                                    
302900     EJECT                                                                
303000                                                                          
303100                                                                          
303200 IMS-GN-ORDT01 SECTION.                                                   
303300                                                                          
303400     STRING 'WLORDT01(WDA5D1KY=>' W-WDA5D1KY-MIN-X                        
303500                    '&WDA5D1KY<=' W-WDA5D1KY-MAX-X                        
303600                    '&IDARTNR  =' W-IDARTNR-X ')'                         
303700          DELIMITED BY SIZE INTO SSA1                                     
303800     MOVE '  GE' TO GODK-STATUSKODER                                      
303900     CALL CBLTDLI USING GN ORDT-PCB DLI-IO-ORDT01 SSA1                    
304000     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
304100     PERFORM IMS-STATUSKONTROLL                                           
304200     .                                                                    
304300     EJECT                                                                
304400                                                                          
304500                                                                          
304600 IMS-GU-ORDT01 SECTION.                                                   
304700                                                                          
304800     STRING 'WLORDT01(WDA5D1KY=>' W-WDA5D1KY-MIN-X                        
304900                    '&WDA5D1KY<=' W-WDA5D1KY-MAX-X                        
305000                    '&IDARTNR  =' W-IDARTNR-X ')'                         
305100          DELIMITED BY SIZE INTO SSA1                                     
305200     MOVE '  GE' TO GODK-STATUSKODER                                      
305300     CALL CBLTDLI USING GU ORDT-PCB DLI-IO-ORDT01 SSA1                    
305400     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
305500     PERFORM IMS-STATUSKONTROLL                                           
305600     .                                                                    
305700     EJECT                                                                
305800                                                                          
305900                                                                          
306000 IMS-GU-ORDP01 SECTION.                                                   
306100                                                                          
306200     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
306300          DELIMITED BY SIZE INTO SSA1                                     
306400     MOVE '  GE' TO GODK-STATUSKODER                                      
306500     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-ORDP01 SSA1                    
306600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
306700     PERFORM IMS-STATUSKONTROLL                                           
306800     .                                                                    
306900     EJECT                                                                
307000                                                                          
307100 IMS-GU-WDB601    SECTION.                                                
307200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
307300          DELIMITED BY SIZE INTO SSA1                                     
307400     MOVE '  ' TO GODK-STATUSKODER                                        
307500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
307600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
307700     PERFORM IMS-STATUSKONTROLL                                           
307800     .                                                                    
307900     EJECT                                                                
308000 IMS-GU-WDB616    SECTION.                                                
308100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
308200          DELIMITED BY SIZE INTO SSA1                                     
308300     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
308400          DELIMITED BY SIZE INTO SSA2                                     
308500     MOVE '  ' TO GODK-STATUSKODER                                        
308600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
308700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
308800     PERFORM IMS-STATUSKONTROLL                                           
308900     .                                                                    
309000     EJECT                                                                
309100 IMS-GU-WDD704 SECTION.                                                   
309200                                                                          
309300     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
309400          DELIMITED BY SIZE INTO SSA1                                     
309500     MOVE 'WDD704   ' TO SSA2                                             
309600     MOVE '  GE' TO GODK-STATUSKODER                                      
309700     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD704 SSA1 SSA2               
309800     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
309900     PERFORM IMS-STATUSKONTROLL                                           
310000     .                                                                    
310100     EJECT                                                                
310200                                                                          
310300 IMS-GU-WDK901 SECTION.                                                   
310400                                                                          
310500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
310600          DELIMITED BY SIZE INTO SSA1                                     
310700     MOVE '  GE' TO GODK-STATUSKODER                                      
310800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
310900     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
311000     PERFORM IMS-STATUSKONTROLL                                           
311100     .                                                                    
311200     EJECT                                                                
311300                                                                          
311400 IMS-STATUSKONTROLL SECTION.                                              
311500     SKIP2                                                                
311600     SET STATUS-IX TO 1                                                   
311700     SEARCH GODK-STATUS                                                   
311800       AT END                                                             
311900         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
312000         DISPLAY FELTEXT                                                  
312100         CALL FELLOG                                                      
312200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
312300         CONTINUE                                                         
312400     END-SEARCH                                                           
312500     .                                                                    
312600     EJECT                                                                
312700 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
312800     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
312900     MOVE 000100  TO GOOD-SQLCODECODES                                    
313000     EXEC SQL                                                             
313100         DECLARE TP1ARTK-CRS CURSOR FOR                                   
313200           SELECT  A.IDKAMP                                               
313300                  ,A.IDARTNR                                              
313400                  ,B.TISTADAT_KAMP                                        
313500                  ,B.TISTODAT_KAMP                                        
313600                  ,B.KDKAMP                                               
313700                                                                          
313800           FROM    TP1ARTK A                                              
313900                  ,TP1KAMP B                                              
314000                                                                          
314100           WHERE   A.IDARTNR = :W-IDARTNR                                 
314200           AND     A.IDKAMP  =  B.IDKAMP                                  
314300                                                                          
314400           ORDER BY A.IDARTNR                                             
314500     END-EXEC                                                             
314600     MOVE SQLCODE TO SQLCODE-WS                                           
314700                                                                          
314800     MOVE 000100  TO GOOD-SQLCODECODES                                    
314900     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
315000     .                                                                    
315100     SKIP3                                                                
315200                                                                          
315300 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
315400     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
315500     MOVE 000100  TO GOOD-SQLCODECODES                                    
315600     EXEC SQL                                                             
315700         FETCH TP1ARTK-CRS INTO                                           
315800                    :TP1KAMP-IDKAMP                                       
315900                   ,:TP1ARTK-IDARTNR                                      
316000                   ,:TP1KAMP-TISTADAT-KAMP                                
316100                   ,:TP1KAMP-TISTODAT-KAMP                                
316200                   ,:TP1KAMP-KDKAMP                                       
316300     END-EXEC                                                             
316400                                                                          
316500     MOVE SQLCODE TO SQLCODE-WS                                           
316600     PERFORM DB2-STATUS-CHECK                                             
316700     .                                                                    
316800     SKIP3                                                                
316900                                                                          
317000 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
317100     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
317200     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
317300     .                                                                    
317400     EJECT                                                                
317500                                                                          
317600 DB2-STATUS-CHECK  SECTION.                                               
317700     SET SQLCODE-IX TO 1                                                  
317800     SEARCH GOOD-SQLCODE                                                  
317900       AT END                                                             
318000          CALL FELLOG                                                     
318100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
318200     END-SEARCH                                                           
318300     .                                                                    
318400     EJECT                                                                
318500                                                                          
318600     EJECT                                                                
318700*    -COPY WY2000P1                                                       
318800     EJECT                                                                
318900*    -COPY WY2000Q1                                                       
319000     EJECT                                                                
319100*    -COPY WY2000P2                                                       
