000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2711000.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/11/29.                                                
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
002100*        UTIFRÅN ERSÄTTNINGSKOD OCH LAGERTILLGÅNG                         
002200*        BEDÖMS OM REFILL TILL DC'T                                       
002300*        I VISSA FALL KAN SUBPROGRAMMET ÖVERLÅTA BEDÖM-                   
002400*        NIGEN TILL HUVUDPROGRAMMET                                       
002500*                                                                         
002600*                                                                         
002700*        PROGRAMMET LÄSER      WLARTC (WDE4)                              
002800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002900*        PROGRAMMET LÄSER      WLOIGA (WDL7)                              
003000*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
003100*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
003200*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
003300*        PROGRAMMET LÄSER              WDQ4B                              
003400*        PROGRAMMET LÄSER              WDQ2                               
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
004900     SELECT W271DC           ASSIGN W27110D1.                             
005000                                                                          
005100*          --- REFILLORDER / REFILLFÖRSLAG                                
005200     SELECT W27110                     ASSIGN TO W27110D2.                
005300                                                                          
005400*          --- ARTIKLAR ATT UPPDATERA I PGM W27136                        
005500     SELECT W27136                     ASSIGN TO W27110D3.                
005600                                                                          
005700                                                                          
005800*          --- ARTIKLAR ATT UPPDATERA I PGM W271F1                        
005900     SELECT W271F1                     ASSIGN TO W27110D4.                
006000                                                                          
006100     EJECT                                                                
006200 DATA DIVISION.                                                           
006300                                                                          
006400 FILE SECTION.                                                            
006500 FD  W271DC                                                               
006600     LABEL RECORD STANDARD                                                
006700     RECORDING F                                                          
006800     BLOCK CONTAINS 0.                                                    
006900 01  FILLER                  PIC X(80).                                   
007000                                                                          
007100 FD  W27110                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400*01  POST -COPY W27111 -PRE  W27110-  -L.                                 
007500                                                                          
007600 FD  W27136                                                               
007700     RECORDING       F                                                    
007800     BLOCK CONTAINS  0.                                                   
007900                                                                          
008000*01  POST -COPY W27136 -PRE  W27136-  -L.                                 
008100                                                                          
008200 FD  W271F1                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500                                                                          
008600*01  POST -COPY W271F1 -PRE  W271F1-  -L.                                 
008700                                                                          
008800     EJECT                                                                
008900 WORKING-STORAGE SECTION.                                                 
009000*    -COPY WY2000W2                                                       
009100     SKIP3                                                                
009200*    -COPY WY2000W1                                                       
009300     SKIP3                                                                
009400 01  FILLER                    PIC X(24)  VALUE 'WORKING STORAGE'.        
009500 77  IDPGM                       PIC X(8)    VALUE 'W2711000'.            
009600 77  JA                          PIC X       VALUE 'J'.                   
009700 77  NEJ                         PIC X       VALUE 'N'.                   
009800                                                                          
009900 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR'.              
010000                                                                          
010100 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-INDEX'.        
010200 01  ARBETSAREOR-INDEX.                                                   
010300                                                                          
010400                                                                          
010500     03 IX                      PIC S9(9)   VALUE ZERO COMP-3.            
010600     03 IX-2                    PIC S9(4)   VALUE ZERO COMP-3.            
010700     03 IX-3                    PIC S9(4)   VALUE ZERO COMP-3.            
010800     03 SUB                     PIC 9(1)    VALUE ZERO.                   
010900     03 VECKO-IX                PIC S9(2)   VALUE ZERO COMP-3.            
011000                                                                          
011100     03 DC-IX                   PIC 9(2)    VALUE ZERO.                   
011200     03 GRP-IX                  PIC 9(5)    VALUE ZERO.                   
011300     03 IDLOPNR-IX PIC 9(5) VALUE ZERO.                                   
011400     03 IDLOPNR-MAX-GRP         PIC 9(3)    VALUE 50.                     
011500     03 DC-MAX-GRP              PIC 9(3)    VALUE 6.                      
011600                                                                          
011700                                                                          
011800 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-TIDER'.        
011900 01  ARBETSAREOR-TIDER.                                                   
012000                                                                          
012100     03 W-DAGENS-DATUM           PIC 9(8)       VALUE ZERO.               
012200     03 WS-TIDATUM-CROSS         PIC 9(6)       VALUE ZERO.               
012300     03 WS-VAR                   PIC X          VALUE SPACE.              
012400                                                                          
012500     03 DAGENS-TIAAVVD           PIC 9(5)       VALUE ZERO.               
012600     03 DAGENS-TIAAVVD-GRP       REDEFINES DAGENS-TIAAVVD.                
012700        05 DAGENS-TIAAVV         PIC 9(4).                                
012800        05 DAGENS-TID            PIC 9(1).                                
012900                                                                          
013000     03 TVA-V-FRAMAT-TIAAVVD      PIC 9(5)       VALUE ZERO.              
013100     03 TVA-V-FRAMAT-TIAAVVD-GRP REDEFINES TVA-V-FRAMAT-TIAAVVD.          
013200        05 TVA-V-FRAMAT-TIAAVV     PIC 9(4).                              
013300        05 TVA-V-FRAMAT-TID        PIC 9(1).                              
013400                                                                          
013500     03 WS-NOLL                  PIC S9(9) VALUE ZERO COMP-3.             
013600     03 WS-TIAAVVD               PIC 9(5)       VALUE ZERO.               
013700     03 WS-TIAAVVD-GRP           REDEFINES WS-TIAAVVD.                    
013800        05 WS-TIAAVV             PIC 9(4).                                
013900        05 WS-TID                PIC 9(1).                                
014000                                                                          
014100     03 WS-INNEV-TIAARP          PIC 9(4)       VALUE ZERO.               
014200     03 WS-INNEV-TIAARP-DELAR    REDEFINES WS-INNEV-TIAARP.               
014300        05 WS-INNEV-TIAA         PIC 9(2).                                
014400        05 WS-INNEV-TIRP         PIC 9(2).                                
014500                                                                          
014600     03 WS-NASTA-TIAARP          PIC 9(4)       VALUE ZERO.               
014700     03 WS-NASTA-TIAARP-DELAR    REDEFINES WS-NASTA-TIAARP.               
014800        05 WS-NASTA-TIAA         PIC 9(2).                                
014900        05 WS-NASTA-TIRP         PIC 9(2).                                
015000                                                                          
015100     03 TVA-VECKOR-FRAMAT        PIC S9(5)  VALUE ZERO COMP-3.            
015200                                                                          
015300     03 WS-NASTA-PER-TIAAMMDD    PIC 9(6)   VALUE ZERO.                   
015400     03 WS-KVSPARR-KVAL          PIC 9(6)   VALUE ZERO.                   
015500     03 DAGENS-VECKA             PIC 9(2)   VALUE ZERO.                   
015600     03 WS-KVVIPER               PIC 9(1)   VALUE ZERO.                   
015700     03 WS-DAG-I-VECKA           PIC 9(1)   VALUE ZERO.                   
015800     03 WS-INNEV-VECKA           PIC 9(2)   VALUE ZERO.                   
015900     03 WS-FIRST-VV-RPER         PIC 9(2)   VALUE ZERO.                   
016000     03 WS-VECKA                 PIC 9(2)   VALUE ZERO.                   
016100     03 WS-WEEKBY2-QUOTIENT      PIC 9(2)   VALUE ZERO.                   
016200     03 WS-WEEKBY2-REMAINDER     PIC 9(2)   VALUE ZERO.                   
016300     03 WS-DAPUBL-AAR            PIC 9(4)   VALUE ZERO.                   
016400     03 WS-DAGENS-AAR            PIC 9(4)   VALUE ZERO.                   
016500     03 WS-DAGENS-AAR-MINUS1     PIC 9(4)   VALUE ZERO.                   
016600     03 PERIOD-VECKA             PIC 9(1)   VALUE ZERO.                   
016700                                                                          
016800     03 WS-FLYGT-MAX-TIAAMMDD    PIC 9(6)   VALUE ZERO.                   
016900     03 WS-FLYGBEHOV-MAX-TIAARP  PIC 9(4)   VALUE ZERO.                   
017000     03 WS-FIRST-TIBERANK        PIC 9(6)  VALUE ZERO.                    
017100     03 WS-FLYGBEHOV-MAX-TIAAMMDD                                         
017200                                 PIC 9(6) VALUE ZERO.                     
017300     03 WS-LOCAL-LEADTIME        PIC S9(3) VALUE ZERO COMP-3.             
017400     03 WS-LTID-A                PIC S9(3) VALUE ZERO COMP-3.             
017500     03 WS-LTID-B                PIC S9(3) VALUE ZERO COMP-3.             
017600     03 WS-LT-WEEKS              PIC S9(3) VALUE ZERO COMP-3.             
017700     03 WS-REST-DAYS             PIC  9(2) VALUE ZERO.                    
017800     03 WS-TIFINLV               PIC S9(5) VALUE ZERO COMP-3.             
017900     03 WS-DAPUBL                PIC 9(8)  VALUE ZERO.                    
018000     03 WS-TIAVRDAT-DISP         PIC 9(6)  VALUE ZERO.                    
018100     03 WS-IDLOPNR-DC            PIC S9(7)   VALUE ZERO COMP-3.           
018200     03 FILLER                   PIC X(16)   VALUE                        
018300                                             'WS-DB2-SEKTION'.            
018400     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
018500     03 WS-DC-TABELL             OCCURS 30.                               
018600      05 WS-IDLOPNR              PIC S9(7)   VALUE ZERO COMP-3.           
018700      05 WS-DC-NR                OCCURS 6                                 
018800                                 PIC X(2).                                
018900     03 WS-DC-MAX                PIC 9(7)    VALUE ZERO.                  
019000                                                                          
019100     03 DC-TRANSGRP-MAX          PIC 9(7)    VALUE 400.                   
019200     03 DC-TRANS-MAX             PIC 9(7)    VALUE 44.                    
019300     03 WS-DC-TRANS-TAB          OCCURS 400.                              
019400      05 WS-IDDC-REC             PIC X(2)    VALUE SPACE.                 
019500      05 WS-IDDC-SEND            OCCURS 44                                
019600                                 PIC X(2)    VALUE SPACE.                 
019700                                                                          
019800     03 WS-IDLAND-REF            PIC X(2)    VALUE SPACE.                 
019900     03 WS-IDDC-LAND             PIC X(2)    VALUE SPACE.                 
020000     03 WS-IDDC-LAND-SEND        PIC X(2)    VALUE SPACE.                 
020100                                                                          
020200     03 WS-TIERSDAT-VIPS         PIC 9(5)    VALUE ZEROES.                
020300     03 WS-KDERS                 PIC 9(3)    VALUE ZEROES.                
020400     03 WS-KVDISP-SEND-DC        PIC S9(7)   VALUE ZERO COMP-3.           
020500                                                                          
020600                                                                          
020700 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-ALLM'.         
020800 01  ARBETSAREOR-ALLM.                                                    
020900                                                                          
021000     03 WS-SPAR-IDARTNR          PIC S9(9) VALUE ZERO COMP-3.             
021100     03 WS-SPAR-IDARTNR-NUM      PIC  9(9) VALUE ZERO.                    
021200     03 WS-AKT-IDDC-NUM          PIC  9(2) VALUE ZERO.                    
021300     03 WS-KDFRAKT               PIC  9(2) VALUE ZERO.                    
021400     03 WS-IDDC-TRANSF           PIC  X(2) VALUE SPACE.                   
021500     03 WS-IDDISTR               PIC 9(5)  VALUE ZERO.                    
021600     03 WS-SNITT-VECKOR          PIC 9(1)V9(2)  VALUE 4.33.               
021700     03 WS-NEXT-WORKDAY          PIC 9(6)       VALUE ZERO.               
021800     03 WS-RA-TILLGANG          PIC S9(7)   VALUE ZERO COMP-3.            
021900     03 WS-SUM-KVAVROP          PIC S9(7)   VALUE ZERO COMP-3.            
022000     03 WS-JMF-KVAVROP          PIC S9(7)   VALUE ZERO COMP-3.            
022100                                                                          
022200     03 WS-KVOI-INNEV        PIC S9(7)     VALUE ZERO COMP-3.             
022300     03 WS-KVREFPKT-HALV     PIC S9(7)     VALUE ZERO COMP-3.             
022400     03 FILLER               PIC X(8)      VALUE 'KVREFPKT'.              
022500     03 WS-KVREFPKT-DC       PIC S9(7)     VALUE ZERO COMP-3.             
022600     03 WS-KVREFPKT-DUBBEL   PIC S9(7)     VALUE ZERO COMP-3.             
022700     03 WS-KVREFOVL-TEST     PIC S9(7)     VALUE ZERO COMP-3.             
022800                                                                          
022900     03 WS-FIX21-KVREFPKT-DC PIC S9(7)     VALUE ZERO COMP-3.             
023000     03 WS-FIX23-KVREFPKT-DC PIC S9(7)     VALUE ZERO COMP-3.             
023100     03 WS-FIX3A-KVREFPKT-DC PIC S9(7)     VALUE ZERO COMP-3.             
023200                                                                          
023300     03 WS-PRIS              PIC S9(7)V9(2) VALUE ZERO COMP-3.            
023400     03  WS-NEW-KVPB-REF      PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
023500     03  WS-KVPB-REF-1        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
023600     03  WS-KVPB-REF-2        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
023700     03  WS-KVPB-REF-3        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
023800     03  WS-KVPB-REF-4        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
023900     03  WS-KVPB-REF-5        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
024000     03  WS-KVPB-REF-6        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
024100     03  WS-SENASTE-KVPB      PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
024200     03  WS-BINNDAY-TIAAMMDD       PIC 9(6)    VALUE ZERO.                
024300     03  WS-WEEKS-IN-PERIOD        PIC 9       VALUE ZERO.                
024400     03  WS-WEEKS-IN-PERIOD-1      PIC 9       VALUE ZERO.                
024500     03  WS-WEEKS-IN-PERIOD-2      PIC 9       VALUE ZERO.                
024600     03  WS-PERIOD-JUST-1          PIC 9(4)    VALUE ZERO.                
024700     03  WS-PERIOD-JUST-2          PIC 9(4)    VALUE ZERO.                
024800     03  WS-TIVV                   PIC 9(2)    VALUE ZERO.                
024900     03  WS-TIVV-VECKA-I-PER       PIC 9(2)    VALUE ZERO.                
025000     03  WS-HIT-VV                 PIC 9(1)    VALUE ZERO.                
025100     03  WS-KVAR-VV                PIC 9(1)    VALUE ZERO.                
025200     03  INNEV-HIT-VV              PIC 9(1)    VALUE ZERO.                
025300     03  INNEV-KVAR-VV             PIC 9(1)    VALUE ZERO.                
025400     03  WS-TIAAMMDD-FOM           PIC 9(6)    VALUE ZERO.                
025500     03  WS-PER-I-TIRP             PIC 9(2)    VALUE ZERO.                
025600     03  WS-PER-II-TIRP            PIC 9(2)    VALUE ZERO.                
025700     03  WS-PER-III-TIRP           PIC 9(2)    VALUE ZERO.                
025800     03  WS-PER-IV-TIRP            PIC 9(2)    VALUE ZERO.                
025900     03  WS-PER-V-TIRP             PIC 9(2)    VALUE ZERO.                
026000     03  WS-PER-VI-TIRP            PIC 9(2)    VALUE ZERO.                
026100                                                                          
026200                                                                          
026300 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-ANTAL'.        
026400 01  ARBETSAREOR-ANTAL.                                                   
026500                                                                          
026600     03 FIX-ANTAL              PIC S9(7) VALUE ZERO COMP-3.               
026700     03 WS-ANTAL-GODK-E3       PIC S9(7) VALUE ZERO COMP-3.               
026800     03 WS-ANTAL-TRANSF-E3     PIC S9(7) VALUE ZERO COMP-3.               
026900     03 WS-ANTAL-RO-RESS-KL1-CDC PIC S9(7) VALUE ZERO COMP-3.             
027000     03 WS-KVROS-DC-TOT        PIC S9(7) VALUE ZERO COMP-3.               
027100     03 WS-ANTAL-BER           PIC S9(7) VALUE ZERO COMP-3.               
027200     03 WS-ANTAL-Q1            PIC S9(7) VALUE ZERO COMP-3.               
027300     03 WS-ANTAL-Q3            PIC S9(7) VALUE ZERO COMP-3.               
027400     03 WS-ANTAL-QX            PIC S9(7) VALUE ZERO COMP-3.               
027500     03 WS-ANTAL-SLUT          PIC S9(7) VALUE ZERO COMP-3.               
027600                                                                          
027700     03 WS-ANTAL-LEDT-FLYG     PIC S9(7)V9(1) VALUE ZERO COMP-3.          
027800     03 WS-ANTAL-FLYG          PIC S9(7)V9(1) VALUE ZERO COMP-3.          
027900                                                                          
028000     03 WS-ANTAL-FLYG-AVRUND       PIC  9(7)V9(1) VALUE ZERO.             
028100     03 WS-ANTAL-FLYG-AVRUND-DELAR                                        
028200                         REDEFINES WS-ANTAL-FLYG-AVRUND.                  
028300        05 WS-ANTAL-FLYG-AVRUND-HELTAL PIC 9(7).                          
028400        05 WS-ANTAL-FLYG-AVRUND-DECTAL PIC 9(1).                          
028500                                                                          
028600     03 WS-FORS-ANDR           PIC S9(5)V9(2) VALUE ZERO COMP-3.          
028700                                                                          
028800     03 WS-SUANTAL-ETA         PIC S9(7) VALUE ZERO COMP-3.               
028900     03 WS-SUPERWEEK           PIC S9(6)V9(1)                             
029000                                         VALUE ZERO COMP-3.               
029100     03 WS-KVDAGAR-FLYG-INNEV  PIC S9(2) VALUE ZERO COMP-3.               
029200     03 WS-KVDAGAR-FLYG-NASTA  PIC S9(3) VALUE ZERO COMP-3.               
029300                                                                          
029400                                                                          
029500 01  FILLER                  PIC X(24) VALUE 'ARBETSAREOR-TILLG'.         
029600 01  ARBETSAREOR-TILLG.                                                   
029700                                                                          
029800     03 WS-KVANT-BER         PIC  9(5)V9(2) VALUE ZERO.                   
029900     03 WS-KVANT-BER-DELAR   REDEFINES WS-KVANT-BER.                      
030000        05 WS-KVANT-BER-HELTAL PIC 9(5).                                  
030100        05 WS-KVANT-BER-DECTAL PIC 9(2).                                  
030200                                                                          
030300     03 WS-KVANT-Q1          PIC  9(5)V9(2) VALUE ZERO.                   
030400     03 WS-KVANT-Q1-DELAR    REDEFINES WS-KVANT-Q1.                       
030500        05 WS-KVANT-Q1-HELTAL PIC 9(5).                                   
030600        05 WS-KVANT-Q1-DECTAL PIC 9(2).                                   
030700                                                                          
030800     03 WS-KVANT-Q3          PIC  9(5)V9(2) VALUE ZERO.                   
030900     03 WS-KVANT-Q3-DELAR    REDEFINES WS-KVANT-Q3.                       
031000        05 WS-KVANT-Q3-HELTAL PIC 9(5).                                   
031100        05 WS-KVANT-Q3-DECTAL PIC 9(2).                                   
031200                                                                          
031300     03 WS-KVANT-QX          PIC  9(5)V9(2) VALUE ZERO.                   
031400     03 WS-KVANT-QX-DELAR    REDEFINES WS-KVANT-QX.                       
031500        05 WS-KVANT-QX-HELTAL PIC 9(5).                                   
031600        05 WS-KVANT-QX-DECTAL PIC 9(2).                                   
031700                                                                          
031800     03 WS-KVANT-SLUT        PIC  9(5)V9(2) VALUE ZERO.                   
031900     03 WS-KVANT-SLUT-DELAR  REDEFINES WS-KVANT-SLUT.                     
032000        05 WS-KVANT-SLUT-HELTAL PIC 9(5).                                 
032100        05 WS-KVANT-SLUT-DECTAL PIC 9(2).                                 
032200                                                                          
032300                                                                          
032400     03 FILLER              PIC X(8)        VALUE 'KVDISP'.               
032500     03 WS-KVDISP-DC        PIC S9(7)       VALUE ZERO COMP-3.            
032600     03 WS-KVLS-BER-DC      PIC S9(7)       VALUE ZERO COMP-3.            
032700     03 WS-KVDISP-FLYG      PIC S9(7)V9(1) VALUE ZERO COMP-3.             
032800     03 WS-KVDISP-LOCAL     PIC S9(7)V9(1) VALUE ZERO COMP-3.             
032900     03 WS-KVROS-TOT-LOCAL  PIC S9(7)      VALUE ZERO COMP-3.             
033000     03 WS-TILLG-CDC        PIC S9(7)      VALUE ZERO COMP-3.             
033100     03 WS-TILLG-KVAR-CDC   PIC S9(7)      VALUE ZERO COMP-3.             
033200     03 WS-BALANCE-ERSATT   PIC S9(7)      VALUE ZERO COMP-3.             
033300     03 FILLER              PIC X(8)       VALUE 'BALANCE'.               
033400     03 WS-BALANCE-DC       PIC S9(7)      VALUE ZERO COMP-3.             
033500     03 WS-KVKUNDRETUR      PIC S9(7)      VALUE ZERO COMP-3.             
033600     03 WS-QX-BRYTNING      PIC  9(2)      VALUE ZERO.                    
033700     03 WS-RADPRIS          PIC S9(7)V9(2) VALUE ZERO COMP-3.             
033800     03 WS-KVBEART-EJ-FAKT  PIC S9(7)       VALUE ZERO COMP-3.            
033900     03 WS-CD-TOT-KVBEART   PIC S9(7)       VALUE ZERO COMP-3.            
034000     03 WS-BOKAD-TOT-KVBEART   PIC S9(7)       VALUE ZERO COMP-3.         
034100     03 WS-HELTAL-BEST      PIC S9(7)      VALUE ZERO COMP-3.             
034200     03 WS-HELTAL-SALDO     PIC S9(7)      VALUE ZERO COMP-3.             
034300     03 WS-SALDO            PIC S9(7)      VALUE ZERO COMP-3.             
034400     03 WS-ARBETSDAGAR      PIC S9         VALUE ZERO COMP-3.             
034500     03 WS-KVQPACK-3        PIC S9(5)      VALUE ZERO COMP-3.             
034600     03 WS-KVBEART-Q-RUP    PIC S9(7)      VALUE ZERO COMP-3.             
034700                                                                          
034800                                                                          
034900 01  FILLER                  PIC X(24)  VALUE 'ARBETSAREOR-BEHOV'.        
035000 01  ARBETSAREOR-BEHOV.                                                   
035100                                                                          
035200     03 WS-KVPB-DAG-DC       PIC S9(6)V9(2) VALUE ZERO COMP-3.            
035300     03 WS-KVPB-DAG-DC-OKAD  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
035400     03 WS-KVPB-DAG-DC-NORM  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
035500     03 WS-KVPB-DAG-LOCAL    PIC S9(7)V9(3) VALUE ZERO COMP-3.            
035600     03 WS-KVPB-LT-BEHOV-DC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
035700     03 WS-CDC-DAGSBEHOV     PIC S9(6)V9(2) VALUE ZERO COMP-3.            
035800     03 WS-4DAG-BEHOV-CDC    PIC S9(7)V9(3) VALUE ZERO COMP-3.            
035900     03 WS-SUM-XDC-BEHOV     PIC S9(7)V9(3) VALUE ZERO COMP-3.            
036000                                                                          
036100     03 WS-KVPB-VECKA-CDC    PIC S9(6)V9(3) VALUE ZERO COMP-3.            
036200     03 WS-KVPB-VECKA-DC     PIC S9(6)V9(3) VALUE ZERO COMP-3.            
036300     03 WS-KVPB-ANTV-DC      PIC S9(6)V9(3) VALUE ZERO COMP-3.            
036400     03 WS-KVPB-DAG-INNEV-RP PIC S9(6)V9(5) VALUE ZERO COMP-3.            
036500     03 WS-KVPB-DAG-NASTA-RP PIC S9(6)V9(5) VALUE ZERO COMP-3.            
036600     03 WS-KVPB-INNEV-RP     PIC S9(6)V9(3) VALUE ZERO COMP-3.            
036700     03 WS-KVPB-NASTA-RP     PIC S9(6)V9(3) VALUE ZERO COMP-3.            
036800     03 WS-KVPB-FLYG         PIC S9(7)V9(1) VALUE ZERO COMP-3.            
036900     03 WS-KVPB-LOCAL        PIC S9(7)V9(1) VALUE ZERO COMP-3.            
037000     03 WS-KVPB-TOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.            
037100                                                                          
037200     03 WS-LT-BEHOV-DC-OKAD  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
037300     03 WS-LT-BEHOV-DC-NORM  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
037400     03 WS-LT-BEHOV-DC-DIFF  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
037500                                                                          
037600     03  WS-KVPBREOI-DAY         PIC S9(6)V9(5) VALUE ZERO                
037700                                                COMP-3.                   
037800     03  WS-KVPB-REF-DAY-PER-I   PIC S9(6)V9(5) VALUE ZERO                
037900                                                COMP-3.                   
038000     03  WS-KVPB-REF-DAY-PER-II  PIC S9(6)V9(5) VALUE ZERO                
038100                                                COMP-3.                   
038200     03  WS-KVPB-REF-DAY-PER-III PIC S9(6)V9(5) VALUE ZERO                
038300                                                COMP-3.                   
038400     03  WS-KVPB-REF-DAY-PER-IV  PIC S9(6)V9(5) VALUE ZERO                
038500                                                COMP-3.                   
038600     03  WS-KVPB-REF-DAY-PER-V   PIC S9(6)V9(5) VALUE ZERO                
038700                                                COMP-3.                   
038800     03  WS-KVPB-REF-DAY-PER-VI  PIC S9(6)V9(5) VALUE ZERO                
038900                                                COMP-3.                   
039000     03 WS-KVPB-ANTV-DC-NUM PIC  9(6)V9(2)  VALUE ZERO.                   
039100     03 WS-KVPB-ANTV-DC-NUM-DELAR REDEFINES WS-KVPB-ANTV-DC-NUM.          
039200        05 WS-KVPB-ANTV-DC-HELTAL-NUM PIC 9(6).                           
039300        05 WS-KVPB-ANTV-DC-DECTAL-NUM PIC 9(2).                           
039400                                                                          
039500     03 WS-KVPB-VECKA-DC-IX  PIC S9(7)V9(5) VALUE ZERO COMP-3.            
039600                                                                          
039700     EJECT                                                                
039800                                                                          
039900 01  FILLER                     PIC X(24)   VALUE 'SWITCHAR'.             
040000                                                                          
040100 77  KONTROLL-SW                    PIC X   VALUE 'N'.                    
040200     88  KONTROLL                           VALUE 'J'.                    
040300                                                                          
040400 77  BEORDRA-SW                     PIC X   VALUE 'N'.                    
040500     88  BEORDRA                            VALUE 'J'.                    
040600                                                                          
040700 77  RELEASEDAY-SW                  PIC X   VALUE 'N'.                    
040800     88  RELEASEDAY                         VALUE 'J'.                    
040900                                                                          
041000 77  BRIST-I-CDC-SW                 PIC X   VALUE 'N'.                    
041100     88  BRIST-I-CDC                        VALUE 'J'.                    
041200                                                                          
041300 77  BRIST-I-DC-SW                  PIC X   VALUE 'N'.                    
041400     88  BRIST-I-DC                         VALUE 'J'.                    
041500     88  EJ-BRIST-I-DC                      VALUE 'N'.                    
041600                                                                          
041700 77  OKAD-FORSALJNING-SW            PIC X   VALUE 'N'.                    
041800     88  OKAD-FORSALJNING                   VALUE 'J'.                    
041900                                                                          
042000 77  FLYGFORSLAG-SW                 PIC X   VALUE 'N'.                    
042100     88  FLYGFORSLAG                        VALUE 'J'.                    
042200                                                                          
042300 77  FORSLAG-SW                     PIC X   VALUE 'N'.                    
042400     88  FORSLAG                            VALUE 'J'.                    
042500                                                                          
042600 77  TRANSFER-SW                    PIC X   VALUE 'N'.                    
042700     88  TRANSFER                           VALUE 'J'.                    
042800                                                                          
042900 77  TRANSFER-GRP-SW                PIC X   VALUE 'N'.                    
043000     88  TRANSFER-GRP                       VALUE 'J'.                    
043100                                                                          
043200 77  ERSATT-SW                      PIC X   VALUE 'N'.                    
043300     88  ERSATT                             VALUE 'J'.                    
043400                                                                          
043500 77  REFWAY-SW                      PIC X   VALUE 'J'.                    
043600     88  REFWAY-NOT-SETUP                   VALUE 'N'.                    
043700                                                                          
043800 77  TILLKOMMANDE-SW                PIC X   VALUE 'N'.                    
043900     88  TILLKOMMANDE                       VALUE 'J'.                    
044000                                                                          
044100 77  KVAL-SPARR-CDC-SW              PIC X   VALUE 'N'.                    
044200     88  KVAL-SPARR-CDC                     VALUE 'J'.                    
044300                                                                          
044400 77  L-STOCK-LOCAL-SW               PIC X   VALUE 'N'.                    
044500     88  L-STOCK-LOCAL                      VALUE 'J'.                    
044600                                                                          
044700 77  NO-PRICE-LOCAL-SW              PIC X   VALUE 'N'.                    
044800     88  NO-PRICE-LOCAL                     VALUE 'J'.                    
044900                                                                          
045000 77  ARTIKEL-UTGANGEN-K6-SW         PIC X   VALUE 'N'.                    
045100     88  ARTIKEL-UTGANGEN-K6                VALUE 'J'.                    
045200                                                                          
045300 77  ARTIKEL-FINNS-L7-SW            PIC X   VALUE 'N'.                    
045400     88  ARTIKEL-FINNS-L7                   VALUE 'J'.                    
045500     88  ARTIKEL-SAKNAS-L7                  VALUE 'N'.                    
045600                                                                          
045700 77  WEEK-SW                        PIC X   VALUE ' '.                    
045800     88  WEEK-EVEN                          VALUE 'J'.                    
045900     88  WEEK-ODD                           VALUE 'N'.                    
046000                                                                          
046100 77  KDFARLIG-SW                    PIC X   VALUE 'N'.                    
046200     88  DANGEROUSGOODS                     VALUE 'J'.                    
046300     88  NOTDANGEROUS                       VALUE 'N'.                    
046400                                                                          
046500 77  ERS-WDK611-SW                  PIC X   VALUE 'N'.                    
046600     88  ERS-WDK611-SEGMENT-FINNS           VALUE 'J'.                    
046700     88  ERS-WDK611-SEGMENT-SAKNAS          VALUE 'N'.                    
046800                                                                          
046900 77  SW-REFILL-KANSKE               PIC X   VALUE 'N'.                    
047000     88  ART-SKALL-KANSKE-REFILLAS          VALUE 'J'.                    
047100                                                                          
047200 77  INTERN-REFILL-SW               PIC X   VALUE 'N'.                    
047300     88  INTERN-REFILL-OK                   VALUE 'J'.                    
047400                                                                          
047500 77  SKRIV-ORDER-SW                 PIC X   VALUE 'J'.                    
047600     88  SKRIV-ORDER                        VALUE 'J'.                    
047700     88  SKRIV-EJ-ORDER                     VALUE 'N'.                    
047800                                                                          
047900 77  WS-WORKDAY-STATUS              PIC X   VALUE 'J'.                    
048000     88  RATT-FRAN-WORKDAY                  VALUE 'J'.                    
048100     88  FEL-FRAN-WORKDAY                   VALUE 'N'.                    
048200                                                                          
048300     EJECT                                                                
048400                                                                          
048500 01  FILLER                    PIC X(24)  VALUE 'KONSTANTER'.             
048600                                                                          
048700*01  -COPY W271RTXT                                                       
048800     EJECT                                                                
048900                                                                          
049000*01  -COPY WWPRODSL                                                       
049100                                                                          
049200*    --- COPYTEXT FÖR ATT KUNNA UR DISTR FÅ MOTTAGANDE IDDC               
049300*01 -COPY WWDIST35                                                        
049400     EJECT                                                                
049500                                                                          
049600 01  DC-POST.                                                             
049700     03  DC-PARAMETER-TIME    PIC X(2).                                   
049800     03  FILLER               PIC X(78).                                  
049900                                                                          
050000 01  FELTEXT.                                                             
050100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
050200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
050300     EJECT                                                                
050400                                                                          
050500                                                                          
050600     EJECT                                                                
050700                                                                          
050800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
050900 01  FILLER REDEFINES DAGENS-DATUM.                                       
051000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
051100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
051200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
051300                                                                          
051400     EJECT                                                                
051500                                                                          
051600*      --- VALID IDDC CODES                                               
051700*                                                                         
051800*01    -COPY WWDC99                                                       
051900*01    -COPY WWDC99    -PRE REF-                                          
052000*01    -COPY WWDCKONS                                                     
052100       EJECT                                                              
052200*                                                                         
052300*01    -COPY WWDCLAND                                                     
052400       EJECT                                                              
052500*                                                                         
052600*01    -COPY WWBYT03                                                      
052700       EJECT                                                              
052800                                                                          
052900 01  FILLER                    PIC X(24)  VALUE 'SUBPROGRAM'.             
053000                                                                          
053100 01  DYNAMISKA-SUBPROGRAM.                                                
053200*                                                                         
053300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
053400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
053500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
053600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
053700     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
053800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
053900     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
054000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
054100     03  W271LTPB                PIC X(8)    VALUE 'W271LTPB'.            
054200     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
054300     EJECT                                                                
054400*    --- PARAMETRAR TILL ABEND                                            
054500                                                                          
054600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
054700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
054800     SKIP2                                                                
054900*    --- PARAMETRAR TILL POSTSUM                                          
055000*                                                                         
055100*01  -COPY W0005   -PRE  POSTSUM-                                         
055200     EJECT                                                                
055300*    --- PARAMETRAR TILL WORKDAY                                          
055400*                                                                         
055500*01  -COPY WORKAREA                                                       
055600     EJECT                                                                
055700*    --- PARAMETRAR TILL DATKONV                                          
055800*                                                                         
055900*01  -COPY WDATAREA                                                       
056000     EJECT                                                                
056100                                                                          
056200*    --- PARAMETRAR TILL DAGKONV                                          
056300*                                                                         
056400*01  -COPY WDAGAREA                                                       
056500     EJECT                                                                
056600                                                                          
056700*    --- PARAMETRAR TILL VECKOADD                                         
056800                                                                          
056900 01  W009VADD-AREA.                                                       
057000     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
057100     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
057200                                                                          
057300                                                                          
057400*    --- PARAMETRAR TILL W271LTPB                                         
057500                                                                          
057600*01  -COPY W271LTPB                                                       
057700                                                                          
057800*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
057900 01  FILLER                     PIC X(16) VALUE 'W218LETA START'.         
058000*01  -COPY W218LETA  -PRE ETA-                                            
058100                                                                          
058200 01  FILLER                     PIC X(12) VALUE 'DUMMY ARTC'.             
058300 01  ETA-ARTC-PCB               PIC X(1).                                 
058400 01  FILLER                     PIC X(12) VALUE 'DUMMY ARTS'.             
058500 01  ETA-ARTS-PCB               PIC X(1).                                 
058600 01  FILLER                     PIC X(12) VALUE 'DUMMY INLC'.             
058700 01  ETA-INLC-PCB               PIC X(1).                                 
058800 01  FILLER                     PIC X(12) VALUE 'DUMMY LEVA'.             
058900 01  ETA-LEVA-PCB               PIC X(1).                                 
059000                                                                          
059100     EJECT                                                                
059200 01  W27110-AREA-START           PIC X(24)   VALUE                        
059300                                             'W27110-AREA-START'.         
059400     SKIP2                                                                
059500                                                                          
059600*01  AREA -COPY W27111     -PRE W27110-                                   
059700*                                                                         
059800*                                                                         
059900 01  W27136-AREA-START           PIC X(24)   VALUE                        
060000                                             'W27136-AREA-START'.         
060100     SKIP2                                                                
060200                                                                          
060300*01  AREA -COPY W27136     -PRE W27136-                                   
060400*                                                                         
060500*                                                                         
060600 01  W271F1-AREA-START           PIC X(24)   VALUE                        
060700                                             'W271F1-AREA-START'.         
060800     SKIP2                                                                
060900                                                                          
061000*01  AREA -COPY W271F1     -PRE W271F1-                                   
061100*                                                                         
061200     EJECT                                                                
061300 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
061400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
061500                                                                          
061600 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
061700 01  DB2-WS.                                                              
061800     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
061900         88  CURSOR-OK                      VALUE 000.                    
062000         88  LINES-FOUND                    VALUE 000.                    
062100         88  LINES-MISSING                  VALUE 100.                    
062200         88  RESOURCE-WRONG                 VALUE 904.                    
062300     03  GOOD-SQLCODECODES.                                               
062400         05  GOOD-SQLCODE OCCURS 5                                        
062500             INDEXED BY SQLCODE-IX PIC 9(3).                              
062600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
062700     EJECT                                                                
062800*  KEYS                                                                   
062900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
063000     SKIP3                                                                
063100 01  NYCKLAR-TILL-DLI.                                                    
063200     03  W-IDARTNR-X.                                                     
063300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
063400                                                                          
063500     03  W-IDARTNR-INT-X.                                                 
063600         05  W-IDARTNR-INT       PIC S9(9)   VALUE ZERO COMP-3.           
063700                                                                          
063800     03  W-IDARTNR-ERS-X.                                                 
063900         05  W-IDARTNR-ERS       PIC S9(9)   VALUE ZERO COMP-3.           
064000                                                                          
064100     03  W-IDLEVNR-X.                                                     
064200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
064300                                                                          
064400     03  W-IDLEVNR-PR-X.                                                  
064500         05  W-IDLEVNR-PR        PIC X(5)    VALUE LOW-VALUE.             
064600                                                                          
064700     03  W-DAPRLIST-PR-N.                                                 
064800         05  W-DAPRLIST-PR       PIC 9(8)    VALUE ZERO.                  
064900                                                                          
065000     03  W-IDLEVNR-21-X.                                                  
065100         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
065200                                                                          
065300     03  W-DAPRLIST-21-N.                                                 
065400         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
065500                                                                          
065600     03  W-IDDC-X.                                                        
065700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
065800                                                                          
065900     03  W-IDLAND-X.                                                      
066000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
066100                                                                          
066200     03  W-IDDC-B6-X.                                                     
066300         05  W-IDDC-B6       PIC X(2)   VALUE LOW-VALUE.                  
066400                                                                          
066500     03  W-IDDC-B616-X.                                                   
066600         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
066700                                                                          
066800     03  W-IDDC-TRANSF-X.                                                 
066900         05  W-IDDC-TRANSF       PIC X(2)    VALUE SPACE.                 
067000                                                                          
067100     03  W-IDDC-ERS-X.                                                    
067200         05  W-IDDC-ERS          PIC X(2)    VALUE SPACE.                 
067300                                                                          
067400     03  W-IDDC-INT-X.                                                    
067500         05  W-IDDC-INT          PIC X(2)    VALUE SPACE.                 
067600                                                                          
067700     03  W-IDDC-LOCAL-X.                                                  
067800         05  W-IDDC-LOCAL        PIC X(2)    VALUE SPACE.                 
067900                                                                          
068000     03  W-KDSEGKEY-X.                                                    
068100         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
068200                                                                          
068300     03  W-IDPTYP-R30-X.                                                  
068400         05  W-IDPTYP-R30        PIC X(3)    VALUE 'R30'.                 
068500                                                                          
068600     03  W-IDPTYP-310-X.                                                  
068700         05  W-IDPTYP-310        PIC X(3)    VALUE '310'.                 
068800                                                                          
068900     03  W-IDARTNR-301-X.                                                 
069000         05  W-IDARTNR-301       PIC S9(9)   VALUE  ZERO COMP-3.          
069100                                                                          
069200     03  W-IDDC-301-X.                                                    
069300         05  W-IDDC-301          PIC X(2)    VALUE SPACE.                 
069400                                                                          
069500     03 W-WDE301KY-MIN-X.                                                 
069600         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
069700         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
069800         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
069900         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
070000         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
070100                                                                          
070200     03 W-WDE301KY-MAX-X.                                                 
070300         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
070400         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE ZERO COMP-3.             
070500         05  W-KDREFTYP-MAX      PIC X     VALUE SPACE.                   
070600         05  W-IDARTNR-MAX       PIC S9(9)                                
070700                                         VALUE +999999999 COMP-3.         
070800         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
070900                                                                          
071000     03  W-WDD7A1KY-MIN-X.                                                
071100       05  W-IDARTNR-TILLK-MIN     PIC S9(9)   COMP-3  VALUE ZERO.        
071200       05  W-IDARTNR-ERS-MIN       PIC S9(9)   COMP-3  VALUE ZERO.        
071300       05  W-IDKORTNR-MIN          PIC S9(3)   COMP-3  VALUE ZERO.        
071400                                                                          
071500     03  W-WDD7A1KY-MAX-X.                                                
071600       05  W-IDARTNR-TILLK-MAX     PIC S9(9)   COMP-3                     
071700                                              VALUE +999999999.           
071800       05  W-IDARTNR-ERS-MAX       PIC S9(9)   COMP-3                     
071900                                              VALUE +999999999.           
072000       05  W-IDKORTNR-MAX          PIC S9(3)   COMP-3  VALUE +999.        
072100                                                                          
072200     03  W-WDA5D1KY-MIN-X.                                                
072300       05  W-IDDISTR-A5D-MIN   PIC S9(5)  VALUE ZERO COMP-3.              
072400       05  W-IDKUNDNR-A5D-MIN  PIC S9(7)  VALUE ZERO COMP-3.              
072500       05  W-IDARTNR-A5D-MIN   PIC S9(9)  VALUE ZERO COMP-3.              
072600       05  W-IDKUNDREF-A5D-MIN PIC X(10)  VALUE LOW-VALUE.                
072700       05  W-IDLOPNR-A5D-MIN   PIC S9(3)  VALUE ZERO COMP-3.              
072800     03  W-WDA5D1KY-MAX-X.                                                
072900       05  W-IDDISTR-A5D-MAX   PIC S9(5)  VALUE +99999 COMP-3.            
073000       05  W-IDKUNDNR-A5D-MAX  PIC S9(7)  VALUE +9999999                  
073100                                                 COMP-3.                  
073200       05  W-IDARTNR-A5D-MAX   PIC S9(9)  VALUE +999999999                
073300                                                 COMP-3.                  
073400       05  W-IDKUNDREF-A5D-MAX PIC X(10)  VALUE HIGH-VALUE.               
073500       05  W-IDLOPNR-A5D-MAX   PIC S9(3)  VALUE +999 COMP-3.              
073600                                                                          
073700     03  W-WDD901KY-X.                                                    
073800         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
073900         05  W-IDDC-D9           PIC X(2)    VALUE '11'.                  
074000                                                                          
074100     03  W-KDAVROP-D9-X.                                                  
074200         05  W-KDAVROP-D9        PIC S9(1)   VALUE ZERO COMP-3.           
074300                                                                          
074400     03  W-WDA501KY-X.                                                    
074500         05  W-WDA501KY          PIC X(24)   VALUE SPACE.                 
074600                                                                          
074700     03  W-WDE4KEY-X.                                                     
074800         05  W-IDDISTR-E4        PIC S9(5)   VALUE ZERO COMP-3.           
074900         05  W-IDKUNDNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
075000         05  W-IDKUNDRF-E4       PIC X(10)   VALUE SPACE.                 
075100         05  W-IDPRODNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
075200         05  W-IDPLKLST-E4       PIC S9(3)   VALUE ZERO COMP-3.           
075300     03  W-WDE4CSEQ-MIN-X.                                                
075400         05  W-WDE4C-IDARTNR-MIN PIC S9(9) VALUE ZERO COMP-3.             
075500                                                                          
075600     03  W-WDE4CSEQ-MAX-X.                                                
075700         05  W-WDE4C-IDARTNR-MAX PIC S9(9) VALUE ZERO COMP-3.             
075800                                                                          
075900     03  W-IDPRODNR-X.                                                    
076000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
076100     03  W-IDKOLLI-X.                                                     
076200         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
076300     SKIP2                                                                
076400     03  W-WDQ4BKY-FOM.                                                   
076500         05  W-Q4-IDARTNR-MIN    PIC  S9(9)    COMP-3.                    
076600         05  FILLER              PIC  X(32)    VALUE LOW-VALUE.           
076700                                                                          
076800     03  W-WDQ4BKY-TOM.                                                   
076900         05  W-Q4-IDARTNR-MAX    PIC  S9(9)    COMP-3.                    
077000         05  FILLER              PIC  X(32)    VALUE HIGH-VALUE.          
077100     SKIP2                                                                
077200     03  W-Q2-IDORDER-X.                                                  
077300         05  W-Q2-IDORDER        PIC S9(7)     COMP-3.                    
077400                                                                          
077500     03  W-Q2-IDDC-X.                                                     
077600         05  W-Q2-IDDC           PIC X(2)       VALUE SPACE.              
077700                                                                          
077800     03  W-Q4-IDDC-X.                                                     
077900         05  W-Q4-IDDC           PIC X(2)       VALUE SPACE.              
078000                                                                          
078100*    --- STATUS-KOD FRÅN IMS                                              
078200 01  STATUS-WS                   PIC XX.                                  
078300     88  SEGMENT-FINNS                       VALUE '  '.                  
078400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
078500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
078600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
078700     88  IMS-EJ-OK                           VALUE 'XD'.                  
078800     SKIP2                                                                
078900 01  GODK-STATUSKODER.                                                    
079000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
079100     SKIP3                                                                
079200 01  SSA1                        PIC X(128).                              
079300 01  SSA2                        PIC X(128).                              
079400 01  SSA3                        PIC X(128).                              
079500     EJECT                                                                
079600*    --- IMS FUNKTIONSKODER                                               
079700*01  -COPY W0003                                                          
079800     EJECT                                                                
079900                                                                          
080000*    ---  DLI INPUT-OUTPUT AREA                                           
080100                                                                          
080200                                                                          
080300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
080400                                                                          
080500 01  DLI-IO-AREA.                                                         
080600     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
080700                                                                          
080800                                                                          
080900     03  WDK701 REDEFINES IO-AREA.                                        
081000*        05  -COPY WDK701 -PRE SB-                                        
081100                                                                          
081200     03  WDK711 REDEFINES IO-AREA.                                        
081300*        05  -COPY WDK711 -PRE SB-                                        
081400     SKIP3                                                                
081500                                                                          
081600                                                                          
081700 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
081800 01  DLI-IO-ARTC01.                                                       
081900*    03  -COPY WDK601     -PRE WDK6-                                      
082000     EJECT                                                                
082100                                                                          
082200 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
082300 01  DLI-IO-ARTC11.                                                       
082400*    03  -COPY WDK611                                                     
082500     EJECT                                                                
082600                                                                          
082700 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC21'.                      
082800 01  DLI-IO-ARTC21.                                                       
082900*    03  -COPY WDK621                                                     
083000     EJECT                                                                
083100                                                                          
083200                                                                          
083300                                                                          
083400 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA01'.                      
083500 01  DLI-IO-OIGA01.                                                       
083600*    03  -COPY WDL701                                                     
083700     EJECT                                                                
083800                                                                          
083900 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
084000 01  DLI-IO-OIGA11.                                                       
084100*    03  -COPY WDL711                                                     
084200     EJECT                                                                
084300                                                                          
084400                                                                          
084500                                                                          
084600 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTM01'.                      
084700 01  DLI-IO-ARTM01.                                                       
084800*    03  -COPY WDK901         -PRE WDK9-                                  
084900     EJECT                                                                
085000                                                                          
085100                                                                          
085200                                                                          
085300 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORDL01'.                      
085400 01  DLI-IO-ORDL01.                                                       
085500*     03  -COPY WDE301                                                    
085600                                                                          
085700     EJECT                                                                
085800                                                                          
085900                                                                          
086000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ERSA13'.                      
086100 01  DLI-IO-ERSA13.                                                       
086200*    03  -COPY WDD704   -PRE ERSA-                                        
086300     EJECT                                                                
086400                                                                          
086500                                                                          
086600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB01'.                      
086700 01  DLI-IO-INLB01.                                                       
086800*    03  -COPY WDD901    -PRE WDD901-                                     
086900     EJECT                                                                
087000                                                                          
087100 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB11'.                      
087200 01  DLI-IO-INLB11.                                                       
087300*    03  -COPY WDD902    -PRE WDD902-                                     
087400     EJECT                                                                
087500                                                                          
087600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB23'.                      
087700 01  DLI-IO-INLB23.                                                       
087800*    03  -COPY WDD905    -PRE WDD905-                                     
087900     EJECT                                                                
088000                                                                          
088100                                                                          
088200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-TRANSF'.               
088300 01  DLI-IO-WDK711-TRANSF.                                                
088400*    03  -COPY WDK711   -PRE  TRANSF-                                     
088500     EJECT                                                                
088600                                                                          
088700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-ERS'.                  
088800 01  DLI-IO-WDK711-ERS.                                                   
088900*    03  -COPY WDK711   -PRE  ERS-                                        
089000     EJECT                                                                
089100                                                                          
089200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-LOCAL'.                
089300 01  DLI-IO-WDK711-LOCAL.                                                 
089400*    03  -COPY WDK711   -PRE  LOC-                                        
089500     EJECT                                                                
089600                                                                          
089700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-INTERN'.               
089800 01  DLI-IO-WDK711-INTERN.                                                
089900*    03  -COPY WDK711   -PRE  INT-                                        
090000     EJECT                                                                
090100                                                                          
090200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK722-INTERN'.               
090300 01  DLI-IO-WDK722-INTERN.                                                
090400*    03  -COPY WDK722                                                     
090500     EJECT                                                                
090600                                                                          
090700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712-LART'.                 
090800 01  DLI-IO-WDK712-LART.                                                  
090900*    03  -COPY WDK712                                                     
091000     EJECT                                                                
091100                                                                          
091200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK724'.                      
091300 01  DLI-IO-WDK724.                                                       
091400*    03  -COPY WDK724                                                     
091500     EJECT                                                                
091600                                                                          
091700                                                                          
091800                                                                          
091900 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLC01'.                      
092000 01  DLI-IO-INLC01.                                                       
092100*    03  -COPY WDL601                                                     
092200     EJECT                                                                
092300                                                                          
092400 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLC11'.                      
092500 01  DLI-IO-INLC11.                                                       
092600*     03  -COPY WDL611                                                    
092700                                                                          
092800     EJECT                                                                
092900                                                                          
093000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01-ERS'.                  
093100 01  DLI-IO-ARTC01-ERS.                                                   
093200*     03  -COPY WDK601       -PRE ERS-                                    
093300                                                                          
093400     EJECT                                                                
093500                                                                          
093600 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11-ERS'.                  
093700 01  DLI-IO-ARTC11-ERS.                                                   
093800*     03  -COPY WDK611       -PRE ERS-                                    
093900                                                                          
094000     EJECT                                                                
094100                                                                          
094200 01  FILLER         PIC X(16) VALUE 'DLI-IO-ERSB01'.                      
094300 01  DLI-IO-ERSB01.                                                       
094400*     03  -COPY WDD7A1       -PRE ERSB-                                   
094500                                                                          
094600     EJECT                                                                
094700 01  FILLER         PIC X(16) VALUE 'DLI-IO-ORDT01'.                      
094800 01  DLI-IO-ORDT01.                                                       
094900      03  -COPY WDA5D1                                                    
095000                                                                          
095100     EJECT                                                                
095200                                                                          
095300 01  FILLER         PIC X(16) VALUE 'DLI-IO-ORDP01'.                      
095400 01  DLI-IO-ORDP01.                                                       
095500      03  -COPY WDA501                                                    
095600                                                                          
095700     EJECT                                                                
095800                                                                          
095900 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA01'.                      
096000 01  DLI-IO-LEVA01.                                                       
096100*    03  -COPY WDF101                                                     
096200     EJECT                                                                
096300                                                                          
096400 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA16'.                      
096500 01  DLI-IO-LEVA16.                                                       
096600*    03  -COPY WDF116                                                     
096700     EJECT                                                                
096800                                                                          
096900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE401'.                      
097000 01  DLI-IO-WDE401.                                                       
097100*    03  -COPY WDE401                                                     
097200     EJECT                                                                
097300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE411'.                      
097400 01  DLI-IO-WDE411.                                                       
097500*    03  -COPY WDE411                                                     
097600     EJECT                                                                
097700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
097800 01  DLI-IO-WDE421.                                                       
097900*    03  -COPY WDE421                                                     
098000     EJECT                                                                
098100 01  FILLER         PIC X(24) VALUE 'DLI-IO-E601'.                        
098200 01  DLI-IO-E601.                                                         
098300*    03  -COPY WDE601                                                     
098400     EJECT                                                                
098500 01  FILLER         PIC X(24) VALUE 'DLI-IO-E611'.                        
098600 01  DLI-IO-E611.                                                         
098700*    03  -COPY WDE611                                                     
098800     EJECT                                                                
098900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4B1'.                      
099000 01      DLI-IO-WDQ4B1.                                                   
099100*     03  -COPY WDQ4B1                                                    
099200     EJECT                                                                
099300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
099400 01      DLI-IO-WDQ201.                                                   
099500*     03  -COPY WDQ201                                                    
099600     EJECT                                                                
099700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ212'.                      
099800 01      DLI-IO-WDQ212.                                                   
099900*     03  -COPY WDQ212                                                    
100000     EJECT                                                                
100100*                                                                         
100200 01  FILLER         PIC X(16) VALUE 'WDB601 AREA'.                        
100300 01   DLI-IO-AREA-B601.                                                   
100400*     03  -COPY WDB601                                                    
100500     EJECT                                                                
100600 01  FILLER         PIC X(16) VALUE 'WDB616 AREA'.                        
100700 01   DLI-IO-AREA-B616.                                                   
100800*     03  -COPY WDB616       -PRE B6-                                     
100900     EJECT                                                                
101000                                                                          
101100 01  FILLER         PIC X(16) VALUE 'WDK629 AREA'.                        
101200 01   DLI-IO-AREA-K629.                                                   
101300*     03  -COPY WDK629       -PRE K6-                                     
101400     EJECT                                                                
101500                                                                          
101600 01  FILLER         PIC X(16) VALUE 'TP5IDDC-AREA'.                       
101700                                                                          
101800*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
101900     EJECT                                                                
102000                                                                          
102100 01  FILLER         PIC X(16) VALUE 'TP4TRAN-AREA'.                       
102200                                                                          
102300*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
102400     EJECT                                                                
102500                                                                          
102600     EJECT                                                                
102700 LINKAGE SECTION.                                                         
102800*01  -COPY W0008  -PRE WDK7-                                              
102900     05  FILLER                  PIC X.                                   
103000     EJECT                                                                
103100*01  -COPY W0008  -PRE ARTC-                                              
103200     05  FILLER                  PIC X.                                   
103300     EJECT                                                                
103400*01  -COPY W0008  -PRE OIGA-                                              
103500     05  FILLER                  PIC X.                                   
103600     EJECT                                                                
103700*01  -COPY W0008  -PRE ARTM-                                              
103800     05  FILLER                  PIC X.                                   
103900     EJECT                                                                
104000*01  -COPY W0008  -PRE ORDL-                                              
104100     05  FILLER                  PIC X.                                   
104200     EJECT                                                                
104300*01  -COPY W0008  -PRE ERSA-                                              
104400     05  FILLER                  PIC X.                                   
104500     EJECT                                                                
104600*01  -COPY W0008  -PRE INLB-                                              
104700     05  FILLER                  PIC X.                                   
104800     EJECT                                                                
104900*01  -COPY W0008  -PRE WDK7-2-                                            
105000     05  FILLER                  PIC X.                                   
105100     EJECT                                                                
105200*01  -COPY W0008  -PRE WDK72-                                             
105300     05  FILLER                  PIC X.                                   
105400     EJECT                                                                
105500*01  -COPY W0008  -PRE INLC-                                              
105600     05  FILLER                  PIC X.                                   
105700     EJECT                                                                
105800*01  -COPY W0008  -PRE ARTC2-                                             
105900     05  FILLER                  PIC X.                                   
106000     EJECT                                                                
106100*01  -COPY W0008  -PRE ERSB-                                              
106200     05  FILLER                  PIC X.                                   
106300     EJECT                                                                
106400*01  -COPY W0008  -PRE ORDT-                                              
106500     05  FILLER                  PIC X.                                   
106600     EJECT                                                                
106700*01  -COPY W0008  -PRE ORDP-                                              
106800     05  FILLER                  PIC X.                                   
106900     EJECT                                                                
107000*01  -COPY W0008  -PRE LEVA-                                              
107100     05  FILLER                  PIC X.                                   
107200     EJECT                                                                
107300*01  -COPY W0008  -PRE WDE4C-                                             
107400     05  FILLER                  PIC X.                                   
107500     EJECT                                                                
107600*01  -COPY W0008  -PRE WDE6-                                              
107700     05  FILLER                  PIC X.                                   
107800     EJECT                                                                
107900*01  -COPY W0008  -PRE WDQ4B-                                             
108000     05  FILLER                  PIC X.                                   
108100     EJECT                                                                
108200*01  -COPY W0008  -PRE WDQ2-                                              
108300     05  FILLER                  PIC X.                                   
108400     EJECT                                                                
108500*01  -COPY W0008  -PRE WDB6-                                              
108600     05  FILLER                  PIC X.                                   
108700*01  -COPY W0008  -PRE WDK73-                                             
108800     05  FILLER                  PIC X.                                   
108900*01  -COPY W0008  -PRE WDK74-                                             
109000     05  FILLER                  PIC X.                                   
109100*01  -COPY W0008  -PRE WDK6-                                              
109200     05  FILLER                  PIC X.                                   
109300     EJECT                                                                
109400 PROCEDURE DIVISION  USING                                                
109500                            WDK7-PCB ARTC-PCB OIGA-PCB ARTM-PCB           
109600                            ORDL-PCB ERSA-PCB INLB-PCB WDK7-2-PCB         
109700                            WDK72-PCB INLC-PCB ARTC2-PCB ERSB-PCB         
109800                            ORDT-PCB ORDP-PCB  LEVA-PCB                   
109900                            WDE4C-PCB WDE6-PCB                            
110000                            WDQ4B-PCB WDQ2-PCB WDB6-PCB                   
110100                            WDK73-PCB WDK74-PCB WDK6-PCB.                 
110200     ENTRY 'DLITCBL' USING                                                
110300                                                                          
110400                            WDK7-PCB ARTC-PCB OIGA-PCB ARTM-PCB           
110500                            ORDL-PCB ERSA-PCB INLB-PCB WDK7-2-PCB         
110600                            WDK72-PCB INLC-PCB ARTC2-PCB ERSB-PCB         
110700                            ORDT-PCB ORDP-PCB  LEVA-PCB                   
110800                            WDE4C-PCB WDE6-PCB                            
110900                            WDQ4B-PCB WDQ2-PCB WDB6-PCB                   
111000                            WDK73-PCB WDK74-PCB WDK6-PCB.                 
111100                                                                          
111200                                                                          
111300     PERFORM A-INIT                                                       
111400     PERFORM IMS-GET-WDK7-SB                                              
111500     PERFORM UNTIL SEGMENT-SLUT                                           
111600        EVALUATE WDK7-SEG-NAME-FB                                         
111700           WHEN 'WDK701  '                                                
111800              MOVE SB-SART-IDARTNR TO WS-SPAR-IDARTNR                     
111900                                      WS-SPAR-IDARTNR-NUM                 
112000              MOVE ZERO TO WS-CD-TOT-KVBEART                              
112100                           WS-BOKAD-TOT-KVBEART                           
112200                           WS-SUM-XDC-BEHOV                               
112300           WHEN 'WDK711  '                                                
112400                                                                          
112500             MOVE JA           TO REFWAY-SW                               
112600             MOVE SB-SLAG-IDDC              TO WS-IDDC                    
112700                                               WS-IDDC-LAND               
112800             MOVE SB-SLAG-IDDC-REF          TO REF-WS-IDDC                
112900                                               WS-IDDC-LAND-SEND          
113000             IF  (SB-SLAG-IDDC NOT = W-IDDC-B6)                           
113100                MOVE SB-SLAG-IDDC       TO W-IDDC-B6                      
113200                PERFORM IMS-GU-WDB601                                     
113300                IF ((DCS-FLLPO = JA) AND                                  
113400                    (SB-SLAG-IDDC-REF = SPACE))                           
113500                  MOVE '11'     TO W-IDDC-B616                            
113600                ELSE                                                      
113700                  MOVE SB-SLAG-IDDC-REF TO W-IDDC-B616                    
113800                END-IF                                                    
113900                PERFORM IMS-GU-WDB616                                     
114000                IF SEGMENT-FINNS                                          
114100                  MOVE B6-REF-KVDLTID-AIRETA TO WS-LTID-A                 
114200                  MOVE B6-REF-KVDLTID-TOT     TO WS-LTID-B                
114300                  PERFORM S80-CURR-WEEK-PLUS-LT                           
114400**VALID REFILLWAY(SETUP EXISTS BETWEEN SENDING AND RECIVING DC)           
114500                ELSE                                                      
114600                  MOVE NEJ     TO REFWAY-SW                               
114700                END-IF                                                    
114800             END-IF                                                       
114900                                                                          
115000             IF DCS-FLSTOREF = JA                                         
115100             OR (NDC-CN                                                   
115200             AND SB-SLAG-IDDC-REF = SPACES)                               
115300             OR (NDC-US                                                   
115400             AND SB-SLAG-IDDC-REF = SPACES)                               
115500             OR REFWAY-NOT-SETUP                                          
115600               CONTINUE                                                   
115700             ELSE                                                         
115800               IF SB-SLAG-IDDC-REF = SPACES                               
115900**               FOR LOCAL PURCHASED PARTS                                
116000                 PERFORM S50B-L-STOCK-KOLL-LOCAL                          
116100               END-IF                                                     
116200                                                                          
116300               PERFORM B-NOLLSTALL                                        
116400               PERFORM S75-SEND-DCLAND                                    
116500               MOVE SB-SLAG-IDDC           TO WS-IDDC                     
116600****************W271V2  DC-PARAMETER = 99***********************          
116700****************W271D2  DC-PARAMETER = 02(RUNS AROUND 02.00)****          
116800****************W271DB  DC-PARAMETER = 05(RUNS AROUND 05.00)****          
116900***PARAMETER(TIREFBAT  IS SET ON 4408 SCREEN TO TELL WHAT BATCH***        
117000               IF (DC-PARAMETER-TIME = '99')                              
117100               OR (DC-PARAMETER-TIME = B6-REF-TIREFBAT)                   
117200*              OR ((DCS-FLLPO = JA) AND                                   
117300*                  (SB-SLAG-IDDC-REF = SPACE))                            
117400                  MOVE SB-SLAG-IDDC TO WS-IDDC                            
117500                                       W-IDDC-LOCAL                       
117600                  IF NDC                                                  
117700                  AND (SB-SLAG-IDDC-REF NOT = '11' AND                    
117800                       SB-SLAG-IDDC-REF NOT = SPACE )                     
117900                    MOVE SB-SLAG-IDDC-REF TO W-IDDC-INT                   
118000                    MOVE WS-SPAR-IDARTNR TO W-IDARTNR-INT                 
118100                    PERFORM IMS-GU-WDK711-INTERN                          
118200                    IF SEGMENT-FINNS                                      
118300                      PERFORM D-CHECK-STOP-DATE                           
118400                      IF INTERN-REFILL-OK                                 
118500                         PERFORM C-BEHANDLA-ARTIKEL                       
118600                      END-IF                                              
118700                    END-IF                                                
118800                  ELSE                                                    
118900                    PERFORM C-BEHANDLA-ARTIKEL                            
119000                  END-IF                                                  
119100               END-IF                                                     
119200             END-IF                                                       
119300        END-EVALUATE                                                      
119400        PERFORM IMS-GET-WDK7-SB                                           
119500     END-PERFORM                                                          
119600                                                                          
119700     PERFORM Z-FINIT                                                      
119800                                                                          
119900     MOVE ZERO TO RETURN-CODE                                             
120000     GOBACK                                                               
120100     .                                                                    
120200     EJECT                                                                
120300                                                                          
120400                                                                          
120500 A-INIT SECTION.                                                          
120600     SKIP2                                                                
120700                                                                          
120800                                                                          
120900***  DATA TILL DATUMFÄLT                                                  
121000                                                                          
121100     OPEN INPUT  W271DC                                                   
121200     OPEN OUTPUT W27110                                                   
121300                 W27136                                                   
121400                 W271F1                                                   
121500                                                                          
121600     MOVE ZERO TO WDK6-ART-IDARTNR                                        
121700     PERFORM S21-LAES-W271DC                                              
121800     ACCEPT DAGENS-DATUM FROM DATE                                        
121900                                                                          
122000*    HÄMTA INNEVARANDE VECKA                                              
122100*                      ÅR-VECKA-DAG                                       
122200*                      R-PERIOD                                           
122300*                      ANTAL VECKOR I PERIODEN                            
122400*                                                                         
122500*                                                                         
122600     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
122700     CALL WDATKONV USING DAT-KDDATFORM                                    
122800                         DAT-I-TIDATUM                                    
122900                         DAT-O-TIDATUM                                    
123000                         DAT-KDSVAR                                       
123100                                                                          
123200     IF DAT-KDSVAR-OK                                                     
123300        MOVE DAT-TIVV    TO DAGENS-VECKA                                  
123400        MOVE DAT-TIAAVVD TO DAGENS-TIAAVVD                                
123500        MOVE DAT-TIAARP  TO WS-INNEV-TIAARP                               
123600        MOVE DAT-KVVIPER TO WS-KVVIPER                                    
123700***********FOR TEST**************                                         
123800*       MOVE 7           TO WS-DAG-I-VECKA                                
123900*********************************                                         
124000        MOVE DAT-TID     TO WS-DAG-I-VECKA                                
124100                            TVA-V-FRAMAT-TID                              
124200        DISPLAY 'WS-DAG-I-VECKA : ' WS-DAG-I-VECKA                        
124300     ELSE                                                                 
124400        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W27110' TO          
124500                                    FELTEXT-STR                           
124600        DISPLAY FELTEXT                                                   
124700        PERFORM S99-ABEND                                                 
124800     END-IF                                                               
124900                                                                          
125000*RÄKNA FRAM NÄSTA PERIOD                                                  
125100                                                                          
125200     MOVE WS-INNEV-TIAARP TO WS-NASTA-TIAARP                              
125300     IF WS-INNEV-TIAARP = 9912                                            
125400        MOVE 0001  TO WS-NASTA-TIAARP                                     
125500     ELSE                                                                 
125600        IF WS-INNEV-TIRP = 12                                             
125700           MOVE 01 TO WS-NASTA-TIRP                                       
125800           ADD 1 TO WS-NASTA-TIAA                                         
125900        ELSE                                                              
126000           ADD 1 TO WS-NASTA-TIRP                                         
126100        END-IF                                                            
126200     END-IF                                                               
126300                                                                          
126400*HÄMTA FÖRSTA VECKAN I INNEVARANDE R-PERIOD                               
126500                                                                          
126600     MOVE 'AARP'           TO DAT-KDDATFORM                               
126700     MOVE WS-INNEV-TIAARP  TO DAT-I-TIDATUM                               
126800     CALL WDATKONV USING   DAT-KDDATFORM                                  
126900                           DAT-I-TIDATUM                                  
127000                           DAT-O-TIDATUM                                  
127100                           DAT-KDSVAR                                     
127200                                                                          
127300     IF DAT-KDSVAR-OK                                                     
127400        MOVE DAT-TIVV    TO WS-FIRST-VV-RPER                              
127500     ELSE                                                                 
127600        MOVE 'FEL FRÅN WDATKONV 2  I A-INIT SECTION I W27110' TO          
127700                                    FELTEXT-STR                           
127800        DISPLAY FELTEXT                                                   
127900        PERFORM S99-ABEND                                                 
128000     END-IF                                                               
128100                                                                          
128200*LETA UPP INNEVARANDE VECKAS RELATIVA PLACERING I INNEVARANDE             
128300*PERIOD                                                                   
128400                                                                          
128500     IF WS-INNEV-TIRP = 1 AND WS-FIRST-VV-RPER = 52                       
128600        MOVE 1 TO WS-FIRST-VV-RPER                                        
128700        COMPUTE WS-KVVIPER = WS-KVVIPER - 1                               
128800     END-IF                                                               
128900                                                                          
129000     MOVE DAGENS-VECKA TO WS-INNEV-VECKA                                  
129100     DIVIDE 2 INTO DAGENS-VECKA  GIVING WS-WEEKBY2-QUOTIENT               
129200                              REMAINDER WS-WEEKBY2-REMAINDER              
129300     IF DAGENS-VECKA = 1 OR WS-WEEKBY2-REMAINDER > 0                      
129400       SET WEEK-ODD        TO TRUE                                        
129500     ELSE                                                                 
129600       SET WEEK-EVEN       TO TRUE                                        
129700     END-IF                                                               
129800     MOVE WS-FIRST-VV-RPER TO WS-VECKA                                    
129900     MOVE +1 TO VECKO-IX                                                  
130000     PERFORM UNTIL VECKO-IX > WS-KVVIPER                                  
130100        IF WS-VECKA = WS-INNEV-VECKA                                      
130200           MOVE VECKO-IX TO PERIOD-VECKA                                  
130300        END-IF                                                            
130400        ADD +1 TO VECKO-IX                                                
130500        ADD +1 TO WS-VECKA                                                
130600     END-PERFORM                                                          
130700                                                                          
130800     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-DATUM                    
130900                                                                          
131000*                                                                         
131100********* HÄMTA ALLA GRUPPERADE DC'N                                      
131200********* GRUPPERING SKER PÅ 2365                                         
131300*                                                                         
131400     PERFORM DB2-OPEN-TP5IDDC-CRS                                         
131500     IF LINES-FOUND                                                       
131600       PERFORM DB2-FETCH-TP5IDDC-CRS                                      
131700     END-IF                                                               
131800                                                                          
131900     MOVE 1                  TO IDLOPNR-IX                                
132000     PERFORM UNTIL SQLCODE > ZERO                                         
132100     OR IDLOPNR-IX > IDLOPNR-MAX-GRP                                      
132200       MOVE TP5IDDC-IDLOPNR-DC                                            
132300                             TO WS-IDLOPNR-DC                             
132400                                WS-IDLOPNR (IDLOPNR-IX)                   
132500                                                                          
132600       MOVE ZERO             TO DC-IX                                     
132700       PERFORM UNTIL SQLCODE > ZERO                                       
132800       OR TP5IDDC-IDLOPNR-DC NOT = WS-IDLOPNR-DC                          
132900         ADD 1               TO DC-IX                                     
133000                                                                          
133100         IF DC-IX > DC-MAX-GRP                                            
133200           MOVE 'DC-IX > DC-MAX-GRP        ' TO FELTEXT-STR               
133300           DISPLAY FELTEXT                                                
133400           PERFORM S99-ABEND                                              
133500         END-IF                                                           
133600         MOVE TP5IDDC-IDDC   TO WS-DC-NR (IDLOPNR-IX, DC-IX)              
133700         PERFORM DB2-FETCH-TP5IDDC-CRS                                    
133800       END-PERFORM                                                        
133900                                                                          
134000       ADD 1                 TO IDLOPNR-IX                                
134100     END-PERFORM                                                          
134200                                                                          
134300     IF IDLOPNR-IX > IDLOPNR-MAX-GRP                                      
134400       MOVE 'IDLOPNR-IX > IDLOPNR-MAX-GRP' TO FELTEXT-STR                 
134500       DISPLAY FELTEXT                                                    
134600       PERFORM S99-ABEND                                                  
134700     END-IF                                                               
134800     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
134900*                                                                         
135000********* HÄMTA ALLA TRANSFERVÄGAR                                        
135100********* SE 2349                                                         
135200*                                                                         
135300     PERFORM DB2-OPEN-TP4TRAN-CRS                                         
135400     IF LINES-FOUND                                                       
135500       PERFORM DB2-FETCH-TP4TRAN-CRS                                      
135600     END-IF                                                               
135700                                                                          
135800     MOVE 1                  TO GRP-IX                                    
135900     PERFORM UNTIL SQLCODE > ZERO                                         
136000     OR GRP-IX > DC-TRANSGRP-MAX                                          
136100       MOVE ZERO             TO DC-IX                                     
136200       MOVE TP4TRAN-IDDC-REC TO WS-IDDC-REC (GRP-IX)                      
136300                                                                          
136400       PERFORM UNTIL SQLCODE > ZERO                                       
136500       OR DC-IX > DC-TRANS-MAX                                            
136600       OR TP4TRAN-IDDC-REC NOT = WS-IDDC-REC (GRP-IX)                     
136700                                                                          
136800         ADD 1               TO DC-IX                                     
136900         IF DC-IX > DC-TRANS-MAX                                          
137000           MOVE 'DC-IX > DC-TRANS-MAX      ' TO FELTEXT-STR               
137100           DISPLAY FELTEXT                                                
137200           PERFORM S99-ABEND                                              
137300         END-IF                                                           
137400         MOVE TP4TRAN-IDDC-SEND                                           
137500                             TO WS-IDDC-SEND (GRP-IX, DC-IX)              
137600                                                                          
137700         PERFORM DB2-FETCH-TP4TRAN-CRS                                    
137800       END-PERFORM                                                        
137900                                                                          
138000       ADD 1                 TO GRP-IX                                    
138100     END-PERFORM                                                          
138200                                                                          
138300     IF GRP-IX > DC-TRANSGRP-MAX                                          
138400       MOVE 'GRP-IX > DC-TRANSGRP-MAX' TO FELTEXT-STR                     
138500       DISPLAY FELTEXT                                                    
138600       PERFORM S99-ABEND                                                  
138700     END-IF                                                               
138800     PERFORM DB2-CLOSE-TP4TRAN-CRS                                        
138900     .                                                                    
139000     EJECT                                                                
139100                                                                          
139200 B-NOLLSTALL SECTION.                                                     
139300                                                                          
139400     MOVE ZERO       TO WS-FORS-ANDR                                      
139500                        WS-IDDISTR                                        
139600                        WS-KVDISP-DC                                      
139700                        WS-KVDISP-SEND-DC                                 
139800                        WS-KVLS-BER-DC                                    
139900                        WS-KVOI-INNEV                                     
140000                        WS-KVPB-DAG-DC                                    
140100                        WS-KVPB-DAG-LOCAL                                 
140200                        WS-KVPB-DAG-DC-OKAD                               
140300                        WS-KVPB-DAG-DC-NORM                               
140400                        WS-KVPB-LT-BEHOV-DC                               
140500                        WS-KVPB-VECKA-CDC                                 
140600                        WS-KVPB-VECKA-DC                                  
140700                        WS-KVPB-ANTV-DC                                   
140800                        WS-KVPB-DAG-INNEV-RP                              
140900                        WS-KVPB-DAG-NASTA-RP                              
141000                        WS-KVPB-INNEV-RP                                  
141100                        WS-KVPB-NASTA-RP                                  
141200                        WS-KVPB-FLYG                                      
141300                        WS-KVPB-LOCAL                                     
141400                        WS-KVREFPKT-HALV                                  
141500                        WS-KVREFPKT-DC                                    
141600                        WS-KVREFPKT-DUBBEL                                
141700                        WS-LT-BEHOV-DC-OKAD                               
141800                        WS-LT-BEHOV-DC-NORM                               
141900                        WS-LT-BEHOV-DC-DIFF                               
142000                        WS-SUANTAL-ETA                                    
142100                        WS-SUPERWEEK                                      
142200                        WS-FLYGT-MAX-TIAAMMDD                             
142300                        WS-FLYGBEHOV-MAX-TIAARP                           
142400                        WS-FIRST-TIBERANK                                 
142500                        WS-KVDISP-FLYG                                    
142600                        WS-KVDISP-LOCAL                                   
142700                        WS-BALANCE-ERSATT                                 
142800                        WS-BALANCE-DC                                     
142900                        WS-KVROS-TOT-LOCAL                                
143000                        WS-KVDAGAR-FLYG-INNEV                             
143100                        WS-KVDAGAR-FLYG-NASTA                             
143200                        WS-TILLG-CDC                                      
143300                        WS-TILLG-KVAR-CDC                                 
143400                        WS-KVREFOVL-TEST                                  
143500                        WS-KVPB-VECKA-DC-IX                               
143600                        WS-NASTA-PER-TIAAMMDD                             
143700                        WS-LOCAL-LEADTIME                                 
143800                        WS-LTID-A                                         
143900                        WS-LTID-B                                         
144000                                                                          
144100                        WS-ANTAL-GODK-E3                                  
144200                        WS-ANTAL-TRANSF-E3                                
144300                        WS-ANTAL-RO-RESS-KL1-CDC                          
144400                        WS-KVROS-DC-TOT                                   
144500                        WS-ANTAL-LEDT-FLYG                                
144600                        WS-ANTAL-FLYG                                     
144700                        WS-ANTAL-BER                                      
144800                        WS-ANTAL-Q1                                       
144900                        WS-ANTAL-Q3                                       
145000                        WS-ANTAL-QX                                       
145100                        WS-ANTAL-SLUT                                     
145200                                                                          
145300                        WS-KVANT-BER                                      
145400                        WS-KVANT-Q1                                       
145500                        WS-KVANT-Q3                                       
145600                        WS-KVANT-QX                                       
145700                        WS-KVANT-SLUT                                     
145800                        WS-KVQPACK-3                                      
145900                                                                          
146000                        WS-KVPBREOI-DAY                                   
146100                        WS-KVPB-REF-DAY-PER-I                             
146200                        WS-KVPB-REF-DAY-PER-II                            
146300                        WS-KVPB-REF-DAY-PER-III                           
146400                        WS-KVPB-REF-DAY-PER-IV                            
146500                        WS-KVPB-REF-DAY-PER-V                             
146600                        WS-KVPB-REF-DAY-PER-VI                            
146700                        WS-KVPB-ANTV-DC-NUM                               
146800                                                                          
146900                        WS-KDERS                                          
147000                        WS-TIERSDAT-VIPS                                  
147100                        WS-KVPB-TOT                                       
147200                                                                          
147300                        IX                                                
147400                                                                          
147500     MOVE NEJ        TO KONTROLL-SW                                       
147600                        BEORDRA-SW                                        
147700                        BRIST-I-CDC-SW                                    
147800                        BRIST-I-DC-SW                                     
147900                        FLYGFORSLAG-SW                                    
148000                        FORSLAG-SW                                        
148100                        TRANSFER-SW                                       
148200                        ERSATT-SW                                         
148300                        KVAL-SPARR-CDC-SW                                 
148400                        NO-PRICE-LOCAL-SW                                 
148500                        L-STOCK-LOCAL-SW                                  
148600                        TILLKOMMANDE-SW                                   
148700                        OKAD-FORSALJNING-SW                               
148800                        ARTIKEL-UTGANGEN-K6-SW                            
148900                        ARTIKEL-FINNS-L7-SW                               
149000                        ERS-WDK611-SW                                     
149100                        INTERN-REFILL-SW                                  
149200                                                                          
149300     INITIALIZE         W271LTPB-W271LTPB                                 
149400                        W27110-AREA                                       
149500                        W27136-AREA                                       
149600                        W271F1-AREA                                       
149700                                                                          
149800     .                                                                    
149900     EJECT                                                                
150000                                                                          
150100                                                                          
150200 C-BEHANDLA-ARTIKEL SECTION.                                              
150300                                                                          
150400     PERFORM CA1-SAMLA-DATA-K6                                            
150500     IF ARTIKEL-UTGANGEN-K6                                               
150600        CONTINUE                                                          
150700     ELSE                                                                 
150800        COMPUTE WS-KVPB-TOT = SB-SLAG-KVPB-REF +                          
150900                              SB-SLAG-KVPBREOI                            
151000        IF NDC-CN OR NDC-NA                                               
151100          MOVE CLAG-KDERS        TO WS-KDERS                              
151200          IF NDC-US                                                       
151300            MOVE 'US'            TO W-IDLAND                              
151400          END-IF                                                          
151500          IF NDC-CA                                                       
151600            MOVE 'CA'            TO W-IDLAND                              
151700          END-IF                                                          
151800          IF NDC-CN                                                       
151900            MOVE 'CN'            TO W-IDLAND                              
152000          END-IF                                                          
152100          PERFORM IMS-GU-WDK712-LART                                      
152200          IF SEGMENT-FINNS                                                
152300            MOVE LART-PRMATRL    TO WS-PRIS                               
152400            MOVE LART-TIERSDAT-VIPS                                       
152500                                   TO WS-TIERSDAT-VIPS                    
152600          ELSE                                                            
152700            MOVE ZERO            TO WS-PRIS                               
152800                                    WS-TIERSDAT-VIPS                      
152900          END-IF                                                          
153000        ELSE                                                              
153100          MOVE CLAG-PRARTSTD     TO WS-PRIS                               
153200        END-IF                                                            
153300        IF REF-NDC                                                        
153400           MOVE WS-IDLAND-REF    TO W-IDLAND                              
153500           PERFORM IMS-GU-WDK712-LART                                     
153600           IF SEGMENT-FINNS                                               
153700              IF LART-KVQPACK-3   > ZERO                                  
153800                MOVE LART-KVQPACK-3                                       
153900                                 TO WS-KVQPACK-3                          
154000              ELSE                                                        
154100                MOVE CLAG-KVQPACK-3                                       
154200                                 TO WS-KVQPACK-3                          
154300              END-IF                                                      
154400           ELSE                                                           
154500             MOVE CLAG-KVQPACK-3                                          
154600                                 TO WS-KVQPACK-3                          
154700           END-IF                                                         
154800        ELSE                                                              
154900          MOVE CLAG-KVQPACK-3    TO WS-KVQPACK-3                          
155000        END-IF                                                            
155100                                                                          
156500        IF NOT CLAG-KDERS = 52                                            
156600        OR NOT CLAG-KDERS = 22                                            
156700        OR NOT CLAG-KDERS = 23                                            
156800        OR NOT CLAG-KDERS = 25                                            
156900        OR NOT CLAG-KDERS = 26                                            
157000        OR (NDC                                                           
157100        AND WS-TIERSDAT-VIPS   = 0                                        
157200        AND WS-KDERS(2:1)      = 2                                        
157300        AND REF-NDC)                                                      
157400          MOVE SB-SLAG-KVREFPKT TO WS-KVREFPKT-DC                         
157500          PERFORM S34B-BER-BALANCE-DC                                     
157600          IF (NDC AND REF-NDC)                                            
157700            PERFORM S34C-CHK-KVDISP-SEND-DC                               
157800          END-IF                                                          
157900          IF WS-BALANCE-DC < WS-KVREFPKT-DC                               
158000             PERFORM CA2-SAMLA-DATA-L7                                    
158100             PERFORM CB-VALJA-ARTIKLAR                                    
158200          END-IF                                                          
158300        END-IF                                                            
158310                                                                          
158311**250225 FUNCTION TO STOP WHEN MI = YES AND ALWAYS AIR = S                
158320        IF SB-SLAG-FLREFNYO = JA                                          
158330        AND SB-SLAG-FLFLYG = 'S'                                          
158350          MOVE NEJ TO KONTROLL-SW                                         
158360        END-IF                                                            
158400*                                                                         
158500        IF KONTROLL                                                       
158600           PERFORM CC-KONTROLERA-DC                                       
158700           IF BRIST-I-DC                                                  
158800              PERFORM CD-BERAKNA-PAFYLLNING                               
158900           END-IF                                                         
159000        END-IF                                                            
159100*                                                                         
159200        IF BEORDRA                                                        
159300           IF WS-ANTAL-SLUT > ZERO                                        
159400           OR (ERSATT AND (CLAG-KDERS = 14 OR 18 OR 24))                  
159500           OR (NDC AND L-STOCK-LOCAL AND NOT ERSATT)                      
159600              PERFORM CE-SKAPA-ORDER                                      
159700**HÄR STOPPAR MAN AUTOMATGODKÄNDA ORDRAR MEDANS MAN I                     
159800**W2711200 STOPPAR MANUELLT GODKÄNDA ORDRAR                               
159900**W27110-KDREFTYP = 'O' ÄR AUTOMATGODKÄND , AUTOREFILL = J                
160000              IF W27110-KDREFTYP = 'O'                                    
160100                MOVE NEJ           TO RELEASEDAY-SW                       
160200                PERFORM CI-CHECK-RELEASEDAY                               
160300              END-IF                                                      
160400**DELAY CROSS DOCKING                                                     
160500              IF  W27110-KDREFTYP NOT = 'O'                               
160600              OR (W27110-KDREFTYP = 'O' AND                               
160700                  RELEASEDAY)                                             
160800                  MOVE JA TO SKRIV-ORDER-SW                               
160900                  IF (SB-SLAG-ADLAGOMR-CD > 0                             
161000                      AND W27110-KDREFTYP = 'O')                          
161100                  OR (SB-SLAG-ADLAGOMR-CD > 0                             
161200                      AND W27110-KDREFTYP = 'B'                           
161300                      AND W27110-KDREFORS = 'P')                          
161400                    ADD W27110-KVBEART TO WS-CD-TOT-KVBEART               
161500                    IF SB-SLAG-TIDATUM-CROSS > 0                          
161600                      ADD W27110-KVBEART TO WS-BOKAD-TOT-KVBEART          
161700                    END-IF                                                
161800                  END-IF                                                  
161900                  IF SB-SLAG-TIDATUM-CROSS = 0                            
162000                    IF (SB-SLAG-ADLAGOMR-CD > 0                           
162100                        AND W27110-KDREFTYP = 'O')                        
162200                    OR (SB-SLAG-ADLAGOMR-CD > 0                           
162300                        AND W27110-KDREFTYP = 'B'                         
162400                        AND W27110-KDREFORS = 'P')                        
162500                      IF CLAG-KVROS = 0                                   
162600                        PERFORM CH-KOLLA-CROSS-DOCKING                    
162700                      END-IF                                              
162800                    END-IF                                                
162900                  ELSE                                                    
163000                    IF (DAGENS-DATUM > SB-SLAG-TIDATUM-CROSS)             
163100                    AND (SB-SLAG-TIDATUM-CROSS > 0)                       
163200                      MOVE 'O' TO W27110-KDREFTYP                         
163300                      MOVE SPACE TO W27110-KDREFORS                       
163400                      MOVE 43    TO W27110-KDFRAKT                        
163500                    ELSE                                                  
163600                      MOVE NEJ TO SKRIV-ORDER-SW                          
163700                    END-IF                                                
163800                  END-IF                                                  
163900                                                                          
164000                  IF SKRIV-ORDER                                          
164100                    PERFORM S11-SKRIV-W27110                              
164200                    PERFORM S70-SUMMERA-ORDRAR                            
164300                  END-IF                                                  
164400              END-IF                                                      
164500           END-IF                                                         
164600           MOVE CLAG-TIREFSTO TO TMP1-YYMMDD                              
164700           MOVE DAGENS-DATUM    TO TMP2-YYMMDD                            
164800           MOVE SB-SLAG-TIREFSTO TO TMP3-YYMMDD                           
164900           PERFORM WY2000Q1                                               
165000           IF ((TMP1-YYMMDD    > +0)        AND                           
165100               (TMP1-YYMMDD < TMP2-YYMMDD))                               
165200           OR                                                             
165300              ((SB-SLAG-TIREFSTO    > +0)   AND                           
165400               (TMP3-YYMMDD < TMP2-YYMMDD))                               
165500           OR                                                             
165600              (SB-SLAG-FLPB-FLYTT = 'N') AND                              
165700              (ERSATT                     )AND                            
165800              (CLAG-KDERS = 11 OR 17 OR 21 OR 27)AND                      
165900              (SB-SLAG-KDREFSTA = 'A'     )                               
166000                                                                          
166100               PERFORM CF-ARTIKELADMINISTRATION                           
166200               PERFORM S12-SKRIV-W27136                                   
166300           END-IF                                                         
166400        ELSE                                                              
166500          IF SB-SLAG-TIDATUM-CROSS > ZERO                                 
166600            MOVE ZERO         TO W271F1-TIDATUM-CROSS                     
166700            MOVE SB-SLAG-IDDC TO W271F1-IDDC                              
166800            MOVE WS-SPAR-IDARTNR TO W271F1-IDARTNR                        
166900            PERFORM S14-SKRIV-W271F1                                      
167000          END-IF                                                          
167100        END-IF                                                            
167200     END-IF                                                               
167300     .                                                                    
167400     EJECT                                                                
167500                                                                          
167600 CA1-SAMLA-DATA-K6 SECTION.                                               
167700                                                                          
167800     IF WS-SPAR-IDARTNR = WDK6-ART-IDARTNR                                
167900        CONTINUE                                                          
168000     ELSE                                                                 
168100        MOVE NEJ TO KDFARLIG-SW                                           
168200        MOVE WS-SPAR-IDARTNR TO W-IDARTNR                                 
168300        MOVE '1'             TO W-KDSEGKEY                                
168400        PERFORM IMS-GU-ART                                                
168500        IF SEGMENT-FINNS                                                  
168600           IF WDK6-ART-KDERS-UTG = 0                                      
168700              PERFORM IMS-GN-CLAG                                         
168800              IF SEGMENT-SAKNAS                                           
168900***ANVÄNDER FLAGGAN FÖR ATT STOPPA OM WDK611 SEGMENT SAKNAS               
169000                MOVE JA TO ARTIKEL-UTGANGEN-K6-SW                         
169100              ELSE                                                        
169200                IF CLAG-KDFARLIG = 4                                      
169300                  MOVE JA TO KDFARLIG-SW                                  
169400                END-IF                                                    
169500              END-IF                                                      
169600           ELSE                                                           
169700              MOVE JA TO ARTIKEL-UTGANGEN-K6-SW                           
169800           END-IF                                                         
169900        ELSE                                                              
170000           DISPLAY 'IDARTNR = ' WS-SPAR-IDARTNR                           
170100           MOVE 'ARTIKEL SAKNAS PÅ WDK601  ' TO FELTEXT-STR               
170200           DISPLAY FELTEXT                                                
170300           PERFORM S99-ABEND                                              
170400        END-IF                                                            
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800                                                                          
170900                                                                          
171000 CA2-SAMLA-DATA-L7 SECTION.                                               
171100                                                                          
171200     MOVE SB-SLAG-IDDC    TO W-IDDC                                       
171300     PERFORM IMS-GU-OIGA                                                  
171400     IF SEGMENT-FINNS                                                     
171500        MOVE JA TO ARTIKEL-FINNS-L7-SW                                    
171600     END-IF                                                               
171700                                                                          
171800     .                                                                    
171900     EJECT                                                                
172000                                                                          
172100                                                                          
172200                                                                          
172300 CB-VALJA-ARTIKLAR SECTION.                                               
172400                                                                          
172500                                                                          
172600***  VÄLJER UT DE ARTTIKLAR DÄR REFILLBEHOV SKALL KONTROLERAS             
172700***  SÅDANA ARTIKLAR SKALL VARA AKTIVA - REFILLSTATUS = A                 
172800***  DE FÅR INTE VARA REFILLSTOPPPADE ELLER HA REFILLSTOP-                
172900***  DATUM FRAMÅT I TIDEN, ARTIKLARNA FÅR INTE VÄNTA                      
173000***  PÅ NY ORDERINGÅNG.                                                   
173100***                                                                       
173200                                                                          
173300     MOVE WS-SPAR-IDARTNR TO BYT03-IDARTNR                                
173400     IF BYT03-OBJEKT OR                                                   
173500        WDK6-ART-KDSORT = 'SW' OR                                         
173600        CLAG-FLLSRDEL = NEJ                                               
173700        MOVE NEJ TO SW-REFILL-KANSKE                                      
173800     ELSE                                                                 
173900        MOVE JA  TO SW-REFILL-KANSKE                                      
174000     END-IF                                                               
174100                                                                          
174200     MOVE WDK6-ART-KDPRODSL      TO TEST-KDPRODSL                         
174300* L7 TO K7 CHANGES                                                        
174400     IF (DCS-SDC                                                          
174500     AND SB-SLAG-FLREFILL = JA)                                           
174600*    HAR ERSATTS AV      OR (DCS-FLEXCP1-REFBER = JA                      
174700     OR (DCS-FLEXCP1-REFBER = JA                                          
174800     AND KDPRODSL-BIMA                                                    
174900     AND ART-SKALL-KANSKE-REFILLAS)                                       
175000     OR NDC                                                               
175100*******ÄNDRAT 060331 ENLIGT SCR 856514**********************              
175200*    ÄVEN ARTIKLAR MED REFILL = N SKALL IGENOM FÖR NDC     *              
175300*    SOM HAR RESTORDER SALDO                               *              
175400************************************************************              
175500* L7 TO K7 CHANGES                                                        
175600       IF (SB-SLAG-FLREFILL = JA                                          
175700       AND WS-PRIS > ZERO)                                                
175800       OR (NDC AND (SB-SLAG-KVROS-BULK > ZERO                             
175900               OR   SB-SLAG-KVROS-DAG    > ZERO))                         
176000       AND WS-PRIS > ZERO                                                 
176100***SCR4550887 MAN VILL BARA ATT WDK6 TIREFSTO SKALL GÄLLA FÖR             
176200***NDC-NA RESTERANDE FÅR MAN STOPPA MED WDK7 TIREFSTO.                    
176300         IF NDC-NA                                                        
176400           MOVE CLAG-TIREFSTO    TO TMP1-YYMMDD                           
176500         ELSE                                                             
176600           MOVE ZERO             TO TMP1-YYMMDD                           
176700         END-IF                                                           
176800         MOVE DAGENS-DATUM       TO TMP2-YYMMDD                           
176900         PERFORM WY2000P1                                                 
177000         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
177100           MOVE SB-SLAG-TIREFSTO        TO TMP1-YYMMDD                    
177200           MOVE DAGENS-DATUM            TO TMP2-YYMMDD                    
177300           PERFORM WY2000P1                                               
177400           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
177500*********                                                                 
177600* L7 TO K7 CHANGES FROM DC-FLREFNYO TO SB-SLAG-FLREFNYO                   
177700                                                                          
177800             IF SB-SLAG-FLREFNYO  = NEJ                                   
177900             OR (NDC AND (SB-SLAG-KVROS-DAG  > ZERO))                     
178100               MOVE WDK6-ART-TIFINLV          TO TMP1-YYWWD               
178200               MOVE DCS-IDLANDX2    TO W-IDLAND                           
178300               PERFORM IMS-GU-WDK712-LART                                 
178400               IF SEGMENT-FINNS                                           
178500                 MOVE LART-DAPUBL TO WS-DAPUBL                            
178600                 IF DCS-NDC                                               
178700                   IF LART-DAPUBL > 0                                     
178800                     MOVE 'AAMMDD'  TO DAT-KDDATFORM                      
178900                     MOVE LART-DAPUBL TO DAT-I-TIDATUM                    
179000                     CALL WDATKONV USING DAT-KDDATFORM                    
179100                                           DAT-I-TIDATUM                  
179200                                           DAT-O-TIDATUM                  
179300                                           DAT-KDSVAR                     
179400                                                                          
179500                     IF DAT-KDSVAR-OK                                     
179600                        MOVE DAT-TIAAVVD TO TMP1-YYWWD                    
179700                     ELSE                                                 
179800                        MOVE 'FEL WDATKONV CB-VALJA-ARTIKLAR' TO          
179900                                              FELTEXT-STR                 
180000                        DISPLAY FELTEXT                                   
180100                        PERFORM S99-ABEND                                 
180200                     END-IF                                               
180300                   END-IF                                                 
180400                 END-IF                                                   
180500               ELSE                                                       
180600                 MOVE ZERO          TO WS-DAPUBL                          
180700               END-IF                                                     
180800               MOVE TVA-V-FRAMAT-TIAAVVD      TO TMP2-YYWWD               
180900               PERFORM WY2000P2                                           
181000               IF TMP1-YYWWD <= TMP2-YYWWD                                
181100                                                                          
181200                                                                          
181300                 MOVE WS-DAPUBL(1:4)       TO WS-DAPUBL-AAR               
181400                 MOVE W-DAGENS-DATUM(1:4)  TO WS-DAGENS-AAR               
181500                 COMPUTE WS-DAGENS-AAR-MINUS1 =                           
181600                         WS-DAGENS-AAR - 1                                
181700                 IF  (WS-DAPUBL > ZERO)                                   
181800                 AND (WS-DAPUBL-AAR >= WS-DAGENS-AAR-MINUS1)              
181900                                                                          
182000                   MOVE B6-REF-KVDLTID-TOT                                
182100                             TO DAG-KVKALDAG                              
182200                   MOVE WS-DAPUBL (3:6)                                   
182300                             TO DAG-TIAAMMDD-TOM                          
182400                   MOVE 003  TO DAG-KDCALL                                
182500                   CALL WDAGKONV USING DAG-KDCALL                         
182600                                       DAG-DATUM-AREA                     
182700                                       DAG-KDSVAR                         
182800                   IF DAG-KDSVAR = SPACE                                  
182900                     CONTINUE                                             
183000                   ELSE                                                   
183100                     MOVE 'FEL FRÅN WDAGKONV, I CB-SECTION'               
183200                                 TO   FELTEXT-STR                         
183300                      DISPLAY FELTEXT                                     
183400                      PERFORM S99-ABEND                                   
183500                   END-IF                                                 
183600                                                                          
183700                   MOVE DAG-TIAAMMDD-FOM                                  
183800                             TO TMP1-YYMMDD                               
183900                   MOVE DAGENS-DATUM                                      
184000                             TO TMP2-YYMMDD                               
184100                 ELSE                                                     
184200                   MOVE ZERO                                              
184300                             TO TMP1-YYMMDD                               
184400                                TMP2-YYMMDD                               
184500                                WS-DAPUBL                                 
184600                 END-IF                                                   
184700                 PERFORM WY2000P1                                         
184800                                                                          
184900                 IF WS-DAPUBL = ZERO                                      
185000                 OR TMP1-YYMMDD < TMP2-YYMMDD                             
185100                                                                          
185200                   IF DCS-SDC                                             
185300                     IF CLAG-REDIRLEV = 1.0                               
185400                       IF SB-SLAG-IDDC = '21'                             
185500                         IF WDK6-ART-IDLEVNR = 'BP8BA'                    
185600                                            OR 'BP3EA'                    
185700                                            OR 'BP7YA'                    
185800                                            OR 'AE4PC'                    
185800                                            OR 'AE420'                    
185800                                            OR 'AEF31'                    
185900                                            OR 'AEL4V'                    
185910                                            OR 'AFGQF'                    
186000                           IF SB-SLAG-KDREFSTA = 'A'                      
186100                             IF CLAG-KDERS = +0                           
186200                                MOVE JA TO KONTROLL-SW                    
186300                             ELSE                                         
186400                                PERFORM CBA-ERSATTNINGAR                  
186500                             END-IF                                       
186600                           END-IF                                         
186700                         END-IF                                           
186800                       END-IF                                             
186900                     ELSE                                                 
187000                       IF SB-SLAG-KDREFSTA = 'A'                          
187100                         IF CLAG-KDERS = +0                               
187200                            MOVE JA TO KONTROLL-SW                        
187300                         ELSE                                             
187400                            PERFORM CBA-ERSATTNINGAR                      
187500                         END-IF                                           
187600                       END-IF                                             
187700                     END-IF                                               
187800                   END-IF                                                 
187900                   IF NDC                                                 
188000                       PERFORM CBA-ERSATTNINGAR                           
188100                       IF KONTROLL-SW = JA                                
188200                         CONTINUE                                         
188300                       ELSE                                               
188400                         IF (NDC                          AND             
188500                             WS-TIERSDAT-VIPS      = 0    AND             
188600                             WS-KDERS(2:1)         = 2    AND             
188700                             REF-NDC)                                     
188800                             MOVE JA TO KONTROLL-SW                       
188900                         END-IF                                           
189000                       END-IF                                             
189100                   END-IF                                                 
189200                 END-IF                                                   
189300               END-IF                                                     
189400             END-IF                                                       
189500           END-IF                                                         
189600         END-IF                                                           
189700       END-IF                                                             
189800     END-IF                                                               
189900     .                                                                    
190000     EJECT                                                                
190100                                                                          
190200                                                                          
190300                                                                          
190400 CBA-ERSATTNINGAR SECTION.                                                
190500                                                                          
190600     IF CLAG-KDERS = ZERO OR                                              
190700                 01 OR 02 OR 03 OR 04 OR                                  
190800                 05 OR 06 OR 07 OR 08 OR                                  
190900                 09 OR 11 OR 14 OR 17 OR                                  
191000                 18 OR 21 OR 24 OR 27                                     
191100       MOVE JA TO KONTROLL-SW                                             
191200     END-IF                                                               
191300     .                                                                    
191400     EJECT                                                                
191500                                                                          
191600 CC-KONTROLERA-DC SECTION.                                                
191700                                                                          
191800     IF DCS-SDC                                                           
191900********OKAD-FORSALJNING = BRANDLÄCKAREN                                  
192000        PERFORM S59-BER-BALANCE-ERSATT                                    
192100        PERFORM S31-KOLLA-OKAD-FORSALJNING                                
192200        IF OKAD-FORSALJNING                                               
192300           PERFORM S32-BER-TILLF-REFILLPUNKT                              
192400        ELSE                                                              
192500           MOVE SB-SLAG-KVREFPKT TO WS-KVREFPKT-DC                        
192600        END-IF                                                            
192700     END-IF                                                               
192800***                                                                       
192900     IF NDC                                                               
193000       IF (SB-SLAG-FLFLYG = NEJ                                           
193100       OR  SB-SLAG-FLFLYG = 'S'                                           
193200       OR  SB-SLAG-FLFLYG = SPACE)                                        
193300        PERFORM S31B-BERAKNA-VECKO-PB                                     
193400        MOVE SB-SLAG-KVREFPKT TO WS-KVREFPKT-DC                           
193500        PERFORM S59-BER-BALANCE-ERSATT                                    
193600        IF REF-CDC-SE                                                     
193700        OR (NDC AND REF-NDC)                                              
193800           PERFORM S51-L-STOCK-KOLL-CDC--NDC                              
193900                                                                          
194000           IF  (WS-KVREFPKT-DC = ZERO                                     
194100           OR  (NDC                                                       
194200           AND  WS-KVPB-TOT <= 0.5))                                      
194300           AND (WS-KVDISP-FLYG > ZERO                                     
194400           OR   WS-KVDISP-FLYG = ZERO)                                    
194500*                                                                         
194600*    OM ORDERPUNKT ÄR NOLL OCH RESTORDERN ÄR TÄCKT                        
194700*    ELLER LÅGFREKVENT ARTIKEL OCH RESTORDERN ÄR TÄCKT                    
194800*    SKA DET INTE BLI NÅGOT FÖRSLAG                                       
194900*                                                                         
195000                CONTINUE                                                  
195100           ELSE                                                           
195200              IF WS-KVDISP-FLYG < WS-KVPB-FLYG                            
195300***************LOW STOCK WARNING POTENTIALLY, **********                  
195400***************NEED TO CHECK PRODSTATUS ALSO  **********                  
195500                 PERFORM S51B-SUM-OF-PROD-STATUS-R-U-P                    
195600                 ADD WS-KVBEART-Q-RUP   TO WS-KVDISP-FLYG                 
195700                                                                          
195800                 IF  (WS-KVREFPKT-DC = ZERO                               
195900                 OR  (NDC                                                 
196000                 AND  WS-KVPB-TOT <= 0.5))                                
196100                 AND (WS-KVDISP-FLYG > ZERO                               
196200                 OR   WS-KVDISP-FLYG = ZERO)                              
196300*                                                                         
196400*           OM ORDERPUNKT ÄR NOLL OCH RESTORDERN ÄR TÄCKT                 
196500*           NÄR PRODSTATUSES R U OCH P INKLUDERAS                         
196600*           ELLER LÅGFREKVENT ARTIKEL OCH RESTORDERN ÄR TÄCKT             
196700*           NÄR PRODSTATUSES R U OCH P INKLUDERAS                         
196800*           SKA DET INTE BLI NÅGOT FÖRSLAG                                
196900*                                                                         
197000                      CONTINUE                                            
197100                 ELSE                                                     
197200                    IF WS-KVDISP-FLYG < WS-KVPB-FLYG                      
197300*********************LOW STOCK WARNING DEFINITELY ****                    
197400                      IF SB-SLAG-FLFLYG = 'S'                             
197500                         MOVE NEJ TO FLYGFORSLAG-SW                       
197501                        IF (SB-SLAG-KVROS-BULK > ZERO                     
197502                        OR SB-SLAG-KVROS-DAG > ZERO)                      
197510                           MOVE JA TO BRIST-I-DC-SW                       
197520                        END-IF                                            
197600                      ELSE                                                
197700                         MOVE JA TO FLYGFORSLAG-SW                        
197800                      END-IF                                              
197810                    END-IF                                                
197900                 END-IF                                                   
198000              END-IF                                                      
198100           END-IF                                                         
198200        ELSE                                                              
198300           PERFORM S50A-BER-BALANCE-LOCAL                                 
198400           IF SB-SLAG-KVPB-REF = ZERO                                     
198500              IF WS-KVROS-TOT-LOCAL > ZERO                                
198600                 MOVE JA TO BRIST-I-DC-SW                                 
198700                 MOVE JA TO L-STOCK-LOCAL-SW                              
198800              END-IF                                                      
198900           ELSE                                                           
199000*             PERFORM S50B-L-STOCK-KOLL-LOCAL                             
199100              IF WS-KVDISP-LOCAL < WS-KVPB-LOCAL                          
199200                 MOVE JA TO BRIST-I-DC-SW                                 
199300                 MOVE JA TO L-STOCK-LOCAL-SW                              
199400              END-IF                                                      
199500           END-IF                                                         
199600        END-IF                                                            
199700       ELSE                                                               
199800        PERFORM S31B-BERAKNA-VECKO-PB                                     
199900       END-IF                                                             
200000     END-IF                                                               
200100                                                                          
200200     IF FLYGFORSLAG                                                       
200300        PERFORM S52-L-STOCK-ANTAL-CDC                                     
200400        IF WS-ANTAL-BER > ZERO                                            
200500           MOVE JA TO BRIST-I-DC-SW                                       
200600        ELSE                                                              
200700           MOVE NEJ TO FLYGFORSLAG-SW                                     
200800        END-IF                                                            
200900     END-IF                                                               
201000     IF EJ-BRIST-I-DC                                                     
201100        IF SB-SLAG-KDREFSTA = 'A'                                         
201200        OR (SB-SLAG-KDREFSTA = 'P'                                        
201300        AND (SB-SLAG-KVROS-BULK > ZERO                                    
201400         OR  SB-SLAG-KVROS-DAG    > ZERO))                                
201500           IF WS-KVREFPKT-DC > +0                                         
201600           OR SB-SLAG-FLFLYG = JA                                         
201700*                                                                         
201800*          OR LDC                                                         
201900*          HAR ERSATTS AV   OR DCS-FLEXCP2-REFBER = JA                    
202000           OR DCS-FLEXCP2-REFBER = JA                                     
202100              PERFORM S50A-BER-BALANCE-LOCAL                              
202200              PERFORM S34-BER-DISPONIBELT-DC                              
202300              IF WS-KVDISP-DC < WS-KVREFPKT-DC                            
202400                MOVE JA TO BRIST-I-DC-SW                                  
202500              END-IF                                                      
202600           ELSE                                                           
202700              IF REF-CDC-SE                                               
202800              OR (NDC AND REF-NDC)                                        
202900                 CONTINUE                                                 
203000              ELSE                                                        
203100                 PERFORM S50A-BER-BALANCE-LOCAL                           
203200                 IF WS-KVDISP-LOCAL < 1                                   
203300                    IF WS-KVREFPKT-DC = ZERO                              
203400                       COMPUTE WS-KVROS-DC-TOT =                          
203500                               SB-SLAG-KVROS-DAG +                        
203600                               SB-SLAG-KVROS-BULK                         
203700                       IF WS-KVROS-DC-TOT > ZERO                          
203800                          MOVE JA TO BRIST-I-DC-SW                        
203900                       END-IF                                             
204000                    END-IF                                                
204100                 END-IF                                                   
204200              END-IF                                                      
204300           END-IF                                                         
204400        END-IF                                                            
204500     END-IF                                                               
204600     .                                                                    
204700     EJECT                                                                
204800                                                                          
204900                                                                          
205000 CD-BERAKNA-PAFYLLNING SECTION.                                           
205100                                                                          
205200     IF L-STOCK-LOCAL                                                     
205300        MOVE ZERO TO WS-ANTAL-SLUT                                        
205400        MOVE JA TO BEORDRA-SW                                             
205500        IF CLAG-KDERS  = 11 OR 14 OR 17 OR 18 OR 21 OR 24 OR 27           
205600            PERFORM S54-ERSATTNINGAR-NDC                                  
205700        ELSE                                                              
205800           PERFORM S58-KOLLA-PRIS-LOKALA                                  
205900        END-IF                                                            
206000     ELSE                                                                 
206100        IF FLYGFORSLAG                                                    
206200           IF CLAG-KDERS = 11 OR 14 OR 17 OR 18 OR 21 OR 24 OR 27         
206300****IF REFILLED FROM ANOTHER THAN CDC AND THE DISP IS GREATER             
206400****THAN ZERO                                                             
206500              IF  REF-NDC                                                 
206600              AND WS-KVDISP-SEND-DC       > 0                             
206700                  CONTINUE                                                
206800              ELSE                                                        
206900                  PERFORM S54-ERSATTNINGAR-NDC                            
207000              END-IF                                                      
207100           END-IF                                                         
207200           IF ERSATT-SW = 'N'                                             
207300              PERFORM S47-BER-KVPB-ANTV-DC                                
207400              IF CLAG-KVQPACK-1 > ZERO                                    
207500                 PERFORM S56C-SATT-QX-PROCENT-BRYTNING                    
207600                 PERFORM S56B-KVQPACK-1                                   
207700                 MOVE WS-ANTAL-QX TO WS-ANTAL-SLUT                        
207800              ELSE                                                        
207900                 MOVE WS-ANTAL-BER TO WS-ANTAL-SLUT                       
208000              END-IF                                                      
208100           END-IF                                                         
208200           MOVE JA TO BEORDRA-SW                                          
208300        ELSE                                                              
208400           IF CLAG-KDERS = 11 OR 14 OR 17 OR 18 OR                        
208500                           21 OR 24 OR 27 OR 28                           
208600              IF REF-NDC                                                  
208700              AND WS-KVDISP-SEND-DC       > 0                             
208800                  CONTINUE                                                
208900              ELSE                                                        
209000                  PERFORM S54-ERSATTNINGAR-NDC                            
209100              END-IF                                                      
209200              MOVE JA TO BEORDRA-SW                                       
209300           END-IF                                                         
209400           IF ERSATT-SW = 'N'                                             
209500              IF SB-SLAG-KVREFPKT = 1 AND                                 
209600                                     SB-SLAG-KVREFBER = 1                 
209700                 MOVE 1 TO WS-ANTAL-BER                                   
209800              ELSE                                                        
209900                 IF SB-SLAG-KVUTRS = 0                                    
210000                    IF DCS-SDC                                            
210100                       COMPUTE WS-KVREFPKT-HALV =                         
210200                                     WS-KVREFPKT-DC / 2                   
210300                       IF WS-KVDISP-DC < WS-KVREFPKT-HALV                 
210400                          PERFORM S35-BER-ANTAL-V-STORRE-BRIST            
210500                       ELSE                                               
210600                          PERFORM S36-BER-ANTAL-V-MINDRE-BRIST            
210700                       END-IF                                             
210800                    END-IF                                                
210900                    IF NDC                                                
211000                       IF SB-SLAG-KVREFPKT > ZERO                         
211100                          IF SB-SLAG-TIREFPAF > ZERO                      
211200                             MOVE SB-SLAG-TIREFPAF TO                     
211300                                              TMP1-YYMMDD                 
211400                             MOVE DAGENS-DATUM  TO TMP2-YYMMDD            
211500                             PERFORM WY2000P1                             
211600                             IF TMP1-YYMMDD <  TMP2-YYMMDD                
211610                               IF (SB-SLAG-KVROS-DAG > 0                  
211620                               OR SB-SLAG-KVROS-BULK > 0) AND             
211621                                  SB-SLAG-FLFLYG = 'S'                    
211630                                  PERFORM                                 
211640                                  S38-BER-ANTAL-BO                        
211650                               ELSE                                       
211700                                  PERFORM                                 
211800                                  S35-BER-ANTAL-V-STORRE-BRIST            
211810                               END-IF                                     
211900                             ELSE                                         
212000                               PERFORM                                    
212100                               S36-BER-ANTAL-V-MINDRE-BRIST               
212200                             END-IF                                       
212300                          ELSE                                            
212310                            IF (SB-SLAG-KVROS-DAG > 0                     
212320                            OR SB-SLAG-KVROS-BULK > 0) AND                
212330                               SB-SLAG-FLFLYG = 'S'                       
212340                               PERFORM                                    
212350                               S38-BER-ANTAL-BO                           
212360                            ELSE                                          
212370                               PERFORM                                    
212380                               S35-BER-ANTAL-V-STORRE-BRIST               
212390                            END-IF                                        
212500                          END-IF                                          
212600                       ELSE                                               
212700                          IF REF-CDC-SE                                   
212800                          OR (NDC AND REF-NDC)                            
212900*   AKUT ÄT.     TÄCK ÄVEN BULKRESTORDER  021003 JOHAN L                  
213000                             COMPUTE WS-KVROS-DC-TOT =                    
213100                               SB-SLAG-KVROS-DAG +                        
213200                               SB-SLAG-KVROS-BULK                         
213300                             IF (SB-SLAG-FLFLYG = JA                      
213301****JN                                                                    
213310                              OR SB-SLAG-FLFLYG = 'S')                    
213400                             AND WS-KVROS-DC-TOT   > ZERO                 
213500                                 MOVE WS-KVROS-DC-TOT                     
213600                                   TO WS-ANTAL-BER                        
213700                             END-IF                                       
213800                          ELSE                                            
213900                             IF WS-KVDISP-LOCAL < 1                       
214000                                COMPUTE WS-KVROS-DC-TOT =                 
214100                                SB-SLAG-KVROS-DAG +                       
214200                                SB-SLAG-KVROS-BULK                        
214300                                IF WS-KVROS-DC-TOT > ZERO                 
214400                                   MOVE WS-KVROS-DC-TOT TO                
214500                                   WS-ANTAL-BER                           
214600                                END-IF                                    
214700                             END-IF                                       
214800                          END-IF                                          
214900                       END-IF                                             
215000                    END-IF                                                
215100                 ELSE                                                     
215200                    PERFORM S37-BER-ANTAL-V-UTRSALDO                      
215300                 END-IF                                                   
215400              END-IF                                                      
215500              IF OKAD-FORSALJNING                                         
215600                 COMPUTE WS-KVREFOVL-TEST =                               
215700                         WS-KVDISP-DC + WS-ANTAL-BER                      
215800                 IF WS-KVREFOVL-TEST > SB-SLAG-KVREFOVL                   
215900                    COMPUTE WS-ANTAL-BER =                                
216000                            SB-SLAG-KVREFOVL - WS-KVDISP-DC               
216100                 END-IF                                                   
216200              END-IF                                                      
216300                                                                          
216400              PERFORM CDA-ANPASSA-ANTAL                                   
216500                                                                          
216600              COMPUTE WS-RADPRIS ROUNDED =                                
216700                      WS-ANTAL-SLUT * WS-PRIS                             
216800                                                                          
216900*                                                                         
217000*     ENDAST RADER ÖVERSTIGANDE 200:- SKA                                 
217100*     GENERERA TRANSFERFÖRSLAG SAMT PRISKLASSEN                           
217200*     SKA VARA STÖRRE ÄN 3                                                
217300*                                                                         
217400              IF NDC                                                      
217500***TILLFÄLLIG STOPP AV TRANSFERFÖRSLAG 170309                             
217600*               IF  WS-RADPRIS > 200                                      
217700*               AND CLAG-KDPRISKL > 3                                     
217800*                 IF NDC-NA OR NDC-PACIFIC                                
217900*                   PERFORM S53-KOLLA-TRANSFER-NDC                        
218000                    CONTINUE                                              
218100*                 ELSE                                                    
218200*                   PERFORM S61-KOLLA-TRANSFER-OVR                        
218300*                 END-IF                                                  
218400*               END-IF                                                    
218500              ELSE                                                        
218600                IF  WS-RADPRIS > 200                                      
218700                AND (CLAG-KDPRISKL = 4                                    
218800                OR   CLAG-KDPRISKL = 5)                                   
218900                  PERFORM S61-KOLLA-TRANSFER-OVR                          
219000                END-IF                                                    
219100              END-IF                                                      
219200                                                                          
219300           END-IF                                                         
219400        END-IF                                                            
219500                                                                          
219600        PERFORM CDD-KOLLA-TILLKOMMANDE                                    
219700                                                                          
219800        IF NDC                                                            
219900           IF REF-CDC-SE                                                  
220000           OR (NDC AND REF-NDC)                                           
220100              CONTINUE                                                    
220200           ELSE                                                           
220300              PERFORM S58-KOLLA-PRIS-LOKALA                               
220400           END-IF                                                         
220500        END-IF                                                            
220600     END-IF                                                               
220700     PERFORM CDE-KONTROLLERA-FORSLAG                                      
220800     .                                                                    
220900     EJECT                                                                
221000                                                                          
221100 CDA-ANPASSA-ANTAL SECTION.                                               
221200                                                                          
221210     IF (SB-SLAG-KVROS-DAG > 0                                            
221220     OR SB-SLAG-KVROS-BULK > 0) AND                                       
221230        SB-SLAG-FLFLYG = 'S'                                              
221240       IF WS-ANTAL-BER > ZERO                                             
221250         MOVE WS-ANTAL-BER            TO WS-ANTAL-SLUT                    
221260       END-IF                                                             
221270     ELSE                                                                 
221300       IF SB-SLAG-KVREFPKT = +1 AND                                       
221400          SB-SLAG-KVREFBER = +1 AND                                       
221500          CLAG-KVQPACK-1 < 2    AND                                       
221600          WS-ANTAL-BER = +1                                               
221700             MOVE +1 TO WS-ANTAL-SLUT                                     
221800       ELSE                                                               
221900          PERFORM S47-BER-KVPB-ANTV-DC                                    
222000          PERFORM S56C-SATT-QX-PROCENT-BRYTNING                           
222100          PERFORM S56-Q-ANPASSA                                           
222200          IF WS-ANTAL-SLUT > WS-KVPB-ANTV-DC                              
222300          AND WS-KVPB-ANTV-DC > ZERO                                      
222400*   REFILLA MAX X ANTAL VECKOR DOCK MINST UPP TILL REFILLPUNKTEN          
222500             IF WS-KVDISP-DC + WS-KVPB-ANTV-DC <                          
222600                SB-SLAG-KVREFPKT                                          
222700                COMPUTE WS-KVPB-ANTV-DC =                                 
222800                SB-SLAG-KVREFPKT - WS-KVDISP-DC                           
222900             END-IF                                                       
223000             IF CLAG-KVQPACK-1 > 1                                        
223100                MOVE WS-KVPB-ANTV-DC                                      
223200                               TO WS-ANTAL-BER                            
223300                PERFORM S56B-KVQPACK-1                                    
223400                MOVE WS-ANTAL-QX                                          
223500                               TO WS-ANTAL-SLUT                           
223600             ELSE                                                         
223700                MOVE WS-KVPB-ANTV-DC                                      
223800                               TO WS-ANTAL-SLUT                           
223900             END-IF                                                       
224000          END-IF                                                          
224100       END-IF                                                             
224110     END-IF                                                               
224200     MOVE JA TO BEORDRA-SW                                                
224300     .                                                                    
224400     EJECT                                                                
224500                                                                          
224600 CDD-KOLLA-TILLKOMMANDE SECTION.                                          
224700                                                                          
224800*    GER JA I TILLKOMMANDE OM ARIKELN/DC SOM KONTROLLERAS                 
224900*    HAR BRIST, ÄR TILLKOMMANDE OCH BEORDRAS FÖRSTA GÅNGEN,               
225000*    DESSUTOM SKALL DEN ARTIKEL SOM DEN AKTUELLA ERSÄTTER VARA            
225100*    LEVANDE OCH HA VISSA ERSÄTTNINGSKODER.                               
225200                                                                          
225300     IF WDK6-ART-FLERS = JA                                               
225400        IF SB-SLAG-TIORDREG = ZERO                                        
225500*          ARTC01 OCH ARTC11 FÖR ERSATT ARTIKEL LÄSES                     
225600*          I S59 SECTION                                                  
225700           IF ERS-WDK611-SEGMENT-FINNS                                    
225800             IF ERS-ART-KDERS-UTG = ZERO                                  
225900               IF ERS-CLAG-KDERS = 02 OR 03 OR 05 OR 06 OR                
226000                                   01 OR 07 OR 11 OR 17 OR                
226100                                   22 OR 23 OR 25 OR 26 OR                
226200                                   21 OR 27                               
226300                  MOVE JA TO TILLKOMMANDE-SW                              
226400******************GIVES MESSAGE REPING ON SCREEN 23X2                     
226500               END-IF                                                     
226600             END-IF                                                       
226700           END-IF                                                         
226800        END-IF                                                            
226900     END-IF                                                               
227000     .                                                                    
227100     EJECT                                                                
227200                                                                          
227300                                                                          
227400 CDE-KONTROLLERA-FORSLAG SECTION.                                         
227500                                                                          
227600                                                                          
227700     IF DCS-SDC                                                           
227800        IF (SB-SLAG-FLREFBEO = NEJ                                        
227900        OR  SB-SLAG-FLREFBEO = 'S'                                        
228000        OR (OKAD-FORSALJNING AND BRIST-I-DC))                             
228100             MOVE JA TO FORSLAG-SW                                        
228200        END-IF                                                            
228300     END-IF                                                               
228400                                                                          
228500     IF (NDC AND REF-NDC)                                                 
228600       MOVE INT-SLAG-KVSPARR-KVAL TO WS-KVSPARR-KVAL                      
228700     ELSE                                                                 
228800       MOVE CLAG-KVSPARR-KVAL     TO WS-KVSPARR-KVAL                      
228900     END-IF                                                               
229000     IF NDC                                                               
229100        IF ERSATT                    OR                                   
229200           TRANSFER                  OR                                   
229300           NO-PRICE-LOCAL            OR                                   
229400           L-STOCK-LOCAL             OR                                   
229500           WS-KVSPARR-KVAL  > 0      OR                                   
229600           SB-SLAG-KDLEVSP    > 0    OR                                   
229700           SB-SLAG-KVUTRS     > 0    OR                                   
229800           SB-SLAG-FLREFBEO = NEJ    OR                                   
229900           SB-SLAG-FLREFBEO = 'S'    OR                                   
229901*                                                                         
229910          (SB-SLAG-FLREFBEO = JA     AND                                  
229920          (SB-SLAG-FLFLYG     = JA)) OR                                   
229921*                                                                         
229930          (SB-SLAG-FLREFBEO = JA     AND                                  
229940          (SB-SLAG-FLFLYG     = 'S') AND                                  
229950          (SB-SLAG-KVROS-BULK > ZERO                                      
229960           OR   SB-SLAG-KVROS-DAG    > ZERO))                             
230200             MOVE JA TO FORSLAG-SW                                        
230300        END-IF                                                            
230400     END-IF                                                               
230500     .                                                                    
230600     EJECT                                                                
230700                                                                          
230800                                                                          
230900 CE-SKAPA-ORDER SECTION.                                                  
231000                                                                          
231100     IF FLYGFORSLAG                                                       
231200        PERFORM CED-SKAPA-FLYGFORSLAG                                     
231300     ELSE                                                                 
231400        IF REF-CDC-SE                                                     
231500        OR (NDC AND REF-NDC)                                              
231600           IF SB-SLAG-FLREFBEO = NEJ                                      
231700           OR SB-SLAG-FLREFBEO = 'S'                                      
231800                MOVE JA TO FORSLAG-SW                                     
231900           END-IF                                                         
232000           IF FORSLAG                                                     
232100              PERFORM CEA-SKAPA-REFILLFORSLAG                             
232200           ELSE                                                           
232300              PERFORM CEB-SKAPA-REFILLORDER                               
232400           END-IF                                                         
232500        ELSE                                                              
232600           PERFORM CEE-FORSLAG-LOKAL-LEVERANTOR                           
232700        END-IF                                                            
232800     END-IF                                                               
232900     .                                                                    
233000     EJECT                                                                
233100                                                                          
233200                                                                          
233300 CEA-SKAPA-REFILLFORSLAG SECTION.                                         
233400                                                                          
233500     IF SB-SLAG-FLFLYG = JA                                               
233600       MOVE 'A'                  TO W27110-KDREFTYP                       
233700     ELSE                                                                 
233800       MOVE 'B'                  TO W27110-KDREFTYP                       
233900     END-IF                                                               
234000     MOVE SB-SLAG-IDDC           TO W27110-IDDC                           
234100     MOVE WS-SPAR-IDARTNR        TO W27110-IDARTNR                        
234200     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
234300     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
234400     MOVE WS-IDDISTR             TO W27110-IDDISTR                        
234500     MOVE ZERO                   TO W27110-IDKUNDNR                       
234600     MOVE WS-ANTAL-SLUT          TO W27110-KVBEART                        
234700     MOVE 'P'                    TO W27110-KDREFORS                       
234800     MOVE SB-SLAG-IDLEVNR        TO W27110-IDLEVNR                        
234900     MOVE '99'                   TO W27110-KDREFTXT                       
235000     MOVE ZERO                   TO W27110-KDFRAKT                        
235100     MOVE SB-SLAG-IDDC-REF       TO W27110-IDDC-REF                       
235200     MOVE SB-SLAG-IDPERSON-BUY                                            
235300                                 TO W27110-IDPERSON-BUY                   
235400                                                                          
235500     PERFORM S57-RANGORDNA-REFTEXT                                        
235600     .                                                                    
235700     EJECT                                                                
235800                                                                          
235900                                                                          
236000 CEB-SKAPA-REFILLORDER SECTION.                                           
236100                                                                          
236200     MOVE 'O'                    TO W27110-KDREFTYP                       
236300     MOVE SPACE                  TO W27110-KDREFORS                       
236400     MOVE ZERO                   TO W27110-KDFRAKT                        
236500     MOVE SB-SLAG-IDDC           TO W27110-IDDC                           
236600     MOVE WS-SPAR-IDARTNR        TO W27110-IDARTNR                        
236700     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
236800     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
236900     MOVE WS-IDDISTR             TO W27110-IDDISTR                        
237000     MOVE ZERO                   TO W27110-IDKUNDNR                       
237100     MOVE WS-ANTAL-SLUT          TO W27110-KVBEART                        
237200     MOVE SB-SLAG-IDLEVNR        TO W27110-IDLEVNR                        
237300     MOVE ZERO                   TO W27110-KDREFTXT                       
237400     MOVE SB-SLAG-IDDC-REF       TO W27110-IDDC-REF                       
237500     MOVE SB-SLAG-IDPERSON-BUY   TO W27110-IDPERSON-BUY                   
237600     .                                                                    
237700     EJECT                                                                
237800                                                                          
237900 CED-SKAPA-FLYGFORSLAG SECTION.                                           
238000                                                                          
238100     IF ((SB-SLAG-KVROS-BULK > ZERO                                       
238200     OR   SB-SLAG-KVROS-DAG    > ZERO)                                    
238300     AND  WS-KVDISP-FLYG < ZERO                                           
238400     AND  NDC)                                                            
238500       MOVE 'C'                     TO W27110-KDREFTYP                    
238600     ELSE                                                                 
238700       MOVE 'A'                     TO W27110-KDREFTYP                    
238800     END-IF                                                               
238900                                                                          
239000     MOVE SB-SLAG-IDDC              TO W27110-IDDC                        
239100     MOVE WS-SPAR-IDARTNR           TO W27110-IDARTNR                     
239200     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
239300     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
239400     MOVE WS-IDDISTR                TO W27110-IDDISTR                     
239500     MOVE ZERO                      TO W27110-IDKUNDNR                    
239600     MOVE WS-ANTAL-SLUT             TO W27110-KVBEART                     
239700     MOVE 'P'                       TO W27110-KDREFORS                    
239800     MOVE SB-SLAG-IDLEVNR           TO W27110-IDLEVNR                     
239900     MOVE '99'                      TO W27110-KDREFTXT                    
240000     MOVE SB-SLAG-IDDC-REF          TO W27110-IDDC-REF                    
240100     MOVE SB-SLAG-IDPERSON-BUY                                            
240200                                    TO W27110-IDPERSON-BUY                
240300     MOVE ZERO                      TO W27110-KDFRAKT                     
240400                                                                          
240500     IF FORSLAG                                                           
240600        PERFORM S57-RANGORDNA-REFTEXT                                     
240700     END-IF                                                               
240800     .                                                                    
240900     EJECT                                                                
241000                                                                          
241100                                                                          
241200 CEE-FORSLAG-LOKAL-LEVERANTOR SECTION.                                    
241300                                                                          
241400     MOVE 'L'                       TO W27110-KDREFTYP                    
241500     MOVE SB-SLAG-IDDC              TO W27110-IDDC                        
241600     MOVE WS-SPAR-IDARTNR           TO W27110-IDARTNR                     
241700     PERFORM S45-FLYTTA-ARTIKELADRESS                                     
241800     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
241900     MOVE WS-IDDISTR                TO W27110-IDDISTR                     
242000     MOVE ZERO                      TO W27110-IDKUNDNR                    
242100     MOVE WS-ANTAL-SLUT             TO W27110-KVBEART                     
242200     MOVE 'P'                       TO W27110-KDREFORS                    
242300     MOVE SB-SLAG-IDLEVNR           TO W27110-IDLEVNR                     
242400     MOVE '99'                      TO W27110-KDREFTXT                    
242500     MOVE SPACES                    TO W27110-IDDC-REF                    
242600     MOVE SB-SLAG-IDPERSON-BUY      TO W27110-IDPERSON-BUY                
242700     MOVE ZERO                      TO W27110-KDFRAKT                     
242800                                                                          
242900     IF FORSLAG                                                           
243000        PERFORM S57-RANGORDNA-REFTEXT                                     
243100     END-IF                                                               
243200     .                                                                    
243300     EJECT                                                                
243400                                                                          
243500                                                                          
243600 CF-ARTIKELADMINISTRATION  SECTION.                                       
243700                                                                          
243800* SKAPAR FIL MED ARTIKLAR SOM SKALL UPPDATERAS I PGM W27136               
243900                                                                          
244000     MOVE WS-SPAR-IDARTNR        TO W27136-IDARTNR                        
244100     MOVE SB-SLAG-IDDC           TO W27136-IDDC                           
244200     MOVE CLAG-KDERS             TO W27136-KDERS                          
244300     MOVE SB-SLAG-IDPERSON-BUY                                            
244400                                 TO W27136-IDPERSON-BUY                   
244500     MOVE CLAG-TIREFSTO TO TMP1-YYMMDD                                    
244600     MOVE DAGENS-DATUM    TO TMP2-YYMMDD                                  
244700     PERFORM WY2000P1                                                     
244800     IF (TMP1-YYMMDD   > +0 AND                                           
244900         TMP1-YYMMDD < TMP2-YYMMDD)                                       
245000        MOVE TMP1-YYMMDD   TO W27136-TIREFSTO-CLAG                        
245100     END-IF                                                               
245200     MOVE SB-SLAG-TIREFSTO     TO TMP1-YYMMDD                             
245300     MOVE DAGENS-DATUM         TO TMP2-YYMMDD                             
245400     PERFORM WY2000P1                                                     
245500     IF (SB-SLAG-TIREFSTO > +0 AND                                        
245600         TMP1-YYMMDD < TMP2-YYMMDD)                                       
245700        MOVE SB-SLAG-TIREFSTO TO W27136-TIREFSTO-SLAG                     
245800     END-IF                                                               
245900                                                                          
246000     IF ((ERSATT                           )AND                           
246100         (CLAG-KDERS = 11 OR 17 OR 21 OR 27)AND                           
246200         (SB-SLAG-KDREFSTA = 'A'           ))                             
246300        MOVE 'P'           TO W27136-KDREFSTA                             
246400     END-IF                                                               
246500**WHY 'N* TO W27136                                                       
246600     MOVE 'N'                  TO W27136-FLPB-FLYTT                       
246700     .                                                                    
246800     EJECT                                                                
246900                                                                          
247000                                                                          
247100 CG-EV-CROSS-DOCKING  SECTION.                                            
247200                                                                          
247300     IF  W27110-KDREFTYP = 'O'                                            
247400     AND SB-SLAG-ADLAGOMR-CD > ZERO                                       
247500                                                                          
247600       MOVE 1                TO IX                                        
247700       PERFORM UNTIL IX > 4                                               
247800       OR SB-SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX)                     
247900         ADD 1               TO IX                                        
248000       END-PERFORM                                                        
248100                                                                          
248200       IF IX > 4                                                          
248300*    SKA INTE KUNNA INTRÄFFA                                              
248400         CONTINUE                                                         
248500       ELSE                                                               
248600                                                                          
248700         IF (CLAG-KVLS-CD (IX) - CLAG-KVRESS-CD (IX))                     
248800                             < CLAG-KVQPACK-3                             
248900           CONTINUE                                                       
249000         ELSE                                                             
249100*JOURFIX                                                                  
249200           IF CLAG-KVQPACK-3 = ZERO                                       
249300              MOVE +1 TO CLAG-KVQPACK-3                                   
249400              DISPLAY 'ARTIKEL MED KVQPACK-3 = 0 '                        
249500                     WDK6-ART-IDARTNR                                     
249600           END-IF                                                         
249700*JOURFIX                                                                  
249800           DIVIDE W27110-KVBEART BY CLAG-KVQPACK-3                        
249900                                 GIVING WS-HELTAL-BEST                    
250000           COMPUTE WS-SALDO =                                             
250100                   CLAG-KVLS-CD (IX) - CLAG-KVRESS-CD (IX)                
250200           DIVIDE WS-SALDO       BY CLAG-KVQPACK-3                        
250300                                 GIVING WS-HELTAL-SALDO                   
250400           IF WS-HELTAL-BEST > WS-HELTAL-SALDO                            
250500             COMPUTE W27110-KVBEART-CD =                                  
250600                             WS-HELTAL-SALDO * CLAG-KVQPACK-3             
250700           ELSE                                                           
250800             COMPUTE W27110-KVBEART-CD =                                  
250900                             WS-HELTAL-BEST * CLAG-KVQPACK-3              
251000           END-IF                                                         
251100           MOVE CLAG-ADLAGOMR-CD (IX)                                     
251200                             TO W27110-ADLAGOMR-CD                        
251300           MOVE CLAG-ADGANG-CD (IX)                                       
251400                             TO W27110-ADGANG-CD                          
251500           MOVE CLAG-ADPLATS-CD (IX)                                      
251600                             TO W27110-ADPLATS-CD                         
251700         END-IF                                                           
251800       END-IF                                                             
251900     END-IF                                                               
252000     .                                                                    
252100     EJECT                                                                
252200                                                                          
252300 CH-KOLLA-CROSS-DOCKING SECTION.                                          
252400                                                                          
252500     MOVE SB-SLAG-IDDC       TO W-IDDC-B6                                 
252600     PERFORM IMS-GU-WDB601                                                
252700     IF DCS-KVDAGAR-CROSS > 0                                             
252800       MOVE ZERO        TO WS-TIDATUM-CROSS                               
252900       MOVE JA          TO WS-WORKDAY-STATUS                              
253000       PERFORM CHB-BERAKNA-SLUT-DATUM                                     
253100       IF RATT-FRAN-WORKDAY                                               
253200         PERFORM CHA-BERAKNA-CDC-TILLG                                    
253300         IF SKRIV-ORDER-SW = NEJ                                          
253400**OM MAN FÖRDRÖJER ORDER OCH MAN INTE REDAN HAR FÖRDRÖJNINGSDATUM         
253500**SÅ SKALL DETTA SÄTTAS PÅ WDK7. TIDATUM-CROSS                            
253600           MOVE WS-TIDATUM-CROSS   TO W271F1-TIDATUM-CROSS                
253700           MOVE SB-SLAG-IDDC TO W271F1-IDDC                               
253800           MOVE WS-SPAR-IDARTNR TO W271F1-IDARTNR                         
253900           PERFORM S14-SKRIV-W271F1                                       
254000         END-IF                                                           
254100       END-IF                                                             
254200     END-IF                                                               
254300                                                                          
254400     .                                                                    
254500     EJECT                                                                
254600 CHA-BERAKNA-CDC-TILLG SECTION.                                           
254700                                                                          
254800     IF WS-SUM-XDC-BEHOV = 0                                              
254900****   SUMMA KVPB-REF FÖR UNDERLIGGANDE LAGER                             
255000       PERFORM CHAC-SUM-XDC-BEHOV                                         
255100     END-IF                                                               
255200     COMPUTE WS-RA-TILLGANG = CLAG-KVLS         +                         
255300                              CLAG-KVAKS-CDC    -                         
255400                              CLAG-KVRESS       -                         
255500                              CLAG-KVUTRS       -                         
255600                              CLAG-KVSPANT      -                         
255700                              CLAG-KVSPARR-KVAL                           
255800                                                                          
255900                                                                          
256000     MOVE WS-SPAR-IDARTNR TO W-IDARTNR                                    
256100     PERFORM IMS-GU-ART-WDK9                                              
256200     IF SEGMENT-FINNS                                                     
256300       COMPUTE WS-RA-TILLGANG = WS-RA-TILLGANG                            
256400                                 - WDK9-ART-KVOKS-BULK                    
256500                                 - WDK9-ART-KVOKS-DAG                     
256600                                 - WDK9-ART-KVOKS-VOR                     
256700     END-IF                                                               
256800     COMPUTE WS-RA-TILLGANG = WS-RA-TILLGANG -                            
256900                              WS-CD-TOT-KVBEART                           
257000     PERFORM CHAA-ANTAL-WDD905                                            
257100     .                                                                    
257200 CHAA-ANTAL-WDD905 SECTION.                                               
257300                                                                          
257400     MOVE ZERO     TO WS-SUM-KVAVROP                                      
257500     MOVE ZERO     TO WS-TIAVRDAT-DISP                                    
257600     MOVE WS-SPAR-IDARTNR TO W-IDARTNR-D9                                 
257700     PERFORM IMS-GU-ART-WDD9                                              
257800     IF SEGMENT-FINNS                                                     
257900       MOVE 2         TO W-KDAVROP-D9                                     
258000       PERFORM IMS-GNP-WDD905-2                                           
258100       PERFORM UNTIL SEGMENT-SAKNAS                                       
258200                  OR SKRIV-EJ-ORDER                                       
258300                  OR WDD905-TIAVRDAT-DISP > WS-TIDATUM-CROSS              
258400         IF WDD905-TIAVRDAT-DISP >= DAGENS-DATUM                          
258500         AND WDD905-TIAVRDAT-DISP <= WS-TIDATUM-CROSS                     
258600           COMPUTE WS-SUM-KVAVROP =                                       
258700                   WS-SUM-KVAVROP + WDD905-KVAVROP                        
258800           MOVE WDD905-TIAVRDAT-DISP  TO WS-TIAVRDAT-DISP                 
258900           COMPUTE WS-JMF-KVAVROP = WS-SUM-KVAVROP -                      
259000                                    WS-BOKAD-TOT-KVBEART                  
259100           IF WS-JMF-KVAVROP >= W27110-KVBEART                            
259200             PERFORM CHAX-PERFORM-UTRAKNING                               
259300           END-IF                                                         
259400         END-IF                                                           
259500         PERFORM IMS-GNP-WDD905-2                                         
259600       END-PERFORM                                                        
259700     END-IF                                                               
259800                                                                          
259900     .                                                                    
260000     EJECT                                                                
260100                                                                          
260200 CHAX-PERFORM-UTRAKNING SECTION.                                          
260300                                                                          
260400*** OM INLEVERANSERNA ÄR MINDRE ÄN BEHOVET SLÄPP ORDER                    
260500*** OM INLEVERANSERNA FÖR ATT KOMMA ÖVER KVBEART INTE SKER                
260600*** FÖRRÄN EFTER SISTA FÖRDRÖJNINGSDAGEN SÅ SLÄPP ORDER                   
260700     COMPUTE WS-RA-TILLGANG = WS-RA-TILLGANG + WS-SUM-KVAVROP             
260800     PERFORM CHAB-CDC-BEHOV-DAG                                           
260900     COMPUTE WS-RA-TILLGANG = WS-RA-TILLGANG - WS-CDC-DAGSBEHOV           
261000*MAN RÄKNAR FRAM 4 DAGARS BEHOV CDC ENLIG REGLER FRT CROSS DOCKIN         
261100     COMPUTE WS-4DAG-BEHOV-CDC =                                          
261200             (4 * ((CLAG-KVPB-SEP + CLAG-KVPB-SATS) / 21.65))             
261300     IF WS-RA-TILLGANG > WS-4DAG-BEHOV-CDC                                
261400       MOVE NEJ TO SKRIV-ORDER-SW                                         
261500       ADD W27110-KVBEART  TO WS-BOKAD-TOT-KVBEART                        
261600     END-IF                                                               
261700                                                                          
261800     .                                                                    
261900     EJECT                                                                
262000 CHAB-CDC-BEHOV-DAG SECTION.                                              
262100                                                                          
262200     MOVE ZERO TO WS-CDC-DAGSBEHOV                                        
262300     COMPUTE WS-CDC-DAGSBEHOV = (CLAG-KVPB-SATS + CLAG-KVPB-SEP           
262400             + CLAG-KVPB-TPO + WS-SUM-XDC-BEHOV) / 21.65                  
262500     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-FOM                        
262600     MOVE WS-TIAVRDAT-DISP    TO WORK-TIAAMMDD-TOM                        
262700     MOVE 001                 TO WORK-KDCALL                              
262800     MOVE SB-SLAG-IDDC-REF    TO WORK-IDDC                                
262900     CALL WORKDAY USING WORK-KDCALL                                       
263000                                    WORK-DATE-AREA                        
263100                                    WORK-KDSVAR                           
263200     IF WORK-KDSVAR-OK                                                    
263300       COMPUTE WS-CDC-DAGSBEHOV = WS-CDC-DAGSBEHOV *                      
263400                                  WORK-KVWORKD                            
263500     ELSE                                                                 
263600**    ?????????????????????????????????????????????????????????           
263700        MOVE ZERO       TO WS-CDC-DAGSBEHOV                               
263800     END-IF                                                               
263900     .                                                                    
264000     EJECT                                                                
264100                                                                          
264200 CHAC-SUM-XDC-BEHOV     SECTION.                                          
264300                                                                          
264400     MOVE WS-SPAR-IDARTNR TO W-IDARTNR                                    
264500     PERFORM IMS-GU-WDK701-CD                                             
264600     IF SEGMENT-FINNS                                                     
264700       PERFORM IMS-GNP-WDK711-CD                                          
264800       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
264900         IF SB-SLAG-IDDC-REF = '11'                                       
265000***ANVÄNDER WDK72 PCB FÖR ATT EJ BEHÖVA SKAPA NYTT                        
265100***DÄRFÖR ERS-**                                                          
265200           COMPUTE WS-SUM-XDC-BEHOV = WS-SUM-XDC-BEHOV +                  
265300                                      ERS-SLAG-KVPB-REF                   
265400         END-IF                                                           
265500         PERFORM IMS-GNP-WDK711-CD                                        
265600       END-PERFORM                                                        
265700     END-IF                                                               
265800     .                                                                    
265900     EJECT                                                                
266000                                                                          
266100 CHB-BERAKNA-SLUT-DATUM SECTION.                                          
266200                                                                          
266300     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-FOM                        
266400     MOVE 002                 TO WORK-KDCALL                              
266500     MOVE DCS-KVDAGAR-CROSS   TO WORK-KVWORKD                             
266600     MOVE SB-SLAG-IDDC-REF    TO WORK-IDDC                                
266700     CALL WORKDAY USING WORK-KDCALL                                       
266800                                    WORK-DATE-AREA                        
266900                                    WORK-KDSVAR                           
267000     IF WORK-KDSVAR-OK                                                    
267100        MOVE WORK-TIAAMMDD-TOM          TO WS-TIDATUM-CROSS               
267200                                                                          
267300     ELSE                                                                 
267400**    SÄTTER NEJ FÖR ATT SLÄPPA REFILLORDER OM FEL FRÅN WORKDAY           
267500**    DETTA FÖR ATT INTE ABENDA OCH FÖRSENA RESTERANDE                    
267600**    REFILLORDRAR.                                                       
267700        MOVE NEJ                        TO WS-WORKDAY-STATUS              
267800     END-IF                                                               
267900                                                                          
268000     .                                                                    
268100     EJECT                                                                
268200                                                                          
268300 CI-CHECK-RELEASEDAY SECTION.                                             
268400                                                                          
268500     MOVE WS-DAG-I-VECKA      TO SUB                                      
268600                                                                          
268700     EVALUATE TRUE                                                        
268800     WHEN NOTDANGEROUS                                                    
268900      AND B6-REF-FLREFBLK(SUB) = 'J'                                      
269000       SET RELEASEDAY         TO TRUE                                     
269100     WHEN DANGEROUSGOODS                                                  
269200      AND (  (B6-REF-KDREFDG(SUB) = 'J')                                  
269300          OR (B6-REF-KDREFDG(SUB) = 'E' AND WEEK-EVEN)                    
269400          OR (B6-REF-KDREFDG(SUB) = 'O' AND WEEK-ODD)  )                  
269500       SET RELEASEDAY         TO TRUE                                     
269600     END-EVALUATE                                                         
269700                                                                          
269800     .                                                                    
269900     EJECT                                                                
270000                                                                          
270100 D-CHECK-STOP-DATE SECTION.                                               
270200                                                                          
270300     PERFORM IMS-GNP-WDK722-INTERN                                        
270400     IF SEGMENT-FINNS                                                     
270500        MOVE XLAG-TIREFSTO-LOC TO TMP1-YYMMDD                             
270600        MOVE DAGENS-DATUM      TO TMP2-YYMMDD                             
270700        PERFORM WY2000Q1                                                  
270800* WHEN WDK722 EXISTS - IF THE STOP DATE IS NOT 0 AND IT IS                
270900* LESS THAN THE CURRENT DATE, REFILL SHOULD BE DONE                       
271000        IF ((TMP1-YYMMDD > +0)                                            
271100        AND (TMP1-YYMMDD < TMP2-YYMMDD))                                  
271200            MOVE JA            TO INTERN-REFILL-SW                        
271300        END-IF                                                            
271400* IF THE STOP DATE IS 0, REFILL SHOULD BE DONE                            
271500        IF TMP1-YYMMDD = +0                                               
271600            MOVE JA            TO INTERN-REFILL-SW                        
271700        END-IF                                                            
271800     ELSE                                                                 
271900* IF WDK722 DOES NOT EXIST, REFILL SHOULD BE DONE                         
272000        MOVE JA                TO INTERN-REFILL-SW                        
272100     END-IF                                                               
272200     .                                                                    
272300     EJECT                                                                
272400                                                                          
272500                                                                          
272600 Z-FINIT SECTION.                                                         
272700                                                                          
272800                                                                          
272900     CLOSE W271DC                                                         
273000           W27110                                                         
273100           W27136                                                         
273200           W271F1                                                         
273300     SKIP2                                                                
273400     MOVE 'S' TO POSTSUM-OPKOD                                            
273500     CALL POSTSUM USING POSTSUM-PARM                                      
273600     .                                                                    
273700     EJECT                                                                
273800                                                                          
273900                                                                          
274000 S11-SKRIV-W27110 SECTION.                                                
274100     SKIP2                                                                
274200     WRITE W27110-POST FROM W27110-AREA                                   
274300                                                                          
274400     MOVE 'W27110 ' TO POSTSUM-FDNAMN                                     
274500     MOVE 'W27110D2' TO POSTSUM-DDNAMN2                                   
274600     CALL POSTSUM USING POSTSUM-PARM                                      
274700     .                                                                    
274800     EJECT                                                                
274900                                                                          
275000                                                                          
275100 S12-SKRIV-W27136 SECTION.                                                
275200     SKIP2                                                                
275300     WRITE W27136-POST FROM W27136-AREA                                   
275400                                                                          
275500     MOVE 'W27136 ' TO POSTSUM-FDNAMN                                     
275600     MOVE 'W27110D3' TO POSTSUM-DDNAMN2                                   
275700     CALL POSTSUM USING POSTSUM-PARM                                      
275800     .                                                                    
275900     EJECT                                                                
276000                                                                          
276100                                                                          
276200 S14-SKRIV-W271F1 SECTION.                                                
276300     SKIP2                                                                
276400     WRITE W271F1-POST FROM W271F1-AREA                                   
276500                                                                          
276600     MOVE 'W271F1 ' TO POSTSUM-FDNAMN                                     
276700     MOVE 'W27110D4' TO POSTSUM-DDNAMN2                                   
276800     CALL POSTSUM USING POSTSUM-PARM                                      
276900     .                                                                    
277000     EJECT                                                                
277100                                                                          
277200                                                                          
277300 S21-LAES-W271DC   SECTION.                                               
277400                                                                          
277500      READ W271DC             INTO DC-POST                                
277600     .                                                                    
277700     SKIP3                                                                
277800 S31-KOLLA-OKAD-FORSALJNING SECTION.                                      
277900                                                                          
278000*                                                                         
278100* AV ANVÄNDARNS KALLAT ' BRANDSLÄCKARSYNDROM '                            
278200*                                                                         
278300     COMPUTE WS-KVPB-VECKA-DC =                                           
278400             (SB-SLAG-KVPB-REF *                                          
278500              SB-SLAG-RESEASON(WS-INNEV-TIRP)) / 4.33                     
278600*                                                                         
278700*    IF SDC-NL OR SDC-ES                                                  
278800*    OR SDC-IT OR SDC-AT OR LDC-GB-3A                                     
278900*    OR NDC-CN OR LDC-CN OR LDC-NL-3N OR LDC-FI-3O                        
279000*    OR (LDC-SE AND NOT LDC-SE-1C)                                        
279100*       CONTINUE                                                          
279200*    ELSE                                                                 
279300*       IF ARTIKEL-FINNS-L7                                               
279400*          IF WS-KVPB-VECKA-DC > +5                                       
279500*             COMPUTE WS-KVOI-INNEV =                                     
279600*                     DC-KVOI-INNEV   (PERIOD-VECKA) +                    
279700*                     DC-KVOI-PP-INNEV(PERIOD-VECKA)                      
279800*             IF WS-KVPB-VECKA-DC = ZERO                                  
279900*                MOVE +1             TO WS-KVPB-VECKA-DC                  
280000*             END-IF                                                      
280100*             COMPUTE WS-FORS-ANDR =                                      
280200*                     WS-KVOI-INNEV / WS-KVPB-VECKA-DC                    
280300*             IF SB-SLAG-KVPB-REF < 50                                    
280400*                IF WS-FORS-ANDR > 1.8                                    
280500*                   MOVE JA       TO OKAD-FORSALJNING-SW                  
280600*                END-IF                                                   
280700*             ELSE                                                        
280800*                IF WS-FORS-ANDR > 1.5                                    
280900*                   MOVE JA       TO OKAD-FORSALJNING-SW                  
281000*                END-IF                                                   
281100*             END-IF                                                      
281200*          END-IF                                                         
281300*       END-IF                                                            
281400*    END-IF                                                               
281500     .                                                                    
281600     EJECT                                                                
281700                                                                          
281800                                                                          
281900 S31B-BERAKNA-VECKO-PB SECTION.                                           
282000                                                                          
282100     COMPUTE WS-KVPB-VECKA-DC =                                           
282200             (SB-SLAG-KVPB-REF *                                          
282300              SB-SLAG-RESEASON(WS-INNEV-TIRP)) / 4.33                     
282400     COMPUTE WS-KVPB-VECKA-DC = WS-KVPB-VECKA-DC +                        
282500                                (SB-SLAG-KVPBREOI / 4.33)                 
282600     .                                                                    
282700     EJECT                                                                
282800                                                                          
282900                                                                          
283000 S32-BER-TILLF-REFILLPUNKT SECTION.                                       
283100*    6 DAY DELIVERY FOR UK                                                
283200     IF LDC-GB                                                            
283300       COMPUTE WS-KVPB-DAG-DC-OKAD = WS-KVOI-INNEV / 6                    
283400                                                                          
283500       COMPUTE WS-LT-BEHOV-DC-OKAD =                                      
283600             B6-REF-KVDLTID-TOT * WS-KVPB-DAG-DC-OKAD                     
283700                                                                          
283800                                                                          
283900*    LT VID NORMAL FÖRSÄLJNING                                            
284000                                                                          
284100       COMPUTE WS-KVPB-DAG-DC-NORM = WS-KVPB-VECKA-DC / 6                 
284200                                                                          
284300       COMPUTE WS-LT-BEHOV-DC-NORM =                                      
284400             B6-REF-KVDLTID-TOT * WS-KVPB-DAG-DC-NORM                     
284500                                                                          
284600                                                                          
284700*--- SKILLNAD                                                             
284800                                                                          
284900       COMPUTE WS-LT-BEHOV-DC-DIFF =                                      
285000             WS-LT-BEHOV-DC-OKAD -      WS-LT-BEHOV-DC-NORM               
285100                                                                          
285200*--- TILLFÄLLIG REFILLPUNKT                                               
285300                                                                          
285400       COMPUTE WS-KVREFPKT-DC =                                           
285500             SB-SLAG-KVREFPKT + WS-LT-BEHOV-DC-DIFF                       
285600                                                                          
285700       COMPUTE WS-KVREFPKT-DUBBEL = SB-SLAG-KVREFPKT * 2                  
285800                                                                          
285900     ELSE                                                                 
286000                                                                          
286100*    LT VID ÖKAD FÖRSÄLJNING                                              
286200*** HÄR BEHÖVS EJ 6 ARBETSDAGAR BARA KINA KOMMER ALDRIG HIT               
286300                                                                          
286400       COMPUTE WS-KVPB-DAG-DC-OKAD = WS-KVOI-INNEV / 5                    
286500                                                                          
286600       COMPUTE WS-LT-BEHOV-DC-OKAD =                                      
286700             B6-REF-KVDLTID-TOT * WS-KVPB-DAG-DC-OKAD                     
286800                                                                          
286900                                                                          
287000*    LT VID NORMAL FÖRSÄLJNING                                            
287100                                                                          
287200       COMPUTE WS-KVPB-DAG-DC-NORM = WS-KVPB-VECKA-DC / 5                 
287300                                                                          
287400       COMPUTE WS-LT-BEHOV-DC-NORM =                                      
287500             B6-REF-KVDLTID-TOT * WS-KVPB-DAG-DC-NORM                     
287600                                                                          
287700                                                                          
287800*--- SKILLNAD                                                             
287900                                                                          
288000       COMPUTE WS-LT-BEHOV-DC-DIFF =                                      
288100             WS-LT-BEHOV-DC-OKAD -      WS-LT-BEHOV-DC-NORM               
288200                                                                          
288300                                                                          
288400*--- TILLFÄLLIG REFILLPUNKT                                               
288500                                                                          
288600       COMPUTE WS-KVREFPKT-DC =                                           
288700             SB-SLAG-KVREFPKT + WS-LT-BEHOV-DC-DIFF                       
288800                                                                          
288900       COMPUTE WS-KVREFPKT-DUBBEL = SB-SLAG-KVREFPKT * 2                  
289000                                                                          
289100     END-IF                                                               
289200     IF WS-KVREFPKT-DC > WS-KVREFPKT-DUBBEL                               
289300        MOVE WS-KVREFPKT-DUBBEL TO WS-KVREFPKT-DC                         
289400     END-IF                                                               
289500     .                                                                    
289600     EJECT                                                                
289700                                                                          
289800                                                                          
289900 S34-BER-DISPONIBELT-DC SECTION.                                          
290000                                                                          
290100     IF DCS-SDC                                                           
290200        COMPUTE WS-KVDISP-DC =  SB-SLAG-KVLS           +                  
290300                                SB-SLAG-KVBEART        +                  
290400                                SB-SLAG-KVAKS-SDC      +                  
290500                                SB-SLAG-KVAKS-PAV      +                  
290600                                WS-BALANCE-ERSATT      -                  
290700                                SB-SLAG-KVOKS-DAG      -                  
290800                                SB-SLAG-KVOKS-BULK     -                  
290900                                SB-SLAG-KVROS-DAG      -                  
291000                                SB-SLAG-KVROS-BULK     -                  
291100                                SB-SLAG-KVSPARR-KVAL                      
291200     END-IF                                                               
291300                                                                          
291400     IF NDC                                                               
291500        COMPUTE WS-KVDISP-DC =  SB-SLAG-KVLS           +                  
291600                                SB-SLAG-KVBEART        +                  
291700                                SB-SLAG-KVAKS-SDC      +                  
291800                                SB-SLAG-KVAKS-PAV      +                  
291900                                WS-BALANCE-ERSATT      -                  
292000                                SB-SLAG-KVOKS-DAG      -                  
292100                                SB-SLAG-KVOKS-BULK     -                  
292200                                SB-SLAG-KVROS-DAG      -                  
292300                                SB-SLAG-KVROS-BULK     -                  
292400                                SB-SLAG-KVSPARR-KVAL -                    
292500                                SB-SLAG-KVRESS         -                  
292600                                WS-KVKUNDRETUR                            
292700     END-IF                                                               
292800     .                                                                    
292900     EJECT                                                                
293000                                                                          
293100                                                                          
293200 S34B-BER-BALANCE-DC SECTION.                                             
293300                                                                          
293400     COMPUTE WS-BALANCE-DC = SB-SLAG-KVLS           -                     
293500                             SB-SLAG-KVOKS-DAG      -                     
293600                             SB-SLAG-KVOKS-BULK     -                     
293700                             SB-SLAG-KVROS-DAG      -                     
293800                             SB-SLAG-KVROS-BULK     -                     
293900                             SB-SLAG-KVSPARR-KVAL                         
294000     .                                                                    
294100     EJECT                                                                
294200                                                                          
294300*THE INT- VALUES ARE FROM IMS-GU-WDK711-INTERN QUERIED WITH ARTNR         
294400*AND SB-SLAG-IDDC-REF                                                     
294500 S34C-CHK-KVDISP-SEND-DC SECTION.                                         
294600                                                                          
294700     MOVE ZEROES               TO WS-KVDISP-SEND-DC                       
294800                                                                          
294900     COMPUTE WS-KVDISP-SEND-DC  = INT-SLAG-KVLS         +                 
295000                                  INT-SLAG-KVBEART      +                 
295100                                  INT-SLAG-KVAKS-SDC    +                 
295200                                  INT-SLAG-KVAKS-PAV    -                 
295300                                  INT-SLAG-KVOKS-DAG    -                 
295400                                  INT-SLAG-KVOKS-BULK   -                 
295500                                  INT-SLAG-KVROS-DAG    -                 
295600                                  INT-SLAG-KVROS-BULK   -                 
295700                                  INT-SLAG-KVSPARR-KVAL -                 
295800                                  INT-SLAG-KVRESS                         
295900     .                                                                    
296000     EJECT                                                                
296100                                                                          
296200                                                                          
296300 S35-BER-ANTAL-V-STORRE-BRIST SECTION.                                    
296400                                                                          
296500     COMPUTE WS-ANTAL-BER =                                               
296600        (WS-KVREFPKT-DC - WS-KVDISP-DC) + SB-SLAG-KVREFBER                
296700     .                                                                    
296800     EJECT                                                                
296900                                                                          
296910 S38-BER-ANTAL-BO SECTION.                                                
296920                                                                          
296930     COMPUTE WS-ANTAL-BER =                                               
296940         SB-SLAG-KVROS-DAG + SB-SLAG-KVROS-BULK                           
296950     .                                                                    
296960     EJECT                                                                
296970                                                                          
297000                                                                          
297100 S36-BER-ANTAL-V-MINDRE-BRIST SECTION.                                    
297200                                                                          
297300     MOVE WS-KVDISP-DC    TO WS-ANTAL-BER                                 
297400                                                                          
297500     IF SB-SLAG-KVREFBER > +0                                             
297600        PERFORM UNTIL WS-ANTAL-BER >= WS-KVREFPKT-DC                      
297700           COMPUTE WS-ANTAL-BER =                                         
297800                   WS-ANTAL-BER + SB-SLAG-KVREFBER                        
297900        END-PERFORM                                                       
298000     ELSE                                                                 
298100        COMPUTE WS-ANTAL-BER = WS-ANTAL-BER + WS-KVREFPKT-DC              
298200     END-IF                                                               
298300     COMPUTE WS-ANTAL-BER = WS-ANTAL-BER - WS-KVDISP-DC                   
298400     .                                                                    
298500     EJECT                                                                
298600                                                                          
298700                                                                          
298800 S37-BER-ANTAL-V-UTRSALDO SECTION.                                        
298900                                                                          
299000     IF OKAD-FORSALJNING                                                  
299100        COMPUTE WS-ANTAL-BER =                                            
299200           (WS-KVREFPKT-DC - WS-KVDISP-DC) + WS-KVOI-INNEV                
299300     ELSE                                                                 
299400        COMPUTE WS-ANTAL-BER =                                            
299500           (WS-KVREFPKT-DC - WS-KVDISP-DC) + WS-KVPB-VECKA-DC             
299600     END-IF                                                               
299700     .                                                                    
299800     EJECT                                                                
299900                                                                          
300000 S45-FLYTTA-ARTIKELADRESS SECTION.                                        
300100                                                                          
300200     IF (NDC AND REF-NDC)                                                 
300300       MOVE INT-SLAG-ADLAGOMR    TO W27110-ADLAGOMR-CDC                   
300400       MOVE INT-SLAG-ADGANG      TO W27110-ADGANG-CDC                     
300500       MOVE INT-SLAG-ADPLATS     TO W27110-ADPLATS-CDC                    
300600     ELSE                                                                 
300700       MOVE CLAG-ADLAGOMR    TO W27110-ADLAGOMR-CDC                       
300800       MOVE CLAG-ADGANG      TO W27110-ADGANG-CDC                         
300900       MOVE CLAG-ADPLATS     TO W27110-ADPLATS-CDC                        
301000     END-IF                                                               
301100     MOVE SB-SLAG-ADLAGOMR TO W27110-ADLAGOMR-SDC                         
301200     MOVE SB-SLAG-ADGANG     TO W27110-ADGANG-SDC                         
301300     MOVE SB-SLAG-ADPLATS    TO W27110-ADPLATS-SDC                        
301400     .                                                                    
301500     EJECT                                                                
301600                                                                          
301700 S47-BER-KVPB-ANTV-DC SECTION.                                            
301800                                                                          
301900       MOVE ZERO   TO WS-KVPB-ANTV-DC                                     
302000       IF DCS-SDC                                                         
302100*                                                                         
302200*        30 VECKORS BEHOV (MAX PÅFYLLNAD)                                 
302300*                                                                         
302400         COMPUTE WS-KVPB-ANTV-DC = WS-KVPB-TOT *                          
302500                                   12 / 52 * 30                           
302600       END-IF                                                             
302700*                                                                         
302800       IF NDC                                                             
302900*                                                                         
303000*        ETT ÅRS BEHOV (MAX PÅFYLLNAD)                                    
303100*                                                                         
303200*        COMPUTE WS-KVPB-ANTV-DC = WS-KVPB-TOT * 12                       
303300         COMPUTE WS-KVPB-ANTV-DC = (SB-SLAG-KVPB-REF +                    
303400                                    SB-SLAG-KVPBREOI) * 12                
303500       END-IF                                                             
303600     .                                                                    
303700     EJECT                                                                
303800                                                                          
303900 S48-HAMTA-REFILLDISTRIKT SECTION.                                        
304000                                                                          
304100     IF (NDC AND REF-NDC)                                                 
304200       MOVE B6-REF-IDDISTR-REFILL TO WS-IDDISTR                           
304300                                     DIST35-IDDISTR                       
304400     ELSE                                                                 
304500       MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                              
304600                                  DIST35-IDDISTR                          
304700     END-IF                                                               
304800                                                                          
304900     .                                                                    
305000     EJECT                                                                
305100                                                                          
305200 S50A-BER-BALANCE-LOCAL SECTION.                                          
305300                                                                          
305400     MOVE ZERO              TO WS-KVKUNDRETUR                             
305500     PERFORM IMS-GU-INLC01                                                
305600     IF SEGMENT-FINNS                                                     
305700*       SAMLA R30(AK PÅ VÄG) OCH 310(AK-NDC)                              
305800        PERFORM IMS-GNP-INLC11                                            
305900        PERFORM UNTIL SEGMENT-SAKNAS                                      
306000           IF (INL-IDPTYP = 'R30'                                         
306100           OR     INL-IDPTYP = 'R31'                                      
306200           OR     INL-IDPTYP = '310')                                     
306300           AND INL-IDDC = SB-SLAG-IDDC                                    
306400           AND INL-KDRT = 7                                               
306500*KDRT = 7 ÄR KUNDRETUR                                                    
306600                                                                          
306700             ADD INL-KVAVIS TO WS-KVKUNDRETUR                             
306800           END-IF                                                         
306900           PERFORM IMS-GNP-INLC11                                         
307000        END-PERFORM                                                       
307100     END-IF                                                               
307200                                                                          
307300*HÄR BORDE ETA LÄSAS                                                      
307400     COMPUTE WS-KVDISP-LOCAL   =                                          
307500             SB-SLAG-KVLS         +                                       
307600             SB-SLAG-KVBEART      +                                       
307700             SB-SLAG-KVAKS-SDC    +                                       
307800             SB-SLAG-KVAKS-PAV    +                                       
307900             WS-BALANCE-ERSATT    -                                       
308000             SB-SLAG-KVOKS-DAG    -                                       
308100             SB-SLAG-KVOKS-BULK -                                         
308200             SB-SLAG-KVROS-DAG    -                                       
308300             SB-SLAG-KVROS-BULK -                                         
308400             SB-SLAG-KVRESS       -                                       
308500             WS-KVKUNDRETUR                                               
308600                                                                          
308700     COMPUTE WS-KVROS-TOT-LOCAL =                                         
308800             SB-SLAG-KVROS-DAG    +                                       
308900             SB-SLAG-KVROS-BULK                                           
309000                                                                          
309100     .                                                                    
309200     EJECT                                                                
309300                                                                          
309400                                                                          
309500 S50B-L-STOCK-KOLL-LOCAL SECTION.                                         
309600                                                                          
309700     IF SB-SLAG-KVDAGAR-MANLT > ZERO                                      
309800       MOVE SB-SLAG-KVDAGAR-MANLT                                         
309900                    TO WS-LOCAL-LEADTIME                                  
310000     ELSE                                                                 
310100                                                                          
310200       MOVE SPACE TO SSA1                                                 
310300       MOVE SB-SLAG-IDLEVNR TO W-IDLEVNR                                  
310400       MOVE SB-SLAG-IDDC      TO W-IDDC                                   
310500       PERFORM IMS-GU-LEVA16                                              
310600       IF SEGMENT-FINNS                                                   
310700          MOVE NDC-KVDAGAR-TBT TO WS-LOCAL-LEADTIME                       
310800       END-IF                                                             
310900     END-IF                                                               
311000                                                                          
311100     IF WS-LOCAL-LEADTIME = ZERO                                          
311200        MOVE 1 TO WS-LOCAL-LEADTIME                                       
311300     END-IF                                                               
311400*    FIXA SASSONG                                                         
311500     COMPUTE WS-KVPB-DAG-LOCAL =                                          
311600              ((SB-SLAG-KVPB-REF / 4.33) / 5)                             
311700                                                                          
311800                                                                          
311900     COMPUTE   WS-KVPB-LOCAL ROUNDED =                                    
312000               WS-KVPB-DAG-LOCAL *                                        
312100               WS-LOCAL-LEADTIME                                          
312200                                                                          
312300     .                                                                    
312400     EJECT                                                                
312500                                                                          
312600                                                                          
312700 S51-L-STOCK-KOLL-CDC--NDC SECTION.                                       
312800                                                                          
312900     PERFORM S55-GODK-E3--RO-CDC--TRANSFER                                
313000     PERFORM S51A-BER-ANK-UNDER-ETA-TID                                   
313100                                                                          
313200*-- NOTE:                                                                 
313300*--  DUE TO PERFORMANCE PROBLEMS                                          
313400*--  WHEN PROCESSING ALL PARTS AND DCS,                                   
313500*--  SECTION S51B- IS ONLY CALLED IF AN                                   
313600*--  AIR-PROPOSAL IS ABOUT TO BE CREATED                                  
313700*--  BEFORE CONSIDERING PRODSTATUSES R U AND P.                           
313800*--  THAT IS, SECTION S51B-                                               
313900*--  IS CALLED DIRECTLY FROM SECTION CC-                                  
314000*--  WHEN PRODSTATUS R U AND P SHOULD BE CHECKED                          
314100*--  BEFORE DECIDING IF AN AIR-PROPOSAL IS ACTUALLY NEEDED                
314200                                                                          
314300     PERFORM S51C-BER-DISPONIBELT-FLYG                                    
314400     PERFORM S51D-BER-BEHOV-FLYG                                          
314500     .                                                                    
314600     EJECT                                                                
314700                                                                          
314800                                                                          
314900 S51A-BER-ANK-UNDER-ETA-TID SECTION.                                      
315000                                                                          
315100*    CALCULATE QTY THAT WILL ARRIVE WITHIN THE LEADTIME                   
315200*    (INCL. PACK-, TRANSPORT- AND BIN-TIME) OF THE AIR ORDER              
315300                                                                          
315400     MOVE 601                TO ETA-KDCALL                                
315500     IF (NDC AND REF-NDC)                                                 
315600       MOVE SB-SLAG-IDDC-REF     TO ETA-IDDC-SEND                         
315700       MOVE SB-SLAG-IDDC         TO ETA-IDDC-REC                          
315800     ELSE                                                                 
315900       MOVE WC-CDC-SE            TO ETA-IDDC-SEND                         
316000       MOVE SB-SLAG-IDDC         TO ETA-IDDC-REC                          
316100     END-IF                                                               
316200     MOVE ZERO               TO ETA-IDARTNR                               
316300                                ETA-KDFRAKT                               
316400     MOVE SPACE              TO ETA-IDLEVNR                               
316500     IF NDC-AU                                                            
316600        MOVE 19              TO ETA-KDFRAKT                               
316700     ELSE                                                                 
316800        MOVE ZERO            TO ETA-KDFRAKT                               
316900     END-IF                                                               
317000     MOVE DAGENS-DATUM       TO ETA-TIAAMMDD-ANROP                        
317100     IF DAGENS-DATUM-AAR < 50                                             
317200       MOVE 20               TO ETA-TISEKEL-ANROP                         
317300     ELSE                                                                 
317400       MOVE 19               TO ETA-TISEKEL-ANROP                         
317500     END-IF                                                               
317600                                                                          
317700     CALL W218ETA USING ETA-W218LETA ETA-ARTC-PCB ETA-ARTS-PCB            
317800                                     ETA-INLC-PCB ETA-LEVA-PCB            
317900                                     WDB6-PCB                             
318000     IF ETA-SVAR-OK = SPACE OR JA                                         
318100        MOVE ETA-TIAAMMDD-SVAR                                            
318200                            TO WS-FLYGT-MAX-TIAAMMDD                      
318300        PERFORM S48-HAMTA-REFILLDISTRIKT                                  
318400        MOVE ZERO           TO WS-KVKUNDRETUR                             
318500        PERFORM IMS-GU-INLC01                                             
318600        IF SEGMENT-FINNS                                                  
318700*          SAMLA R30(AK PÅ VÄG) OCH 310(AK-NDC)                           
318800           PERFORM IMS-GNP-INLC11                                         
318900           PERFORM UNTIL SEGMENT-SAKNAS                                   
319000              IF INL-IDPTYP = 'R30'                                       
319100              OR INL-IDPTYP = 'R31'                                       
319200              OR INL-IDPTYP = '310'                                       
319300                 IF INL-IDDC = SB-SLAG-IDDC                               
319400                   IF INL-KDRT = 7                                        
319500*KDRT = 7 ÄR KUNDRETUR                                                    
319600                     ADD INL-KVAVIS TO WS-KVKUNDRETUR                     
319700                                                                          
319800                   ELSE                                                   
319900                                                                          
320000                     MOVE INL-TIBERANK           TO TMP1-YYMMDD           
320100                     MOVE WS-FLYGT-MAX-TIAAMMDD  TO TMP2-YYMMDD           
320200                     MOVE DAGENS-DATUM           TO TMP3-YYMMDD           
320300                     PERFORM WY2000Q1                                     
320400                     IF TMP1-YYMMDD <= TMP2-YYMMDD                        
320410                     OR (INL-KDFRAKT = 17 OR 18 OR 19)                    
320700                       IF INL-TIINLINL = ZERO                             
320800                          COMPUTE WS-SUANTAL-ETA =                        
320900                                  WS-SUANTAL-ETA + INL-KVAVIS             
321000                       END-IF                                             
321100                     END-IF                                               
321200                     MOVE INL-TIBERANK           TO TMP1-YYMMDD           
321300                     MOVE WS-FLYGT-MAX-TIAAMMDD  TO TMP2-YYMMDD           
321400                     MOVE WS-FIRST-TIBERANK      TO TMP3-YYMMDD           
321500                     PERFORM WY2000Q1                                     
321600                     IF TMP1-YYMMDD > TMP2-YYMMDD                         
321700                       IF INL-TIINLINL = ZERO                             
321800                          IF WS-FIRST-TIBERANK = ZERO                     
321900                             MOVE INL-TIBERANK TO                         
322000                                           WS-FIRST-TIBERANK              
322100                          ELSE                                            
322200                             IF TMP1-YYMMDD < TMP3-YYMMDD                 
322300                                MOVE INL-TIBERANK TO                      
322400                                              WS-FIRST-TIBERANK           
322500                             END-IF                                       
322600                          END-IF                                          
322700                       END-IF                                             
322800                     END-IF                                               
322900                   END-IF                                                 
323000                 END-IF                                                   
323100              END-IF                                                      
323200              PERFORM IMS-GNP-INLC11                                      
323300           END-PERFORM                                                    
323400        END-IF                                                            
323500     ELSE                                                                 
323600        MOVE ' FEL I SUBPGM W218ETA   '                                   
323700                                 TO   FELTEXT-STR                         
323800        DISPLAY FELTEXT                                                   
323900        PERFORM S99-ABEND                                                 
324000     END-IF                                                               
324100     .                                                                    
324200     EJECT                                                                
324300                                                                          
324400 S51B-SUM-OF-PROD-STATUS-R-U-P SECTION.                                   
324500                                                                          
324600     MOVE WS-SPAR-IDARTNR   TO W-Q4-IDARTNR-MIN                           
324700                               W-Q4-IDARTNR-MAX                           
324800                                                                          
324900     PERFORM S48-HAMTA-REFILLDISTRIKT                                     
325000                                                                          
325100*** ADD PRODSTATUS R QUANTITIES FROM WDQ2/WDQ4                            
325200     MOVE ZERO              TO WS-KVBEART-Q-RUP                           
325300     MOVE SB-SLAG-IDDC-REF  TO W-Q4-IDDC                                  
325400     PERFORM IMS-GU-WDQ4B1                                                
325500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
325600        IF  SEQB-IDDISTR  = WS-IDDISTR                                    
325700           MOVE SEQB-IDORDER    TO W-Q2-IDORDER                           
325800           PERFORM IMS-GU-WDQ201                                          
325900           IF  OHUV-KDORDKL    = 1                                        
326000           AND OHUV-FLKLAR     = JA                                       
326100              MOVE SEQB-IDDC   TO W-Q2-IDDC                               
326200              PERFORM IMS-GNP-WDQ212                                      
326300              IF ARB-KDTRPKAT = 'A'                                       
326400                 ADD SEQB-KVBEART-Q  TO WS-KVBEART-Q-RUP                  
326500              END-IF                                                      
326600           END-IF                                                         
326700        END-IF                                                            
326800        PERFORM IMS-GN-WDQ4B1                                             
326900     END-PERFORM                                                          
327000                                                                          
327100*** ADD PRODSTATUS U AND P QUANTITIES FROM WDE4                           
327200     MOVE LOW-VALUE         TO W-WDE4CSEQ-MIN-X                           
327300     MOVE HIGH-VALUE        TO W-WDE4CSEQ-MAX-X                           
327400     MOVE WS-SPAR-IDARTNR   TO W-WDE4C-IDARTNR-MIN                        
327500                               W-WDE4C-IDARTNR-MAX                        
327600     PERFORM IMS-GU-WDE4CSEQ                                              
327700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
327800        IF     ORAD-KDRADSTA < +4                                         
327900*------------  PRODSTATUS U                                               
328000                                                                          
328100        OR (   ORAD-KDRADSTA > +3                                         
328200           AND ORAD-KVAVBART > ZERO)                                      
328300*------------  PRODSTATUS P                                               
328400                                                                          
328500           PERFORM IMS-GNP-WDE401                                         
328600           IF  KORD-IDDISTR = WS-IDDISTR                                  
328700           AND KORD-KDORDKL = 1                                           
328800              IF ORAD-KDRADSTA = +4                                       
328900                 PERFORM IMS-GNP-WDE421                                   
329000                 IF SEGMENT-FINNS                                         
329100                    MOVE KKOLLI-IDPRODNR    TO W-IDPRODNR                 
329200                    MOVE KKOLLI-IDKOLLI     TO W-IDKOLLI                  
329300                    PERFORM IMS-GU-WDE611                                 
329400                    IF SEGMENT-FINNS                                      
329500                       IF KOLLI-KDKOLSTA < 8                              
329600                          ADD ORAD-KVAVBART TO WS-KVBEART-Q-RUP           
329700                       END-IF                                             
329800                    ELSE                                                  
329900                       ADD ORAD-KVAVBART TO WS-KVBEART-Q-RUP              
330000                    END-IF                                                
330100                 ELSE                                                     
330200                    ADD ORAD-KVAVBART TO WS-KVBEART-Q-RUP                 
330300                 END-IF                                                   
330400              ELSE                                                        
330500                 ADD ORAD-KVBEART TO WS-KVBEART-Q-RUP                     
330600              END-IF                                                      
330700                                                                          
330800           END-IF                                                         
330900        END-IF                                                            
331000                                                                          
331100        PERFORM IMS-GN-WDE4CSEQ                                           
331200     END-PERFORM                                                          
331300     .                                                                    
331400     EJECT                                                                
331500                                                                          
331600 S51C-BER-DISPONIBELT-FLYG SECTION.                                       
331700                                                                          
331800     COMPUTE WS-KVDISP-FLYG =                                             
331900          ((SB-SLAG-KVLS + WS-SUANTAL-ETA +                               
332000            WS-ANTAL-RO-RESS-KL1-CDC + WS-ANTAL-GODK-E3 +                 
332100            WS-ANTAL-TRANSF-E3 + WS-BALANCE-ERSATT) -                     
332200                                                                          
332300          (SB-SLAG-KVOKS-DAG + SB-SLAG-KVOKS-BULK +                       
332400           SB-SLAG-KVUTRS    +                                            
332500           SB-SLAG-KVROS-DAG + SB-SLAG-KVROS-BULK))                       
332600     .                                                                    
332700     EJECT                                                                
332800                                                                          
332900                                                                          
333000 S51D-BER-BEHOV-FLYG SECTION.                                             
333100                                                                          
333200*    HUR MYCKET BEHÖVS UNDER FLYGTIDEN PLUS X DAGAR                       
333300     MOVE B6-REF-KVDLTID-AIRREQ     TO DAG-KVKALDAG                       
333400     MOVE DAGENS-DATUM           TO DAG-TIAAMMDD-FOM                      
333500     MOVE 002                    TO DAG-KDCALL                            
333600     CALL WDAGKONV USING DAG-KDCALL                                       
333700                         DAG-DATUM-AREA                                   
333800                         DAG-KDSVAR                                       
333900     IF DAG-KDSVAR = SPACE                                                
334000        MOVE DAG-TIAAMMDD-TOM    TO WS-FLYGBEHOV-MAX-TIAAMMDD             
334100     ELSE                                                                 
334200        MOVE 'FEL FRÅN WDAGKONV, I S51D-SECTION I W27110'                 
334300                                 TO   FELTEXT-STR                         
334400        DISPLAY FELTEXT                                                   
334500        PERFORM S99-ABEND                                                 
334600     END-IF                                                               
334700                                                                          
334800*    I VILKEN PERIOD ÄR GODSET FRAMME                                     
334900     MOVE WS-FLYGBEHOV-MAX-TIAAMMDD TO DAT-I-TIDATUM                      
335000     MOVE 'AAMMDD'                  TO DAT-KDDATFORM                      
335100     CALL WDATKONV USING               DAT-KDDATFORM                      
335200                                       DAT-I-TIDATUM                      
335300                                       DAT-O-TIDATUM                      
335400                                       DAT-KDSVAR                         
335500                                                                          
335600     IF DAT-KDSVAR-OK                                                     
335700        MOVE DAT-TIAARP   TO WS-FLYGBEHOV-MAX-TIAARP                      
335800        IF WS-INNEV-TIAARP = WS-FLYGBEHOV-MAX-TIAARP                      
335900*          SAMMA SOM INNEVARANDE                                          
336000           PERFORM S51D1-BEHOV-FLYGT-INNEV-PER                            
336100        ELSE                                                              
336200*          ELLER NÄSTA                                                    
336300           PERFORM S51D2-BEHOV-FLYGT-NASTA-PER                            
336400        END-IF                                                            
336500     ELSE                                                                 
336600        MOVE 'FEL FRÅN WDATKONV  I S51D SECTION I W27110'                 
336700                              TO   FELTEXT-STR                            
336800        DISPLAY FELTEXT                                                   
336900        PERFORM S99-ABEND                                                 
337000     END-IF                                                               
337100                                                                          
337200     .                                                                    
337300     EJECT                                                                
337400                                                                          
337500                                                                          
337600 S51D1-BEHOV-FLYGT-INNEV-PER SECTION.                                     
337700                                                                          
337800     IF NDC-CN OR NDC-JP OR NDC-IN OR NDC-AE                              
337900***    6 ARBETSDAGAR                                                      
338000       COMPUTE WS-KVPB-DAG-INNEV-RP =                                     
338100                (((SB-SLAG-KVPB-REF *                                     
338200                SB-SLAG-RESEASON(WS-INNEV-TIRP)) / 4.33) / 6)             
338300                                                                          
338400       COMPUTE WS-KVPB-DAG-INNEV-RP = WS-KVPB-DAG-INNEV-RP +              
338500                ((SB-SLAG-KVPBREOI / 4.33) / 6)                           
338600     ELSE                                                                 
338700       COMPUTE WS-KVPB-DAG-INNEV-RP =                                     
338800                (((SB-SLAG-KVPB-REF *                                     
338900                SB-SLAG-RESEASON(WS-INNEV-TIRP)) / 4.33) / 5)             
339000                                                                          
339100       COMPUTE WS-KVPB-DAG-INNEV-RP = WS-KVPB-DAG-INNEV-RP +              
339200                ((SB-SLAG-KVPBREOI / 4.33) / 5)                           
339300     END-IF                                                               
339400                                                                          
339500     COMPUTE   WS-KVPB-FLYG ROUNDED =                                     
339600               WS-KVPB-DAG-INNEV-RP *                                     
339700               B6-REF-KVDLTID-AIRREQ                                      
339800                                                                          
339900     .                                                                    
340000     EJECT                                                                
340100                                                                          
340200                                                                          
340300 S51D2-BEHOV-FLYGT-NASTA-PER SECTION.                                     
340400                                                                          
340500*    GODSET ANKOMMER I NÄSTA PERIOD                                       
340600*       VILKET DATUM BÖRJAR NÄSTA PERIOD                                  
340700     MOVE WS-FLYGBEHOV-MAX-TIAARP TO DAT-I-TIDATUM                        
340800     MOVE 'AARP  '                TO DAT-KDDATFORM                        
340900     CALL WDATKONV USING             DAT-KDDATFORM                        
341000                                     DAT-I-TIDATUM                        
341100                                     DAT-O-TIDATUM                        
341200                                     DAT-KDSVAR                           
341300                                                                          
341400     IF DAT-KDSVAR-OK                                                     
341500        MOVE DAT-TIAAMMDD     TO WS-NASTA-PER-TIAAMMDD                    
341600     ELSE                                                                 
341700        MOVE 'FEL FRÅN WDATKONV, I S51D2-SECTION I W27110'                
341800                                 TO   FELTEXT-STR                         
341900        DISPLAY FELTEXT                                                   
342000        PERFORM S99-ABEND                                                 
342100     END-IF                                                               
342200                                                                          
342300*    HUR MÅNGA KALENDERDAGAR ÄR DET TILLS NÄSTA PERIOD BÖRJAR             
342400     MOVE DAGENS-DATUM          TO DAG-TIAAMMDD-FOM                       
342500     MOVE WS-NASTA-PER-TIAAMMDD TO DAG-TIAAMMDD-TOM                       
342600     MOVE 001                   TO DAG-KDCALL                             
342700     CALL WDAGKONV USING DAG-KDCALL                                       
342800                         DAG-DATUM-AREA                                   
342900                         DAG-KDSVAR                                       
343000     IF DAG-KDSVAR = SPACE                                                
343100        MOVE DAG-KVKALDAG       TO WS-KVDAGAR-FLYG-INNEV                  
343200                                                                          
343300        COMPUTE WS-KVDAGAR-FLYG-NASTA =                                   
343400                B6-REF-KVDLTID-AIRREQ    -                                
343500                WS-KVDAGAR-FLYG-INNEV                                     
343600                                                                          
343700        IF NDC-CN OR NDC-JP OR NDC-IN OR NDC-AE                           
343800***    6 ARBETSDAGAR                                                      
343900          COMPUTE WS-KVPB-DAG-INNEV-RP =                                  
344000                  (((SB-SLAG-KVPB-REF *                                   
344100                  SB-SLAG-RESEASON(WS-INNEV-TIRP)) / 4.33) / 6)           
344200                                                                          
344300          COMPUTE WS-KVPB-DAG-INNEV-RP = WS-KVPB-DAG-INNEV-RP +           
344400                  ((SB-SLAG-KVPBREOI / 4.33) / 6)                         
344500                                                                          
344600          COMPUTE WS-KVPB-DAG-NASTA-RP =                                  
344700                  (((SB-SLAG-KVPB-REF *                                   
344800                  SB-SLAG-RESEASON(WS-NASTA-TIRP)) / 4.33) / 6)           
344900                                                                          
345000          COMPUTE WS-KVPB-DAG-NASTA-RP = WS-KVPB-DAG-NASTA-RP +           
345100                  ((SB-SLAG-KVPBREOI / 4.33) / 6)                         
345200        ELSE                                                              
345300          COMPUTE WS-KVPB-DAG-INNEV-RP =                                  
345400                  (((SB-SLAG-KVPB-REF *                                   
345500                  SB-SLAG-RESEASON(WS-INNEV-TIRP)) / 4.33) / 5)           
345600                                                                          
345700          COMPUTE WS-KVPB-DAG-INNEV-RP = WS-KVPB-DAG-INNEV-RP +           
345800                  ((SB-SLAG-KVPBREOI / 4.33) / 5)                         
345900                                                                          
346000          COMPUTE WS-KVPB-DAG-NASTA-RP =                                  
346100                  (((SB-SLAG-KVPB-REF *                                   
346200                  SB-SLAG-RESEASON(WS-NASTA-TIRP)) / 4.33) / 5)           
346300                                                                          
346400          COMPUTE WS-KVPB-DAG-NASTA-RP = WS-KVPB-DAG-NASTA-RP +           
346500                  ((SB-SLAG-KVPBREOI / 4.33) / 5)                         
346600        END-IF                                                            
346700                                                                          
346800        COMPUTE WS-KVPB-INNEV-RP =                                        
346900                WS-KVDAGAR-FLYG-INNEV * WS-KVPB-DAG-INNEV-RP              
347000                                                                          
347100        COMPUTE WS-KVPB-NASTA-RP =                                        
347200                WS-KVDAGAR-FLYG-NASTA * WS-KVPB-DAG-NASTA-RP              
347300                                                                          
347400        COMPUTE WS-KVPB-FLYG ROUNDED =                                    
347500                WS-KVPB-INNEV-RP + WS-KVPB-NASTA-RP                       
347600     ELSE                                                                 
347700        MOVE 'FEL FRÅN WDATKONV 1  I S51D2 SECTION I W27110'              
347800                                TO  FELTEXT-STR                           
347900        DISPLAY FELTEXT                                                   
348000        PERFORM S99-ABEND                                                 
348100     END-IF                                                               
348200     .                                                                    
348300     EJECT                                                                
348400                                                                          
348500 S52-L-STOCK-ANTAL-CDC SECTION.                                           
348600                                                                          
348700     INITIALIZE W271LTPB-W271LTPB                                         
348800     IF WS-FIRST-TIBERANK > ZERO                                          
348900*       ANTAL BEHOV DAGAR FRAM TILL FÖRSTA INLEVERANS                     
349000        MOVE WS-FIRST-TIBERANK   TO W271LTPB-BINNDAY-TIAAMMDD             
349100        MOVE SB-SLAG-IDDC    TO W271LTPB-IDDC                             
349200        MOVE SB-SLAG-IDDC-REF    TO W271LTPB-IDDC-REF                     
349300        MOVE ZERO            TO W271LTPB-START-DATUM                      
349400     ELSE                                                                 
349500*       BEHOV UNDER BÅTLEDTID                                             
349600        MOVE SB-SLAG-IDDC    TO W271LTPB-IDDC                             
349700        MOVE SB-SLAG-IDDC-REF    TO W271LTPB-IDDC-REF                     
349800        MOVE B6-REF-KVDLTID-TOT TO  W271LTPB-KVDLTID-TOT                  
349900        MOVE B6-REF-KVDLTID-BOATPAC                                       
350000                             TO  W271LTPB-KVDLTID-BOATPAC                 
350100        MOVE B6-REF-KVDLTID-BOATTRP                                       
350200                             TO  W271LTPB-KVDLTID-BOATTRP                 
350300        MOVE B6-REF-KVDLTID-BOAT2DC                                       
350400                             TO  W271LTPB-KVDLTID-BOAT2DC                 
350500        MOVE B6-REF-KVDLTID-BOATINS                                       
350600                             TO  W271LTPB-KVDLTID-BOATINS                 
350700        MOVE B6-REF-KVDLTID-AIRREQ                                        
350800                             TO  W271LTPB-KVDLTID-AIRREQ                  
350900        MOVE B6-REF-KVDLTID-AIRETA                                        
351000                             TO  W271LTPB-KVDLTID-AIRETA                  
351100        MOVE ZERO            TO W271LTPB-START-DATUM                      
351200     END-IF                                                               
351300                                                                          
351400     CALL W271LTPB USING                                                  
351500                       W271LTPB-W271LTPB                                  
351600                                                                          
351700     MOVE W271LTPB-PER-I-TIRP     TO WS-PER-I-TIRP                        
351800     MOVE W271LTPB-PER-II-TIRP    TO WS-PER-II-TIRP                       
351900     MOVE W271LTPB-PER-III-TIRP   TO WS-PER-III-TIRP                      
352000     MOVE W271LTPB-PER-IV-TIRP    TO WS-PER-IV-TIRP                       
352100     MOVE W271LTPB-PER-I-TIRP     TO WS-PER-V-TIRP                        
352200     MOVE W271LTPB-PER-VI-TIRP    TO WS-PER-VI-TIRP                       
352300*      SUMMERA BEHOV FÖR DE ARBETSDAGAR SOM LIGGER INOM                   
352400*      LEDTIDEN                                                           
352500                                                                          
352600****INGEN SÄSONGSANPASSNING PÅ KVPBREOI                                   
352700     COMPUTE WS-KVPBREOI-DAY =                                            
352800        ((SB-SLAG-KVPBREOI / 4.33) / 7)                                   
352900                                                                          
353000     COMPUTE WS-KVPB-REF-DAY-PER-I =                                      
353100       (((SB-SLAG-KVPB-REF *                                              
353200       SB-SLAG-RESEASON(W271LTPB-PER-I-TIRP)) / 4.33) / 7)                
353300       + WS-KVPBREOI-DAY                                                  
353400                                                                          
353500     COMPUTE WS-KVPB-REF-DAY-PER-II =                                     
353600        (((SB-SLAG-KVPB-REF *                                             
353700        SB-SLAG-RESEASON(W271LTPB-PER-II-TIRP)) / 4.33) / 7)              
353800       + WS-KVPBREOI-DAY                                                  
353900                                                                          
354000     COMPUTE WS-KVPB-REF-DAY-PER-III =                                    
354100         (((SB-SLAG-KVPB-REF *                                            
354200        SB-SLAG-RESEASON(W271LTPB-PER-III-TIRP)) / 4.33) / 7)             
354300       + WS-KVPBREOI-DAY                                                  
354400                                                                          
354500     COMPUTE WS-KVPB-REF-DAY-PER-IV =                                     
354600         (((SB-SLAG-KVPB-REF *                                            
354700        SB-SLAG-RESEASON(W271LTPB-PER-IV-TIRP)) / 4.33) / 7)              
354800       + WS-KVPBREOI-DAY                                                  
354900                                                                          
355000     COMPUTE WS-KVPB-REF-DAY-PER-V =                                      
355100         (((SB-SLAG-KVPB-REF *                                            
355200        SB-SLAG-RESEASON(W271LTPB-PER-V-TIRP)) / 4.33) / 7)               
355300       + WS-KVPBREOI-DAY                                                  
355400                                                                          
355500     COMPUTE WS-KVPB-REF-DAY-PER-VI =                                     
355600         (((SB-SLAG-KVPB-REF *                                            
355700        SB-SLAG-RESEASON(W271LTPB-PER-VI-TIRP)) / 4.33) / 7)              
355800       + WS-KVPBREOI-DAY                                                  
355900                                                                          
356000     COMPUTE WS-ANTAL-LEDT-FLYG ROUNDED =                                 
356100        (W271LTPB-KVDAGAR-PER-I * WS-KVPB-REF-DAY-PER-I)     +            
356200        (W271LTPB-KVDAGAR-PER-II * WS-KVPB-REF-DAY-PER-II)   +            
356300        (W271LTPB-KVDAGAR-PER-III * WS-KVPB-REF-DAY-PER-III) +            
356400        (W271LTPB-KVDAGAR-PER-IV  * WS-KVPB-REF-DAY-PER-IV)  +            
356500        (W271LTPB-KVDAGAR-PER-V   * WS-KVPB-REF-DAY-PER-V)   +            
356600        (W271LTPB-KVDAGAR-PER-VI  * WS-KVPB-REF-DAY-PER-VI)               
356700                                                                          
356800     COMPUTE WS-ANTAL-FLYG               =                                
356900                WS-ANTAL-LEDT-FLYG       +                                
357000                SB-SLAG-KVOKS-DAG        +                                
357100                SB-SLAG-KVOKS-BULK       +                                
357200                SB-SLAG-KVROS-DAG        +                                
357300                SB-SLAG-KVROS-BULK       -                                
357400                WS-ANTAL-RO-RESS-KL1-CDC -                                
357500                SB-SLAG-KVLS             -                                
357600                WS-SUANTAL-ETA           -                                
357700                WS-ANTAL-GODK-E3         -                                
357800                WS-KVBEART-Q-RUP                                          
357900                                                                          
358000     IF WS-ANTAL-FLYG < ZERO                                              
358100        IF WS-KVPB-TOT  = ZERO   AND                                      
358200           SB-SLAG-KVROS-DAG > ZERO                                       
358300           MOVE SB-SLAG-KVROS-DAG TO WS-ANTAL-BER                         
358400        ELSE                                                              
358500           MOVE ZERO        TO WS-ANTAL-BER                               
358600        END-IF                                                            
358700     ELSE                                                                 
358800        MOVE WS-ANTAL-FLYG    TO WS-ANTAL-FLYG-AVRUND                     
358900        IF WS-ANTAL-FLYG-AVRUND-HELTAL = ZERO                             
359000           IF WS-ANTAL-FLYG-AVRUND-DECTAL < 4                             
359100              CONTINUE                                                    
359200           ELSE                                                           
359300              MOVE 1 TO WS-ANTAL-FLYG-AVRUND-HELTAL                       
359400           END-IF                                                         
359500        ELSE                                                              
359600           IF WS-ANTAL-FLYG-AVRUND-DECTAL < 7                             
359700              CONTINUE                                                    
359800           ELSE                                                           
359900              ADD 1 TO WS-ANTAL-FLYG-AVRUND-HELTAL                        
360000           END-IF                                                         
360100        END-IF                                                            
360200        MOVE WS-ANTAL-FLYG-AVRUND-HELTAL  TO WS-ANTAL-BER                 
360300     END-IF                                                               
360400     .                                                                    
360500     EJECT                                                                
360600                                                                          
360700                                                                          
360800 S53-KOLLA-TRANSFER-NDC SECTION.                                          
360900                                                                          
361000                                                                          
361100     MOVE 1 TO IX                                                         
361200     MOVE 41 TO DC-IX                                                     
361300     PERFORM UNTIL IX > 4 OR TRANSFER                                     
361400        IF DC-IX = SB-SLAG-IDDC                                           
361500           CONTINUE                                                       
361600        ELSE                                                              
361700           MOVE DC-IX TO WS-IDDC-TRANSF                                   
361800                         W-IDDC-TRANSF                                    
361900                                                                          
362000           PERFORM IMS-GU-WDK711-TRANSF                                   
362100              IF SEGMENT-FINNS                                            
362200              COMPUTE WS-KVPB-VECKA-DC-IX =                               
362300                      TRANSF-SLAG-KVPB-REF / 4.33                         
362400                                                                          
362500              IF WS-KVPB-VECKA-DC-IX = ZERO                               
362600                 MOVE 1 TO WS-KVPB-VECKA-DC-IX                            
362700              END-IF                                                      
362800              COMPUTE WS-SUPERWEEK =                                      
362900               (((TRANSF-SLAG-KVLS)                                       
363000                 -                                                        
363100               (TRANSF-SLAG-KVOKS-DAG + TRANSF-SLAG-KVOKS-BULK +          
363200                TRANSF-SLAG-KVROS-DAG + TRANSF-SLAG-KVROS-BULK +          
363300                TRANSF-SLAG-KVSPARR-KVAL))                                
363400                   /                                                      
363500                   WS-KVPB-VECKA-DC-IX )                                  
363600              IF WS-SUPERWEEK > ZERO                                      
363700                 IF NDC-US                                                
363800                    IF DC-IX < 44                                         
363900                       IF CLAG-KDPRISKL < 6                               
364000                          IF WS-SUPERWEEK > +52                           
364100                             MOVE JA TO TRANSFER-SW                       
364200                          END-IF                                          
364300                       ELSE                                               
364400                          IF WS-SUPERWEEK > +39                           
364500                             MOVE JA TO TRANSFER-SW                       
364600                          END-IF                                          
364700                       END-IF                                             
364800*                   ELSE                                                  
364900*                      IF DC-IX = 51                                      
365000*                         IF CLAG-KDPRISKL < 6                            
365100*                            IF WS-SUPERWEEK > +52                        
365200*                               MOVE JA TO TRANSFER-SW                    
365300*                            END-IF                                       
365400*                         ELSE                                            
365500*                            IF WS-SUPERWEEK > +26                        
365600*                               MOVE JA TO TRANSFER-SW                    
365700*                            END-IF                                       
365800*                         END-IF                                          
365900*                      END-IF                                             
366000                    END-IF                                                
366100                 END-IF                                                   
366200                 IF NDC-CA                                                
366300                    IF DC-IX < 44                                         
366400                       IF CLAG-KDPRISKL < 6                               
366500                          IF WS-SUPERWEEK > +52                           
366600                             MOVE JA TO TRANSFER-SW                       
366700                          END-IF                                          
366800                       ELSE                                               
366900                          IF WS-SUPERWEEK > +26                           
367000                             MOVE JA TO TRANSFER-SW                       
367100                          END-IF                                          
367200                       END-IF                                             
367300                    END-IF                                                
367400                 END-IF                                                   
367500              END-IF                                                      
367600           END-IF                                                         
367700        END-IF                                                            
367800        ADD 1 TO IX                                                       
367900        IF DC-IX < 43                                                     
368000           ADD 1 TO DC-IX                                                 
368100        ELSE                                                              
368200           MOVE 51 TO DC-IX                                               
368300        END-IF                                                            
368400     END-PERFORM                                                          
368500     .                                                                    
368600     EJECT                                                                
368700                                                                          
368800                                                                          
368900 S54-ERSATTNINGAR-NDC SECTION.                                            
369000                                                                          
369100     MOVE JA TO ERSATT-SW                                                 
369200     MOVE ZERO TO WS-ANTAL-SLUT                                           
369300      .                                                                   
369400     EJECT                                                                
369500                                                                          
369600                                                                          
369700 S55-GODK-E3--RO-CDC--TRANSFER SECTION.                                   
369800                                                                          
369900*       GODKÄNDA FLYGFÖRSLAG                                              
370000     MOVE ZERO TO WS-ANTAL-GODK-E3                                        
370100     MOVE SB-SLAG-IDDC           TO W-IDDC                                
370200                                    W-IDDC-MIN                            
370300                                    W-IDDC-MAX                            
370400     MOVE ZERO                   TO W-IDPERSON-BUY-MIN                    
370500     MOVE 999                    TO W-IDPERSON-BUY-MAX                    
370600                                                                          
370700     MOVE 'A'                    TO W-KDREFTYP-MIN                        
370800     MOVE 'T'                    TO W-KDREFTYP-MAX                        
370900     MOVE WS-SPAR-IDARTNR        TO W-IDARTNR                             
371000                                                                          
371100     PERFORM IMS-GU-ORDL01                                                
371200     PERFORM UNTIL SEGMENT-SAKNAS                                         
371300        IF REF-KDREFTYP = 'A'                                             
371400        OR REF-KDREFTYP = 'C'                                             
371500           IF REF-KDREFORS = 'O'                                          
371600              COMPUTE WS-ANTAL-GODK-E3 =                                  
371700                      WS-ANTAL-GODK-E3 + REF-KVBEART                      
371800           END-IF                                                         
371900        END-IF                                                            
372000        IF REF-KDREFTYP = 'O'                                             
372100           COMPUTE WS-ANTAL-GODK-E3 =                                     
372200                   WS-ANTAL-GODK-E3 + REF-KVBEART                         
372300        END-IF                                                            
372400        IF REF-KDREFTYP = 'T'                                             
372500           COMPUTE WS-ANTAL-TRANSF-E3 =                                   
372600                   WS-ANTAL-TRANSF-E3 + REF-KVBEART                       
372700        END-IF                                                            
372800        PERFORM IMS-GN-ORDL01                                             
372900     END-PERFORM                                                          
373000                                                                          
373100*                                                                         
373200     IF (REF-CDC-SE AND                                                   
373300        (CLAG-KVROS > ZERO OR CLAG-KVRESS > ZERO))                        
373400     OR ((NDC AND REF-NDC) AND                                            
373500        (INT-SLAG-KVROS-BULK > 0 OR INT-SLAG-KVROS-DAG > 0 OR             
373600         INT-SLAG-KVRESS > 0))                                            
373700        PERFORM S48-HAMTA-REFILLDISTRIKT                                  
373800        MOVE WS-IDDISTR TO W-IDDISTR-A5D-MIN                              
373900                           W-IDDISTR-A5D-MAX                              
374000        PERFORM IMS-GU-ORDT01                                             
374100        PERFORM UNTIL SEGMENT-SAKNAS                                      
374200           IF SEQD-KDORDKL = 1                                            
374300              IF SEQD-KDSTARAD = '2' OR '3'                               
374400                 MOVE SEQD-IDWDA501 TO W-WDA501KY                         
374500                 PERFORM IMS-GU-ORDP01                                    
374600                 IF SEGMENT-FINNS                                         
374700                    COMPUTE WS-ANTAL-RO-RESS-KL1-CDC =                    
374800                            WS-ANTAL-RO-RESS-KL1-CDC +                    
374900                            RAD-KVRO                                      
375000                 END-IF                                                   
375100              END-IF                                                      
375200           END-IF                                                         
375300           PERFORM IMS-GN-ORDT01                                          
375400        END-PERFORM                                                       
375500     END-IF                                                               
375600     .                                                                    
375700     EJECT                                                                
375800                                                                          
375900                                                                          
376000 S56-Q-ANPASSA SECTION.                                                   
376100                                                                          
376200     MOVE ZERO               TO WS-KVANT-QX                               
376300     MOVE WS-ANTAL-BER       TO WS-ANTAL-QX                               
376400                                                                          
376500     IF CLAG-KVQPACK-4 > 1                                                
376600     OR WS-KVQPACK-3   > 1                                                
376700     OR CLAG-KVQPACK-2 > 1                                                
376800     OR CLAG-KVQPACK-1 > 1                                                
376900     OR CLAG-KVQPACK-0 > 1                                                
377000                                                                          
377100*KVQPACK-4                                                                
377200       IF CLAG-KVQPACK-4 > 1                                              
377300         IF DCS-SDC                                                       
377400         OR NDC-US-LA                                                     
377500         OR NDC-JP                                                        
377600         OR NDC-IN                                                        
377700         OR NDC-NX                                                        
377800         OR NDC-NS                                                        
377900           MOVE ZERO               TO WS-KVANT-QX-HELTAL                  
378000         ELSE                                                             
378100           COMPUTE WS-KVANT-QX =                                          
378200                   WS-ANTAL-BER / CLAG-KVQPACK-4                          
378300           IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                         
378400             IF WS-KVANT-QX-HELTAL > ZERO                                 
378500               COMPUTE WS-ANTAL-QX =                                      
378600                       WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                
378700             END-IF                                                       
378800           ELSE                                                           
378900             COMPUTE WS-KVANT-QX-HELTAL =                                 
379000                     WS-KVANT-QX-HELTAL + 1                               
379100             COMPUTE WS-ANTAL-QX =                                        
379200                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-4                  
379300           END-IF                                                         
379400         END-IF                                                           
379500       END-IF                                                             
379600                                                                          
379700*KVQPACK-3                                                                
379800       IF WS-KVQPACK-3   > 1  AND                                         
379900          WS-KVANT-QX-HELTAL = ZERO                                       
380000         COMPUTE WS-KVANT-QX =                                            
380100                 WS-ANTAL-BER / WS-KVQPACK-3                              
380200         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
380300           IF WS-KVANT-QX-HELTAL > ZERO                                   
380400             COMPUTE WS-ANTAL-QX =                                        
380500                     WS-KVANT-QX-HELTAL * WS-KVQPACK-3                    
380600           END-IF                                                         
380700         ELSE                                                             
380800           COMPUTE WS-KVANT-QX-HELTAL =                                   
380900                   WS-KVANT-QX-HELTAL + 1                                 
381000           COMPUTE WS-ANTAL-QX =                                          
381100                   WS-KVANT-QX-HELTAL * WS-KVQPACK-3                      
381200         END-IF                                                           
381300       END-IF                                                             
381400                                                                          
381500*KVQPACK-2                                                                
381600       IF CLAG-KVQPACK-2 > 1  AND                                         
381700          WS-KVANT-QX-HELTAL = ZERO                                       
381800         COMPUTE WS-KVANT-QX =                                            
381900                 WS-ANTAL-BER / CLAG-KVQPACK-2                            
382000         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
382100           IF WS-KVANT-QX-HELTAL > ZERO                                   
382200             COMPUTE WS-ANTAL-QX =                                        
382300                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                  
382400           END-IF                                                         
382500         ELSE                                                             
382600           COMPUTE WS-KVANT-QX-HELTAL =                                   
382700                   WS-KVANT-QX-HELTAL + 1                                 
382800           COMPUTE WS-ANTAL-QX =                                          
382900                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-2                    
383000         END-IF                                                           
383100       END-IF                                                             
383200                                                                          
383300*KVQPACK-0                                                                
383400       IF CLAG-KVQPACK-0 > 1   AND                                        
383500          WS-KVANT-QX-HELTAL = ZERO                                       
383600         COMPUTE WS-KVANT-QX =                                            
383700                 WS-ANTAL-BER / CLAG-KVQPACK-0                            
383800         IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                           
383900           IF WS-KVANT-QX-HELTAL < 1                                      
384000             IF CLAG-KVQPACK-1 > 0                                        
384100               PERFORM S56B-KVQPACK-1                                     
384200             ELSE                                                         
384300               MOVE WS-ANTAL-BER TO WS-ANTAL-QX                           
384400             END-IF                                                       
384500           ELSE                                                           
384600             COMPUTE WS-ANTAL-QX =                                        
384700                     WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                  
384800           END-IF                                                         
384900         ELSE                                                             
385000           COMPUTE WS-KVANT-QX-HELTAL =                                   
385100                   WS-KVANT-QX-HELTAL + 1                                 
385200           COMPUTE WS-ANTAL-QX =                                          
385300                   WS-KVANT-QX-HELTAL * CLAG-KVQPACK-0                    
385400         END-IF                                                           
385500       END-IF                                                             
385600                                                                          
385700*KVQPACK-1                                                                
385800       IF CLAG-KVQPACK-1 > 1                                              
385900          IF WS-KVANT-QX-HELTAL > ZERO                                    
386000             MOVE WS-ANTAL-QX TO WS-ANTAL-BER                             
386100          END-IF                                                          
386200          PERFORM S56B-KVQPACK-1                                          
386300       END-IF                                                             
386400     END-IF                                                               
386500     MOVE WS-ANTAL-QX TO WS-ANTAL-SLUT                                    
386600                                                                          
386700     .                                                                    
386800     EJECT                                                                
386900                                                                          
387000 S56B-KVQPACK-1 SECTION.                                                  
387100                                                                          
387200     COMPUTE WS-KVANT-QX =                                                
387300             WS-ANTAL-BER / CLAG-KVQPACK-1                                
387400     IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                               
387500       IF WS-KVANT-QX-HELTAL < 1                                          
387600         COMPUTE WS-ANTAL-QX = 1 * CLAG-KVQPACK-1                         
387700       ELSE                                                               
387800         COMPUTE WS-ANTAL-QX =                                            
387900                 WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                      
388000       END-IF                                                             
388100     ELSE                                                                 
388200       COMPUTE WS-KVANT-QX-HELTAL =                                       
388300               WS-KVANT-QX-HELTAL + 1                                     
388400       COMPUTE WS-ANTAL-QX =                                              
388500               WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                        
388600     END-IF                                                               
388700     .                                                                    
388800     EJECT                                                                
388900                                                                          
389000 S56C-SATT-QX-PROCENT-BRYTNING SECTION.                                   
389100                                                                          
389200     MOVE DCS-REQXBRYT       TO WS-QX-BRYTNING                            
389300     .                                                                    
389400     EJECT                                                                
389500                                                                          
389600                                                                          
389700 S57-RANGORDNA-REFTEXT SECTION.                                           
389800                                                                          
389900     IF SB-SLAG-FLREFBEO = NEJ                                            
390000     OR SB-SLAG-FLREFBEO = 'S'                                            
390100        MOVE '70'                   TO W27110-KDREFTXT                    
390200     END-IF                                                               
390300                                                                          
390400     IF SB-SLAG-RETREND > 1.20                                            
390500     AND NOT NDC-NA                                                       
390600        MOVE '60'                   TO W27110-KDREFTXT                    
390700     END-IF                                                               
390800                                                                          
390900     IF SB-SLAG-RETREND-REOI > 1.20                                       
391000     AND NOT NDC-NA                                                       
391100        MOVE '60'                   TO W27110-KDREFTXT                    
391200     END-IF                                                               
391300                                                                          
391400     IF TRANSFER                                                          
391500        MOVE '55'                   TO W27110-KDREFTXT                    
391600     END-IF                                                               
391700                                                                          
391800     IF SB-SLAG-KVUTRS > 0                                                
391900        MOVE '50'                   TO W27110-KDREFTXT                    
392000     END-IF                                                               
392100                                                                          
392200     IF TILLKOMMANDE                                                      
392300        MOVE '15'                   TO W27110-KDREFTXT                    
392400     END-IF                                                               
392500                                                                          
392600     IF ((ERSATT) AND (CLAG-KDERS = 14 OR 18 OR 24))                      
392700        MOVE '10'                   TO W27110-KDREFTXT                    
392800     END-IF                                                               
392900                                                                          
393000     IF L-STOCK-LOCAL                                                     
393100        MOVE '06'                   TO W27110-KDREFTXT                    
393200     END-IF                                                               
393300                                                                          
393400     IF NO-PRICE-LOCAL                                                    
393500        MOVE '05'                   TO W27110-KDREFTXT                    
393600     END-IF                                                               
393700                                                                          
393800     IF (NDC AND REF-NDC)                                                 
393900       MOVE INT-SLAG-KVSPARR-KVAL TO WS-KVSPARR-KVAL                      
394000     ELSE                                                                 
394100       MOVE CLAG-KVSPARR-KVAL     TO WS-KVSPARR-KVAL                      
394200     END-IF                                                               
394300     IF WS-KVSPARR-KVAL > 0                                               
394400        MOVE '03'                   TO W27110-KDREFTXT                    
394500     END-IF                                                               
394600     .                                                                    
394700     EJECT                                                                
394800                                                                          
394900 S58-KOLLA-PRIS-LOKALA SECTION.                                           
395000     MOVE NEJ                  TO NO-PRICE-LOCAL-SW                       
395100     IF DCS-CHINA OR DCS-USA                                              
395200       PERFORM S58A-KOLLA-PRIS-LOKALA                                     
395300     ELSE                                                                 
395400       PERFORM S58B-KOLLA-PRIS-LOKALA                                     
395500     END-IF                                                               
395600     .                                                                    
395700     EJECT                                                                
395800                                                                          
395900 S58A-KOLLA-PRIS-LOKALA SECTION.                                          
396000     PERFORM IMS-GU-WDK711-LOCAL                                          
396100     IF SEGMENT-FINNS                                                     
396200        MOVE SB-SLAG-IDLEVNR TO W-IDLEVNR-PR                              
396300        COMPUTE W-DAPRLIST-PR = 99999999 - W-DAGENS-DATUM                 
396400        PERFORM IMS-GNP-WDK724                                            
396500        IF SEGMENT-FINNS                                                  
396600          IF SPRL-PRARTBEL-PR = ZERO                                      
396700            MOVE JA TO NO-PRICE-LOCAL-SW                                  
396800          END-IF                                                          
396900        ELSE                                                              
397000           MOVE JA             TO NO-PRICE-LOCAL-SW                       
397100        END-IF                                                            
397200     ELSE                                                                 
397300        MOVE JA                TO NO-PRICE-LOCAL-SW                       
397400     END-IF                                                               
397500     .                                                                    
397600     EJECT                                                                
397700                                                                          
397800 S58B-KOLLA-PRIS-LOKALA SECTION.                                          
397900     PERFORM IMS-GU-ARTC11                                                
398000     IF SEGMENT-FINNS                                                     
398100        MOVE SB-SLAG-IDLEVNR TO W-IDLEVNR-21                              
398200        COMPUTE W-DAPRLIST-21 = 99999999 - W-DAGENS-DATUM                 
398300        PERFORM IMS-GNP-ARTC21                                            
398400        IF SEGMENT-FINNS                                                  
398500          IF PRL-PRARTBEL-PR = ZERO                                       
398600            MOVE JA            TO NO-PRICE-LOCAL-SW                       
398700          END-IF                                                          
398800        ELSE                                                              
398900           MOVE JA             TO NO-PRICE-LOCAL-SW                       
399000        END-IF                                                            
399100     ELSE                                                                 
399200        MOVE JA                TO NO-PRICE-LOCAL-SW                       
399300     END-IF                                                               
399400     .                                                                    
399500     EJECT                                                                
399600                                                                          
399700 S59-BER-BALANCE-ERSATT SECTION.                                          
399800     MOVE ZERO TO WS-BALANCE-ERSATT                                       
399900     IF WDK6-ART-FLERS = JA                                               
400000        MOVE WS-SPAR-IDARTNR TO W-IDARTNR-TILLK-MIN                       
400100                                W-IDARTNR-TILLK-MAX                       
400200        PERFORM IMS-GU-ERSB01                                             
400300        IF SEGMENT-FINNS                                                  
400400           MOVE ERSB-ERS-IDARTNR TO W-IDARTNR-ERS                         
400500           PERFORM IMS-GU-ART-ERS                                         
400600           IF SEGMENT-FINNS                                               
400700              IF ERS-ART-KDERS-UTG = ZERO                                 
400800                 PERFORM IMS-GNP-CLAG-ERS                                 
400900                 IF SEGMENT-FINNS                                         
401000                    MOVE JA TO  ERS-WDK611-SW                             
401100*FIX FÖR ATT FÅ ORDRAR OCH FÖRSLAG PÅ DE ERSÄTTANDE ARTIKLAR              
401200*SOM FELAKTIGT STARTATS I KONVERTERINGEN FÖRE DE ERSATTA                  
401300*ERSÄTTNINGSPASSIVERATS, TILLGÅNG PÅ DEN ERSATTA SKALL EJ                 
401400*MEDRÄKNAS                                                                
401500                    IF SB-SLAG-KVROS-DAG > 0                              
401600                    OR SB-SLAG-KVROS-BULK > 0                             
401700                       CONTINUE                                           
401800                    ELSE                                                  
401900*SLUTFIX                                                                  
402000                       IF ERS-CLAG-KDERS = 01                             
402100                                        OR 11                             
402200                                        OR 17                             
402300                                        OR 21                             
402400                                        OR 27                             
402500                          MOVE SB-SLAG-IDDC TO W-IDDC-ERS                 
402600                          PERFORM IMS-GU-WDK711-ERS                       
402700                          IF SEGMENT-FINNS                                
402800                             COMPUTE WS-BALANCE-ERSATT =                  
402900                                        ERS-SLAG-KVLS     +               
403000                                        ERS-SLAG-KVBEART  +               
403100                                        ERS-SLAG-KVAKS-PAV +              
403200                                        ERS-SLAG-KVAKS-SDC -              
403300                                        ERS-SLAG-KVRESS   -               
403400                                        ERS-SLAG-KVROS-DAG -              
403500                                        ERS-SLAG-KVROS-BULK -             
403600                                        ERS-SLAG-KVOKS-DAG -              
403700                                        ERS-SLAG-KVOKS-BULK               
403800                          END-IF                                          
403900                       END-IF                                             
404000                    END-IF                                                
404100                 END-IF                                                   
404200              END-IF                                                      
404300           END-IF                                                         
404400        END-IF                                                            
404500     END-IF                                                               
404600     .                                                                    
404700     EJECT                                                                
404800                                                                          
404900 S61-KOLLA-TRANSFER-OVR SECTION.                                          
405000                                                                          
405100     MOVE NEJ                TO TRANSFER-GRP-SW                           
405200                                                                          
405300     MOVE 1                  TO GRP-IX                                    
405400                                                                          
405500     PERFORM UNTIL GRP-IX > DC-TRANSGRP-MAX                               
405600     OR WS-IDDC-REC (GRP-IX) = SPACE                                      
405700     OR WS-IDDC-REC (GRP-IX) = SB-SLAG-IDDC                               
405800                                                                          
405900       ADD 1                 TO GRP-IX                                    
406000                                                                          
406100     END-PERFORM                                                          
406200                                                                          
406300     IF GRP-IX > DC-TRANSGRP-MAX                                          
406400     OR WS-IDDC-REC (GRP-IX) = SPACE                                      
406500       CONTINUE                                                           
406600     ELSE                                                                 
406700       MOVE JA               TO TRANSFER-GRP-SW                           
406800     END-IF                                                               
406900                                                                          
407000                                                                          
407100     IF TRANSFER-GRP                                                      
407200                                                                          
407300       MOVE 1 TO DC-IX                                                    
407400       PERFORM UNTIL DC-IX > DC-TRANS-MAX                                 
407500       OR WS-IDDC-SEND (GRP-IX, DC-IX) = SPACE                            
407600       OR TRANSFER                                                        
407700         MOVE WS-IDDC-SEND (GRP-IX, DC-IX)                                
407800                           TO WS-IDDC-TRANSF                              
407900                              W-IDDC-TRANSF                               
408000                                                                          
408100         PERFORM IMS-GU-WDK711-TRANSF                                     
408200         IF SEGMENT-FINNS                                                 
408300            COMPUTE WS-KVPB-VECKA-DC-IX =                                 
408400                   (TRANSF-SLAG-KVPB-REF + TRANSF-SLAG-KVPBREOI)          
408500                    / 4.33                                                
408600                                                                          
408700            IF WS-KVPB-VECKA-DC-IX = ZERO                                 
408800               MOVE 1 TO WS-KVPB-VECKA-DC-IX                              
408900            END-IF                                                        
409000            COMPUTE WS-SUPERWEEK =                                        
409100             (((TRANSF-SLAG-KVLS)                                         
409200               -                                                          
409300             (TRANSF-SLAG-KVOKS-DAG + TRANSF-SLAG-KVOKS-BULK +            
409400              TRANSF-SLAG-KVROS-DAG + TRANSF-SLAG-KVROS-BULK +            
409500              TRANSF-SLAG-KVSPARR-KVAL))                                  
409600                 /                                                        
409700                 WS-KVPB-VECKA-DC-IX )                                    
409800            IF NDC-CN                                                     
409900              IF CLAG-KDPRISKL < 6                                        
410000                 IF WS-SUPERWEEK > +52                                    
410100                    MOVE JA TO TRANSFER-SW                                
410200                 END-IF                                                   
410300              ELSE                                                        
410400                 IF WS-SUPERWEEK > +39                                    
410500                    MOVE JA TO TRANSFER-SW                                
410600                 END-IF                                                   
410700              END-IF                                                      
410800            ELSE                                                          
410900              IF WS-SUPERWEEK > +52                                       
411000                 MOVE JA TO TRANSFER-SW                                   
411100              END-IF                                                      
411200            END-IF                                                        
411300         END-IF                                                           
411400         ADD 1 TO DC-IX                                                   
411500       END-PERFORM                                                        
411600     END-IF                                                               
411700     .                                                                    
411800     EJECT                                                                
411900                                                                          
412000                                                                          
412100 S70-SUMMERA-ORDRAR SECTION.                                              
412200                                                                          
412300     MOVE 'W27110'               TO POSTSUM-FDNAMN                        
412400     MOVE 'DC'                   TO POSTSUM-TRANSTYP (1:2)                
412500     MOVE W27110-IDDC            TO POSTSUM-TRANSTYP (3:2)                
412600     EVALUATE W27110-KDREFTYP                                             
412700        WHEN 'A'                                                          
412800              MOVE 'FLYG '       TO POSTSUM-DDNAMN2                       
412900        WHEN 'C'                                                          
413000              MOVE 'FLYG '       TO POSTSUM-DDNAMN2                       
413100        WHEN 'B'                                                          
413200              MOVE 'BÅT '        TO POSTSUM-DDNAMN2                       
413300        WHEN 'L'                                                          
413400              MOVE 'LOKAL '      TO POSTSUM-DDNAMN2                       
413500        WHEN 'O'                                                          
413600              MOVE 'REFILL  '    TO POSTSUM-DDNAMN2                       
413700     END-EVALUATE                                                         
413800     .                                                                    
413900     EJECT                                                                
414000                                                                          
414100 S75-SEND-DCLAND SECTION.                                                 
414200                                                                          
414300     MOVE SPACES                   TO WS-IDLAND-REF                       
414400     IF REF-NDC                                                           
414500       SEARCH ALL DC-LAND                                                 
414600         AT END                                                           
414700           MOVE SPACE        TO WS-IDLAND-REF                             
414800         WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC-LAND-SEND                 
414900           MOVE DCLAND-IDLANDX2(DCLAND-IX) TO WS-IDLAND-REF               
415000       END-SEARCH                                                         
415100     END-IF                                                               
415200     .                                                                    
415300     EJECT                                                                
415400                                                                          
415500 S80-CURR-WEEK-PLUS-LT SECTION.                                           
415600                                                                          
415700*    CALCULATE DATUM TVA IN WEEK FORMAT                                   
415800*    ADD LEATTIME. MINIMUM 2 WEEKS                                        
415900                                                                          
416000     MOVE ZERO             TO WS-LT-WEEKS                                 
416100                              WS-REST-DAYS                                
416200                                                                          
416300     IF SB-SLAG-FLFLYG = JA                                               
416400        DIVIDE  WS-LTID-A  BY 7                                           
416500                       GIVING WS-LT-WEEKS                                 
416600                    REMAINDER WS-REST-DAYS                                
416700     ELSE                                                                 
416800        DIVIDE  WS-LTID-B  BY 7                                           
416900                       GIVING WS-LT-WEEKS                                 
417000                    REMAINDER WS-REST-DAYS                                
417100     END-IF                                                               
417200*                                                                         
417300     IF WS-REST-DAYS > ZERO                                               
417400        ADD +1             TO WS-LT-WEEKS                                 
417500     END-IF                                                               
417600*                                                                         
417700     IF WS-LT-WEEKS < +2                                                  
417800        MOVE +2            TO WS-LT-WEEKS                                 
417900     END-IF                                                               
418000*                                                                         
418100     MOVE DAGENS-TIAAVV    TO VADD-DATUM-AAVV                             
418200     MOVE WS-LT-WEEKS      TO VADD-ANTAL                                  
418300                                                                          
418400     CALL W009VADD      USING VADD-DATUM-AAVV VADD-ANTAL                  
418500                                                                          
418600     MOVE VADD-DATUM-AAVV  TO TVA-V-FRAMAT-TIAAVV                         
418700                                                                          
418800     .                                                                    
418900     EJECT                                                                
419000                                                                          
419100 S99-ABEND SECTION.                                                       
419200                                                                          
419300     SKIP2                                                                
419400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
419500     .                                                                    
419600     EJECT                                                                
419700                                                                          
419800                                                                          
419900                                                                          
420000* --- IMS SEKTIONER ---                                                   
420100*     IMSSEKT                                                             
420200                                                                          
420300 IMS-GET-WDK7-SB SECTION.                                                 
420400                                                                          
420500     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA                           
420600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
420700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
420800     PERFORM IMS-STATUSKONTROLL                                           
420900     .                                                                    
421000     EJECT                                                                
421100                                                                          
421200                                                                          
421300 IMS-GU-ART SECTION.                                                      
421400                                                                          
421500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
421600          DELIMITED BY SIZE INTO SSA1                                     
421700     MOVE '  GE' TO GODK-STATUSKODER                                      
421800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
421900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
422000     PERFORM IMS-STATUSKONTROLL                                           
422100     .                                                                    
422200     EJECT                                                                
422300                                                                          
422400                                                                          
422500 IMS-GN-CLAG SECTION.                                                     
422600                                                                          
422700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
422800          DELIMITED BY SIZE INTO SSA1                                     
422900     MOVE '  GE' TO GODK-STATUSKODER                                      
423000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
423100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
423200     PERFORM IMS-STATUSKONTROLL                                           
423300     .                                                                    
423400     EJECT                                                                
423500                                                                          
423600                                                                          
423700 IMS-GU-ARTC11 SECTION.                                                   
423800                                                                          
423900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
424000          DELIMITED BY SIZE INTO SSA1                                     
424100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
424200          DELIMITED BY SIZE INTO SSA2                                     
424300     MOVE '  GE' TO GODK-STATUSKODER                                      
424400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
424500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
424600     PERFORM IMS-STATUSKONTROLL                                           
424700     .                                                                    
424800     EJECT                                                                
424900                                                                          
425000                                                                          
425100 IMS-GNP-ARTC21 SECTION.                                                  
425200                                                                          
425300     STRING 'WLARTC21(DAPRLIST>=' W-DAPRLIST-21-N                         
425400                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
425500          DELIMITED BY SIZE INTO SSA1                                     
425600     MOVE '  GE' TO GODK-STATUSKODER                                      
425700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC21 SSA1                   
425800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
425900     PERFORM IMS-STATUSKONTROLL                                           
426000     .                                                                    
426100     EJECT                                                                
426200                                                                          
426300 IMS-GU-OIGA SECTION.                                                     
426400                                                                          
426500     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
426600          DELIMITED BY SIZE INTO SSA1                                     
426700     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
426800          DELIMITED BY SIZE INTO SSA2                                     
426900     MOVE '  GE' TO GODK-STATUSKODER                                      
427000     CALL CBLTDLI USING GU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2               
427100     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
427200     PERFORM IMS-STATUSKONTROLL                                           
427300     .                                                                    
427400     EJECT                                                                
427500                                                                          
427600 IMS-GU-ART-WDK9 SECTION.                                                 
427700                                                                          
427800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
427900          DELIMITED BY SIZE INTO SSA1                                     
428000     MOVE '  GE' TO GODK-STATUSKODER                                      
428100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-ARTM01 SSA1                    
428200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
428300     PERFORM IMS-STATUSKONTROLL                                           
428400     .                                                                    
428500     EJECT                                                                
428600                                                                          
428700                                                                          
428800 IMS-GU-ART-WDD9 SECTION.                                                 
428900                                                                          
429000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
429100          DELIMITED BY SIZE INTO SSA1                                     
429200     MOVE '  GE' TO GODK-STATUSKODER                                      
429300     CALL CBLTDLI USING GU INLB-PCB DLI-IO-INLB01 SSA1                    
429400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
429500     PERFORM IMS-STATUSKONTROLL                                           
429600     .                                                                    
429700     EJECT                                                                
429800                                                                          
429900                                                                          
430000 IMS-GNP-WDD902 SECTION.                                                  
430100                                                                          
430200     MOVE 'WLINLB11 ' TO SSA1                                             
430300     MOVE '  GE' TO GODK-STATUSKODER                                      
430400     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB11 SSA1                   
430500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
430600     PERFORM IMS-STATUSKONTROLL                                           
430700     .                                                                    
430800     EJECT                                                                
430900                                                                          
431000 IMS-GNP-WDD905-2 SECTION.                                                
431100     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-D9-X ')'                      
431200          DELIMITED BY SIZE INTO SSA1                                     
431300     MOVE '  GE' TO GODK-STATUSKODER                                      
431400     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB23 SSA1                   
431500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
431600     PERFORM IMS-STATUSKONTROLL                                           
431700     .                                                                    
431800     EJECT                                                                
431900                                                                          
432000 IMS-GNP-WDD905 SECTION.                                                  
432100                                                                          
432200     MOVE 'WLINLB23 ' TO SSA1                                             
432300     MOVE '  GE' TO GODK-STATUSKODER                                      
432400     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB23 SSA1                   
432500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
432600     PERFORM IMS-STATUSKONTROLL                                           
432700     .                                                                    
432800     EJECT                                                                
432900                                                                          
433000                                                                          
433100 IMS-GN-ORDL01 SECTION.                                                   
433200                                                                          
433300     STRING 'WLORDL01(WDE301KY=>' W-WDE301KY-MIN-X                        
433400                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
433500                    '&IDARTNR  =' W-IDARTNR-X ')'                         
433600          DELIMITED BY SIZE INTO SSA1                                     
433700     MOVE '  GE' TO GODK-STATUSKODER                                      
433800     CALL CBLTDLI USING GN ORDL-PCB DLI-IO-ORDL01 SSA1                    
433900     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
434000     PERFORM IMS-STATUSKONTROLL                                           
434100     .                                                                    
434200     EJECT                                                                
434300                                                                          
434400                                                                          
434500 IMS-GU-ORDL01 SECTION.                                                   
434600                                                                          
434700     STRING 'WLORDL01(WDE301KY=>' W-WDE301KY-MIN-X                        
434800                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
434900                    '&IDARTNR  =' W-IDARTNR-X ')'                         
435000          DELIMITED BY SIZE INTO SSA1                                     
435100     MOVE '  GE' TO GODK-STATUSKODER                                      
435200     CALL CBLTDLI USING GU ORDL-PCB DLI-IO-ORDL01 SSA1                    
435300     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
435400     PERFORM IMS-STATUSKONTROLL                                           
435500     .                                                                    
435600     EJECT                                                                
435700                                                                          
435800                                                                          
435900 IMS-GU-WDK711-TRANSF SECTION.                                            
436000                                                                          
436100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
436200          DELIMITED BY SIZE INTO SSA1                                     
436300     STRING 'WDK711  (IDDC     =' W-IDDC-TRANSF-X ')'                     
436400          DELIMITED BY SIZE INTO SSA2                                     
436500     MOVE '  GE' TO GODK-STATUSKODER                                      
436600     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK711-TRANSF                
436700          SSA1 SSA2                                                       
436800     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
436900     PERFORM IMS-STATUSKONTROLL                                           
437000     .                                                                    
437100     EJECT                                                                
437200                                                                          
437300                                                                          
437400 IMS-GU-WDK712-LART   SECTION.                                            
437500                                                                          
437600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
437700          DELIMITED BY SIZE INTO SSA1                                     
437800     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
437900          DELIMITED BY SIZE INTO SSA2                                     
438000     MOVE '  GE' TO GODK-STATUSKODER                                      
438100     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK712-LART                  
438200          SSA1 SSA2                                                       
438300     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
438400     PERFORM IMS-STATUSKONTROLL                                           
438500     .                                                                    
438600     EJECT                                                                
438700                                                                          
438800                                                                          
438900 IMS-GU-WDK711-ERS SECTION.                                               
439000                                                                          
439100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-ERS-X ')'                     
439200          DELIMITED BY SIZE INTO SSA1                                     
439300     STRING 'WDK711  (IDDC     =' W-IDDC-ERS-X ')'                        
439400          DELIMITED BY SIZE INTO SSA2                                     
439500     MOVE '  GE' TO GODK-STATUSKODER                                      
439600     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK711-ERS                    
439700          SSA1 SSA2                                                       
439800     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
439900     PERFORM IMS-STATUSKONTROLL                                           
440000     .                                                                    
440100     EJECT                                                                
440200                                                                          
440300 IMS-GU-WDK701-CD  SECTION.                                               
440400                                                                          
440500***ANVÄNDER WDK72 PCB FÖR ATT EJ BEHÖVA SKAPA NYTT                        
440600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
440700          DELIMITED BY SIZE INTO SSA1                                     
440800     MOVE '  GE' TO GODK-STATUSKODER                                      
440900     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK711-ERS                    
441000          SSA1                                                            
441100     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
441200     PERFORM IMS-STATUSKONTROLL                                           
441300     .                                                                    
441400     EJECT                                                                
441500                                                                          
441600 IMS-GNP-WDK711-CD SECTION.                                               
441700                                                                          
441800***ANVÄNDER WDK72 PCB FÖR ATT EJ BEHÖVA SKAPA NYTT                        
441900     MOVE 'WDK711  '          TO SSA1                                     
442000     MOVE '  GEGB'            TO GODK-STATUSKODER                         
442100     CALL CBLTDLI USING GNP WDK72-PCB DLI-IO-WDK711-ERS                   
442200          SSA1                                                            
442300     MOVE WDK72-STATUS-CODE   TO STATUS-WS                                
442400     PERFORM IMS-STATUSKONTROLL                                           
442500     .                                                                    
442600     EJECT                                                                
442700                                                                          
442800 IMS-GU-WDK711-INTERN SECTION.                                            
442900                                                                          
443000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-INT-X ')'                     
443100          DELIMITED BY SIZE INTO SSA1                                     
443200     STRING 'WDK711  (IDDC     =' W-IDDC-INT-X ')'                        
443300          DELIMITED BY SIZE INTO SSA2                                     
443400     MOVE '  GE' TO GODK-STATUSKODER                                      
443500     CALL CBLTDLI USING GU WDK74-PCB DLI-IO-WDK711-INTERN                 
443600          SSA1 SSA2                                                       
443700     MOVE WDK74-STATUS-CODE TO STATUS-WS                                  
443800     PERFORM IMS-STATUSKONTROLL                                           
443900     .                                                                    
444000     EJECT                                                                
444100                                                                          
444200 IMS-GNP-WDK722-INTERN SECTION.                                           
444300                                                                          
444400     MOVE 'WDK722  '          TO SSA1                                     
444500     MOVE '  GE'              TO GODK-STATUSKODER                         
444600     CALL CBLTDLI USING GNP WDK74-PCB DLI-IO-WDK722-INTERN                
444700          SSA1                                                            
444800     MOVE WDK74-STATUS-CODE   TO STATUS-WS                                
444900     PERFORM IMS-STATUSKONTROLL                                           
445000     .                                                                    
445100     EJECT                                                                
445200 IMS-GU-WDK711-LOCAL SECTION.                                             
445300                                                                          
445400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
445500          DELIMITED BY SIZE INTO SSA1                                     
445600     STRING 'WDK711  (IDDC     =' W-IDDC-LOCAL-X ')'                      
445700          DELIMITED BY SIZE INTO SSA2                                     
445800     MOVE '  GE' TO GODK-STATUSKODER                                      
445900     CALL CBLTDLI USING GU WDK73-PCB DLI-IO-WDK711-LOCAL                  
446000          SSA1 SSA2                                                       
446100     MOVE WDK73-STATUS-CODE TO STATUS-WS                                  
446200     PERFORM IMS-STATUSKONTROLL                                           
446300     .                                                                    
446400     EJECT                                                                
446500                                                                          
446600 IMS-GNP-WDK724 SECTION.                                                  
446700     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-PR-N                         
446800                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
446900          DELIMITED BY SIZE INTO SSA1                                     
447000     MOVE '  GE' TO GODK-STATUSKODER                                      
447100     CALL CBLTDLI USING GNP WDK73-PCB DLI-IO-WDK724 SSA1                  
447200     MOVE WDK73-STATUS-CODE TO STATUS-WS                                  
447300     PERFORM IMS-STATUSKONTROLL                                           
447400     .                                                                    
447500     EJECT                                                                
447600                                                                          
447700 IMS-GU-ART-ERS SECTION.                                                  
447800                                                                          
447900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ERS-X ')'                     
448000          DELIMITED BY SIZE INTO SSA1                                     
448100     MOVE '  GE' TO GODK-STATUSKODER                                      
448200     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-ARTC01-ERS SSA1               
448300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
448400     PERFORM IMS-STATUSKONTROLL                                           
448500     .                                                                    
448600     EJECT                                                                
448700                                                                          
448800                                                                          
448900 IMS-GNP-CLAG-ERS SECTION.                                                
449000                                                                          
449100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
449200          DELIMITED BY SIZE INTO SSA1                                     
449300     MOVE '  GE' TO GODK-STATUSKODER                                      
449400     CALL CBLTDLI USING GNP ARTC2-PCB DLI-IO-ARTC11-ERS SSA1              
449500     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
449600     PERFORM IMS-STATUSKONTROLL                                           
449700     .                                                                    
449800     EJECT                                                                
449900                                                                          
450000                                                                          
450100 IMS-GU-ERSB01 SECTION.                                                   
450200                                                                          
450300     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN-X                        
450400                    '&WDD7A1KY<=' W-WDD7A1KY-MAX-X ')'                    
450500            DELIMITED BY SIZE INTO SSA1                                   
450600     MOVE '  GE' TO GODK-STATUSKODER                                      
450700     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-ERSB01 SSA1                    
450800     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
450900     PERFORM IMS-STATUSKONTROLL                                           
451000     .                                                                    
451100     EJECT                                                                
451200                                                                          
451300                                                                          
451400 IMS-GU-INLC01 SECTION.                                                   
451500                                                                          
451600     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
451700          DELIMITED BY SIZE INTO SSA1                                     
451800     MOVE '  GE' TO GODK-STATUSKODER                                      
451900     CALL CBLTDLI USING GU INLC-PCB DLI-IO-INLC01 SSA1                    
452000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
452100     PERFORM IMS-STATUSKONTROLL                                           
452200     .                                                                    
452300     EJECT                                                                
452400                                                                          
452500                                                                          
452600 IMS-GNP-INLC11 SECTION.                                                  
452700                                                                          
452800     STRING 'WLINLC11(IDDC     =' W-IDDC-X ')'                            
452900          DELIMITED BY SIZE INTO SSA1                                     
453000     MOVE '  GE' TO GODK-STATUSKODER                                      
453100     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-INLC11 SSA1                   
453200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
453300     PERFORM IMS-STATUSKONTROLL                                           
453400     .                                                                    
453500     EJECT                                                                
453600                                                                          
453700 IMS-GU-WDQ4B1 SECTION.                                                   
453800                                                                          
453900     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4BKY-FOM                           
454000                    '&WDQ4B1KY<=' W-WDQ4BKY-TOM                           
454100                    '&IDDC     =' W-Q4-IDDC-X   ')'                       
454200            DELIMITED BY SIZE INTO SSA1                                   
454300     MOVE '  GE'              TO GODK-STATUSKODER                         
454400     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
454500     MOVE WDQ4B-STATUS-CODE   TO STATUS-WS                                
454600     PERFORM IMS-STATUSKONTROLL                                           
454700     .                                                                    
454800     EJECT                                                                
454900                                                                          
455000 IMS-GN-WDQ4B1 SECTION.                                                   
455100                                                                          
455200     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4BKY-FOM                           
455300                    '&WDQ4B1KY<=' W-WDQ4BKY-TOM                           
455400                    '&IDDC     =' W-Q4-IDDC-X   ')'                       
455500            DELIMITED BY SIZE INTO SSA1                                   
455600     MOVE '  GEGB'            TO GODK-STATUSKODER                         
455700     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
455800     MOVE WDQ4B-STATUS-CODE   TO STATUS-WS                                
455900     PERFORM IMS-STATUSKONTROLL                                           
456000     .                                                                    
456100     EJECT                                                                
456200                                                                          
456300 IMS-GU-WDQ201      SECTION.                                              
456400                                                                          
456500     STRING 'WDQ201  (IDORDER  =' W-Q2-IDORDER-X ')'                      
456600            DELIMITED BY SIZE INTO SSA1                                   
456700     MOVE  '    '            TO GODK-STATUSKODER                          
456800     CALL CBLTDLI USING GU   WDQ2-PCB DLI-IO-WDQ201 SSA1                  
456900     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
457000     PERFORM IMS-STATUSKONTROLL                                           
457100     .                                                                    
457200                                                                          
457300 IMS-GNP-WDQ212 SECTION.                                                  
457400                                                                          
457500     STRING 'WDQ212  (IDDC     =' W-Q2-IDDC-X ')'                         
457600            DELIMITED BY SIZE INTO SSA1                                   
457700     MOVE  '    '            TO GODK-STATUSKODER                          
457800     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-WDQ212 SSA1                  
457900     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
458000     PERFORM IMS-STATUSKONTROLL                                           
458100     .                                                                    
458200     EJECT                                                                
458300 IMS-GN-ORDT01 SECTION.                                                   
458400                                                                          
458500     STRING 'WLORDT01(WDA5D1KY=>' W-WDA5D1KY-MIN-X                        
458600                    '&WDA5D1KY<=' W-WDA5D1KY-MAX-X                        
458700                    '&IDARTNR  =' W-IDARTNR-X ')'                         
458800          DELIMITED BY SIZE INTO SSA1                                     
458900     MOVE '  GE' TO GODK-STATUSKODER                                      
459000     CALL CBLTDLI USING GN ORDT-PCB DLI-IO-ORDT01 SSA1                    
459100     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
459200     PERFORM IMS-STATUSKONTROLL                                           
459300     .                                                                    
459400     EJECT                                                                
459500                                                                          
459600                                                                          
459700 IMS-GU-ORDT01 SECTION.                                                   
459800                                                                          
459900     STRING 'WLORDT01(WDA5D1KY=>' W-WDA5D1KY-MIN-X                        
460000                    '&WDA5D1KY<=' W-WDA5D1KY-MAX-X                        
460100                    '&IDARTNR  =' W-IDARTNR-X ')'                         
460200          DELIMITED BY SIZE INTO SSA1                                     
460300     MOVE '  GE' TO GODK-STATUSKODER                                      
460400     CALL CBLTDLI USING GU ORDT-PCB DLI-IO-ORDT01 SSA1                    
460500     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
460600     PERFORM IMS-STATUSKONTROLL                                           
460700     .                                                                    
460800     EJECT                                                                
460900                                                                          
461000                                                                          
461100 IMS-GU-ORDP01 SECTION.                                                   
461200                                                                          
461300     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
461400          DELIMITED BY SIZE INTO SSA1                                     
461500     MOVE '  GE' TO GODK-STATUSKODER                                      
461600     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-ORDP01 SSA1                    
461700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
461800     PERFORM IMS-STATUSKONTROLL                                           
461900     .                                                                    
462000     EJECT                                                                
462100                                                                          
462200                                                                          
462300 IMS-GU-LEVA16 SECTION.                                                   
462400                                                                          
462500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
462600          DELIMITED BY SIZE INTO SSA1                                     
462700     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
462800          DELIMITED BY SIZE INTO SSA2                                     
462900     MOVE '  GE' TO GODK-STATUSKODER                                      
463000     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA16 SSA1 SSA2               
463100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
463200     PERFORM IMS-STATUSKONTROLL                                           
463300     .                                                                    
463400     EJECT                                                                
463500 IMS-GU-WDE4CSEQ SECTION.                                                 
463600                                                                          
463700     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
463800                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
463900          DELIMITED BY SIZE INTO SSA1                                     
464000     MOVE '  GE'            TO GODK-STATUSKODER                           
464100     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE411 SSA1                   
464200     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
464300     PERFORM IMS-STATUSKONTROLL                                           
464400     .                                                                    
464500                                                                          
464600 IMS-GN-WDE4CSEQ SECTION.                                                 
464700                                                                          
464800     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
464900                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
465000          DELIMITED BY SIZE INTO SSA1                                     
465100     MOVE '  GEGB'          TO GODK-STATUSKODER                           
465200     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE411 SSA1                   
465300     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
465400     PERFORM IMS-STATUSKONTROLL                                           
465500     .                                                                    
465600                                                                          
465700 IMS-GNP-WDE401 SECTION.                                                  
465800                                                                          
465900     MOVE 'WDE401 '         TO SSA1                                       
466000     MOVE '  '              TO GODK-STATUSKODER                           
466100     CALL CBLTDLI USING GNP WDE4C-PCB DLI-IO-WDE401 SSA1                  
466200     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
466300     PERFORM IMS-STATUSKONTROLL                                           
466400     .                                                                    
466500     EJECT                                                                
466600 IMS-GNP-WDE421  SECTION.                                                 
466700                                                                          
466800     MOVE 'WDE421 '         TO SSA1                                       
466900     MOVE '  GE' TO GODK-STATUSKODER                                      
467000     CALL CBLTDLI USING GNP WDE4C-PCB DLI-IO-WDE421 SSA1                  
467100     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
467200     PERFORM IMS-STATUSKONTROLL                                           
467300     .                                                                    
467400     SKIP3                                                                
467500 IMS-GU-WDE611 SECTION.                                                   
467600                                                                          
467700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
467800          DELIMITED BY SIZE INTO SSA1                                     
467900     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
468000          DELIMITED BY SIZE INTO SSA2                                     
468100     MOVE '  GE' TO GODK-STATUSKODER                                      
468200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
468300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
468400     PERFORM IMS-STATUSKONTROLL                                           
468500     .                                                                    
468600     EJECT                                                                
468700                                                                          
468800 IMS-GU-WDB601    SECTION.                                                
468900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
469000          DELIMITED BY SIZE INTO SSA1                                     
469100     MOVE '  ' TO GODK-STATUSKODER                                        
469200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
469300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
469400     PERFORM IMS-STATUSKONTROLL                                           
469500     .                                                                    
469600     EJECT                                                                
469700 IMS-GU-WDB616    SECTION.                                                
469800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
469900          DELIMITED BY SIZE INTO SSA1                                     
470000     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
470100          DELIMITED BY SIZE INTO SSA2                                     
470200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
470300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
470400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
470500     PERFORM IMS-STATUSKONTROLL                                           
470600     .                                                                    
470700     EJECT                                                                
470800 IMS-STATUSKONTROLL SECTION.                                              
470900     SKIP2                                                                
471000     SET STATUS-IX TO 1                                                   
471100     SEARCH GODK-STATUS                                                   
471200       AT END                                                             
471300         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
471400         DISPLAY FELTEXT                                                  
471500         CALL FELLOG                                                      
471600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
471700         CONTINUE                                                         
471800     END-SEARCH                                                           
471900     .                                                                    
472000     EJECT                                                                
472100                                                                          
472200 DB2-OPEN-TP5IDDC-CRS SECTION.                                            
472300     MOVE 'DB2-OPEN-TP5IDDC-CRS' TO  WS-DB2-SEKTION                       
472400                                                                          
472500     EXEC SQL DECLARE TP5IDDC-CRS CURSOR FOR                              
472600         SELECT  IDLOPNR_DC                                               
472700                ,IDDC                                                     
472800                                                                          
472900         FROM    TP5IDDC                                                  
473000                                                                          
473100         ORDER BY IDLOPNR_DC                                              
473200                                                                          
473300     END-EXEC                                                             
473400                                                                          
473500     MOVE SQLCODE TO SQLCODE-WS                                           
473600     MOVE 000     TO GOOD-SQLCODECODES                                    
473700     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
473800     PERFORM DB2-STATUS-CHECK                                             
473900     .                                                                    
474000     EJECT                                                                
474100 DB2-FETCH-TP5IDDC-CRS SECTION.                                           
474200     MOVE 'DB2-FETCH-TP5IDDC-CRS' TO  WS-DB2-SEKTION                      
474300     MOVE 000100  TO GOOD-SQLCODECODES                                    
474400     EXEC SQL FETCH TP5IDDC-CRS INTO                                      
474500                :TP5IDDC-IDLOPNR-DC                                       
474600               ,:TP5IDDC-IDDC                                             
474700                                                                          
474800     END-EXEC                                                             
474900                                                                          
475000     MOVE SQLCODE TO SQLCODE-WS                                           
475100     PERFORM DB2-STATUS-CHECK                                             
475200     .                                                                    
475300     SKIP3                                                                
475400                                                                          
475500 DB2-CLOSE-TP5IDDC-CRS SECTION.                                           
475600     MOVE 'DB2-CLOSE-TP5IDDC-CRS' TO  WS-DB2-SEKTION                      
475700                                                                          
475800     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
475900     .                                                                    
476000     EJECT                                                                
476100                                                                          
476200 DB2-OPEN-TP4TRAN-CRS SECTION.                                            
476300     MOVE 'DB2-OPEN-TP4TRAN-CRS' TO  WS-DB2-SEKTION                       
476400                                                                          
476500     EXEC SQL DECLARE TP4TRAN-CRS CURSOR FOR                              
476600         SELECT  IDDC_SEND                                                
476700                ,IDDC_REC                                                 
476800                                                                          
476900         FROM    TP4TRAN                                                  
477000                                                                          
477100         WHERE KDARBTYP = 'ESC'                                           
477200                                                                          
477300         ORDER BY IDDC_REC                                                
477400                                                                          
477500     END-EXEC                                                             
477600                                                                          
477700     MOVE SQLCODE TO SQLCODE-WS                                           
477800     MOVE 000     TO GOOD-SQLCODECODES                                    
477900     EXEC SQL OPEN TP4TRAN-CRS END-EXEC                                   
478000     PERFORM DB2-STATUS-CHECK                                             
478100     .                                                                    
478200     EJECT                                                                
478300 DB2-FETCH-TP4TRAN-CRS SECTION.                                           
478400     MOVE 'DB2-FETCH-TP4TRAN-CRS' TO  WS-DB2-SEKTION                      
478500     MOVE 000100  TO GOOD-SQLCODECODES                                    
478600     EXEC SQL FETCH TP4TRAN-CRS INTO                                      
478700                :TP4TRAN-IDDC-SEND                                        
478800               ,:TP4TRAN-IDDC-REC                                         
478900                                                                          
479000     END-EXEC                                                             
479100                                                                          
479200     MOVE SQLCODE TO SQLCODE-WS                                           
479300     PERFORM DB2-STATUS-CHECK                                             
479400     .                                                                    
479500     SKIP3                                                                
479600                                                                          
479700 DB2-CLOSE-TP4TRAN-CRS SECTION.                                           
479800     MOVE 'DB2-CLOSE-TP4TRAN-CRS' TO  WS-DB2-SEKTION                      
479900                                                                          
480000     EXEC SQL CLOSE TP4TRAN-CRS END-EXEC                                  
480100     .                                                                    
480200     EJECT                                                                
480300                                                                          
480400 DB2-STATUS-CHECK  SECTION.                                               
480500                                                                          
480600     SET SQLCODE-IX TO 1                                                  
480700     SEARCH GOOD-SQLCODE                                                  
480800       AT END                                                             
480900*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
481000*         DELIMITED BY SIZE INTO ERROR-TEXT                               
481100          CALL ABEND USING RKOD-ABEND-DB2                                 
481200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
481300     END-SEARCH                                                           
481400     .                                                                    
481500     EJECT                                                                
481600*    -COPY WY2000P1                                                       
481700     EJECT                                                                
481800*    -COPY WY2000Q1                                                       
481900     EJECT                                                                
482000*    -COPY WY2000P2                                                       
