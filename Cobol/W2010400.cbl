000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2010400.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   93/08/31.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*        ÄNDRING 03-01-30                                                 
000900*        FCB-PUBVECKA TILLAGT, LÄGGER UT MED. NÄR TIFINLV >               
001000*        DAGENS-AAVVD - 2 ÅR                                              
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        TP-PROGRAM ANSKAFFNING - ORDERINGÅNG                             
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001600*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001700*        PROGRAMMET LÄSER      WLOIGB (WDL8)                              
001800*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W2T104 W2T104U                                      
002200*        MID:         W2I10401                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W2O10401                                            
002600*                                                                         
002700*   ÄNDRINGAR:                                                            
002800*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002900*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003000*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003100*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003200*                                                                         
003300*    2016-03-11    E'TRACKER 10243132  KINA EXPORT 2015,                  
003400*                  KRAV 70011.SÄTT NEJ PÅ FLREFNYO PÅ WDK629 VID          
003500*                  CLAG-KVPB-SEP FORECAST-ÄNDRING.                        
003600*                                                                         
003700*    2016-10-25    E'TRACKER 10287369 RÄTTA ATT DET HAMNAR SKRÄP          
003800*                  I FÄLTET WDG32202-KVPB-SEP-GAMMAL PÅ SATSER.           
003900*                                                                         
004000*                                                                         
004100                                                                          
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 WORKING-STORAGE SECTION.                                                 
004700*    -COPY WY2000W2                                                       
004800     SKIP3                                                                
004900 77  IDPGM                       PIC X(08)   VALUE 'W2010400'.            
005000                                                                          
005100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  YES                         PIC X       VALUE 'Y'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  WS-IDLEVNR-8        PIC X(8)    VALUE SPACE.                         
005800 77  VECKA-IX            PIC S9(9)   COMP SYNC.                           
005900 77  PER-IX              PIC S9(9)   COMP SYNC.                           
006000 77  MOD-IX              PIC S9(9)   COMP SYNC.                           
006100                                                                          
006200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006300*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
006400*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
006500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0965 COMP SYNC.        
006600                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-FEL                          VALUE 'N'.                   
007300                                                                          
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007800 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
007900     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
008000     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
008100                                                                          
008200 77  C2-SW                       PIC X       VALUE 'J'.                   
008300     88  C2-SEG-FINNS                        VALUE 'J'.                   
008400     88  C2-SEG-SAKNAS                       VALUE 'N'.                   
008500                                                                          
008600 77  WDK626-SW                   PIC X       VALUE 'N'.                   
008700     88  TA-BORT-WDK626                      VALUE 'J'.                   
008800                                                                          
008900 77  SW-US-KVPB                  PIC X       VALUE 'N'.                   
009000 77  SW-CN-KVPB                  PIC X       VALUE 'N'.                   
009100                                                                          
009200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009300     88  EGEN-MID                            VALUE '2104'.                
009400     88  GODK-MID                            VALUE '2101' '2102'          
009500                                                   '2103' '2104'          
009600                                                   '2105' '2106'          
009700                                                   '2107' '2108'          
009800                                                   '2109'.                
009900     88  HELP-MID                            VALUE '0551'.                
010000                                                                          
010100 77  CL-IX               PIC S9(9)   COMP SYNC.                           
010200 77  INDX                PIC S9(9)   COMP SYNC.                           
010300 77  INDX2               PIC S9(9)   COMP SYNC.                           
010400 77  INDX-AAR            PIC S9(9)   COMP SYNC.                           
010500 77  INDX-PERIOD         PIC S9(9)   COMP SYNC.                           
010600 77  TABW200-INDEX       PIC S9(4)   COMP SYNC.                           
010700 77  SPARAD-KDERS        PIC S9(3)   COMP-3.                              
010800 77  SPARAD-KDERS-UTG    PIC S9(3)   COMP-3.                              
010900 77  SPARAD-TIFINLV      PIC S9(5)   COMP-3.                              
011000 77  SEASON-SW-C1        PIC  X(1) VALUE SPACE.                           
011100 77  SEASON-SW-C2        PIC  X(1) VALUE SPACE.                           
011200 77  SW-SEASON           PIC  X(1) VALUE SPACE.                           
011300 77  SW-ERSATT           PIC  X(1) VALUE SPACE.                           
011400 77  WS-AVVIKELSE        PIC  S9(4)V999 VALUE ZERO COMP-3.                
011500     SKIP2                                                                
011600                                                                          
011700 01  WS-FALT.                                                             
011800     03 WS-TEST.                                                          
011900       05 WS-TEST-A              PIC X(6)    VALUE SPACE.                 
012000       05 FILLER                 PIC X       VALUE '/'.                   
012100       05 WS-TEST-B              PIC X(6)    VALUE SPACE.                 
012200       05 FILLER                 PIC X       VALUE '/'.                   
012300       05 WS-TEST-C              PIC X(1)    VALUE SPACE.                 
012400       05 FILLER                 PIC X       VALUE '/'.                   
012500       05 WS-TEST-D              PIC X(5)    VALUE SPACE.                 
012600       05 FILLER                 PIC X       VALUE '/'.                   
012700       05 WS-TEST-E              PIC X(6)    VALUE SPACE.                 
012800     03 WS-KVOI-PROG           PIC S9(9)       VALUE ZERO.                
012900     03 WS-KVOI-DC-VK          PIC S9(9)       VALUE ZERO.                
013000     03 WS-KVOI-DIV            PIC S9(9)       VALUE ZERO.                
013100     03 WS-KVOI-SATS           PIC S9(9)       VALUE ZERO.                
013200     03 WS-KVOI-PROG-SUM       PIC S9(9)       VALUE ZERO.                
013300     03 WS-KVOI-DC-VK-SUM      PIC S9(9)       VALUE ZERO.                
013400     03 WS-KVOI-DIV-SUM        PIC S9(9)       VALUE ZERO.                
013500     03 WS-KVOI-SATS-SUM       PIC S9(9)       VALUE ZERO.                
013600     03 WS-KVOI-SNITT-12-PROG  PIC S9(9)       VALUE ZERO.                
013700     03 WS-KVOI-SNITT-12-DC-VK PIC S9(9)       VALUE ZERO.                
013800     03 WS-IDLEVNR             PIC X(5)       VALUE SPACE.                
013900     03 WS-IDLKTO              PIC S9(7)      VALUE ZERO COMP-3.          
014000     03 WS-KDUART              PIC  X(1)      VALUE 'X'.                  
014100     03 WS-KDLTK               PIC S9(3)      VALUE ZERO COMP-3.          
014200     03 WS-TILTK               PIC S9(5)      VALUE ZERO COMP-3.          
014300     03 WS-KVLS-C2             PIC S9(7)      VALUE ZERO COMP-3.          
014400     03 WS-KVRESS-C2           PIC S9(7)      VALUE ZERO COMP-3.          
014500     03 W-KVPB-REF             PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
014600     03 W-KVPBREOI             PIC S9(8)V9(1) COMP-3 VALUE ZERO.          
014700     03 WS-KVPB-PLAN           PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
014800     03 WS-KVPB-TREND          PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
014900     03 WS-SKILLNAD-PB         PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
015000     03 WS-CLAG-FALT           OCCURS 2.                                  
015100       05 WS-KDERS             PIC S9(3)      COMP-3.                     
015200       05 WS-KVPB-SEP          PIC S9(6)V9(1) COMP-3.                     
015300       05 WS-TIPBDAT           PIC S9(5)      COMP-3.                     
015400       05 WS-RVPROFEL          PIC S9(3)      COMP-3.                     
015500       05 WS-RVPROURS          PIC S9(3)      COMP-3.                     
015600       05 WS-KVUTJFEL          PIC S9(6)V9(1) COMP-3.                     
015700       05 WS-KVPB-SATS         PIC S9(6)V9(1) COMP-3.                     
015800       05 WS-KVMAD-SEP         PIC S9(6)V9(1) COMP-3.                     
015900       05 WS-KVMAD-TOT         PIC S9(6)V9(1) COMP-3.                     
016000     03 WS-KVMAD-TOT-DEC       PIC S9(6)V9(5) COMP-3.                     
016100     03 WS-PBTOT-GAMMAL        PIC S9(6)V9(1) COMP-3.                     
016200     03 WS-PBTOT-NY            PIC S9(6)V9(1) COMP-3.                     
016300     03 WS-PER                   PIC 9(4)   VALUE ZERO.                   
016400     03 FILLER REDEFINES WS-PER.                                          
016500             05 WS-PER-AA        PIC 9(2).                                
016600             05 WS-PER-PP        PIC 9(2).                                
016700     03 WS-PERIODTABELL-AR-2     OCCURS 12.                               
016800        05 WS-PERTAB-TIAAPP-2    PIC 9(4).                                
016900        05 WS-PERTAB-START-VV-2  PIC 9(2).                                
017000        05 WS-PERTAB-SLUT-VV-2   PIC 9(2).                                
017100        05 WS-PERTAB-KVVIPER-2   PIC 9(2).                                
017200     03 WS-PERIODTABELL-AR-1     OCCURS 12.                               
017300        05 WS-PERTAB-TIAAPP-1    PIC 9(4).                                
017400        05 WS-PERTAB-START-VV-1  PIC 9(2).                                
017500        05 WS-PERTAB-SLUT-VV-1   PIC 9(2).                                
017600        05 WS-PERTAB-KVVIPER-1   PIC 9(2).                                
017700     03 WS-PERIODTABELL-AR-0     OCCURS 12.                               
017800        05 WS-PERTAB-TIAAPP-0    PIC 9(4).                                
017900        05 WS-PERTAB-START-VV-0  PIC 9(2).                                
018000        05 WS-PERTAB-SLUT-VV-0   PIC 9(2).                                
018100        05 WS-PERTAB-KVVIPER-0   PIC 9(2).                                
018200     03 FILLER                   PIC X(10)   VALUE 'WS-TIAAPP'.           
018300     03 WS-TIAAPP                PIC 9(4)    VALUE ZERO.                  
018400     03  FILLER REDEFINES WS-TIAAPP.                                      
018500        05 WS-TIAA               PIC 9(2).                                
018600        05 WS-TIPP               PIC 9(2).                                
018700     03 WS-SLUT-VV               PIC 9(2)    VALUE ZERO.                  
018800                                                                          
018900     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
019000     03  FILLER REDEFINES WS-TIAAVV.                                      
019100         05 WS-TIAAVV-AA         PIC 9(2).                                
019200         05 WS-TIAAVV-VV         PIC 9(2).                                
019300                                                                          
019400     03  WS-AKT-VECKA.                                                    
019500         05 WS-AKT-VECKA-TEXT    PIC X(3)    VALUE SPACE.                 
019600         05 WS-VECKA-I-PER       PIC 9(1)    VALUE ZERO.                  
019700         05 WS-KOLON             PIC X(1)    VALUE SPACE.                 
019800         05 WS-KVVIPER           PIC 9(1)    VALUE ZERO.                  
019900                                                                          
020000*    -COPY W221PERT                                                       
020100                                                                          
020200*    -COPY WWDC99                                                         
020300                                                                          
020400*    -COPY WWDCKONS                                                       
020500                                                                          
020600                                                                          
020700 01  IN-FALT.                                                             
020800     03 IN-KVPB-SEP OCCURS 2.                                             
020900       05 WS-IN-KVPB-SEP       PIC S9(6)V9(1) COMP-3.                     
021000     03 WS-IN-FLMPB-C1         PIC X(1)      VALUE SPACE.                 
021100     03 WS-IN-FLOREGPB         PIC X(1)      VALUE SPACE.                 
021200     03 WS-IN-TIPBLOCK         PIC 9(6)      VALUE ZERO.                  
021300                                                                          
021400 01  KVPB-X.                                                              
021500     03  WS-HELTAL       PIC 9(6).                                        
021600     03  WS-PUNKT        PIC X.                                           
021700     03  WS-DECIMAL      PIC 9.                                           
021800     SKIP2                                                                
021900 01  WS-KVPB.                                                             
022000     03  KVPB-HELTAL     PIC 9(6).                                        
022100     03  KVPB-DECIMAL    PIC 9.                                           
022200     SKIP2                                                                
022300 01  WS-KVPB-SEP-NUM      PIC 9(6)V9 VALUE ZERO.                          
022400     SKIP2                                                                
022500 01  WS-MEDDELANDE       PIC X(34)    VALUE SPACE.                        
022600     SKIP2                                                                
022700 01  TRANS-SW            PIC X.                                           
022800     88  TRANS-EJ-AKTUELL  VALUE 'N'.                                     
022900     88  TRANS-AKTUELL     VALUE 'N'.                                     
023000     EJECT                                                                
023100 01  FL-SEASON           PIC X   VALUE 'N'.                               
023200     SKIP2                                                                
023300 01  KONSTANTER.                                                          
023400     03  VIP             PIC X       VALUE 'V'.                           
023500     03  C1-PROGNOS      PIC S9(3) COMP-3 VALUE +11.                      
023600     03  C1-DIVERSE      PIC S9(3) COMP-3 VALUE +13.                      
023700     03  C1-SATS         PIC S9(3) COMP-3 VALUE +14.                      
023800     03  C2-PROGNOS      PIC S9(3) COMP-3 VALUE +21.                      
023900     03  C2-DIVERSE      PIC S9(3) COMP-3 VALUE +23.                      
024000     03  C2-SATS         PIC S9(3) COMP-3 VALUE +24.                      
024100     EJECT                                                                
024200***************************                                               
024300******  DATUMAREOR  *******                                               
024400***************************                                               
024500                                                                          
024600 01  WS-DAGENS-AAAAMMDD      PIC 9(8).                                    
024700 01  WS-TISEKEL-AA.                                                       
024800     03  WS-DAGENS-SEKEL     PIC 99.                                      
024900     03  WS-DAGENS-AA        PIC 99.                                      
025000 01  FILLER REDEFINES WS-TISEKEL-AA.                                      
025100     03  WS-DAGENS-TIAAAA    PIC 9(4).                                    
025200 01  WS-DAGENS-TIAAAA-1      PIC 9(4).                                    
025300     SKIP2                                                                
025400 01  WS-DAGENS-AAVVD.                                                     
025500     03  DAGENS-AAR          PIC 99.                                      
025600     03  DAGENS-VECKA        PIC 99.                                      
025700     03  DAGENS-DAG          PIC 9.                                       
025800 01  DAGENS-AAVVD-2AAR       PIC 9(5).                                    
025900     SKIP2                                                                
026000 01  DAGENS-AAVVD            PIC 9(5).                                    
026100 01  DAGENS-PERIOD           PIC 9(2).                                    
026200     SKIP2                                                                
026300 01  WS-PERIOD               PIC 9.                                       
026400     SKIP2                                                                
026500 01  START-DATUM.                                                         
026600     03  START-AAR           PIC 99.                                      
026700     03  START-PERIOD        PIC 99.                                      
026800     SKIP2                                                                
026900 01  START-TIFINLV-AAR       PIC 99.                                      
027000     SKIP2                                                                
027100 01  AKTUELLT-AAR            PIC 99.                                      
027200     SKIP2                                                                
027300 01  OIREG-DATUM             PIC 9(3).                                    
027400 01  FILLER REDEFINES OIREG-DATUM.                                        
027500     03  OIREG-AAR           PIC 99.                                      
027600     03  OIREG-PERIOD        PIC 9.                                       
027700     EJECT                                                                
027800*01  -COPY W200W001C0                                                     
027900     SKIP2                                                                
028000 01  HALV-REAAR              PIC 99V9.                                    
028100     EJECT                                                                
028200***********************************                                       
028300******   ACKUMULATORER   **********                                       
028400***********************************                                       
028500     SKIP2                                                                
028600*    *** ÅRS-ACKAR ***                                                    
028700 01  AARS-ACK.                                                            
028800   02  FILLER OCCURS 3.                                                   
028900     03  ACK-AAR-C1          PIC S9(9) COMP-3.                            
029000     03  ACK-AAR-C2          PIC S9(9) COMP-3.                            
029100     03  ACK-AAR-DIV         PIC S9(9) COMP-3.                            
029200     03  ACK-AAR-SATS        PIC S9(9) COMP-3.                            
029300     03  ACK-AAR-C1-C2       PIC S9(9) COMP-3.                            
029400                                                                          
029500*    *** RULLANDE 12 PERIODER ***                                         
029600 01  FILLER.                                                              
029700     03  WS-KVOI-PROG-RULL-12    PIC S9(9)       VALUE ZERO.              
029800     03  WS-KVOI-DC-VK-RULL-12   PIC S9(9)       VALUE ZERO.              
029900     03  WS-KVOI-DIV-RULL-12     PIC S9(9)       VALUE ZERO.              
030000     03  WS-KVOI-SATS-RULL-12    PIC S9(9)       VALUE ZERO.              
030100     03  WS-KVOI-PROG-RULL-6     PIC S9(9)       VALUE ZERO.              
030200     03  WS-KVOI-DC-VK-RULL-6    PIC S9(9)       VALUE ZERO.              
030300     03  ACK-RULL-12-C1      PIC S9(9) COMP-3.                            
030400     03  ACK-RULL-12-C2      PIC S9(9) COMP-3.                            
030500     03  ACK-RULL-12-DIV     PIC S9(9) COMP-3.                            
030600     03  ACK-RULL-12-SATS    PIC S9(9) COMP-3.                            
030700     03  ACK-RULL-12-C1-C2   PIC S9(9) COMP-3.                            
030800                                                                          
030900*    *** RULLANDE 6 PERIODER ***                                          
031000 01  FILLER.                                                              
031100     03  ACK-RULL-6-C1       PIC S9(9) COMP-3.                            
031200     03  ACK-RULL-6-C2       PIC S9(9) COMP-3.                            
031300     03  ACK-RULL-6-C1-C2    PIC S9(9) COMP-3.                            
031400                                                                          
031500*    *** PERIODBEHOV ***                                                  
031600 01  FILLER.                                                              
031700     03  ACK-KVPB-SEP        PIC S9(8)V9 COMP-3.                          
031800     03  ACK-KVPB-SATS       PIC S9(8)V9 COMP-3.                          
031900                                                                          
032000*    *** ORDERINGÅNG C1-C2 ***                                            
032100 01  KVOI-C1-C2.                                                          
032200   02  FILLER OCCURS 17.                                                  
032300     03  WS-PERIOD-C1-C2     PIC S9(9) COMP-3.                            
032400     SKIP2                                                                
032500     EJECT                                                                
032600 01  MESSAGE-CODES.                                                       
032700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
032800     03  CONFLICT                PIC X(3)    VALUE '002'.                 
032900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
033000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
033100     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
033200     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
033300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
033400     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
033500     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
033600     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
033700     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
033800     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
033900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
034000     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
034100     03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                 
034200                                                                          
034300 01  MEDDELANDE.                                                          
034400                                                                          
034500     03 MED-1-AREA.                                                       
034600        05 FILLER                PIC X(34)                                
034700           VALUE 'SÄSONG REGISTRERAD                '.                    
034800        05 FILLER                PIC X(34)                                
034900           VALUE 'SEASON EXIST                      '.                    
035000     03 FILLER REDEFINES MED-1-AREA.                                      
035100        05 MED-1 OCCURS 2        PIC X(34).                               
035200                                                                          
035300     03 MED-2-AREA.                                                       
035400        05 FILLER                PIC X(34)                                
035500           VALUE 'KONTO SAKNAS                      '.                    
035600        05 FILLER                PIC X(34)                                
035700           VALUE 'ACCOUNT DONT EXIST                '.                    
035800     03 FILLER REDEFINES MED-2-AREA.                                      
035900        05 MED-2 OCCURS 2        PIC X(34).                               
036000                                                                          
036100     03 MED-3-AREA.                                                       
036200        05 FILLER                PIC X(34)                                
036300           VALUE 'PB EJ ÄNDRAT                      '.                    
036400        05 FILLER                PIC X(34)                                
036500           VALUE 'FORECAST NOT CHANGED              '.                    
036600     03 FILLER REDEFINES MED-3-AREA.                                      
036700        05 MED-3 OCCURS 2        PIC X(34).                               
036800                                                                          
036900     03 MED-4-AREA.                                                       
037000        05 FILLER                PIC X(34)                                
037100           VALUE 'PUB VECKA INOM 2 ÅR               '.                    
037200        05 FILLER                PIC X(34)                                
037300           VALUE 'PUBL WEEK WITHIN 2 YEARS          '.                    
037400     03 FILLER REDEFINES MED-4-AREA.                                      
037500        05 MED-4 OCCURS 2        PIC X(34).                               
037600                                                                          
037700     EJECT                                                                
037800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
037900 01  GENERELLA-SUBPROGRAM.                                                
038000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
038100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
038200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
038300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
038400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
038500     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
038600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
038700     EJECT                                                                
038800*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
038900*01  -COPY WDECAREA                                                       
039000     EJECT                                                                
039100*    --- PARAMETRAR TILL SUBPROGRAM W222PBTO                              
039200*01  -COPY W222PBTO                                                       
039300     EJECT                                                                
039400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
039500*01 -COPY WMEDAREA                                                        
039600     SKIP3                                                                
039700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
039800*01 -COPY WDATAREA                                                        
039900     SKIP3                                                                
040000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
040100*01 -COPY WMSGINIT                                                        
040200     EJECT                                                                
040300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
040400*                                                                         
040500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
040600     SKIP3                                                                
040700*01  MID -COPY W2I10401                                                   
040800     EJECT                                                                
040900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
041000     SKIP3                                                                
041100*01  -COPY WMSGAREA                                                       
041200     EJECT                                                                
041300     03  MOD REDEFINES MSG-AREA.                                          
041400*      05  -COPY W2O10401                                                 
041500     EJECT                                                                
041600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
041700     SKIP3                                                                
041800*01  -COPY WMFSAREA                                                       
041900     EJECT                                                                
042000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042100*                                                                         
042200     EJECT                                                                
042300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
042400     SKIP3                                                                
042500 01  NYCKLAR-TILL-DLI.                                                    
042600     03  W-IDARTNR-X.                                                     
042700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
042800     03  W-KDSGEKEY-X.                                                    
042900         05  W-KDSGEKEY          PIC X(1)    VALUE SPACE.                 
043000     03  W-KDCLAGER-X.                                                    
043100         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
043200     03  W-IDSKYLT-X.                                                     
043300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
043400     03  W-2202KEY-X.                                                     
043500         05  W-2202              PIC X(4)    VALUE '2202'.                
043600         05  W-NYCKEL-VALFRI     PIC X(26)   VALUE LOW-VALUE.             
043700     03  W-2227KEY-X.                                                     
043800         05  W-2227              PIC X(4)    VALUE '2227'.                
043900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
044000     03  W-TIAAAA-X.                                                      
044100         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
044200     SKIP2                                                                
044300*    --- STATUS-KOD FRÅN IMS                                              
044400 01  STATUS-WS                   PIC XX.                                  
044500     88  SEGMENT-FINNS                       VALUE '  '.                  
044600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
044700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
044800     SKIP2                                                                
044900 01  GODK-STATUSKODER.                                                    
045000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
045100     SKIP3                                                                
045200 01  SSA1                        PIC X(64).                               
045300 01  SSA2                        PIC X(64).                               
045400 01  SSA3                        PIC X(64).                               
045500 01  SSA4                        PIC X(64).                               
045600     EJECT                                                                
045700*    --- IMS FUNKTIONSKODER                                               
045800*01  -COPY W0003                                                          
045900     EJECT                                                                
046000*    ---  DLI INPUT-OUTPUT AREA                                           
046100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
046200     SKIP3                                                                
046300 01  DLI-IO-AREA-01.                                                      
046400     03  IO-AREA-01              PIC X(150)  VALUE SPACE.                 
046500     SKIP3                                                                
046600     03  WLARTC01 REDEFINES IO-AREA-01.                                   
046700*        05  -COPY WDK601                                                 
046800     EJECT                                                                
046900 01  DLI-IO-AREA-11.                                                      
047000     03  IO-AREA-11              PIC X(900)  VALUE SPACE.                 
047100     SKIP3                                                                
047200     03  WLARTC11 REDEFINES IO-AREA-11.                                   
047300*        05  -COPY WDK611                                                 
047400     EJECT                                                                
047500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK629'.         
047600     SKIP2                                                                
047700 01  DLI-IO-AREA-WDK629.                                                  
047800*    03  -COPY WDK629                                                     
047900     EJECT                                                                
048000 01  DLI-IO-AREA.                                                         
048100     03  IO-AREA                 PIC X(2500)  VALUE SPACE.                
048200     SKIP3                                                                
048300     03  WLARTC31 REDEFINES IO-AREA.                                      
048400*        05  -COPY WDK626                                                 
048500     EJECT                                                                
048600     03  WLARTS01 REDEFINES IO-AREA.                                      
048700*        05  -COPY WDK701                                                 
048800     EJECT                                                                
048900     03  WLARTS11 REDEFINES IO-AREA.                                      
049000*        05  -COPY WDK711                                                 
049100     EJECT                                                                
049200     03  WLOIGB01 REDEFINES IO-AREA.                                      
049300*        05  -COPY WDL801  -PRE WDL8-                                     
049400     SKIP3                                                                
049500     03  WLOIGB11 REDEFINES IO-AREA.                                      
049600*        05  -COPY WDL811                                                 
049700     SKIP3                                                                
049800     03  WLBENA11 REDEFINES IO-AREA.                                      
049900*        05  -COPY WDD311  -PRE WDD311-                                   
050000*    ---  DLI INPUT-OUTPUT AREA 2                                         
050100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-2'.         
050200     SKIP3                                                                
050300 01  DLI-IO-AREA-2.                                                       
050400*    03  -COPY WDG32202  -PRE 2202-                                       
050500     03  FILLER                 PIC X(11).                                
050600     EJECT                                                                
050700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-3'.         
050800     SKIP3                                                                
050900 01  DLI-IO-AREA-3.                                                       
051000*    03  -COPY WDGX2228                                                   
051100     EJECT                                                                
051200 LINKAGE SECTION.                                                         
051300     SKIP2                                                                
051400*01  -COPY W0009     -PRE MSG-                                            
051500     EJECT                                                                
051600*01  -COPY W0008     -PRE USEA-.                                          
051700         05  FILLER           PIC X.                                      
051800     EJECT                                                                
051900*01  -COPY W0008     -PRE WDK6-.                                          
052000         05  FILLER           PIC X.                                      
052100     EJECT                                                                
052200*01  -COPY W0008     -PRE ARTS-.                                          
052300         05  FILLER           PIC X.                                      
052400     EJECT                                                                
052500*01  -COPY W0008     -PRE OIGB-.                                          
052600         05  FILLER           PIC S9(9)  COMP-3.                          
052700         05  KEY-FB-KDOI      PIC S9(3)  COMP-3.                          
052800     EJECT                                                                
052900*01  -COPY W0008     -PRE BENA-                                           
053000         05  FILLER           PIC X.                                      
053100     EJECT                                                                
053200*01  -COPY W0008     -PRE XXCK-                                           
053300         05  FILLER           PIC X.                                      
053400     EJECT                                                                
053500*01  -COPY W0008     -PRE XXBW-                                           
053600         05  FILLER           PIC X.                                      
053700     EJECT                                                                
053800 01  PBTO-WDK6-PCB            PIC X.                                      
053900 01  PBTO-WDK7-PCB            PIC X.                                      
054000 01  PBTO-ARTM-PCB            PIC X.                                      
054100 01  PBTO-2501-PCB            PIC X.                                      
054200 01  PBTO-WDB6R-PCB           PIC X.                                      
054300 01  PBTO-WDK7R-PCB           PIC X.                                      
054400 01  PBTO-WDB6-PCB            PIC X.                                      
054500 01  PBTO-WDD7-PCB            PIC X.                                      
054600 01  PBTO-WDK7E-PCB           PIC X.                                      
054700 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
054800 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
054900 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
055000 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
055100 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
055200 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
055300 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
055400 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
055500     EJECT                                                                
055600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
055700                           WDK6-PCB ARTS-PCB                              
055800                 OIGB-PCB  BENA-PCB XXCK-PCB  XXBW-PCB                    
055900                 PBTO-WDK6-PCB PBTO-WDK7-PCB  PBTO-ARTM-PCB               
056000                 PBTO-2501-PCB PBTO-WDB6R-PCB PBTO-WDK7R-PCB              
056100                 PBTO-WDB6-PCB PBTO-WDD7-PCB                              
056200                 PBTO-WDK7E-PCB                                           
056300                 PBTO-W222-UTIL-WDK6-PCB                                  
056400                 PBTO-W222-UTIL-WDK7-PCB                                  
056500                 PBTO-W222-UTIL-WDB6-PCB                                  
056600                 PBTO-W222-UTUP-WDK7-PCB                                  
056700                 PBTO-W222-UTUP-WDB6-PCB                                  
056800                 PBTO-W222-UTUP-UTIL-WDK6-PCB                             
056900                 PBTO-W222-UTUP-UTIL-WDK7-PCB                             
057000                 PBTO-W222-UTUP-UTIL-WDB6-PCB                             
057100                 .                                                        
057200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
057300                           WDK6-PCB ARTS-PCB                              
057400                 OIGB-PCB  BENA-PCB XXCK-PCB  XXBW-PCB                    
057500                 PBTO-WDK6-PCB PBTO-WDK7-PCB  PBTO-ARTM-PCB               
057600                 PBTO-2501-PCB PBTO-WDB6R-PCB PBTO-WDK7R-PCB              
057700                 PBTO-WDB6-PCB PBTO-WDD7-PCB                              
057800                 PBTO-WDK7E-PCB                                           
057900                 PBTO-W222-UTIL-WDK6-PCB                                  
058000                 PBTO-W222-UTIL-WDK7-PCB                                  
058100                 PBTO-W222-UTIL-WDB6-PCB                                  
058200                 PBTO-W222-UTUP-WDK7-PCB                                  
058300                 PBTO-W222-UTUP-WDB6-PCB                                  
058400                 PBTO-W222-UTUP-UTIL-WDK6-PCB                             
058500                 PBTO-W222-UTUP-UTIL-WDK7-PCB                             
058600                 PBTO-W222-UTUP-UTIL-WDB6-PCB                             
058700                 .                                                        
058800                                                                          
058900     PERFORM IMS-GET-MSG                                                  
059000     IF SEGMENT-FINNS                                                     
059100       PERFORM A-INIT                                                     
059200       PERFORM B-KOLLA-NYCKLAR                                            
059300       IF NYCKLAR-OK                                                      
059400         PERFORM S1-SECURITY-CHECK-PARTNO-IDLEV                           
059500         IF PASSED-SECURITY-CHECK                                         
059600           IF MFS-UPDATE                                                  
059700             PERFORM G-KOLLA-INPUT                                        
059800             IF INDATA-OK                                                 
059900               PERFORM H-UPPDATERA                                        
060000             END-IF                                                       
060100           ELSE                                                           
060200             IF MFS-FIRST                                                 
060300               PERFORM C-FOERSTA-SIDA                                     
060400             ELSE                                                         
060500               PERFORM E-SAMMA-SIDA                                       
060600             END-IF                                                       
060700           END-IF                                                         
060800           PERFORM F-LAES-VISA-INFO                                       
060900         END-IF                                                           
061000       END-IF                                                             
061100*      MOVE WS-TEST          TO MOD-TEMFSINF                              
061200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O10401 + 4                      
061300       PERFORM IMS-INSERT-MSG                                             
061400     END-IF                                                               
061500                                                                          
061600     MOVE ZERO TO RETURN-CODE                                             
061700     GOBACK                                                               
061800     .                                                                    
061900     EJECT                                                                
062000                                                                          
062100                                                                          
062200 A-INIT SECTION.                                                          
062300                                                                          
062400     IF MSG-DUBBLA-TRANSKODER                                             
062500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10401                 
062600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
062700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
062800     ELSE                                                                 
062900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I10401                  
063000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
063100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
063200     END-IF                                                               
063300                                                                          
063400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
063500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
063600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
063700                                                                          
063800     MOVE LOW-VALUE TO MSG-AREA                                           
063900     MOVE 'W2O104N1' TO MFS-IDMOD                                         
064000     MOVE '2104' TO MOD-IDTRANS                                           
064100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
064200                                                                          
064300                                                                          
064400     IF EGEN-MID OR HELP-MID                                              
064500       CONTINUE                                                           
064600     ELSE                                                                 
064700       MOVE SPACE TO MFS-KDTRTYP                                          
064800       MOVE '7' TO MFS-IDPFK                                              
064900     END-IF                                                               
065000                                                                          
065100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
065200                                                                          
065300     PERFORM F1-DATUM                                                     
065400     PERFORM AA-INITERA-PERIODTAB                                         
065500                                                                          
065600     MOVE WS-DAGENS-TIAAAA-1 (3:2)                                        
065700                             TO WS-PER-AA                                 
065800     MOVE DAGENS-PERIOD      TO WS-PER-PP                                 
065900     MOVE +1                 TO MOD-IX                                    
066000                                                                          
066100     PERFORM UNTIL MOD-IX > +12                                           
066200       MOVE WS-PER           TO MOD-TIAARP (MOD-IX)                       
066300       ADD +1                TO MOD-IX                                    
066400                                WS-PER-PP                                 
066500       IF WS-PER-PP > +12                                                 
066600         MOVE +1             TO WS-PER-PP                                 
066700         IF WS-PER-AA = 99                                                
066800           MOVE ZERO         TO WS-PER-AA                                 
066900         ELSE                                                             
067000           ADD 1             TO WS-PER-AA                                 
067100         END-IF                                                           
067200       END-IF                                                             
067300     END-PERFORM                                                          
067400     .                                                                    
067500     EJECT                                                                
067600                                                                          
067700                                                                          
067800 AA-INITERA-PERIODTAB SECTION.                                            
067900*--------------------------------------------------------------*          
068000* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
068100* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
068200* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
068300* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
068400*--------------------------------------------------------------*          
068500                                                                          
068600     COMPUTE WS-DAGENS-TIAAAA-1 = WS-DAGENS-TIAAAA - 1                    
068700                                                                          
068800     PERFORM AAA-INITERA-PERIODTAB-AR-1                                   
068900     PERFORM AAB-INITERA-PERIODTAB-AR-0                                   
069000     .                                                                    
069100     EJECT                                                                
069200                                                                          
069300                                                                          
069400 AAA-INITERA-PERIODTAB-AR-1 SECTION.                                      
069500*--------------------------------------------------------------*          
069600* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
069700* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
069800* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
069900* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
070000*--------------------------------------------------------------*          
070100                                                                          
070200     MOVE WS-DAGENS-TIAAAA-1 (3:2)                                        
070300                             TO WS-TIAA                                   
070400     MOVE 1                  TO WS-TIPP                                   
070500                                                                          
070600     PERFORM UNTIL WS-TIPP > 12                                           
070700                                                                          
070800       MOVE 'AARP'           TO DAT-KDDATFORM                             
070900       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
071000                                                                          
071100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
071200                           DAT-O-TIDATUM DAT-KDSVAR                       
071300                                                                          
071400       IF DAT-KDSVAR-OK                                                   
071500*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
071600         IF DAT-TIVV = +52 OR +53                                         
071700            MOVE 1           TO WS-PERTAB-START-VV-1 (WS-TIPP)            
071800         ELSE                                                             
071900            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-1 (WS-TIPP)            
072000         END-IF                                                           
072100         MOVE DAT-KVVIPER    TO WS-PERTAB-KVVIPER-1 (WS-TIPP)             
072200                                                                          
072300       ELSE                                                               
072400           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-1'                    
072500           DELIMITED BY SIZE INTO FELTEXT                                 
072600           CALL FELLOG                                                    
072700       END-IF                                                             
072800                                                                          
072900       ADD 1                 TO WS-TIPP                                   
073000     END-PERFORM                                                          
073100                                                                          
073200     MOVE 1                  TO PER-IX                                    
073300                                                                          
073400     PERFORM UNTIL PER-IX > 11                                            
073500                                                                          
073600       COMPUTE WS-PERTAB-SLUT-VV-1 (PER-IX) =                             
073700               WS-PERTAB-START-VV-1 (PER-IX + 1) - 1                      
073800                                                                          
073900       ADD 1                 TO PER-IX                                    
074000     END-PERFORM                                                          
074100                                                                          
074200     MOVE WS-TIAA            TO WS-TIAAVV-AA                              
074300     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
074400     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-1 (12)                  
074500     .                                                                    
074600     EJECT                                                                
074700                                                                          
074800                                                                          
074900 AAB-INITERA-PERIODTAB-AR-0 SECTION.                                      
075000*--------------------------------------------------------------*          
075100* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
075200* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
075300* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
075400* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
075500*--------------------------------------------------------------*          
075600                                                                          
075700     MOVE WS-DAGENS-TIAAAA (3:2)                                          
075800                             TO WS-TIAA                                   
075900     MOVE 1                  TO WS-TIPP                                   
076000                                                                          
076100     PERFORM UNTIL WS-TIPP > 12                                           
076200                                                                          
076300       MOVE 'AARP'           TO DAT-KDDATFORM                             
076400       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
076500                                                                          
076600       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
076700                           DAT-O-TIDATUM DAT-KDSVAR                       
076800                                                                          
076900       IF DAT-KDSVAR-OK                                                   
077000*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
077100         IF DAT-TIVV = +52 OR +53                                         
077200            MOVE 1           TO WS-PERTAB-START-VV-0 (WS-TIPP)            
077300         ELSE                                                             
077400            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-0 (WS-TIPP)            
077500         END-IF                                                           
077600         MOVE DAT-KVVIPER    TO WS-PERTAB-KVVIPER-0 (WS-TIPP)             
077700                                                                          
077800       ELSE                                                               
077900           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-0'                    
078000           DELIMITED BY SIZE INTO FELTEXT                                 
078100           CALL FELLOG                                                    
078200       END-IF                                                             
078300                                                                          
078400       ADD 1                 TO WS-TIPP                                   
078500     END-PERFORM                                                          
078600                                                                          
078700     MOVE 1                  TO PER-IX                                    
078800                                                                          
078900     PERFORM UNTIL PER-IX > 11                                            
079000                                                                          
079100       COMPUTE WS-PERTAB-SLUT-VV-0 (PER-IX) =                             
079200               WS-PERTAB-START-VV-0 (PER-IX + 1) - 1                      
079300                                                                          
079400       ADD 1                 TO PER-IX                                    
079500     END-PERFORM                                                          
079600                                                                          
079700     MOVE WS-TIAA            TO WS-TIAAVV-AA                              
079800     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
079900     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-0 (12)                  
080000     .                                                                    
080100     EJECT                                                                
080200                                                                          
080300                                                                          
080400 AB-KOLLA-ANTAL-VECKOR SECTION.                                           
080500                                                                          
080600* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
080700                                                                          
080800     MOVE 53        TO WS-TIAAVV-VV                                       
080900     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
081000     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
081100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
081200                         DAT-O-TIDATUM DAT-KDSVAR                         
081300     IF DAT-KDSVAR-OK                                                     
081400       MOVE 53 TO WS-SLUT-VV                                              
081500     ELSE                                                                 
081600       MOVE 52 TO WS-SLUT-VV                                              
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000                                                                          
082100                                                                          
082200                                                                          
082300                                                                          
082400 B-KOLLA-NYCKLAR SECTION.                                                 
082500                                                                          
082600     MOVE JA TO NYCKLAR-SW                                                
082700                                                                          
082800*    -- KONTROLL AV IDARTNR                                               
082900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
083000                                                                          
083100     IF MID-IDARTNR-IN NOT = ALL '+'                                      
083200       MOVE '7'         TO MFS-IDPFK                                      
083300       MOVE SPACE       TO MFS-KDTRTYP                                    
083400     END-IF                                                               
083500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
083600     MOVE '001'             TO MSGI-KDCALL                                
083700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
083800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
083900     MOVE '2104'            TO MSGI-IDTRANS                               
084000     IF MFS-IDTRANS(1:2) NOT = '42'                                       
084100       IF MFS-IDTRANS = '2104'                                            
084200       OR (MID-IDARTNR-IN NUMERIC                                         
084300       AND MID-IDARTNR-IN > ZERO)                                         
084400           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
084500       END-IF                                                             
084600     END-IF                                                               
084700                                                                          
084800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
084900                                                                          
085000     IF MSGI-IDLAND-SPR = 'SE'                                            
085100        MOVE '0'             TO MFS-KDHUVOMR                              
085200        MOVE 'S  '           TO W-IDSKYLT  MED-IDSKYLT                    
085300        MOVE +1 TO SPRAK-IX                                               
085400        MOVE 'NUV'           TO WS-AKT-VECKA-TEXT                         
085500     ELSE                                                                 
085600        MOVE 'GB '           TO W-IDSKYLT  MED-IDSKYLT                    
085700        MOVE +2 TO SPRAK-IX                                               
085800        MOVE 'CUR'           TO WS-AKT-VECKA-TEXT                         
085900     END-IF                                                               
086000     MOVE WS-AKT-VECKA       TO MOD-VECKA-I-AKT-PER                       
086100                                                                          
086200     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
086300                                                                          
086400     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
086500     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
086600       MOVE WS-IDARTNR TO W-IDARTNR                                       
086700     ELSE                                                                 
086800       MOVE NEJ TO NYCKLAR-SW                                             
086900     END-IF                                                               
087000                                                                          
087100     IF GODK-MID OR NYCKLAR-OK                                            
087200       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
087300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
087400     ELSE                                                                 
087500       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
087600     END-IF                                                               
087700                                                                          
087800     IF NYCKLAR-FEL                                                       
087900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
088000       CALL WMEDKONV USING MED-WMEDAREA                                   
088100       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
088200       PERFORM MFS-RENSA-FAELT-IN                                         
088300       PERFORM MFS-RENSA-FAELT-UT                                         
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700                                                                          
088800                                                                          
088900 C-FOERSTA-SIDA SECTION.                                                  
089000                                                                          
089100     PERFORM MFS-RENSA-FAELT-IN                                           
089200     .                                                                    
089300     EJECT                                                                
089400                                                                          
089500                                                                          
089600 E-SAMMA-SIDA SECTION.                                                    
089700                                                                          
089800     IF EGEN-MID OR HELP-MID                                              
089900       IF MID-INPUT = ALL '+'                                             
090000         PERFORM MFS-RENSA-FAELT-IN                                       
090100       ELSE                                                               
090200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
090300         CALL WMEDKONV USING MED-WMEDAREA                                 
090400         MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                
090500         PERFORM EA-MID-INDATA-TILL-MOD                                   
090600       END-IF                                                             
090700     ELSE                                                                 
090800       PERFORM MFS-RENSA-FAELT-IN                                         
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200                                                                          
091300                                                                          
091400 EA-MID-INDATA-TILL-MOD SECTION.                                          
091500                                                                          
091600     IF MID-KVPB-C1-IN NOT = ALL '+'                                      
091700        MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KVPB-CDC-IN-ATTR            
091800        MOVE MFS-ROER-EJ-FAELT         TO MOD-KVPB-CDC-IN                 
091900     ELSE                                                                 
092000        MOVE MFS-RENSA-FAELT            TO MOD-KVPB-CDC-IN                
092100     END-IF                                                               
092200                                                                          
092300        MOVE MFS-STAENG-FAELT          TO MOD-KVPB-C2-IN-ATTR             
092400        MOVE MFS-RENSA-FAELT            TO MOD-KVPB-C2-IN                 
092500                                                                          
092600     IF MID-FLMPB-C1-IN NOT = ALL '+'                                     
092700        MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLMPB-IN-ATTR               
092800        MOVE MFS-ROER-EJ-FAELT         TO MOD-FLMPB-IN                    
092900     ELSE                                                                 
093000        MOVE MFS-RENSA-FAELT            TO MOD-FLMPB-IN                   
093100     END-IF                                                               
093200                                                                          
093300     IF MID-FLOREGPB-IN NOT = ALL '+'                                     
093400        MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLOREGPB-IN-ATTR            
093500        MOVE MFS-ROER-EJ-FAELT         TO MOD-FLOREGPB-IN                 
093600     ELSE                                                                 
093700        MOVE MFS-RENSA-FAELT            TO MOD-FLOREGPB-IN                
093800     END-IF                                                               
093900                                                                          
094000     IF MID-TIPBLOCK-IN NOT = ALL '+'                                     
094100        MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-TIPBLOCK-IN-ATTR            
094200        MOVE MFS-ROER-EJ-FAELT         TO MOD-TIPBLOCK-IN                 
094300     ELSE                                                                 
094400        MOVE MFS-RENSA-FAELT            TO MOD-TIPBLOCK-IN                
094500     END-IF                                                               
094600     .                                                                    
094700     EJECT                                                                
094800                                                                          
094900                                                                          
095000 F-LAES-VISA-INFO SECTION.                                                
095100                                                                          
095200     PERFORM FA-LAES-GRUNDDATA                                            
095300                                                                          
095400     IF SEGMENT-SAKNAS                                                    
095500*       --- HANTERING AV FELMEDDELANDE FLYTTAD TILL S1- SECTION           
095600        CONTINUE                                                          
095700     ELSE                                                                 
095800        PERFORM FB-NOLLSTALL                                              
095900        PERFORM FC-ARTIKELDATA                                            
096000        PERFORM FD-ORDERINGONG                                            
096100        PERFORM FF-LAES-BENAMNING-WDD3                                    
096200        MOVE WS-MEDDELANDE TO MOD-TEMFSINF                                
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600                                                                          
096700                                                                          
096800 FA-LAES-GRUNDDATA SECTION.                                               
096900                                                                          
097000     PERFORM IMS-GET-WDK601                                               
097100     .                                                                    
097200     EJECT                                                                
097300                                                                          
097400                                                                          
097500 FB-NOLLSTALL SECTION.                                                    
097600     SKIP2                                                                
097700     MOVE ZERO TO ACK-RULL-12-C1                                          
097800                  ACK-RULL-12-C2                                          
097900                  ACK-RULL-12-DIV                                         
098000                  ACK-RULL-12-SATS                                        
098100                  ACK-RULL-12-C1-C2                                       
098200                  ACK-RULL-6-C1                                           
098300                  ACK-RULL-6-C2                                           
098400                  ACK-RULL-6-C1-C2                                        
098500                  ACK-KVPB-SEP                                            
098600                  ACK-KVPB-SATS                                           
098700                  WS-PERIOD-C1-C2 (17)                                    
098800     MOVE +1 TO INDX                                                      
098900     PERFORM UNTIL INDX > 3                                               
099000       MOVE ZERO TO ACK-AAR-C1    (INDX)                                  
099100                    ACK-AAR-C2    (INDX)                                  
099200                    ACK-AAR-DIV   (INDX)                                  
099300                    ACK-AAR-SATS  (INDX)                                  
099400                    ACK-AAR-C1-C2 (INDX)                                  
099500       ADD +1 TO INDX                                                     
099600     END-PERFORM                                                          
099700     EJECT                                                                
099800     MOVE +1 TO INDX                                                      
099900     PERFORM UNTIL INDX > 12                                              
100000       MOVE ZERO TO WS-PERIOD-C1-C2 (INDX)                                
100100       ADD +1 TO INDX                                                     
100200     END-PERFORM                                                          
100300     .                                                                    
100400     EJECT                                                                
100500                                                                          
100600                                                                          
100700 FC-ARTIKELDATA   SECTION.                                                
100800     SKIP2                                                                
100900     MOVE  NEJ  TO SW-SEASON                                              
101000     MOVE  NEJ  TO SW-ERSATT                                              
101100     MOVE SPACE TO MOD-FLMPB                                              
101200     PERFORM IMS-GET-WDK601                                               
101300     MOVE ART-TIFINLV TO SPARAD-TIFINLV                                   
101400     PERFORM FCB-PUBVECKA                                                 
101500     MOVE ART-KDERS-UTG TO SPARAD-KDERS                                   
101600     IF ART-KDERS-UTG > 0                                                 
101700       MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                                
101800       CALL WMEDKONV USING MED-WMEDAREA                                   
101900       MOVE MED-TEMFSFEL TO WS-MEDDELANDE                                 
102000     ELSE                                                                 
102100       PERFORM IMS-GET-WDK611                                             
102200       IF SEGMENT-FINNS                                                   
102300         ADD CLAG-KVPB-SEP   TO ACK-KVPB-SEP                              
102400         MOVE CLAG-KVPB-SEP  TO MOD-KVPB-SEP-CDC                          
102500                               MOD-KVPB-SEP-CDC-2                         
102600         ADD CLAG-KVPB-SATS  TO ACK-KVPB-SATS                             
102700         MOVE CLAG-KVPB-SATS TO MOD-KVPB-SATS                             
102800         MOVE CLAG-TIPBLOCK  TO MOD-TIPBLOCK                              
102900         IF CLAG-FLMPB = JA                                               
103000           IF MSGI-IDLAND-SPR = 'SE'                                      
103100              MOVE ' JA'     TO MOD-FLMPB                                 
103200           ELSE                                                           
103300              MOVE 'YES'     TO MOD-FLMPB                                 
103400           END-IF                                                         
103500         ELSE                                                             
103600           IF MSGI-IDLAND-SPR = 'SE'                                      
103700              MOVE 'NEJ'     TO MOD-FLMPB                                 
103800           ELSE                                                           
103900              MOVE 'NO '     TO MOD-FLMPB                                 
104000           END-IF                                                         
104100         END-IF                                                           
104200         IF CLAG-FLOREGPB = JA                                            
104300           IF MSGI-IDLAND-SPR = 'SE'                                      
104400              MOVE ' JA'     TO MOD-FLOREGPB                              
104500           ELSE                                                           
104600              MOVE 'YES'     TO MOD-FLOREGPB                              
104700           END-IF                                                         
104800         ELSE                                                             
104900           IF MSGI-IDLAND-SPR = 'SE'                                      
105000              MOVE 'NEJ'     TO MOD-FLOREGPB                              
105100           ELSE                                                           
105200              MOVE 'NO '     TO MOD-FLOREGPB                              
105300           END-IF                                                         
105400         END-IF                                                           
105500                                                                          
105600         IF CLAG-KVPB-TREND  NOT = ZERO                                   
105700            COMPUTE WS-KVPB-TREND =                                       
105800                  CLAG-KVPB-TREND                                         
105900******************CLAG-KVPB-TREND / 4.33                                  
106000            MOVE WS-KVPB-TREND   TO                                       
106100                 MOD-KVPB-TREND                                           
106200         ELSE                                                             
106300            MOVE ZERO TO MOD-KVPB-TREND                                   
106400         END-IF                                                           
106500                                                                          
106600         PERFORM IMS-GET-WDK626                                           
106700         IF SEGMENT-FINNS AND WS-MEDDELANDE = SPACE                       
106800           MOVE +1 TO INDX                                                
106900           PERFORM UNTIL INDX > 12                                        
107000             IF JUST-RESEASON (INDX) NOT = +1                             
107100               MOVE MED-1 (SPRAK-IX)                                      
107200                              TO WS-MEDDELANDE                            
107300               MOVE JA        TO SW-SEASON                                
107400               MOVE JA TO FL-SEASON                                       
107500             END-IF                                                       
107600             ADD +1 TO INDX                                               
107700           END-PERFORM                                                    
107800         END-IF                                                           
107900       END-IF                                                             
108000                                                                          
108100       PERFORM IMS-GET-SART                                               
108200       IF SEGMENT-FINNS                                                   
108300         MOVE ZERO TO W-KVPB-REF                                          
108400         PERFORM FCC-BER-PERIODBEHOV                                      
108500         ADD  W-KVPB-REF TO ACK-KVPB-SEP                                  
108600         MOVE W-KVPB-REF TO MOD-KVPB-SEP-DC                               
108700                            MOD-KVPB-SEP-DC-2                             
108800       END-IF                                                             
108900                                                                          
109000       MOVE CLAG-KVPB-PLAN  TO MOD-KVPB-PLAN                              
109100       MOVE CLAG-DAPBPLAN (3:6)                                           
109200                            TO MOD-DAPBPLAN                               
109300       IF CLAG-DAPBPLAN > ZERO                                            
109400          MOVE 'MAN'        TO MOD-PBPLANTYP                              
109500          IF CLAG-DAPBPLAN < WS-DAGENS-AAAAMMDD                           
109600             PERFORM FCA-BER-NYTT-MASK-KVPB-PLAN                          
109700          END-IF                                                          
109800       ELSE                                                               
109900          MOVE 'MASK'       TO MOD-PBPLANTYP                              
110000          PERFORM FCA-BER-NYTT-MASK-KVPB-PLAN                             
110100       END-IF                                                             
110200       MOVE CLAG-KDERS      TO SPARAD-KDERS                               
110300       IF CLAG-KDERS > 0                                                  
110400         MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                              
110500         CALL WMEDKONV USING MED-WMEDAREA                                 
110600         MOVE MED-TEMFSFEL                                                
110700                             TO WS-MEDDELANDE                             
110800         MOVE JA             TO SW-ERSATT                                 
110900       ELSE                                                               
111000         IF CLAG-IDDC-REF NOT = SPACE                                     
111100            MOVE INF-REFILL-PART TO MED-IDMFSFEL                          
111200            CALL WMEDKONV     USING MED-WMEDAREA                          
111300            MOVE MED-MFSFEL      TO MOD-TEMFSFEL                          
111400            PERFORM MFS-CLOSE-FAELT-ATTR                                  
111500         END-IF                                                           
111600       END-IF                                                             
111700     END-IF                                                               
111800     .                                                                    
111900     EJECT                                                                
112000                                                                          
112100 FCA-BER-NYTT-MASK-KVPB-PLAN  SECTION.                                    
112200     SKIP2                                                                
112300     MOVE ART-IDARTNR TO PBTO-IDARTNR                                     
112400     CALL W222PBTO USING PBTO-W222PBTO                                    
112500                         PBTO-WDK6-PCB                                    
112600                         PBTO-WDK7-PCB                                    
112700                         PBTO-ARTM-PCB                                    
112800                         PBTO-2501-PCB                                    
112900                         PBTO-WDB6R-PCB                                   
113000                         PBTO-WDK7R-PCB                                   
113100                         PBTO-WDB6-PCB                                    
113200                         PBTO-WDD7-PCB                                    
113300                         PBTO-WDK7E-PCB                                   
113400                         PBTO-W222-UTIL-WDK6-PCB                          
113500                         PBTO-W222-UTIL-WDK7-PCB                          
113600                         PBTO-W222-UTIL-WDB6-PCB                          
113700                         PBTO-W222-UTUP-WDK7-PCB                          
113800                         PBTO-W222-UTUP-WDB6-PCB                          
113900                         PBTO-W222-UTUP-UTIL-WDK6-PCB                     
114000                         PBTO-W222-UTUP-UTIL-WDK7-PCB                     
114100                         PBTO-W222-UTUP-UTIL-WDB6-PCB                     
114200                                                                          
114300     IF PBTO-KDSVAR = JA                                                  
114400        MOVE PBTO-KVPB-PLAN TO MOD-KVPB-PLAN                              
114500        MOVE 'MASK'         TO MOD-PBPLANTYP                              
114600     END-IF                                                               
114700     .                                                                    
114800     EJECT                                                                
114900                                                                          
115000 FCB-PUBVECKA  SECTION.                                                   
115100     MOVE ART-TIFINLV             TO TMP1-YYWWD                           
115200     MOVE DAGENS-AAVVD-2AAR       TO TMP2-YYWWD                           
115300     PERFORM WY2000P2                                                     
115400     SUBTRACT +2000       FROM TMP2-YYWWD                                 
115500     IF (TMP1-YYWWD > TMP2-YYWWD)                                         
115600               MOVE MED-4 (SPRAK-IX)                                      
115700                              TO WS-MEDDELANDE                            
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100 FCC-BER-PERIODBEHOV SECTION.                                             
116200                                                                          
116300     PERFORM IMS-GET-SLAG                                                 
116400     PERFORM UNTIL SEGMENT-SAKNAS                                         
116500        MOVE SLAG-IDDC             TO WS-IDDC                             
116600        IF SLAG-IDDC-REF = WC-CDC-SE                                      
116700           COMPUTE W-KVPB-REF = W-KVPB-REF    +                           
116800                                SLAG-KVPB-REF +                           
116900                                SLAG-KVPBREOI                             
117000           ADD SLAG-KVPBREOI TO W-KVPBREOI                                
117100        END-IF                                                            
117200        PERFORM IMS-GET-SLAG                                              
117300     END-PERFORM                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 FD-ORDERINGONG     SECTION.                                              
117700     SKIP2                                                                
117800*    *****  HÄMTA DATUM                                                   
117900*                                                                         
118000     MOVE DAGENS-PERIOD TO START-PERIOD                                   
118100     IF DAGENS-AAR = 00                                                   
118200       MOVE 98          TO START-AAR                                      
118300     ELSE                                                                 
118400       IF DAGENS-AAR = 01                                                 
118500         MOVE 99        TO START-AAR                                      
118600       ELSE                                                               
118700         COMPUTE START-AAR = DAGENS-AAR - 2                               
118800       END-IF                                                             
118900     END-IF                                                               
119000                                                                          
119100     MOVE +1                 TO MOD-IX                                    
119200     MOVE ZERO               TO WS-KVOI-PROG-SUM                          
119300                                WS-KVOI-DC-VK-SUM                         
119400                                WS-KVOI-DIV-SUM                           
119500                                WS-KVOI-SATS-SUM                          
119600                                WS-KVOI-PROG-RULL-12                      
119700                                WS-KVOI-DC-VK-RULL-12                     
119800                                WS-KVOI-DIV-RULL-12                       
119900                                WS-KVOI-SATS-RULL-12                      
120000                                WS-KVOI-PROG-RULL-6                       
120100                                WS-KVOI-DC-VK-RULL-6                      
120200                                                                          
120300************* START ÅR ************                                       
120400     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 2                              
120500     MOVE W-TIAAAA (3:2)     TO MOD-AAR (1)                               
120600     PERFORM IMS-GET-WDL811                                               
120700                                                                          
120800     IF SEGMENT-FINNS                                                     
120900                                                                          
121000       MOVE +1               TO VECKA-IX                                  
121100                                                                          
121200       PERFORM UNTIL VECKA-IX > +52                                       
121300         ADD AAR-KVOI-PROG     (VECKA-IX)                                 
121400                             TO WS-KVOI-PROG-SUM                          
121500         ADD AAR-KVOI-SDC      (VECKA-IX)                                 
121600                             TO WS-KVOI-DC-VK-SUM                         
121700         ADD AAR-KVOI-NDC      (VECKA-IX)                                 
121800                             TO WS-KVOI-DC-VK-SUM                         
121900         ADD AAR-KVOI-DIV      (VECKA-IX)                                 
122000                             TO WS-KVOI-DIV-SUM                           
122100         ADD AAR-KVOI-SATS     (VECKA-IX)                                 
122200                             TO WS-KVOI-SATS-SUM                          
122300         ADD +1              TO VECKA-IX                                  
122400       END-PERFORM                                                        
122500                                                                          
122600       MOVE WS-KVOI-PROG-SUM TO MOD-AAR-CDC (1)                           
122700       MOVE WS-KVOI-DC-VK-SUM                                             
122800                             TO MOD-AAR-DCVK (1)                          
122900       MOVE WS-KVOI-DIV-SUM                                               
123000                             TO MOD-AAR-DIV  (1)                          
123100       MOVE WS-KVOI-SATS-SUM                                              
123200                             TO MOD-AAR-SATS (1)                          
123300     END-IF                                                               
123400************* FÖREGÅENDE ÅR ************                                  
123500     MOVE ZERO               TO WS-KVOI-PROG-SUM                          
123600                                WS-KVOI-DC-VK-SUM                         
123700                                WS-KVOI-DIV-SUM                           
123800                                WS-KVOI-SATS-SUM                          
123900     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 1                              
124000     MOVE W-TIAAAA (3:2)     TO MOD-AAR (2)                               
124100     PERFORM IMS-GET-WDL811                                               
124200                                                                          
124300     IF SEGMENT-FINNS                                                     
124400                                                                          
124500       MOVE +1               TO PER-IX                                    
124600       MOVE WS-PERTAB-START-VV-1 (PER-IX)                                 
124700                             TO VECKA-IX                                  
124800                                                                          
124900       PERFORM UNTIL VECKA-IX NOT <                                       
125000                     WS-PERTAB-START-VV-1 (DAGENS-PERIOD)                 
125100         ADD AAR-KVOI-PROG     (VECKA-IX)                                 
125200                             TO WS-KVOI-PROG-SUM                          
125300         ADD AAR-KVOI-SDC      (VECKA-IX)                                 
125400                             TO WS-KVOI-DC-VK-SUM                         
125500         ADD AAR-KVOI-NDC      (VECKA-IX)                                 
125600                             TO WS-KVOI-DC-VK-SUM                         
125700         ADD AAR-KVOI-DIV      (VECKA-IX)                                 
125800                             TO WS-KVOI-DIV-SUM                           
125900         ADD AAR-KVOI-SATS     (VECKA-IX)                                 
126000                             TO WS-KVOI-SATS-SUM                          
126100         ADD +1              TO VECKA-IX                                  
126200       END-PERFORM                                                        
126300                                                                          
126400       MOVE DAGENS-PERIOD    TO PER-IX                                    
126500       MOVE WS-PERTAB-START-VV-1 (PER-IX)                                 
126600                             TO VECKA-IX                                  
126700                                                                          
126800       PERFORM UNTIL VECKA-IX > +52                                       
126900                                                                          
127000         MOVE ZERO           TO WS-KVOI-PROG                              
127100                                WS-KVOI-DC-VK                             
127200                                WS-KVOI-DIV                               
127300                                WS-KVOI-SATS                              
127400         PERFORM UNTIL VECKA-IX >                                         
127500                       WS-PERTAB-SLUT-VV-1 (PER-IX)                       
127600                                                                          
127700           ADD AAR-KVOI-PROG (VECKA-IX)                                   
127800                             TO WS-KVOI-PROG                              
127900                                WS-KVOI-PROG-SUM                          
128000           ADD AAR-KVOI-SDC    (VECKA-IX)                                 
128100                             TO WS-KVOI-DC-VK                             
128200                                WS-KVOI-DC-VK-SUM                         
128300           ADD AAR-KVOI-NDC    (VECKA-IX)                                 
128400                             TO WS-KVOI-DC-VK                             
128500                                WS-KVOI-DC-VK-SUM                         
128600           ADD AAR-KVOI-DIV    (VECKA-IX)                                 
128700                             TO WS-KVOI-DIV                               
128800                                WS-KVOI-DIV-SUM                           
128900           ADD AAR-KVOI-SATS   (VECKA-IX)                                 
129000                             TO WS-KVOI-SATS                              
129100                                WS-KVOI-SATS-SUM                          
129200           ADD +1            TO VECKA-IX                                  
129300         END-PERFORM                                                      
129400                                                                          
129500         MOVE WS-PERTAB-KVVIPER-1 (PER-IX)                                
129600                             TO MOD-KVVIPER      (MOD-IX)                 
129700         MOVE WS-KVOI-PROG   TO MOD-KVOI-CDC     (MOD-IX)                 
129800         MOVE WS-KVOI-DC-VK  TO MOD-KVOI-DCVK    (MOD-IX)                 
129900         MOVE WS-KVOI-DIV    TO MOD-KVOI-DIV     (MOD-IX)                 
130000         MOVE WS-KVOI-SATS   TO MOD-KVOI-SATS    (MOD-IX)                 
130100                                                                          
130200         ADD WS-KVOI-PROG    TO WS-KVOI-PROG-RULL-12                      
130300         ADD WS-KVOI-DC-VK   TO WS-KVOI-DC-VK-RULL-12                     
130400         ADD WS-KVOI-DIV     TO WS-KVOI-DIV-RULL-12                       
130500         ADD WS-KVOI-SATS    TO WS-KVOI-SATS-RULL-12                      
130600         IF MOD-IX > +6                                                   
130700           ADD WS-KVOI-PROG  TO WS-KVOI-PROG-RULL-6                       
130800           ADD WS-KVOI-DC-VK TO WS-KVOI-DC-VK-RULL-6                      
130900         END-IF                                                           
131000         ADD +1              TO MOD-IX                                    
131100                                PER-IX                                    
131200                                                                          
131300       END-PERFORM                                                        
131400       MOVE WS-KVOI-PROG-SUM TO MOD-AAR-CDC (2)                           
131500       MOVE WS-KVOI-DC-VK-SUM                                             
131600                             TO MOD-AAR-DCVK (2)                          
131700       MOVE WS-KVOI-DIV-SUM                                               
131800                             TO MOD-AAR-DIV  (2)                          
131900       MOVE WS-KVOI-SATS-SUM                                              
132000                             TO MOD-AAR-SATS (2)                          
132100     ELSE                                                                 
132200       ADD +13               TO MOD-IX                                    
132300       SUBTRACT DAGENS-PERIOD                                             
132400                             FROM MOD-IX                                  
132500     END-IF                                                               
132600************* I ÅR ************                                           
132700     MOVE ZERO               TO WS-KVOI-PROG-SUM                          
132800                                WS-KVOI-DC-VK-SUM                         
132900                                WS-KVOI-DIV-SUM                           
133000                                WS-KVOI-SATS-SUM                          
133100     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA                                  
133200     MOVE W-TIAAAA (3:2)     TO MOD-AAR (3)                               
133300     PERFORM IMS-GET-WDL811                                               
133400                                                                          
133500     IF SEGMENT-FINNS                                                     
133600                                                                          
133700       MOVE +1               TO PER-IX                                    
133800       MOVE WS-PERTAB-START-VV-0 (PER-IX)                                 
133900                             TO VECKA-IX                                  
134000                                                                          
134100       IF DAGENS-PERIOD > 1                                               
134200                                                                          
134300         PERFORM UNTIL VECKA-IX >                                         
134400                         WS-PERTAB-SLUT-VV-0 (DAGENS-PERIOD - 1)          
134500                                                                          
134600           MOVE ZERO         TO WS-KVOI-PROG                              
134700                                WS-KVOI-DC-VK                             
134800                                WS-KVOI-DIV                               
134900                                WS-KVOI-SATS                              
135000           PERFORM UNTIL VECKA-IX >                                       
135100                         WS-PERTAB-SLUT-VV-0 (PER-IX)                     
135200                                                                          
135300             ADD AAR-KVOI-PROG (VECKA-IX)                                 
135400                             TO WS-KVOI-PROG                              
135500                                WS-KVOI-PROG-SUM                          
135600             ADD AAR-KVOI-SDC    (VECKA-IX)                               
135700                             TO WS-KVOI-DC-VK                             
135800                                WS-KVOI-DC-VK-SUM                         
135900             ADD AAR-KVOI-NDC    (VECKA-IX)                               
136000                             TO WS-KVOI-DC-VK                             
136100                                WS-KVOI-DC-VK-SUM                         
136200             ADD AAR-KVOI-DIV    (VECKA-IX)                               
136300                             TO WS-KVOI-DIV                               
136400                                WS-KVOI-DIV-SUM                           
136500             ADD AAR-KVOI-SATS   (VECKA-IX)                               
136600                             TO WS-KVOI-SATS                              
136700                                WS-KVOI-SATS-SUM                          
136800             ADD +1          TO VECKA-IX                                  
136900           END-PERFORM                                                    
137000                                                                          
137100           MOVE WS-PERTAB-KVVIPER-0 (PER-IX)                              
137200                             TO MOD-KVVIPER      (MOD-IX)                 
137300           MOVE WS-KVOI-PROG TO MOD-KVOI-CDC  (MOD-IX)                    
137400           MOVE WS-KVOI-DC-VK                                             
137500                             TO MOD-KVOI-DCVK (MOD-IX)                    
137600           MOVE WS-KVOI-DIV                                               
137700                             TO MOD-KVOI-DIV  (MOD-IX)                    
137800           MOVE WS-KVOI-SATS                                              
137900                             TO MOD-KVOI-SATS (MOD-IX)                    
138000           ADD WS-KVOI-PROG  TO WS-KVOI-PROG-RULL-12                      
138100           ADD WS-KVOI-DC-VK                                              
138200                             TO WS-KVOI-DC-VK-RULL-12                     
138300           ADD WS-KVOI-DIV                                                
138400                             TO WS-KVOI-DIV-RULL-12                       
138500           ADD WS-KVOI-SATS                                               
138600                             TO WS-KVOI-SATS-RULL-12                      
138700           IF MOD-IX > +6                                                 
138800             ADD WS-KVOI-PROG                                             
138900                             TO WS-KVOI-PROG-RULL-6                       
139000             ADD WS-KVOI-DC-VK                                            
139100                             TO WS-KVOI-DC-VK-RULL-6                      
139200           END-IF                                                         
139300           ADD +1            TO MOD-IX                                    
139400                                PER-IX                                    
139500                                                                          
139600         END-PERFORM                                                      
139700                                                                          
139800       END-IF                                                             
139900                                                                          
140000       MOVE ZERO             TO WS-KVOI-PROG                              
140100                                WS-KVOI-DC-VK                             
140200                                WS-KVOI-DIV                               
140300                                WS-KVOI-SATS                              
140400       PERFORM UNTIL VECKA-IX >                                           
140500                     WS-PERTAB-SLUT-VV-0 (PER-IX)                         
140600                                                                          
140700         ADD AAR-KVOI-PROG   (VECKA-IX)                                   
140800                             TO WS-KVOI-PROG                              
140900                                WS-KVOI-PROG-SUM                          
141000         ADD AAR-KVOI-SDC    (VECKA-IX)                                   
141100                             TO WS-KVOI-DC-VK                             
141200                                WS-KVOI-DC-VK-SUM                         
141300         ADD AAR-KVOI-NDC    (VECKA-IX)                                   
141400                             TO WS-KVOI-DC-VK                             
141500                                WS-KVOI-DC-VK-SUM                         
141600         ADD AAR-KVOI-DIV    (VECKA-IX)                                   
141700                             TO WS-KVOI-DIV                               
141800                                WS-KVOI-DIV-SUM                           
141900         ADD AAR-KVOI-SATS   (VECKA-IX)                                   
142000                             TO WS-KVOI-SATS                              
142100                                WS-KVOI-SATS-SUM                          
142200         ADD +1              TO VECKA-IX                                  
142300       END-PERFORM                                                        
142400                                                                          
142500       MOVE WS-KVOI-PROG     TO MOD-KVOI-INNEV-CDC                        
142600       MOVE WS-KVOI-DC-VK    TO MOD-KVOI-INNEV-DCVK                       
142700       MOVE WS-KVOI-DIV      TO MOD-KVOI-INNEV-DIV                        
142800       MOVE WS-KVOI-SATS     TO MOD-KVOI-INNEV-SATS                       
142900                                                                          
143000       MOVE WS-KVOI-PROG-SUM                                              
143100                             TO MOD-AAR-CDC (3)                           
143200       MOVE WS-KVOI-DC-VK-SUM                                             
143300                             TO MOD-AAR-DCVK (3)                          
143400       MOVE WS-KVOI-DIV-SUM                                               
143500                             TO MOD-AAR-DIV  (3)                          
143600       MOVE WS-KVOI-SATS-SUM                                              
143700                             TO MOD-AAR-SATS (3)                          
143800     END-IF                                                               
143900                                                                          
144000     PERFORM F6-REDIGERA-TOTALER                                          
144100     .                                                                    
144200     EJECT                                                                
144300                                                                          
144400                                                                          
144500 FF-LAES-BENAMNING-WDD3    SECTION.                                       
144600                                                                          
144700     PERFORM IMS-GET-BENA11-BSEQ                                          
144800     MOVE WDD311-TEXT-BEART TO MOD-BEART                                  
144900     .                                                                    
145000     EJECT                                                                
145100                                                                          
145200 F1-DATUM SECTION.                                                        
145300                                                                          
145400                                                                          
145500     MOVE 'IDAG  '          TO DAT-KDDATFORM                              
145600                                                                          
145700     CALL WDATKONV  USING  DAT-KDDATFORM                                  
145800                           DAT-I-TIDATUM                                  
145900                           DAT-O-TIDATUM                                  
146000                           DAT-KDSVAR                                     
146100                                                                          
146200     MOVE DAT-TISEKEL       TO WS-DAGENS-SEKEL                            
146300     MOVE DAT-TIAA          TO DAGENS-AAR    WS-DAGENS-AA                 
146400     MOVE DAT-TIVV          TO DAGENS-VECKA                               
146500     MOVE DAT-TID           TO DAGENS-DAG                                 
146600     MOVE DAT-TIRP          TO DAGENS-PERIOD                              
146700     MOVE WS-DAGENS-AAVVD   TO DAGENS-AAVVD                               
146800                               DAGENS-AAVVD-2AAR                          
146900     MOVE 'AARP  '           TO DAT-KDDATFORM                             
147000     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
147100                                                                          
147200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
147300                     DAT-O-TIDATUM DAT-KDSVAR                             
147400                                                                          
147500     IF DAT-KDSVAR-OK                                                     
147600****             RÄKNA UT VECKA I AKTUELL PERIOD                          
147700                                                                          
147800       COMPUTE WS-VECKA-I-PER = DAGENS-VECKA - DAT-TIVV + 1               
147900       MOVE ':'              TO WS-KOLON                                  
148000       MOVE DAT-KVVIPER      TO WS-KVVIPER                                
148100                                                                          
148200     ELSE                                                                 
148300         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
148400         DELIMITED BY SIZE INTO FELTEXT                                   
148500         CALL FELLOG                                                      
148600     END-IF                                                               
148700     .                                                                    
148800     EJECT                                                                
148900                                                                          
149000                                                                          
149100                                                                          
149200 F6-REDIGERA-TOTALER SECTION.                                             
149300                                                                          
149400     MOVE WS-KVOI-PROG-RULL-12                                            
149500                             TO MOD-KVOI-RULL-12-CDC                      
149600     MOVE WS-KVOI-DIV-RULL-12                                             
149700                             TO MOD-KVOI-RULL-12-DIV                      
149800     MOVE WS-KVOI-DC-VK-RULL-12                                           
149900                             TO MOD-KVOI-RULL-12-DCVK                     
150000     MOVE WS-KVOI-SATS-RULL-12                                            
150100                             TO MOD-KVOI-RULL-12-SATS                     
150200                                                                          
150300     COMPUTE WS-KVOI-SNITT-12-PROG ROUNDED =                              
150400             WS-KVOI-PROG-RULL-12 / 12                                    
150500     COMPUTE WS-KVOI-SNITT-12-DC-VK ROUNDED =                             
150600            (WS-KVOI-DC-VK-RULL-12 + W-KVPBREOI) / 12                     
150700     MOVE WS-KVOI-SNITT-12-PROG                                           
150800                             TO MOD-AARSFORB-CDC                          
150900     MOVE WS-KVOI-SNITT-12-DC-VK                                          
151000                             TO MOD-AARSFORB-DC                           
151100                                                                          
151200     COMPUTE MOD-AARSFORB-HALV-CDC =                                      
151300         WS-KVOI-PROG-RULL-6 / 6                                          
151400     COMPUTE MOD-AARSFORB-HALV-DC ROUNDED =                               
151500        (WS-KVOI-DC-VK-RULL-6 + W-KVPBREOI) / 6                           
151600                                                                          
151700*                                                                         
151800**** REDIGERA ÅRSTOTALER ****  TOMMA                                      
151900*                                                                         
152000     MOVE +1 TO INDX                                                      
152100     PERFORM UNTIL INDX > 3                                               
152200       MOVE START-AAR TO MOD-AAR (INDX)                                   
152300       ADD +1 TO START-AAR                                                
152400       ADD +1 TO INDX                                                     
152500     END-PERFORM                                                          
152600*                                                                         
152700     .                                                                    
152800     EJECT                                                                
152900                                                                          
153000 G-KOLLA-INPUT SECTION.                                                   
153100                                                                          
153200     MOVE JA  TO INDATA-SW                                                
153300     IF MID-INPUT = ALL '+'                                               
153400        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
153500        CALL WMEDKONV USING MED-WMEDAREA                                  
153600        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
153700        PERFORM MFS-RENSA-FAELT-IN                                        
153800        PERFORM MFS-ROER-EJ-FAELT-UT                                      
153900        MOVE NEJ TO INDATA-SW                                             
154000     ELSE                                                                 
154100        PERFORM GA-INDATA-KONTROLL                                        
154200        IF INDATA-OK                                                      
154300           PERFORM GB-DATABAS-KONTROLL                                    
154400           IF INDATA-OK                                                   
154500             PERFORM GC-SAMLA-DATA                                        
154600             PERFORM GD-BEARBETA                                          
154700           END-IF                                                         
154800        ELSE                                                              
154900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
155000           CALL WMEDKONV USING MED-WMEDAREA                               
155100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
155200        END-IF                                                            
155300        IF INDATA-FEL                                                     
155400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
155500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
155600        END-IF                                                            
155700     END-IF                                                               
155800     .                                                                    
155900     EJECT                                                                
156000                                                                          
156100                                                                          
156200 GA-INDATA-KONTROLL SECTION.                                              
156300                                                                          
156400                                                                          
156500***KVPB-C1                                                                
156600                                                                          
156700     IF MID-KVPB-C1-IN NOT = ALL '+'                                      
156800        MOVE MID-KVPB-C1-IN TO KVPB-X                                     
156900                               DEC-IDFRIDATA                              
157000                                                                          
157100        MOVE 6               TO DEC-KVHELTAL                              
157200        MOVE 1               TO DEC-KVDECIMAL                             
157300        CALL WDECEDIT USING DEC-WDECAREA                                  
157400        IF DEC-KDSVAR-OK                                                  
157500          MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPB-CDC-IN-ATTR                
157600          MOVE DEC-IDEDITDATA TO WS-IN-KVPB-SEP(1)                        
157700        ELSE                                                              
157800           MOVE NEJ TO INDATA-SW                                          
157900           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-CDC-IN-ATTR                 
158000        END-IF                                                            
158100     ELSE                                                                 
158200        MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPB-CDC-IN-ATTR                  
158300     END-IF                                                               
158400                                                                          
158500                                                                          
158600***KVPB-C2                                                                
158700                                                                          
158800                                                                          
158900***FLMPB-C1                                                               
159000                                                                          
159100     IF MID-FLMPB-C1-IN NOT = ALL '+'                                     
159200        IF MID-FLMPB-C1-IN = JA OR YES OR NEJ                             
159300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMPB-IN-ATTR                 
159400           MOVE MID-FLMPB-C1-IN TO WS-IN-FLMPB-C1                         
159500        ELSE                                                              
159600           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMPB-IN-ATTR                   
159700           MOVE NEJ TO INDATA-SW                                          
159800        END-IF                                                            
159900     ELSE                                                                 
160000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMPB-IN-ATTR                    
160100     END-IF                                                               
160200                                                                          
160300                                                                          
160400***FLOREGPB                                                               
160500                                                                          
160600     IF MID-FLOREGPB-IN NOT = ALL '+'                                     
160700        IF MID-FLOREGPB-IN = JA OR YES OR NEJ                             
160800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOREGPB-IN-ATTR              
160900           MOVE MID-FLOREGPB-IN TO WS-IN-FLOREGPB                         
161000        ELSE                                                              
161100           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOREGPB-IN-ATTR                
161200           MOVE NEJ TO INDATA-SW                                          
161300        END-IF                                                            
161400     ELSE                                                                 
161500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOREGPB-IN-ATTR                 
161600     END-IF                                                               
161700                                                                          
161800***TIPBLOCK                                                               
161900                                                                          
162000     IF MID-TIPBLOCK-IN NOT = ALL '+'                                     
162100        MOVE 'AAMMDD'          TO DAT-KDDATFORM                           
162200        MOVE MID-TIPBLOCK-IN   TO DAT-I-TIDATUM                           
162300                                                                          
162400                                                                          
162500        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
162600                            DAT-O-TIDATUM DAT-KDSVAR                      
162700                                                                          
162800        IF DAT-KDSVAR-OK                                                  
162900          MOVE MFS-NUM-FAELT-RAETT                                        
163000                               TO MOD-TIPBLOCK-IN-ATTR                    
163100          MOVE MID-TIPBLOCK-IN TO WS-IN-TIPBLOCK                          
163200        ELSE                                                              
163300          MOVE MFS-ALFA-FAELT-FEL                                         
163400                               TO MOD-TIPBLOCK-IN-ATTR                    
163500          MOVE NEJ TO INDATA-SW                                           
163600        END-IF                                                            
163700     ELSE                                                                 
163800        MOVE MFS-NUM-FAELT-RAETT TO MOD-TIPBLOCK-IN-ATTR                  
163900     END-IF                                                               
164000     .                                                                    
164100     EJECT                                                                
164200                                                                          
164300                                                                          
164400 GB-DATABAS-KONTROLL SECTION.                                             
164500                                                                          
164600     PERFORM IMS-GET-WDK601                                               
164700     IF SEGMENT-FINNS                                                     
164800        MOVE ART-IDLEVNR           TO WS-IDLEVNR                          
164900     ELSE                                                                 
165000        MOVE NEJ                   TO INDATA-SW                           
165100        MOVE '017'                 TO MED-IDMFSFEL                        
165200        CALL WMEDKONV           USING MED-WMEDAREA                        
165300        MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                        
165400     END-IF                                                               
165500     .                                                                    
165600     EJECT                                                                
165700                                                                          
165800                                                                          
165900 GC-SAMLA-DATA SECTION.                                                   
166000                                                                          
166100     MOVE +1 TO CL-IX                                                     
166200     PERFORM UNTIL CL-IX > 2                                              
166300        MOVE ZERO TO  WS-KDERS(CL-IX)                                     
166400                      WS-KVPB-SEP(CL-IX)                                  
166500                      WS-TIPBDAT(CL-IX)                                   
166600                      WS-RVPROFEL(CL-IX)                                  
166700                      WS-RVPROURS(CL-IX)                                  
166800                      WS-KVUTJFEL(CL-IX)                                  
166900                      WS-KVPB-SATS(CL-IX)                                 
167000                      WS-KVMAD-SEP(CL-IX)                                 
167100                      WS-KVMAD-TOT(CL-IX)                                 
167200                                                                          
167300        ADD +1 TO CL-IX                                                   
167400     END-PERFORM                                                          
167500                                                                          
167600                                                                          
167700*WDD650                                                                   
167800                                                                          
167900     PERFORM IMS-GET-WDK611                                               
168000     IF SEGMENT-FINNS                                                     
168100        MOVE CLAG-IDLKTO TO WS-IDLKTO                                     
168200*WDD660                                                                   
168300        MOVE CLAG-KDUART   TO WS-KDUART                                   
168400*WDD661                                                                   
168500        MOVE CLAG-KDERS    TO WS-KDERS(1)                                 
168600*WDD631                                                                   
168700        MOVE CLAG-KVPB-SEP    TO WS-KVPB-SEP(1)                           
168800        MOVE CLAG-KVPB-SATS   TO WS-KVPB-SATS(1)                          
168900        MOVE CLAG-KVMAD-SEP   TO WS-KVMAD-SEP(1)                          
169000        MOVE CLAG-KVMAD-TOT   TO WS-KVMAD-TOT(1)                          
169100     END-IF                                                               
169200     EJECT                                                                
169300*SDC                                                                      
169400     PERFORM IMS-GET-SART                                                 
169500     IF SEGMENT-FINNS                                                     
169600        MOVE ZERO            TO WS-KVPB-SATS (2)                          
169700        MOVE ZERO            TO WS-KVMAD-SEP (2)                          
169800        MOVE ZERO            TO WS-KDERS     (2)                          
169900        MOVE ZERO            TO WS-KVRESS-C2                              
170000        MOVE ZERO            TO W-KVPB-REF                                
170100        PERFORM IMS-GET-SLAG                                              
170200        PERFORM UNTIL SEGMENT-SAKNAS                                      
170300           ADD SLAG-KVPB-REF TO W-KVPB-REF                                
170400           PERFORM IMS-GET-SLAG                                           
170500        END-PERFORM                                                       
170600        MOVE W-KVPB-REF      TO WS-KVPB-SEP (2)                           
170700     END-IF                                                               
170800     .                                                                    
170900     EJECT                                                                
171000                                                                          
171100                                                                          
171200 GD-BEARBETA SECTION.                                                     
171300                                                                          
171400     IF MID-KVPB-C1-IN NOT = ALL '+'                                      
171500        PERFORM GDA-KOLLA-KVPB-C1                                         
171600     END-IF                                                               
171700                                                                          
171800     IF INDATA-OK                                                         
171900        IF MID-FLMPB-C1-IN NOT = ALL '+'                                  
172000        PERFORM GDC-KOLLA-FLMPB-C1                                        
172100        END-IF                                                            
172200     END-IF                                                               
172300                                                                          
172400     IF INDATA-OK                                                         
172500        IF MID-FLOREGPB-IN NOT = ALL '+'                                  
172600        PERFORM GDD-KOLLA-FLOREGPB                                        
172700        END-IF                                                            
172800     END-IF                                                               
172900                                                                          
173000     IF INDATA-OK                                                         
173100        IF MID-TIPBLOCK-IN NOT = ALL '+'                                  
173200        PERFORM GDE-KOLLA-TIPBLOCK                                        
173300        END-IF                                                            
173400     END-IF                                                               
173500     .                                                                    
173600     EJECT                                                                
173700                                                                          
173800                                                                          
173900  GDA-KOLLA-KVPB-C1 SECTION.                                              
174000                                                                          
174100     IF WS-KDERS(1) > 10                                                  
174200        MOVE NEJ TO INDATA-SW                                             
174300        MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                               
174400        CALL WMEDKONV USING MED-WMEDAREA                                  
174500        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
174600        MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-CDC-IN-ATTR                    
174700     ELSE                                                                 
174800        IF WS-KVPB-SEP (1) NOT = WS-IN-KVPB-SEP (1)                       
174900           PERFORM G1-BEARBETA-PROGNOS                                    
175000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVPB-SEP-CDC-ATTR            
175100        ELSE                                                              
175200           MOVE NEJ TO INDATA-SW                                          
175300           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-CDC-IN-ATTR                 
175400           MOVE MED-3 (SPRAK-IX) TO MOD-TEMFSFEL                          
175500        END-IF                                                            
175600     END-IF                                                               
175700     .                                                                    
175800     EJECT                                                                
175900                                                                          
176000                                                                          
176100 GDC-KOLLA-FLMPB-C1 SECTION.                                              
176200                                                                          
176300     IF MID-FLMPB-C1-IN NOT = ALL '+'                                     
176400        IF WS-KDERS(1) > 10                                               
176500           MOVE NEJ TO INDATA-SW                                          
176600           MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                            
176700           CALL WMEDKONV USING MED-WMEDAREA                               
176800           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
176900           MOVE MFS-ROER-EJ-FAELT TO MOD-FLMPB-IN                         
177000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMPB-IN-ATTR                   
177100        ELSE                                                              
177200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMPB-ATTR                   
177300        END-IF                                                            
177400     END-IF                                                               
177500     .                                                                    
177600     EJECT                                                                
177700                                                                          
177800                                                                          
177900 GDD-KOLLA-FLOREGPB SECTION.                                              
178000                                                                          
178100     IF MID-FLOREGPB-IN NOT = ALL '+'                                     
178200        IF WS-KDERS(1) > 10                                               
178300           MOVE NEJ TO INDATA-SW                                          
178400           MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                            
178500           CALL WMEDKONV USING MED-WMEDAREA                               
178600           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
178700           MOVE MFS-ROER-EJ-FAELT TO MOD-FLOREGPB-IN                      
178800           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOREGPB-IN-ATTR                
178900        ELSE                                                              
179000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLOREGPB-ATTR                
179100        END-IF                                                            
179200     END-IF                                                               
179300     .                                                                    
179400     EJECT                                                                
179500                                                                          
179600 GDE-KOLLA-TIPBLOCK SECTION.                                              
179700                                                                          
179800     IF MID-TIPBLOCK-IN NOT = ALL '+'                                     
179900        IF WS-KDERS(1) > 10                                               
180000           MOVE NEJ TO INDATA-SW                                          
180100           MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                            
180200           CALL WMEDKONV USING MED-WMEDAREA                               
180300           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
180400           MOVE MFS-ROER-EJ-FAELT TO MOD-TIPBLOCK-IN                      
180500           MOVE MFS-ALFA-FAELT-FEL TO MOD-TIPBLOCK-IN-ATTR                
180600        ELSE                                                              
180700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIPBLOCK-IN-ATTR             
180800        END-IF                                                            
180900     END-IF                                                               
181000     .                                                                    
181100     EJECT                                                                
181200                                                                          
181300 G1-BEARBETA-PROGNOS SECTION.                                             
181400                                                                          
181500     MOVE 'IDAG' TO DAT-KDDATFORM                                         
181600     CALL WDATKONV USING DAT-KDDATFORM                                    
181700                         DAT-I-TIDATUM                                    
181800                         DAT-O-TIDATUM                                    
181900                         DAT-KDSVAR                                       
182000     IF DAT-KDSVAR-OK                                                     
182100        MOVE DAT-TIAAVVD TO WS-TIPBDAT(1)                                 
182200     END-IF                                                               
182300     MOVE ZERO TO WS-RVPROFEL(1)                                          
182400     MOVE ZERO TO WS-RVPROURS(1)                                          
182500     MOVE ZERO TO WS-KVUTJFEL(1)                                          
182600     IF WS-IN-KVPB-SEP(1)     > ZERO                                      
182700       IF  WS-KDUART  = 'P'                                               
182800         MOVE SPACE TO WS-KDUART                                          
182900       END-IF                                                             
183000*--------------------------------------------MANUELLT KVPB ?              
183100       IF CLAG-DAPBPLAN > ZERO                                            
183200          MOVE CLAG-KVPB-PLAN TO WS-KVPB-PLAN                             
183300       ELSE                                                               
183400                                                                          
183500          MOVE ART-IDARTNR TO PBTO-IDARTNR                                
183600          CALL W222PBTO USING PBTO-W222PBTO                               
183700                              PBTO-WDK6-PCB                               
183800                              PBTO-WDK7-PCB                               
183900                              PBTO-ARTM-PCB                               
184000                              PBTO-2501-PCB                               
184100                              PBTO-WDB6R-PCB                              
184200                              PBTO-WDK7R-PCB                              
184300                              PBTO-WDB6-PCB                               
184400                              PBTO-WDD7-PCB                               
184500                              PBTO-WDK7E-PCB                              
184600                              PBTO-W222-UTIL-WDK6-PCB                     
184700                              PBTO-W222-UTIL-WDK7-PCB                     
184800                              PBTO-W222-UTIL-WDB6-PCB                     
184900                              PBTO-W222-UTUP-WDK7-PCB                     
185000                              PBTO-W222-UTUP-WDB6-PCB                     
185100                              PBTO-W222-UTUP-UTIL-WDK6-PCB                
185200                              PBTO-W222-UTUP-UTIL-WDK7-PCB                
185300                              PBTO-W222-UTUP-UTIL-WDB6-PCB                
185400                                                                          
185500          IF PBTO-KDSVAR = JA                                             
185600             MOVE PBTO-KVPB-PLAN TO WS-KVPB-PLAN                          
185700*          * DÅ KVPB-PLAN SOM HÄMTATS EJ HUNNIT PÅVERKAS AV DET           
185800*          * NYA KVPB-SEP, JUSTERAS KVPB-PLAN MED SKILLNADEN FÖR          
185900*          * KVPB-SEP                                                     
186000             COMPUTE WS-SKILLNAD-PB =                                     
186100                CLAG-KVPB-SEP - WS-IN-KVPB-SEP (1)                        
186200             COMPUTE WS-KVPB-PLAN = WS-KVPB-PLAN - WS-SKILLNAD-PB         
186300          ELSE                                                            
186400             MOVE ZERO           TO WS-KVPB-PLAN                          
186500          END-IF                                                          
186600       END-IF                                                             
186700*--------------------------------------------                             
186800       IF WS-IN-KVPB-SEP(1) < WS-KVPB-SEP(1)                              
186900         COMPUTE WS-KVMAD-SEP(1)     ROUNDED =                            
187000                 WS-KVMAD-SEP(1) * WS-IN-KVPB-SEP(1) /                    
187100                                   WS-KVPB-SEP(1)                         
187200       END-IF                                                             
187300       IF WS-IN-KVPB-SEP(1) < WS-KVPB-SEP(1)                              
187400          COMPUTE WS-PBTOT-GAMMAL = WS-KVPB-SEP (1)                       
187500                                  + WS-KVPB-SEP (2)                       
187600                                  + WS-KVPB-SATS(1)                       
187700          COMPUTE WS-PBTOT-NY     = WS-IN-KVPB-SEP (1)                    
187800                                  + WS-KVPB-SEP (2)                       
187900                                  + WS-KVPB-SATS(1)                       
188000          COMPUTE WS-KVMAD-TOT-DEC ROUNDED =                              
188100                  WS-PBTOT-NY / WS-PBTOT-GAMMAL                           
188200                * WS-KVMAD-TOT(1)                                         
188300          COMPUTE WS-KVMAD-TOT(1) ROUNDED =                               
188400                  WS-KVMAD-TOT-DEC * 1                                    
188500       END-IF                                                             
188600     END-IF                                                               
188700     IF WS-KVPB-SEP(1)     NOT = WS-IN-KVPB-SEP(1)                        
188800        IF WS-IDLEVNR = '1002 ' AND WS-KDERS(1) = +0                      
188900          PERFORM G2-SATSARTIKEL                                          
189000        END-IF                                                            
189100        PERFORM G3-TIDISPIN                                               
189200     END-IF                                                               
189300     .                                                                    
189400     EJECT                                                                
189500                                                                          
189600                                                                          
189700 G2-SATSARTIKEL SECTION.                                                  
189800                                                                          
189900*    DÅ LEVERANTÖRSNUMMER = 1002, ÄR DET EN SATSARTIKEL    *              
190000     MOVE LOW-VALUE             TO DLI-IO-AREA-2                          
190100     MOVE WS-IDARTNR            TO 2202-IDARTNR-SATS                      
190200     MOVE +1                    TO 2202-KDCLAGER                          
190300     MOVE WS-IN-KVPB-SEP(1)     TO 2202-KVPB-SEP-NY                       
190400     MOVE ZERO                  TO 2202-KVPB-SEP-GAMMAL                   
190500     PERFORM IMS-ISRT-2202                                                
190600     .                                                                    
190700     EJECT                                                                
190800                                                                          
190900 G3-TIDISPIN SECTION.                                                     
191000                                                                          
191100     MOVE W-IDARTNR           TO 2228-IDARTNR                             
191200     MOVE LOW-VALUE           TO 2228-LOW-VALUE                           
191300     MOVE SPACE               TO 2228-FILLER                              
191400     PERFORM IMS-ISRT-2228                                                
191500     .                                                                    
191600     EJECT                                                                
191700                                                                          
191800                                                                          
191900 H-UPPDATERA SECTION.                                                     
192000                                                                          
192100     IF MID-KVPB-C1-IN NOT = ALL '+'                                      
192200       PERFORM HA-UPPDATERA-KVPB-C1                                       
192300     END-IF                                                               
192400                                                                          
192500     IF MID-FLMPB-C1-IN NOT = ALL '+'                                     
192600        PERFORM HC-UPPDATERA-FLMPB-C1                                     
192700     END-IF                                                               
192800                                                                          
192900     IF MID-FLOREGPB-IN NOT = ALL '+'                                     
193000        PERFORM HD-UPPDATERA-FLOREGPB                                     
193100     END-IF                                                               
193200                                                                          
193300     IF MID-TIPBLOCK-IN NOT = ALL '+'                                     
193400        PERFORM HE-UPPDATERA-TIPBLOCK                                     
193500     END-IF                                                               
193600                                                                          
193700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
193800     CALL WMEDKONV USING MED-WMEDAREA                                     
193900     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
194000     PERFORM MFS-RENSA-FAELT-IN                                           
194100     PERFORM MFS-FORM-ATTR                                                
194200     .                                                                    
194300     EJECT                                                                
194400                                                                          
194500                                                                          
194600 HA-UPPDATERA-KVPB-C1 SECTION.                                            
194700                                                                          
194800                                                                          
194900     PERFORM IMS-GHU-WDK611                                               
195000                                                                          
195100     IF SEGMENT-FINNS                                                     
195200        IF  WS-KDUART  = SPACE                                            
195300           MOVE WS-KDUART            TO CLAG-KDUART                       
195400        END-IF                                                            
195500        MOVE WS-TIPBDAT     (1)      TO CLAG-TIPBDAT                      
195600        MOVE WS-RVPROFEL    (1)      TO CLAG-RVPROFEL                     
195700        MOVE WS-RVPROURS    (1)      TO CLAG-RVPROURS                     
195800        MOVE WS-KVUTJFEL    (1)      TO CLAG-KVUTJFEL                     
195900        MOVE WS-KVMAD-SEP   (1)      TO CLAG-KVMAD-SEP                    
196000        MOVE WS-KVMAD-TOT   (1)      TO CLAG-KVMAD-TOT                    
196100        MOVE WS-IN-KVPB-SEP (1)      TO CLAG-KVPB-SEP                     
196200                                        CLAG-KVPB-HIST                    
196300        PERFORM IMS-REPL-WDK6-11                                          
196400                                                                          
196500        IF CLAG-IDDC-REF NOT = SPACE                                      
196600          PERFORM IMS-GHNP-WDK629                                         
196700          IF SEGMENT-FINNS                                                
196800            IF CREF-FLREFNYO = JA                                         
196900              MOVE NEJ  TO CREF-FLREFNYO                                  
197000              PERFORM IMS-REPL-WDK629                                     
197100            END-IF                                                        
197200          END-IF                                                          
197300        END-IF                                                            
197400     END-IF                                                               
197500     .                                                                    
197600     EJECT                                                                
197700                                                                          
197800 HC-UPPDATERA-FLMPB-C1 SECTION.                                           
197900                                                                          
198000     PERFORM IMS-GHU-WDK611                                               
198100     IF WS-IN-FLMPB-C1 = YES OR JA                                        
198200        MOVE JA              TO CLAG-FLMPB                                
198300     ELSE                                                                 
198400        MOVE WS-IN-FLMPB-C1  TO CLAG-FLMPB                                
198500     END-IF                                                               
198600     PERFORM IMS-REPL-WDK6-11                                             
198700     .                                                                    
198800     EJECT                                                                
198900                                                                          
199000 HD-UPPDATERA-FLOREGPB SECTION.                                           
199100                                                                          
199200     PERFORM IMS-GHU-WDK611                                               
199300     IF WS-IN-FLOREGPB = YES OR JA                                        
199400        MOVE JA              TO CLAG-FLOREGPB                             
199500        MOVE ZERO            TO CLAG-RVPROURS                             
199600     ELSE                                                                 
199700        MOVE WS-IN-FLOREGPB  TO CLAG-FLOREGPB                             
199800     END-IF                                                               
199900     PERFORM IMS-REPL-WDK6-11                                             
200000     .                                                                    
200100     EJECT                                                                
200200                                                                          
200300                                                                          
200400 HE-UPPDATERA-TIPBLOCK SECTION.                                           
200500                                                                          
200600     PERFORM IMS-GHU-WDK611                                               
200700     MOVE WS-IN-TIPBLOCK     TO CLAG-TIPBLOCK                             
200800     PERFORM IMS-REPL-WDK6-11                                             
200900     .                                                                    
201000     EJECT                                                                
201100                                                                          
201200                                                                          
201300 MFS-RENSA-FAELT-UT SECTION.                                              
201400                                                                          
201500*    --- ALLA UTDATA-FÄLT                                                 
201600     MOVE MFS-RENSA-FAELT         TO MOD-BEART                            
201700                                                                          
201800     MOVE +1 TO INDX                                                      
201900     PERFORM UNTIL INDX > 12                                              
202000        MOVE MFS-RENSA-FAELT      TO MOD-TIAARP (INDX)                    
202100        ADD +1 TO INDX                                                    
202200     END-PERFORM                                                          
202300                                                                          
202400     MOVE +1 TO INDX                                                      
202500     PERFORM UNTIL INDX > 12                                              
202600        MOVE MFS-RENSA-FAELT      TO MOD-KVOI-CDC(INDX)                   
202700        ADD +1 TO INDX                                                    
202800     END-PERFORM                                                          
202900                                                                          
203000     MOVE +1 TO INDX                                                      
203100     PERFORM UNTIL INDX > 12                                              
203200        MOVE MFS-RENSA-FAELT      TO MOD-KVOI-DCVK(INDX)                  
203300        ADD +1 TO INDX                                                    
203400     END-PERFORM                                                          
203500                                                                          
203600     MOVE +1 TO INDX                                                      
203700     PERFORM UNTIL INDX > 12                                              
203800        MOVE MFS-RENSA-FAELT      TO MOD-KVOI-DIV(INDX)                   
203900        ADD +1 TO INDX                                                    
204000     END-PERFORM                                                          
204100                                                                          
204200     MOVE +1 TO INDX                                                      
204300     PERFORM UNTIL INDX > 12                                              
204400        MOVE MFS-RENSA-FAELT      TO MOD-KVOI-SATS(INDX)                  
204500        ADD +1 TO INDX                                                    
204600     END-PERFORM                                                          
204700                                                                          
204800     MOVE MFS-RENSA-FAELT         TO MOD-KVOI-INNEV-CDC                   
204900                                     MOD-KVOI-INNEV-DCVK                  
205000                                     MOD-KVOI-INNEV-DIV                   
205100                                     MOD-KVOI-INNEV-SATS                  
205200                                     MOD-KVOI-RULL-12-CDC                 
205300                                     MOD-KVOI-RULL-12-DCVK                
205400                                     MOD-KVOI-RULL-12-DIV                 
205500                                     MOD-KVOI-RULL-12-SATS                
205600     MOVE +1 TO INDX                                                      
205700     PERFORM UNTIL INDX > 3                                               
205800        MOVE MFS-RENSA-FAELT       TO MOD-AAR(INDX)                       
205900                                     MOD-AAR-CDC(INDX)                    
206000                                     MOD-AAR-DCVK(INDX)                   
206100                                     MOD-AAR-DIV(INDX)                    
206200                                     MOD-AAR-SATS(INDX)                   
206300        ADD +1 TO INDX                                                    
206400     END-PERFORM                                                          
206500                                                                          
206600     MOVE MFS-RENSA-FAELT          TO MOD-AARSFORB-CDC                    
206700                                      MOD-AARSFORB-DC                     
206800                                      MOD-AARSFORB-HALV-CDC               
206900                                      MOD-AARSFORB-HALV-DC                
207000                                      MOD-KVPB-SEP-CDC                    
207100                                      MOD-KVPB-SEP-CDC-2                  
207200                                      MOD-KVPB-SEP-DC                     
207300                                      MOD-KVPB-SEP-DC-2                   
207400                                      MOD-KVPB-PLAN                       
207500                                      MOD-PBPLANTYP                       
207600                                      MOD-KVPB-SATS                       
207700                                                                          
207800     .                                                                    
207900     EJECT                                                                
208000                                                                          
208100                                                                          
208200 MFS-RENSA-FAELT-IN SECTION.                                              
208300                                                                          
208400*    --- ALLA INDATA-FÄLT                                                 
208500     MOVE MFS-RENSA-FAELT TO MOD-KVPB-CDC-IN                              
208600                             MOD-KVPB-C2-IN                               
208700                             MOD-FLMPB-IN                                 
208800                             MOD-FLOREGPB-IN                              
208900                             MOD-TIPBLOCK-IN                              
209000     .                                                                    
209100     EJECT                                                                
209200                                                                          
209300                                                                          
209400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
209500                                                                          
209600*    --- ALLA UTDATA-FÄLT                                                 
209700     MOVE MFS-ROER-EJ-FAELT       TO MOD-BEART                            
209800                                                                          
209900     MOVE +1 TO INDX                                                      
210000     PERFORM UNTIL INDX > 12                                              
210100        MOVE MFS-ROER-EJ-FAELT    TO MOD-TIAARP (INDX)                    
210200        ADD +1 TO INDX                                                    
210300     END-PERFORM                                                          
210400                                                                          
210500     MOVE +1 TO INDX                                                      
210600     PERFORM UNTIL INDX > 12                                              
210700        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVOI-CDC(INDX)                   
210800        ADD +1 TO INDX                                                    
210900     END-PERFORM                                                          
211000                                                                          
211100     MOVE +1 TO INDX                                                      
211200     PERFORM UNTIL INDX > 12                                              
211300        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVOI-DCVK(INDX)                  
211400        ADD +1 TO INDX                                                    
211500     END-PERFORM                                                          
211600                                                                          
211700     MOVE +1 TO INDX                                                      
211800     PERFORM UNTIL INDX > 12                                              
211900        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVOI-DIV(INDX)                   
212000        ADD +1 TO INDX                                                    
212100     END-PERFORM                                                          
212200                                                                          
212300     MOVE +1 TO INDX                                                      
212400     PERFORM UNTIL INDX > 12                                              
212500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVOI-SATS(INDX)                  
212600        ADD +1 TO INDX                                                    
212700     END-PERFORM                                                          
212800                                                                          
212900     MOVE MFS-ROER-EJ-FAELT       TO MOD-KVOI-INNEV-CDC                   
213000                                     MOD-KVOI-INNEV-DCVK                  
213100                                     MOD-KVOI-INNEV-DIV                   
213200                                     MOD-KVOI-INNEV-SATS                  
213300                                     MOD-KVOI-RULL-12-CDC                 
213400                                     MOD-KVOI-RULL-12-DCVK                
213500                                     MOD-KVOI-RULL-12-DIV                 
213600                                     MOD-KVOI-RULL-12-SATS                
213700     MOVE +1 TO INDX                                                      
213800     PERFORM UNTIL INDX > 3                                               
213900        MOVE MFS-ROER-EJ-FAELT     TO MOD-AAR(INDX)                       
214000                                     MOD-AAR-CDC(INDX)                    
214100                                     MOD-AAR-DCVK(INDX)                   
214200                                     MOD-AAR-DIV(INDX)                    
214300                                     MOD-AAR-SATS(INDX)                   
214400        ADD +1 TO INDX                                                    
214500     END-PERFORM                                                          
214600                                                                          
214700     MOVE MFS-ROER-EJ-FAELT        TO MOD-AARSFORB-CDC                    
214800                                      MOD-AARSFORB-DC                     
214900                                      MOD-AARSFORB-HALV-CDC               
215000                                      MOD-AARSFORB-HALV-DC                
215100                                      MOD-KVPB-SEP-CDC                    
215200                                      MOD-KVPB-SEP-CDC-2                  
215300                                      MOD-KVPB-SEP-DC                     
215400                                      MOD-KVPB-SEP-DC-2                   
215500                                      MOD-KVPB-PLAN                       
215600                                      MOD-PBPLANTYP                       
215700                                      MOD-KVPB-SATS                       
215800     .                                                                    
215900     EJECT                                                                
216000                                                                          
216100                                                                          
216200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
216300                                                                          
216400*    --- ALLA INDATA-FÄLT                                                 
216500     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-CDC-IN                            
216600                               MOD-KVPB-C2-IN                             
216700                               MOD-FLMPB-IN                               
216800                               MOD-FLOREGPB-IN                            
216900                               MOD-TIPBLOCK-IN                            
217000     .                                                                    
217100     EJECT                                                                
217200                                                                          
217300                                                                          
217400 MFS-FORM-ATTR SECTION.                                                   
217500                                                                          
217600*    --- ALLA INDATA-FÄLT                                                 
217700     MOVE MFS-FORMATETS-ATTR TO MOD-KVPB-CDC-IN-ATTR                      
217800                                MOD-KVPB-C2-IN-ATTR                       
217900                                MOD-FLMPB-IN-ATTR                         
218000                                MOD-FLOREGPB-IN-ATTR                      
218100                                MOD-TIPBLOCK-IN-ATTR                      
218200     .                                                                    
218300     EJECT                                                                
218400 MFS-CLOSE-FAELT-ATTR SECTION.                                            
218500                                                                          
218600     MOVE MFS-CLOSE-FIELD       TO MOD-KVPB-CDC-IN-ATTR                   
218700                                   MOD-KVPB-C2-IN-ATTR                    
218800                                   MOD-FLMPB-ATTR                         
218900                                   MOD-FLMPB-IN-ATTR                      
219000                                   MOD-KVPB-SEP-CDC-ATTR                  
219100                                   MOD-KVPB-SEP-DC-ATTR                   
219200                                   MOD-FLOREGPB-ATTR                      
219300                                   MOD-FLOREGPB-IN-ATTR                   
219400                                   MOD-TIPBLOCK-IN-ATTR                   
219500     .                                                                    
219600     EJECT                                                                
219700 S1-SECURITY-CHECK-PARTNO-IDLEV SECTION.                                  
219800     SKIP2                                                                
219900*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
220000     PERFORM IMS-GET-WDK601                                               
220100     IF  SEGMENT-FINNS                                                    
220200       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
220300       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
220400       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
220500*        --- BEHÖRIG USER                                                 
220600         SET PASSED-SECURITY-CHECK TO TRUE                                
220700       ELSE                                                               
220800*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
220900           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
221000           CALL WMEDKONV USING MED-WMEDAREA                               
221100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
221200       END-IF                                                             
221300     ELSE                                                                 
221400*       -- ARTIKEL SAKNAS PÅ K6                                           
221500        MOVE '017' TO MED-IDMFSFEL                                        
221600        CALL WMEDKONV USING MED-WMEDAREA                                  
221700        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
221800        PERFORM MFS-RENSA-FAELT-UT                                        
221900        PERFORM MFS-RENSA-FAELT-IN                                        
222000     END-IF                                                               
222100     .                                                                    
222200     EJECT                                                                
222300                                                                          
222400                                                                          
222500* --- IMS SEKTIONER ---                                                   
222600                                                                          
222700                                                                          
222800                                                                          
222900 IMS-GET-MSG SECTION.                                                     
223000                                                                          
223100     MOVE '  QC' TO GODK-STATUSKODER                                      
223200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
223300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
223400     PERFORM IMS-STATUSKONTROLL                                           
223500     .                                                                    
223600     EJECT                                                                
223700                                                                          
223800                                                                          
223900 IMS-INSERT-MSG SECTION.                                                  
224000                                                                          
224100     IF MSGI-IDLAND-SPR = 'GB'                                            
224200       MOVE 'N' TO MFS-KDHUVOMR                                           
224300     END-IF                                                               
224400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
224500     MOVE SPACE TO GODK-STATUSKODER                                       
224600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
224700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     EJECT                                                                
225100                                                                          
225200                                                                          
225300 IMS-GET-WDK601 SECTION.                                                  
225400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
225500          DELIMITED BY SIZE INTO SSA1                                     
225600     MOVE '  GE' TO GODK-STATUSKODER                                      
225700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-01 SSA1                   
225800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
225900     PERFORM IMS-STATUSKONTROLL                                           
226000     .                                                                    
226100     EJECT                                                                
226200                                                                          
226300                                                                          
226400 IMS-GET-WDK611 SECTION.                                                  
226500     MOVE 'WDK611     ' TO SSA1                                           
226600     MOVE '  GE' TO GODK-STATUSKODER                                      
226700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-11 SSA1                  
226800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
226900     PERFORM IMS-STATUSKONTROLL                                           
227000     .                                                                    
227100     SKIP3                                                                
227200                                                                          
227300                                                                          
227400 IMS-GHU-WDK611 SECTION.                                                  
227500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
227600          DELIMITED BY SIZE INTO SSA1                                     
227700     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
227800     MOVE '  GE' TO GODK-STATUSKODER                                      
227900     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-11 SSA1 SSA2             
228000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300     EJECT                                                                
228400                                                                          
228500                                                                          
228600 IMS-GET-WDK626 SECTION.                                                  
228700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
228800          DELIMITED BY SIZE INTO SSA1                                     
228900     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
229000     MOVE 'WDK626   ' TO SSA3                                             
229100     MOVE '  GE' TO GODK-STATUSKODER                                      
229200     CALL CBLTDLI USING GNP                                               
229300                      WDK6-PCB DLI-IO-AREA SSA1 SSA2 SSA3                 
229400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
229500     PERFORM IMS-STATUSKONTROLL                                           
229600     .                                                                    
229700     EJECT                                                                
229800                                                                          
229900                                                                          
230000 IMS-REPL-WDK6-11 SECTION.                                                
230100                                                                          
230200     MOVE '  ' TO GODK-STATUSKODER                                        
230300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-11                      
230400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
230500     PERFORM IMS-STATUSKONTROLL                                           
230600     .                                                                    
230700     SKIP3                                                                
230800                                                                          
230900 IMS-GHNP-WDK629 SECTION.                                                 
231000                                                                          
231100     MOVE 'WDK629     ' TO SSA1                                           
231200     MOVE '  GE' TO GODK-STATUSKODER                                      
231300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA-WDK629 SSA1             
231400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
231500     PERFORM IMS-STATUSKONTROLL                                           
231600     .                                                                    
231700     SKIP3                                                                
231800                                                                          
231900 IMS-REPL-WDK629 SECTION.                                                 
232000                                                                          
232100     MOVE '  ' TO GODK-STATUSKODER                                        
232200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK629                  
232300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
232400     PERFORM IMS-STATUSKONTROLL                                           
232500     .                                                                    
232600     SKIP3                                                                
232700                                                                          
232800                                                                          
232900 IMS-GET-SART SECTION.                                                    
233000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
233100          DELIMITED BY SIZE INTO SSA1                                     
233200     MOVE '  GE' TO GODK-STATUSKODER                                      
233300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
233400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700     SKIP3                                                                
233800                                                                          
233900                                                                          
234000 IMS-GET-SLAG   SECTION.                                                  
234100     MOVE 'WLARTS11   ' TO SSA1                                           
234200     MOVE '  GE' TO GODK-STATUSKODER                                      
234300     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
234400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
234500     PERFORM IMS-STATUSKONTROLL                                           
234600     .                                                                    
234700     EJECT                                                                
234800                                                                          
234900                                                                          
235000 IMS-GET-WDL811  SECTION.                                                 
235100                                                                          
235200     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
235300          DELIMITED BY SIZE INTO SSA1                                     
235400     STRING 'WLOIGB11(TIAAAA   =' W-TIAAAA-X ')'                          
235500          DELIMITED BY SIZE INTO SSA2                                     
235600     MOVE '  GE' TO GODK-STATUSKODER                                      
235700     CALL CBLTDLI USING GU OIGB-PCB                                       
235800                                 DLI-IO-AREA SSA1 SSA2                    
235900     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
236000     PERFORM IMS-STATUSKONTROLL                                           
236100     .                                                                    
236200     EJECT                                                                
236300                                                                          
236400                                                                          
236500 IMS-GET-BENA11-BSEQ SECTION.                                             
236600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
236700          DELIMITED BY SIZE INTO SSA1                                     
236800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
236900          DELIMITED BY SIZE INTO SSA2                                     
237000     MOVE '  GE' TO GODK-STATUSKODER                                      
237100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
237200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
237300     PERFORM IMS-STATUSKONTROLL                                           
237400     .                                                                    
237500     EJECT                                                                
237600                                                                          
237700                                                                          
237800 IMS-ISRT-2202 SECTION.                                                   
237900                                                                          
238000     STRING 'WLXXCK01(WDG3KEY  =' W-2202KEY-X ')'                         
238100             DELIMITED BY SIZE INTO SSA1                                  
238200     MOVE 'WLXXCK11 ' TO SSA2                                             
238300     MOVE '  ' TO GODK-STATUSKODER                                        
238400     CALL CBLTDLI USING ISRT XXCK-PCB DLI-IO-AREA-2 SSA1 SSA2             
238500     MOVE XXCK-STATUS-CODE TO STATUS-WS                                   
238600     PERFORM IMS-STATUSKONTROLL                                           
238700     .                                                                    
238800     SKIP3                                                                
238900                                                                          
239000                                                                          
239100 IMS-ISRT-2228 SECTION.                                                   
239200     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
239300            DELIMITED BY SIZE INTO SSA1                                   
239400     MOVE   'WLXXBW11 ' TO SSA2                                           
239500     MOVE '  II' TO GODK-STATUSKODER                                      
239600     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA-3 SSA1 SSA2             
239700     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
239800     PERFORM IMS-STATUSKONTROLL                                           
239900     .                                                                    
240000     EJECT                                                                
240100                                                                          
240200                                                                          
240300 IMS-STATUSKONTROLL SECTION.                                              
240400                                                                          
240500     SET STATUS-IX TO 1                                                   
240600     SEARCH GODK-STATUS                                                   
240700       AT END                                                             
240800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
240900         DELIMITED BY SIZE INTO FELTEXT                                   
241000         CALL FELLOG                                                      
241100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
241200         CONTINUE                                                         
241300     END-SEARCH                                                           
241400     .                                                                    
241500     EJECT                                                                
241600*    -COPY WY2000P2                                                       
