000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2013700.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   AUGUSTI 1997.                                            
000600                                                                          
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        TP-PROGRAM ANSKAFFNING - UPPDATERING AV SÄSONGSINDEX             
001200*                                 SOM STYR DET TOTALA PB                  
001300*                                 SAMT MÖJLIGHET ATT SIMULERA             
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001600*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001700*        PROGRAMMET LÄSER      WLOIGB (WDL8)                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W2T137                                              
002100*                     W2T137U                                             
002200*        MID:         W2I13701                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W2O13701                                            
002600*                                                                         
002700*   ÄNDRINGAR:                                                            
002800*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002900*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003000*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003100*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900*    -COPY WY2000W2                                                       
004000     SKIP3                                                                
004100 77  IDPGM                       PIC X(08)   VALUE 'W2013700'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
005100*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0965 COMP SYNC.        
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005600                                                                          
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  C2-SW                       PIC X       VALUE 'J'.                   
006600     88  C2-SEG-FINNS                        VALUE 'J'.                   
006700     88  C2-SEG-SAKNAS                       VALUE 'N'.                   
006800                                                                          
006900 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
007000     88  SIM-ANTAL-JA                        VALUE 'J'.                   
007100     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
007200                                                                          
007300 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
007400     88  SIM-INDEX-JA                        VALUE 'J'.                   
007500     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
007600                                                                          
007700 77  WDK611-SW                   PIC X       VALUE 'N'.                   
007800     88  WDK611-FINNS                        VALUE 'J'.                   
007900                                                                          
008000 77  WDK626-SW                   PIC X       VALUE 'N'.                   
008100     88  TA-BORT-WDK626                      VALUE 'J'.                   
008200                                                                          
008300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008400     88  EGEN-MID                            VALUE '2137'.                
008500     88  GODK-MID                            VALUE '2101' '2102'          
008600                                                   '2103' '2104'          
008700                                                   '2105' '2106'          
008800                                                   '2107' '2108'          
008900                                                   '2109'.                
009000     88  HELP-MID                            VALUE '0551'.                
009100                                                                          
009200 77  CL-IX               PIC S9(9)   COMP SYNC.                           
009300 77  INDX                PIC S9(9)   COMP SYNC.                           
009400 77  INDX2               PIC S9(9)   COMP SYNC.                           
009500 77  INDX-AAR            PIC S9(9)   COMP SYNC.                           
009600 77  INDX-PERIOD         PIC S9(9)   COMP SYNC.                           
009700 77  TABW200-INDEX       PIC S9(4)   COMP SYNC.                           
009800 77  VECKA-IX            PIC S9(9)   COMP SYNC.                           
009900 77  PER-IX              PIC S9(9)   COMP SYNC.                           
010000 77  PERIOD5-IX          PIC S9(9)   COMP SYNC.                           
010100 77  SPARAD-KDERS        PIC S9(3)   COMP-3.                              
010200 77  SPARAD-KDERS-UTG    PIC S9(3)   COMP-3.                              
010300 77  SPARAD-TIFINLV      PIC S9(5)   COMP-3.                              
010400 77  SEASON-SW-C1        PIC  X(1) VALUE SPACE.                           
010500 77  SEASON-SW-C2        PIC  X(1) VALUE SPACE.                           
010600 77  SW-SEASON           PIC  X(1) VALUE SPACE.                           
010700 77  SW-ERSATT           PIC  X(1) VALUE SPACE.                           
010800 77  WS-AVVIKELSE        PIC  S9(4)V999 VALUE ZERO COMP-3.                
010900     SKIP2                                                                
011000                                                                          
011100 01  WS-FALT.                                                             
011200                                                                          
011300     03  WS-CURRENT-DATE.                                                 
011400         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
011500         05  FILLER              PIC 9(4)   VALUE ZERO.                   
011600         05  FILLER              PIC 9(6)   VALUE ZERO.                   
011700                                                                          
011800     03  FILLER REDEFINES WS-CURRENT-DATE.                                
011900*-----   INKLUSIVE SEKEL                                                  
012000         05  WS-DAGENS-DATUM     PIC 9(8).                                
012100         05  WS-DAGENS-TID.                                               
012200             07 WS-DAGENS-TIMME  PIC 9(2).                                
012300             07 WS-DAGENS-MINUT  PIC 9(2).                                
012400             07 WS-DAGENS-SEKUND PIC 9(2).                                
012500     03 WS-DAGENS-TIAAAA-1       PIC 9(4).                                
012600     03 WS-DAGENS-TIAAAA-2       PIC 9(4).                                
012700     03 WS-DAGENS-PER            PIC 9(4)   VALUE ZERO.                   
012800     03 WS-DAG-PER REDEFINES WS-DAGENS-PER.                               
012900             05 WS-DAGENS-AA     PIC 9(2).                                
013000             05 WS-DAGENS-PP     PIC 9(2).                                
013100     03 WS-DAGENS-AAR            PIC 9(4)    VALUE ZERO.                  
013200     03 WS-DAGENS-VECKA          PIC 9(2)    VALUE ZERO.                  
013300     03 WS-TIAAPP                PIC 9(4)   VALUE ZERO.                   
013400     03 FILLER REDEFINES WS-TIAAPP.                                       
013500             05 WS-TIAAPP-AA     PIC 9(2).                                
013600             05 WS-TIAAPP-PP     PIC 9(2).                                
013700     03 WS-TIAAVV                PIC 9(4)   VALUE ZERO.                   
013800     03 FILLER REDEFINES WS-TIAAVV.                                       
013900             05 WS-TIAAVV-AA     PIC 9(2).                                
014000             05 WS-TIAAVV-VV     PIC 9(2).                                
014100     03 WS-SLUT-VV               PIC 9(2)    VALUE ZERO.                  
014200     03 WS-PER                   PIC 9(4)   VALUE ZERO.                   
014300     03 FILLER REDEFINES WS-PER.                                          
014400             05 WS-PER-A         PIC 9.                                   
014500             05 WS-PER-PP        PIC 9(2).                                
014600     03 WS-PERIODTABELL-AR-2     OCCURS 12.                               
014700        05 WS-PERTAB-TIAAPP-2    PIC 9(4).                                
014800        05 WS-PERTAB-START-VV-2  PIC 9(2).                                
014900        05 WS-PERTAB-SLUT-VV-2   PIC 9(2).                                
015000     03 WS-PERIODTABELL-AR-1     OCCURS 12.                               
015100        05 WS-PERTAB-TIAAPP-1    PIC 9(4).                                
015200        05 WS-PERTAB-START-VV-1  PIC 9(2).                                
015300        05 WS-PERTAB-SLUT-VV-1   PIC 9(2).                                
015400     03 WS-PERIODTABELL-AR-0     OCCURS 12.                               
015500        05 WS-PERTAB-TIAAPP-0    PIC 9(4).                                
015600        05 WS-PERTAB-START-VV-0  PIC 9(2).                                
015700        05 WS-PERTAB-SLUT-VV-0   PIC 9(2).                                
015800                                                                          
015900     03 WS-IDLEVNR             PIC X(5)       VALUE SPACE.                
016000     03 WS-IDLEVNR-8           PIC X(8)       VALUE SPACE.                
016100     03 WS-IDLKTO              PIC S9(7)      VALUE ZERO COMP-3.          
016200     03 WS-KDUART              PIC  X(1)      VALUE 'X'.                  
016300     03 WS-KDLTK               PIC S9(3)      VALUE ZERO COMP-3.          
016400     03 WS-TILTK               PIC S9(5)      VALUE ZERO COMP-3.          
016500     03 WS-KVLS-C2             PIC S9(7)      VALUE ZERO COMP-3.          
016600     03 WS-KVRESS-C2           PIC S9(7)      VALUE ZERO COMP-3.          
016700     03 W-KVPB-REF             PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
016800     03 WS-KVOI-PER            PIC S9(7)      VALUE ZERO COMP-3.          
016900     03 WS-CLAG-FALT           OCCURS 2.                                  
017000       05 WS-KDERS             PIC S9(3)      COMP-3.                     
017100       05 WS-KVPB-SEP          PIC S9(6)V9(1) COMP-3.                     
017200       05 WS-TIPBDAT           PIC S9(5)      COMP-3.                     
017300       05 WS-RVPROFEL          PIC S9(3)      COMP-3.                     
017400       05 WS-RVPROURS          PIC S9(3)      COMP-3.                     
017500       05 WS-KVUTJFEL          PIC S9(6)V9(1) COMP-3.                     
017600       05 WS-KVPB-SATS         PIC S9(6)V9(1) COMP-3.                     
017700       05 WS-KVMAD-SEP         PIC S9(6)V9(1) COMP-3.                     
017800       05 WS-KVMAD-TOT         PIC S9(6)V9(1) COMP-3.                     
017900       05 FL-TA-BORT-TREND     PIC X(1).                                  
018000     03 WS-START-AAR-SEKEL     PIC 9(4)       VALUE ZERO.                 
018100     03 WS-OIREG-AAR-SEKEL     PIC 9(4)       VALUE ZERO.                 
018200     03 WS-RED-KVPB-REF        PIC Z(4)9.9 VALUE ZERO.                    
018300     03 WS-KVPB-SIM-SEASON     OCCURS 12                                  
018400                               PIC S9(6)V9(1) COMP-3.                     
018500     03 WS-KVPB-SIM            PIC S9(6)V9(1) COMP-3.                     
018600     03 WS-RESEASON-SIM        OCCURS 12                                  
018700                               PIC 9V9(2)     COMP-3.                     
018800     03 WS-RED-RESEASON        PIC 9.9(2)  VALUE ZERO.                    
018900     03 WS-SIMIX-SUM           PIC 9(2)V9(2)                              
019000                                           VALUE ZERO.                    
019100     03 WS-KVPB-SIM-TOM        PIC S9(6)   VALUE ZERO.                    
019200     03 WS-SIMIX-TOT           PIC S9(2)V9(2)                             
019300                                           VALUE ZERO.                    
019400     03 WS-SIMANT-TOT          PIC S9(2)V9(2)                             
019500                                           VALUE ZERO.                    
019600     03 WS-TOT-SIMANT          PIC S9(9)   VALUE ZERO.                    
019700     03 WS-INDEX-DEC           PIC S9(9)V9(5)                             
019800                                           VALUE ZERO.                    
019900     03 WS-JUSTERA             PIC S9V9(2) VALUE ZERO  COMP-3.            
020000     03 WS-KVOI-CDC-SUM        OCCURS 12                                  
020100                               PIC S9(9)   VALUE ZERO.                    
020200     03 WS-KVOI-DC-REF-SUM     OCCURS 12                                  
020300                               PIC S9(9)   VALUE ZERO.                    
020400     03 WS-KVOI-DC-KUND-SUM    OCCURS 12                                  
020500                               PIC S9(9)   VALUE ZERO.                    
020600     03 WS-KVOI-PROG           PIC S9(9)   VALUE ZERO.                    
020700     03 WS-KVOI-REFILL         PIC S9(9)   VALUE ZERO.                    
020800     03 WS-KVOI-LEDTID         PIC S9(9)   VALUE ZERO.                    
020900     03 WS-KVOI-CDC-SNITT      PIC S9(9)   VALUE ZERO.                    
021000     03 WS-KVOI-DC-REF-SNITT   PIC S9(9)   VALUE ZERO.                    
021100     03 WS-KVOI-DC-KUND-SNITT  PIC S9(9)   VALUE ZERO.                    
021200     03 WS-KVOI-CDC-TOT        PIC S9(9)   VALUE ZERO.                    
021300     03 WS-KVOI-DC-REF-TOT     PIC S9(9)   VALUE ZERO.                    
021400     03 WS-KVOI-DC-KUND-TOT    PIC S9(9)   VALUE ZERO.                    
021500     03 WS-ANTAL               PIC S9(9)   VALUE ZERO.                    
021600     03 WS-RESEASON-HIST       OCCURS 12                                  
021700                               PIC 9V9(2)  VALUE ZERO.                    
021800     03 WS-TISEASON            PIC X(8)       VALUE SPACE.                
021900*********************************************************                 
022000*    WS-MSGI-AREA ANVÄNDS FÖR ATT SPARA DET SOM LIGGER                    
022100*                 I MOD FÖR DE FÄLT DÄR SAMMA FÄLT ANVÄNDS                
022200*                 FÖR MID OCH MOD                                         
022300*    WS-MSGI-AREA SPARAS UNDAN PÅ MSGI-SPAR-AREA I                        
022400*                 NYCKELDATABASEN                                         
022500*********************************************************                 
022600     03 WS-MSGI-AREA.                                                     
022700       10 WS-MSGI-IDTRANS-2137   PIC X(4)    VALUE '2137'.                
022800       10 WS-MSGI-SIMIX          OCCURS 12                                
022900                                 PIC 9V9(2)  VALUE ZERO.                  
023000       10 WS-MSGI-SIMANT         OCCURS 12                                
023100                                 PIC 9(7)    VALUE ZERO.                  
023200       10 WS-MSGI-TISEASON       PIC X(6)    VALUE SPACE.                 
023300       10 FILLER                 PIC X(110)  VALUE SPACE.                 
023400     03 WS-TEMFSINF.                                                      
023500       05 WS-TEMFSINF-1        PIC 9V9(2)  VALUE ZERO.                    
023600       05 FILLER               PIC X       VALUE '/'.                     
023700       05 WS-TEMFSINF-2        PIC 9V9(2)  VALUE ZERO.                    
023800       05 FILLER               PIC X       VALUE '/'.                     
023900       05 WS-TEMFSINF-3        PIC 9V9(2)  VALUE ZERO.                    
024000       05 FILLER               PIC X       VALUE '/'.                     
024100       05 WS-TEMFSINF-4        PIC 9V9(2)  VALUE ZERO.                    
024200       05 FILLER               PIC X       VALUE '/'.                     
024300       05 WS-TEMFSINF-5        PIC 9V9(2)  VALUE ZERO.                    
024400       05 FILLER               PIC X       VALUE '/'.                     
024500       05 WS-TEMFSINF-6        PIC 9V9(2)  VALUE ZERO.                    
024600       05 FILLER               PIC X       VALUE '/'.                     
024700       05 WS-TEMFSINF-7        PIC 9V9(2)  VALUE ZERO.                    
024800       05 FILLER               PIC X       VALUE '/'.                     
024900       05 WS-TEMFSINF-8        PIC 9V9(2)  VALUE ZERO.                    
025000       05 FILLER               PIC X       VALUE '/'.                     
025100       05 WS-TEMFSINF-9        PIC 9V9(2)  VALUE ZERO.                    
025200       05 FILLER               PIC X       VALUE '/'.                     
025300       05 WS-TEMFSINF-10       PIC 9V9(2)  VALUE ZERO.                    
025400       05 FILLER               PIC X       VALUE '/'.                     
025500       05 WS-TEMFSINF-11       PIC 9V9(2)  VALUE ZERO.                    
025600       05 FILLER               PIC X       VALUE '/'.                     
025700       05 WS-TEMFSINF-12       PIC 9V9(2)  VALUE ZERO.                    
025800                                                                          
025900*    -COPY W221PERT                                                       
026000                                                                          
026100 01  IN-FALT.                                                             
026200     03 IN-KVPB-SEP OCCURS 2.                                             
026300       05 WS-IN-KVPB-SEP       PIC S9(6)V9(1) COMP-3.                     
026400     03 WS-IN-FLMPB-C1         PIC X(1)      VALUE SPACE.                 
026500     03 WS-IN-FLMPB-C2         PIC X(1)      VALUE SPACE.                 
026600                                                                          
026700 01  KVPB-X.                                                              
026800     03  WS-HELTAL       PIC 9(6).                                        
026900     03  WS-PUNKT        PIC X.                                           
027000     03  WS-DECIMAL      PIC 9.                                           
027100     SKIP2                                                                
027200 01  WS-KVPB.                                                             
027300     03  KVPB-HELTAL     PIC 9(6).                                        
027400     03  KVPB-DECIMAL    PIC 9.                                           
027500     SKIP2                                                                
027600 01  WS-KVPB-SEP-NUM      PIC 9(6)V9 VALUE ZERO.                          
027700     SKIP2                                                                
027800 01  WS-MEDDELANDE       PIC X(34)    VALUE SPACE.                        
027900     SKIP2                                                                
028000 01  TRANS-SW            PIC X.                                           
028100     88  TRANS-EJ-AKTUELL  VALUE 'N'.                                     
028200     88  TRANS-AKTUELL     VALUE 'N'.                                     
028300     EJECT                                                                
028400 01  FL-SEASON           PIC X   VALUE 'N'.                               
028500     SKIP2                                                                
028600 01  KONSTANTER.                                                          
028700     03  VIP             PIC X       VALUE 'V'.                           
028800     03  C1-PROGNOS      PIC S9(3) COMP-3 VALUE +11.                      
028900     03  C1-DIVERSE      PIC S9(3) COMP-3 VALUE +13.                      
029000     03  C1-SATS         PIC S9(3) COMP-3 VALUE +14.                      
029100     03  C2-PROGNOS      PIC S9(3) COMP-3 VALUE +21.                      
029200     03  C2-DIVERSE      PIC S9(3) COMP-3 VALUE +23.                      
029300     03  C2-SATS         PIC S9(3) COMP-3 VALUE +24.                      
029400     EJECT                                                                
029500***************************                                               
029600******  DATUMAREOR  *******                                               
029700***************************                                               
029800     SKIP2                                                                
029900 01  DAGENS-DATUM-AAVVD.                                                  
030000     03  DAGENS-AAR          PIC 99.                                      
030100     03  DAGENS-VECKA        PIC 99.                                      
030200     03  DAGENS-DAG          PIC 9.                                       
030300     SKIP2                                                                
030400 01  DAGENS-DATUM            PIC S9(5).                                   
030500 01  DAGENS-PERIOD           PIC 9(2).                                    
030600     SKIP2                                                                
030700 01  WS-PERIOD               PIC 9.                                       
030800     SKIP2                                                                
030900 01  START-DATUM.                                                         
031000     03  START-AAR           PIC 99.                                      
031100     03  START-PERIOD        PIC 9.                                       
031200     SKIP2                                                                
031300 01  START-TIFINLV-AAR       PIC 99.                                      
031400     SKIP2                                                                
031500 01  AKTUELLT-AAR            PIC 99.                                      
031600     SKIP2                                                                
031700 01  OIREG-DATUM             PIC 9(3).                                    
031800 01  FILLER REDEFINES OIREG-DATUM.                                        
031900     03  OIREG-AAR           PIC 99.                                      
032000     03  OIREG-PERIOD        PIC 9.                                       
032100     EJECT                                                                
032200*01  -COPY W200W001C0                                                     
032300     SKIP2                                                                
032400 01  HALV-REAAR              PIC 99V9.                                    
032500     EJECT                                                                
032600***********************************                                       
032700******   ACKUMULATORER   **********                                       
032800***********************************                                       
032900     SKIP2                                                                
033000*    *** ÅRS-ACKAR ***                                                    
033100 01  AARS-ACK.                                                            
033200   02  FILLER OCCURS 3.                                                   
033300     03  ACK-AAR-C1          PIC S9(9) COMP-3.                            
033400     03  ACK-AAR-DC-REF      PIC S9(9) COMP-3.                            
033500                                                                          
033600*    *** RULLANDE 8 PERIODER ***                                          
033700 01  FILLER.                                                              
033800     03  ACK-RULL-8-C1       PIC S9(9) COMP-3.                            
033900     03  ACK-RULL-8-DC-REF   PIC S9(9) COMP-3.                            
034000                                                                          
034100*    *** PERIODBEHOV ***                                                  
034200 01  FILLER.                                                              
034300     03  ACK-KVPB-SEP        PIC S9(8)V9 COMP-3.                          
034400     03  ACK-KVPB-SATS       PIC S9(8)V9 COMP-3.                          
034500                                                                          
034600*    *** ORDERINGÅNG C1 ***                                               
034700 01  KVOI-C1.                                                             
034800   02  FILLER OCCURS 24.                                                  
034900     03  WS-KVOI-C1          PIC S9(9) COMP-3.                            
035000                                                                          
035100*    *** ORDERINGÅNG C2 ***                                               
035200 01  KVOI-C2.                                                             
035300   02  FILLER OCCURS 24.                                                  
035400     03  WS-KVOI-C2          PIC S9(9) COMP-3.                            
035500                                                                          
035600     SKIP2                                                                
035700 01  FILLER.                                                              
035800     03  KVOI-SEASON-C1      PIC S9(9) COMP-3 OCCURS 12.                  
035900     03  KVOI-SEASON-C2      PIC S9(9) COMP-3 OCCURS 12.                  
036000     SKIP2                                                                
036100 01  FILLER.                                                              
036200     03  WS-KVOI-MED-C1      PIC S9(9) COMP-3.                            
036300     03  WS-KVOI-MED-C2      PIC S9(9) COMP-3.                            
036400     SKIP2                                                                
036500 01  SEASON-INDX.                                                         
036600     03  SEASON-INDX-C1      PIC S9(3) COMP-3 OCCURS 24.                  
036700     03  SEASON-INDX-C2      PIC S9(3) COMP-3 OCCURS 24.                  
036800     SKIP2                                                                
036900 01  FILLER.                                                              
037000     03  SUM-KVOI-C1         PIC S9(9) COMP-3.                            
037100     03  SUM-KVOI-C2         PIC S9(9) COMP-3.                            
037200     03  WS-AVVIK-C1         PIC S9(9) COMP-3.                            
037300     03  WS-AVVIK-C2         PIC S9(9) COMP-3.                            
037400     03  AVVIK-1-C1          PIC S9(9) COMP-3.                            
037500     03  AVVIK-1-C2          PIC S9(9) COMP-3.                            
037600     03  AVVIK-2-C1          PIC S9(9) COMP-3.                            
037700     03  AVVIK-2-C2          PIC S9(9) COMP-3.                            
037800                                                                          
037900     SKIP2                                                                
038000 01  MESSAGE-CODES.                                                       
038100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
038200     03  CONFLICT                PIC X(3)    VALUE '002'.                 
038300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
038400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
038500     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
038600     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
038700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
038800     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
038900     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
039000     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
039100     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
039200     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
039300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
039400     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
039500     03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
039600                                                                          
039700     SKIP2                                                                
039800 01      MEDDELANDE.                                                      
039900   03    ARTREG-ARTIKEL-SAKNAS PIC X(35) VALUE                            
040000     'ARTIKELN SAKNAS PÅ ARTIKELREGISTRET'.                               
040100   03    UTGONGEN        PIC X(17)   VALUE 'ARTIKELN UTGÅNGEN'.           
040200   03    ERSATT          PIC X(15)   VALUE 'ARTIKELN ERSATT'.             
040300   03    ORDERING-SAKNAS PIC X(18)   VALUE 'ORDERINGÅNG SAKNAS'.          
040400   03    SEASON-C1       PIC X(21)   VALUE                                
040500     'SÄSONG REGISTRERAD C1'.                                             
040600   03    SEASON-C2       PIC X(21)   VALUE                                
040700     'SÄSONG REGISTRERAD C2'.                                             
040800   03    SEASON-C1-C2    PIC X(24)   VALUE                                
040900     'SÄSONG REGISTRERAD C1+C2'.                                          
041000                                                                          
041100   03    SIGNIF-C1-C2    PIC X(24)   VALUE                                
041200     'SIGNIFIKANT SÄSONG C1+C2'.                                          
041300   03    SIGNIF-C1       PIC X(21)   VALUE                                
041400     'SIGNIFIKANT SÄSONG C1'.                                             
041500   03    SIGNIF-C2       PIC X(21)   VALUE                                
041600     'SIGNIFIKANT SÄSONG C2'.                                             
041700   03    FINNS-EJ-C2     PIC X(23)   VALUE                                
041800     'ARTIKELN FINNS EJ PÅ C2'.                                           
041900   03    KDERS-OVER-10   PIC X(23)   VALUE                                
042000     'ARTIKELN ERSATT        '.                                           
042100   03    FINNS-EJ-C2-KDERS-C1-OVER-10     PIC X(40)   VALUE               
042200     'ARTIKELN FINNS EJ PÅ C2, ARTIKELN ERSATT'.                          
042300   03    KONTO-SAKNAS                     PIC X(40)   VALUE               
042400     'KONTO SAKNAS                            '.                          
042500   03    PB-EJ-ANDRAT                     PIC X(40)   VALUE               
042600     'PB EJ ÄNDRAT                            '.                          
042700*  03    MED-1                  PIC X(30)                                 
042800*        VALUE 'TOTAL OF INDEX IS NOT 12.00'.                             
042900*  03    MED-2.                                                           
043000*     05 FILLER                 PIC X(18)                                 
043100*        VALUE 'TOTAL OF INDEX IS '.                                      
043200*     05 MED-2-INDEX            PIC 99.99.                                
043300*     05 FILLER                 PIC X(16)                                 
043400*        VALUE ' IT MUST BE 12.00'.                                       
043500*    03  MED-3                  PIC X(30)                                 
043600*        VALUE 'FORECAST WRONG                '.                          
043700*    03  MED-4                  PIC X(30)                                 
043800*        VALUE 'DATE IS NOT CORRECT           '.                          
043900*    03  MED-5                  PIC X(30)                                 
044000*        VALUE 'NO VALID PB-PLAN              '.                          
044100   03    MED-1                  PIC X(30)                                 
044200         VALUE 'TOTALEN AV INDEX ÄR INTE 12.00'.                          
044300   03    MED-2.                                                           
044400      05 FILLER                 PIC X(20)                                 
044500         VALUE 'TOTALEN AV INDEX ÄR '.                                    
044600      05 MED-2-INDEX            PIC 99.99.                                
044700      05 FILLER                 PIC X(20)                                 
044800         VALUE ' MÅSTE VARA 12.00'.                                       
044900     03  MED-3                  PIC X(30)                                 
045000         VALUE 'FELAKTIGT SÄSONGSINDEX        '.                          
045100     03  MED-4                  PIC X(30)                                 
045200         VALUE 'FELAKTIGT DATUM               '.                          
045300                                                                          
045400     EJECT                                                                
045500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
045600 01  GENERELLA-SUBPROGRAM.                                                
045700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
045800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
045900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
046000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
046100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
046200     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
046300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
046400     EJECT                                                                
046500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
046600*01 -COPY WMEDAREA                                                        
046700     SKIP3                                                                
046800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
046900*01 -COPY WDATAREA                                                        
047000     SKIP3                                                                
047100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
047200*01 -COPY WMSGINIT                                                        
047300     SKIP3                                                                
047400*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
047500*01  -COPY WDECAREA                                                       
047600     EJECT                                                                
047700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
047800*                                                                         
047900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
048000     SKIP3                                                                
048100*01  MID -COPY W2I13701                                                   
048200     EJECT                                                                
048300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
048400     SKIP3                                                                
048500*01  -COPY WMSGAREA                                                       
048600     EJECT                                                                
048700     03  MOD REDEFINES MSG-AREA.                                          
048800*      05  -COPY W2O13701                                                 
048900     EJECT                                                                
049000*    --- PARAMETRAR TILL SUBPROGRAM W222PBTO                              
049100*                                                                         
049200 01  FILLER                      PIC X(16)   VALUE 'W222PBTO'.            
049300     SKIP3                                                                
049400*01 -COPY W222PBTO                                                        
049500     SKIP3                                                                
049600     EJECT                                                                
049700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
049800     SKIP3                                                                
049900*01  -COPY WMFSAREA                                                       
050000     EJECT                                                                
050100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
050200*                                                                         
050300     EJECT                                                                
050400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
050500     SKIP3                                                                
050600 01  NYCKLAR-TILL-DLI.                                                    
050700     03  W-IDARTNR-X.                                                     
050800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
050900     03  W-KDSEGKEY-X.                                                    
051000         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
051100     03  W-IDDCREF-X.                                                     
051200         05  W-IDDC-REF          PIC X(02)   VALUE SPACE.                 
051300     03  W-KDCLAGER-X.                                                    
051400         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
051500     03  W-IDSKYLT-X.                                                     
051600         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
051700     03  W-2202KEY-X.                                                     
051800         05  W-2202              PIC X(4)    VALUE '2202'.                
051900         05  W-NYCKEL-VALFRI     PIC X(26)   VALUE LOW-VALUE.             
052000     03  W-2227KEY-X.                                                     
052100         05  W-2227              PIC X(4)    VALUE '2227'.                
052200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
052300     03  W-TIAAAA-X.                                                      
052400         05  W-TIAAAA        PIC 9(4)   VALUE ZERO.                       
052500     SKIP2                                                                
052600*    --- STATUS-KOD FRÅN IMS                                              
052700 01  STATUS-WS                   PIC XX.                                  
052800     88  SEGMENT-FINNS                       VALUE '  '.                  
052900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
053000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
053100     SKIP2                                                                
053200 01  GODK-STATUSKODER.                                                    
053300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
053400     SKIP3                                                                
053500 01  SSA1                        PIC X(64).                               
053600 01  SSA2                        PIC X(64).                               
053700 01  SSA3                        PIC X(64).                               
053800 01  SSA4                        PIC X(64).                               
053900     EJECT                                                                
054000*    --- IMS FUNKTIONSKODER                                               
054100*01  -COPY W0003                                                          
054200     EJECT                                                                
054300*    ---  DLI INPUT-OUTPUT AREA                                           
054400     EJECT                                                                
054500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
054600     SKIP2                                                                
054700 01  DLI-IO-AREA-WDK601.                                                  
054800*    03  -COPY WDK601                                                     
054900     EJECT                                                                
055000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
055100     SKIP2                                                                
055200 01  DLI-IO-AREA-WDK611.                                                  
055300*    03  -COPY WDK611                                                     
055400     EJECT                                                                
055500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK629'.         
055600     SKIP2                                                                
055700 01  DLI-IO-AREA-WDK629.                                                  
055800*    03  -COPY WDK629                                                     
055900     EJECT                                                                
056000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-BENA11'.         
056100     SKIP2                                                                
056200 01  DLI-IO-AREA-BENA11.                                                  
056300*    03  -COPY WDD311                                                     
056400     EJECT                                                                
056500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-OIGB01'.         
056600     SKIP2                                                                
056700 01  DLI-IO-AREA-OIGB01.                                                  
056800*    03  -COPY WDL801                                                     
056900     EJECT                                                                
057000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-OIGB11'.         
057100     SKIP2                                                                
057200 01  DLI-IO-AREA-OIGB11.                                                  
057300*    03  -COPY WDL811                                                     
057400     EJECT                                                                
057500 LINKAGE SECTION.                                                         
057600     SKIP2                                                                
057700*01  -COPY W0009     -PRE MSG-                                            
057800     EJECT                                                                
057900*01  -COPY W0008     -PRE USEA-.                                          
058000         05  FILLER                PIC X.                                 
058100     EJECT                                                                
058200*01  -COPY W0008     -PRE WDK6-.                                          
058300         05  FILLER                PIC X.                                 
058400     EJECT                                                                
058500*01  -COPY W0008     -PRE BENA-.                                          
058600         05  FILLER                PIC X.                                 
058700     EJECT                                                                
058800*01  -COPY W0008     -PRE OIGB-                                           
058900     05  FILLER                    PIC X.                                 
059000     EJECT                                                                
059100 01  PBTO-WDK6-PCB                 PIC X.                                 
059200 01  PBTO-WDK7-PCB                 PIC X.                                 
059300 01  PBTO-ARTM-PCB                 PIC X.                                 
059400 01  PBTO-2501-PCB                 PIC X.                                 
059500 01  PBTO-WDB6R-PCB                PIC X.                                 
059600 01  PBTO-WDK7R-PCB                PIC X.                                 
059700 01  PBTO-WDB6-PCB                 PIC X.                                 
059800 01  PBTO-WDD7-PCB                 PIC X.                                 
059900 01  PBTO-WDK7E-PCB                PIC X.                                 
060000 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
060100 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
060200 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
060300 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
060400 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
060500 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
060600 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
060700 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
060800     EJECT                                                                
060900 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
061000                 WDK6-PCB  BENA-PCB OIGB-PCB                              
061100                 PBTO-WDK6-PCB  PBTO-WDK7-PCB                             
061200                 PBTO-ARTM-PCB  PBTO-2501-PCB                             
061300                 PBTO-WDB6R-PCB PBTO-WDK7R-PCB                            
061400                 PBTO-WDB6-PCB  PBTO-WDD7-PCB PBTO-WDK7E-PCB              
061500                 PBTO-W222-UTIL-WDK6-PCB                                  
061600                 PBTO-W222-UTIL-WDK7-PCB                                  
061700                 PBTO-W222-UTIL-WDB6-PCB                                  
061800                 PBTO-W222-UTUP-WDK7-PCB                                  
061900                 PBTO-W222-UTUP-WDB6-PCB                                  
062000                 PBTO-W222-UTUP-UTIL-WDK6-PCB                             
062100                 PBTO-W222-UTUP-UTIL-WDK7-PCB                             
062200                 PBTO-W222-UTUP-UTIL-WDB6-PCB                             
062300                 .                                                        
062400     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
062500                 WDK6-PCB  BENA-PCB OIGB-PCB                              
062600                 PBTO-WDK6-PCB  PBTO-WDK7-PCB                             
062700                 PBTO-ARTM-PCB  PBTO-2501-PCB                             
062800                 PBTO-WDB6R-PCB PBTO-WDK7R-PCB                            
062900                 PBTO-WDB6-PCB  PBTO-WDD7-PCB PBTO-WDK7E-PCB              
063000                 PBTO-W222-UTIL-WDK6-PCB                                  
063100                 PBTO-W222-UTIL-WDK7-PCB                                  
063200                 PBTO-W222-UTIL-WDB6-PCB                                  
063300                 PBTO-W222-UTUP-WDK7-PCB                                  
063400                 PBTO-W222-UTUP-WDB6-PCB                                  
063500                 PBTO-W222-UTUP-UTIL-WDK6-PCB                             
063600                 PBTO-W222-UTUP-UTIL-WDK7-PCB                             
063700                 PBTO-W222-UTUP-UTIL-WDB6-PCB                             
063800                 .                                                        
063900                                                                          
064000     PERFORM IMS-GET-MSG                                                  
064100     IF SEGMENT-FINNS                                                     
064200       PERFORM A-INIT                                                     
064300*      -- ACCESS CHECK PERFORMED IN B-KOLLA-NYCKLAR                       
064400       PERFORM B-KOLLA-NYCKLAR                                            
064500       IF NYCKLAR-OK                                                      
064600         IF MFS-UPDATE                                                    
064700           PERFORM G-KOLLA-INPUT                                          
064800           IF INDATA-OK                                                   
064900             PERFORM H-UPPDATERA                                          
065000             PERFORM F-LAES-VISA-INFO                                     
065100           END-IF                                                         
065200         ELSE                                                             
065300           PERFORM F-LAES-VISA-INFO                                       
065400           IF WDK611-FINNS                                                
065500             PERFORM E-ENTER-TRYCKNING                                    
065600           END-IF                                                         
065700         END-IF                                                           
065800                                                                          
065900         IF INDATA-OK AND WDK611-FINNS                                    
066000*   UPPDATERA BILDEN                                                      
066100           MOVE +1           TO PER-IX                                    
066200           PERFORM UNTIL PER-IX > +12                                     
066300             MOVE WS-MSGI-SIMIX (PER-IX)                                  
066400                             TO WS-RED-RESEASON                           
066500             MOVE WS-RED-RESEASON                                         
066600                             TO MOD-RESEASON-SIM (PER-IX)                 
066700             MOVE WS-MSGI-SIMANT (PER-IX)                                 
066800                             TO MOD-KVPB-PER-SIM (PER-IX)                 
066900             ADD +1          TO PER-IX                                    
067000           END-PERFORM                                                    
067100           MOVE WS-MSGI-TISEASON                                          
067200                             TO MOD-TISEASON-SIM                          
067300         END-IF                                                           
067400                                                                          
067500         MOVE '002'             TO MSGI-KDCALL                            
067600         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
067700         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
067800         MOVE '2137'            TO MSGI-IDTRANS                           
067900         MOVE WS-MSGI-AREA      TO MSGI-SPAR-AREA                         
068000         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
068100       END-IF                                                             
068200*      MOVE WS-TEMFSINF         TO MOD-TEMFSINF                           
068300*      MOVE MID-W2I13701        TO MOD-TEMFSINF                           
068400*      MOVE MID-INPUT           TO MOD-TEMFSINF                           
068500*      MOVE MID-RESEASON-SIM-GRP TO MOD-TEMFSINF                          
068600*      MOVE MID-TISEASON-SIM    TO MOD-TEMFSINF                           
068700       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O13701 + 4                      
068800       PERFORM IMS-INSERT-MSG                                             
068900     END-IF                                                               
069000                                                                          
069100     MOVE ZERO TO RETURN-CODE                                             
069200     GOBACK                                                               
069300     .                                                                    
069400     EJECT                                                                
069500                                                                          
069600                                                                          
069700 A-INIT SECTION.                                                          
069800                                                                          
069900     IF MSG-DUBBLA-TRANSKODER                                             
070000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13701                 
070100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
070200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
070300     ELSE                                                                 
070400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13701                  
070500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
070600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
070700     END-IF                                                               
070800                                                                          
070900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
071000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
071100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
071200                                                                          
071300     MOVE LOW-VALUE TO MSG-AREA                                           
071400     MOVE 'W2O13701' TO MFS-IDMOD                                         
071500     MOVE '2137' TO MOD-IDTRANS                                           
071600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
071700                                                                          
071800     IF EGEN-MID OR HELP-MID                                              
071900       CONTINUE                                                           
072000     ELSE                                                                 
072100       MOVE SPACE TO MFS-KDTRTYP                                          
072200       MOVE '7' TO MFS-IDPFK                                              
072300     END-IF                                                               
072400                                                                          
072500     MOVE FUNCTION CURRENT-DATE                                           
072600                             TO WS-CURRENT-DATE                           
072700                                                                          
072800     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
072900     MOVE WS-DAGENS-DATUM (3:6)                                           
073000                             TO DAT-I-TIDATUM                             
073100                                                                          
073200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
073300                     DAT-O-TIDATUM DAT-KDSVAR                             
073400                                                                          
073500     IF DAT-KDSVAR-OK                                                     
073600****             HÄMTA SEKELSIFFROR                                       
073700                                                                          
073800       MOVE DAT-TIAARP       TO WS-DAGENS-PER                             
073900       MOVE DAT-TIVV         TO WS-DAGENS-VECKA                           
074000       MOVE DAT-TIAA        TO DAGENS-AAR                                 
074100       MOVE DAT-TIVV        TO DAGENS-VECKA                               
074200       MOVE DAT-TID         TO DAGENS-DAG                                 
074300       MOVE DAT-TIRP        TO DAGENS-PERIOD                              
074400       MOVE DAGENS-DATUM-AAVVD                                            
074500                            TO DAGENS-DATUM                               
074600                                                                          
074700     ELSE                                                                 
074800         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
074900         DELIMITED BY SIZE INTO FELTEXT                                   
075000         CALL FELLOG                                                      
075100     END-IF                                                               
075200                                                                          
075300     PERFORM AA-INIT-PERTAB                                               
075400                                                                          
075500     MOVE +1                 TO PER-IX                                    
075600                                                                          
075700     PERFORM UNTIL PER-IX > +12                                           
075800       MOVE PER-IX           TO MOD-TIPP (PER-IX)                         
075900       ADD +1                TO PER-IX                                    
076000     END-PERFORM                                                          
076100     .                                                                    
076200     EJECT                                                                
076300                                                                          
076400                                                                          
076500 AA-INIT-PERTAB SECTION.                                                  
076600*--------------------------------------------------------------*          
076700* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
076800* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
076900* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
077000* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
077100*--------------------------------------------------------------*          
077200                                                                          
077300     COMPUTE WS-DAGENS-TIAAAA-1 = WS-DAGENS-TIAAAA - 1                    
077400                                                                          
077500     COMPUTE WS-DAGENS-TIAAAA-2 = WS-DAGENS-TIAAAA - 2                    
077600                                                                          
077700     PERFORM AAA-INIT-PERTAB-AR-2                                         
077800     PERFORM AAB-INIT-PERTAB-AR-1                                         
077900     PERFORM AAC-INIT-PERTAB-AKTUELLT-AR                                  
078000     .                                                                    
078100     EJECT                                                                
078200                                                                          
078300                                                                          
078400 AAA-INIT-PERTAB-AR-2 SECTION.                                            
078500*--------------------------------------------------------------*          
078600* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
078700* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
078800* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
078900* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
079000*--------------------------------------------------------------*          
079100                                                                          
079200     MOVE WS-DAGENS-TIAAAA-2 (3:2)                                        
079300                             TO WS-TIAAPP-AA                              
079400     MOVE 1                  TO WS-TIAAPP-PP                              
079500                                                                          
079600     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
079700                                                                          
079800       MOVE 'AARP'           TO DAT-KDDATFORM                             
079900       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
080000                                                                          
080100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
080200                           DAT-O-TIDATUM DAT-KDSVAR                       
080300                                                                          
080400       IF DAT-KDSVAR-OK                                                   
080500*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
080600         IF DAT-TIVV = +52 OR +53                                         
080700            MOVE 1           TO WS-PERTAB-START-VV-2(WS-TIAAPP-PP)        
080800         ELSE                                                             
080900            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-2(WS-TIAAPP-PP)        
081000         END-IF                                                           
081100                                                                          
081200       ELSE                                                               
081300           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-2'                    
081400           DELIMITED BY SIZE INTO FELTEXT                                 
081500           CALL FELLOG                                                    
081600       END-IF                                                             
081700                                                                          
081800       ADD 1                 TO WS-TIAAPP                                 
081900     END-PERFORM                                                          
082000                                                                          
082100     MOVE 1                  TO PER-IX                                    
082200                                                                          
082300     PERFORM UNTIL PER-IX > 11                                            
082400                                                                          
082500       COMPUTE WS-PERTAB-SLUT-VV-2 (PER-IX) =                             
082600               WS-PERTAB-START-VV-2 (PER-IX + 1) - 1                      
082700                                                                          
082800       ADD 1                 TO PER-IX                                    
082900     END-PERFORM                                                          
083000                                                                          
083100     MOVE WS-TIAAPP-AA       TO WS-TIAAVV-AA                              
083200     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
083300     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-2 (12)                  
083400     .                                                                    
083500     EJECT                                                                
083600                                                                          
083700                                                                          
083800 AAB-INIT-PERTAB-AR-1 SECTION.                                            
083900*--------------------------------------------------------------*          
084000* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
084100* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
084200* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
084300* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
084400*--------------------------------------------------------------*          
084500                                                                          
084600     MOVE WS-DAGENS-TIAAAA-1 (3:2)                                        
084700                             TO WS-TIAAPP-AA                              
084800     MOVE 1                  TO WS-TIAAPP-PP                              
084900                                                                          
085000     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
085100                                                                          
085200       MOVE 'AARP'           TO DAT-KDDATFORM                             
085300       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
085400                                                                          
085500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
085600                           DAT-O-TIDATUM DAT-KDSVAR                       
085700                                                                          
085800       IF DAT-KDSVAR-OK                                                   
085900*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
086000         IF DAT-TIVV = +52 OR +53                                         
086100            MOVE 1           TO WS-PERTAB-START-VV-1(WS-TIAAPP-PP)        
086200         ELSE                                                             
086300            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-1(WS-TIAAPP-PP)        
086400         END-IF                                                           
086500                                                                          
086600       ELSE                                                               
086700           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-1'                    
086800           DELIMITED BY SIZE INTO FELTEXT                                 
086900           CALL FELLOG                                                    
087000       END-IF                                                             
087100                                                                          
087200       ADD 1                 TO WS-TIAAPP                                 
087300     END-PERFORM                                                          
087400                                                                          
087500     MOVE 1                  TO PER-IX                                    
087600                                                                          
087700     PERFORM UNTIL PER-IX > 11                                            
087800                                                                          
087900       COMPUTE WS-PERTAB-SLUT-VV-1 (PER-IX) =                             
088000               WS-PERTAB-START-VV-1 (PER-IX + 1) - 1                      
088100                                                                          
088200       ADD 1                 TO PER-IX                                    
088300     END-PERFORM                                                          
088400                                                                          
088500     MOVE WS-TIAAPP-AA       TO WS-TIAAVV-AA                              
088600     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
088700     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-1 (12)                  
088800     .                                                                    
088900     EJECT                                                                
089000                                                                          
089100                                                                          
089200 AAC-INIT-PERTAB-AKTUELLT-AR SECTION.                                     
089300*--------------------------------------------------------------*          
089400* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
089500* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
089600* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
089700* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
089800*--------------------------------------------------------------*          
089900                                                                          
090000     MOVE WS-DAGENS-TIAAAA (3:2)                                          
090100                             TO WS-TIAAPP-AA                              
090200     MOVE 1                  TO WS-TIAAPP-PP                              
090300                                                                          
090400     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
090500                                                                          
090600       MOVE 'AARP'           TO DAT-KDDATFORM                             
090700       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
090800                                                                          
090900       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
091000                           DAT-O-TIDATUM DAT-KDSVAR                       
091100                                                                          
091200       IF DAT-KDSVAR-OK                                                   
091300*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
091400         IF DAT-TIVV = +52 OR +53                                         
091500            MOVE 1           TO WS-PERTAB-START-VV-0(WS-TIAAPP-PP)        
091600         ELSE                                                             
091700            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-0(WS-TIAAPP-PP)        
091800         END-IF                                                           
091900                                                                          
092000       ELSE                                                               
092100           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-0'                    
092200           DELIMITED BY SIZE INTO FELTEXT                                 
092300           CALL FELLOG                                                    
092400       END-IF                                                             
092500                                                                          
092600       ADD 1                 TO WS-TIAAPP                                 
092700     END-PERFORM                                                          
092800                                                                          
092900     MOVE 1                  TO PER-IX                                    
093000                                                                          
093100     PERFORM UNTIL PER-IX > 11                                            
093200                                                                          
093300       COMPUTE WS-PERTAB-SLUT-VV-0 (PER-IX) =                             
093400               WS-PERTAB-START-VV-0 (PER-IX + 1) - 1                      
093500                                                                          
093600       ADD 1                 TO PER-IX                                    
093700     END-PERFORM                                                          
093800                                                                          
093900     MOVE WS-TIAAPP-AA       TO WS-TIAAVV-AA                              
094000     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
094100     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-0 (12)                  
094200     .                                                                    
094300     EJECT                                                                
094400                                                                          
094500                                                                          
094600 AB-KOLLA-ANTAL-VECKOR SECTION.                                           
094700                                                                          
094800* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
094900                                                                          
095000     MOVE 53        TO WS-TIAAVV-VV                                       
095100     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
095200     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
095300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
095400                         DAT-O-TIDATUM DAT-KDSVAR                         
095500     IF DAT-KDSVAR-OK                                                     
095600       MOVE 53 TO WS-SLUT-VV                                              
095700     ELSE                                                                 
095800       MOVE 52 TO WS-SLUT-VV                                              
095900     END-IF                                                               
096000     .                                                                    
096100     EJECT                                                                
096200                                                                          
096300                                                                          
096400                                                                          
096500                                                                          
096600 B-KOLLA-NYCKLAR SECTION.                                                 
096700                                                                          
096800     MOVE JA TO NYCKLAR-SW                                                
096900                INDATA-SW                                                 
097000                                                                          
097100*    -- KONTROLL AV IDARTNR                                               
097200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
097300                                                                          
097400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
097500     MOVE '001'             TO MSGI-KDCALL                                
097600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
097700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
097800     MOVE '2137'            TO MSGI-IDTRANS                               
097900     IF EGEN-MID                                                          
098000       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
098100     END-IF                                                               
098200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
098300                                                                          
098400     IF MSGI-IDLAND-SPR = 'GB'                                            
098500       MOVE +2 TO SPRAK-IX                                                
098600       MOVE 'GB ' TO MED-IDSKYLT                                          
098700                     W-IDSKYLT                                            
098800     ELSE                                                                 
098900       MOVE +1 TO SPRAK-IX                                                
099000       MOVE 'S  ' TO MED-IDSKYLT                                          
099100                     W-IDSKYLT                                            
099200     END-IF                                                               
099300                                                                          
099400     IF MSGI-SPAR-AREA (1:4) = '2137'                                     
099500       MOVE MSGI-SPAR-AREA  TO WS-MSGI-AREA                               
099600     ELSE                                                                 
099700*    --   DETTA GÖRS ENBART IFALL MAN KOMMER FRÅN                         
099800*    --   WHELP OCH I DEN TOMMA BILDEN INTE FYLLER I                      
099900*    --   ARTNR SÅ SKALL MAN LÄGGA                                        
100000*    --   UT INFO I BILDEN (GÖRS I E-ENTER SECTION)                       
100100*    --                                                                   
100200       INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO               
100300       MOVE MSGI-IDARTNR     TO MID-IDARTNR-IN                            
100400     END-IF                                                               
100500                                                                          
100600     MOVE JA TO NYCKLAR-SW                                                
100700                                                                          
100800*    -- KONTROLL AV IDARTNR                                               
100900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
101000                                                                          
101100     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
101200     IF MSGI-IDARTNR NUMERIC                                              
101300       MOVE MSGI-IDARTNR     TO WS-IDARTNR                                
101400                                W-IDARTNR                                 
101500     ELSE                                                                 
101600       MOVE NEJ              TO NYCKLAR-SW                                
101700     END-IF                                                               
101800                                                                          
101900     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
102000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
102100                                                                          
102200     IF NYCKLAR-OK                                                        
102300*      --- ACCESS CHECK ---                                               
102400       PERFORM IMS-GET-WDK601                                             
102500       IF SEGMENT-FINNS                                                   
102600          MOVE ART-IDLEVNR TO WS-IDLEVNR-8                                
102700*         --- CHECK IF LIMITATIONS APPLIES FOR THE USER                   
102800          IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                       
102900          OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                 
103000*            --- NO LIMITATIONS ON SUPPLIER CODE                          
103100             CONTINUE                                                     
103200          ELSE                                                            
103300             MOVE NEJ TO NYCKLAR-SW                                       
103400                         INDATA-SW                                        
103500             MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                      
103600             CALL WMEDKONV USING MED-WMEDAREA                             
103700             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
103800             PERFORM MFS-RENSA-FAELT-IN                                   
103900             PERFORM MFS-RENSA-FAELT-UT                                   
104000          END-IF                                                          
104100       ELSE                                                               
104200*         --- THIS CHECK IS DONE ELSEWHERE                                
104300          CONTINUE                                                        
104400       END-IF                                                             
104500     ELSE                                                                 
104600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
104700       CALL WMEDKONV USING MED-WMEDAREA                                   
104800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
104900       PERFORM MFS-RENSA-FAELT-IN                                         
105000       PERFORM MFS-RENSA-FAELT-UT                                         
105100     END-IF                                                               
105200     .                                                                    
105300     EJECT                                                                
105400                                                                          
105500                                                                          
105600 E-ENTER-TRYCKNING SECTION.                                               
105700                                                                          
105800     IF NOT EGEN-MID                                                      
105900     OR MID-IDARTNR-IN NOT = ALL '+'                                      
106000                                                                          
106100       IF CLAG-DASEASON > ZERO                                            
106200         MOVE CLAG-DASEASON (3:6)                                         
106300                             TO MOD-TISEASON-SIM                          
106400                                WS-MSGI-TISEASON                          
106500                                                                          
106600         MOVE +1             TO PER-IX                                    
106700         PERFORM UNTIL PER-IX > +12                                       
106800           MOVE CLAG-RESEASON-PLAN (PER-IX)                               
106900                             TO WS-MSGI-SIMIX (PER-IX)                    
107000           COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                      
107100                   CLAG-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM              
107200           ADD +1            TO PER-IX                                    
107300         END-PERFORM                                                      
107400                                                                          
107500       ELSE                                                               
107600                                                                          
107700         IF CLAG-IDDC-REF NOT = SPACE                                     
107800            MOVE CLAG-IDDC-REF  TO W-IDDC-REF                             
107900            PERFORM IMS-GU-WDK629                                         
108000            IF SEGMENT-FINNS                                              
108100              MOVE SPACE        TO WS-MSGI-TISEASON                       
108200                                                                          
108300              MOVE +1           TO PER-IX                                 
108400              PERFORM UNTIL PER-IX > +12                                  
108500                MOVE CREF-RESEASON-PLAN (PER-IX)                          
108600                                  TO WS-MSGI-SIMIX (PER-IX)               
108700                COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                 
108800                        CREF-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM         
108900                ADD +1          TO PER-IX                                 
109000              END-PERFORM                                                 
109100            ELSE                                                          
109200              MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                     
109300              CALL WMEDKONV        USING MED-WMEDAREA                     
109400              MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                     
109500              PERFORM MFS-RENSA-FAELT-IN                                  
109600              PERFORM MFS-RENSA-FAELT-UT                                  
109700            END-IF                                                        
109800         ELSE                                                             
109900           MOVE W-IDARTNR      TO PBTO-IDARTNR                            
110000           CALL W222PBTO USING PBTO-W222PBTO  PBTO-WDK6-PCB               
110100                               PBTO-WDK7-PCB  PBTO-ARTM-PCB               
110200                               PBTO-2501-PCB  PBTO-WDB6R-PCB              
110300                               PBTO-WDK7R-PCB PBTO-WDB6-PCB               
110400                               PBTO-WDD7-PCB  PBTO-WDK7E-PCB              
110500                               PBTO-W222-UTIL-WDK6-PCB                    
110600                               PBTO-W222-UTIL-WDK7-PCB                    
110700                               PBTO-W222-UTIL-WDB6-PCB                    
110800                               PBTO-W222-UTUP-WDK7-PCB                    
110900                               PBTO-W222-UTUP-WDB6-PCB                    
111000                               PBTO-W222-UTUP-UTIL-WDK6-PCB               
111100                               PBTO-W222-UTUP-UTIL-WDK7-PCB               
111200                               PBTO-W222-UTUP-UTIL-WDB6-PCB               
111300                                                                          
111400           IF PBTO-KDSVAR = JA                                            
111500             MOVE SPACE        TO WS-MSGI-TISEASON                        
111600                                                                          
111700             MOVE +1           TO PER-IX                                  
111800             PERFORM UNTIL PER-IX > +12                                   
111900               MOVE PBTO-RESEASON-PLAN (PER-IX)                           
112000                                 TO WS-MSGI-SIMIX (PER-IX)                
112100               COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                  
112200                       PBTO-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM          
112300               ADD +1          TO PER-IX                                  
112400             END-PERFORM                                                  
112500           ELSE                                                           
112600             MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                      
112700             CALL WMEDKONV USING MED-WMEDAREA                             
112800             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
112900             PERFORM MFS-RENSA-FAELT-IN                                   
113000             PERFORM MFS-RENSA-FAELT-UT                                   
113100           END-IF                                                         
113200         END-IF                                                           
113300       END-IF                                                             
113400                                                                          
113500     ELSE                                                                 
113600       IF MID-INPUT = ALL '+'                                             
113700         PERFORM MFS-RENSA-FAELT-IN                                       
113800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
113900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
114000       ELSE                                                               
114100         PERFORM I-KTRL-INPUT                                             
114200         IF INDATA-OK                                                     
114300           PERFORM EA-SIMULERA-INDEX                                      
114400           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
114500           CALL WMEDKONV USING MED-WMEDAREA                               
114600           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
114700         END-IF                                                           
114800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
114900       END-IF                                                             
115000     END-IF                                                               
115100     .                                                                    
115200     EJECT                                                                
115300 EA-SIMULERA-INDEX SECTION.                                               
115400                                                                          
115500     MOVE ZERO               TO WS-TOT-SIMANT                             
115600     MOVE +1                 TO PER-IX                                    
115700     PERFORM UNTIL PER-IX > +12                                           
115800       ADD WS-MSGI-SIMANT (PER-IX)                                        
115900                             TO WS-TOT-SIMANT                             
116000       ADD +1                TO PER-IX                                    
116100     END-PERFORM                                                          
116200                                                                          
116300     IF SIM-ANTAL-JA                                                      
116400                                                                          
116500       MOVE +1                TO PER-IX                                   
116600                                                                          
116700       PERFORM UNTIL PER-IX > +12                                         
116800         COMPUTE WS-INDEX-DEC ROUNDED =                                   
116900                 WS-MSGI-SIMANT (PER-IX)                                  
117000                                / (WS-TOT-SIMANT / 12)                    
117100                ON SIZE ERROR                                             
117200                    MOVE ZERO     TO WS-INDEX-DEC                         
117300         END-COMPUTE                                                      
117400         COMPUTE WS-MSGI-SIMIX (PER-IX) ROUNDED =                         
117500                 WS-INDEX-DEC * 1                                         
117600         ADD +1              TO PER-IX                                    
117700       END-PERFORM                                                        
117800                                                                          
117900       MOVE ZERO             TO WS-SIMIX-TOT                              
118000       MOVE +1               TO PER-IX                                    
118100       PERFORM UNTIL PER-IX > +12                                         
118200         ADD WS-MSGI-SIMIX (PER-IX)                                       
118300                             TO WS-SIMIX-TOT                              
118400         ADD +1              TO PER-IX                                    
118500       END-PERFORM                                                        
118600                                                                          
118700       IF WS-SIMIX-TOT < +10.00                                           
118800       OR WS-SIMIX-TOT > +14.00                                           
118900         MOVE +1.00          TO WS-MSGI-SIMIX (1)                         
119000                                  WS-MSGI-SIMIX (2)                       
119100                                  WS-MSGI-SIMIX (3)                       
119200                                  WS-MSGI-SIMIX (4)                       
119300                                  WS-MSGI-SIMIX (5)                       
119400                                  WS-MSGI-SIMIX (6)                       
119500                                  WS-MSGI-SIMIX (7)                       
119600                                  WS-MSGI-SIMIX (8)                       
119700                                  WS-MSGI-SIMIX (9)                       
119800                                  WS-MSGI-SIMIX (10)                      
119900                                  WS-MSGI-SIMIX (11)                      
120000                                  WS-MSGI-SIMIX (12)                      
120100         MOVE +12.00         TO WS-SIMIX-TOT                              
120200       END-IF                                                             
120300                                                                          
120400****                                                                      
120500****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 1.00               
120600****                                                                      
120700                                                                          
120800       IF WS-SIMIX-TOT > +12.00                                           
120900         MOVE -0.01          TO WS-JUSTERA                                
121000       ELSE                                                               
121100         MOVE +0.01          TO WS-JUSTERA                                
121200       END-IF                                                             
121300                                                                          
121400       PERFORM UNTIL WS-SIMIX-TOT = +12.00                                
121500                                                                          
121600         MOVE +1             TO PER-IX                                    
121700         PERFORM UNTIL WS-SIMIX-TOT = +12.00                              
121800         OR PER-IX > +12                                                  
121900           IF WS-MSGI-SIMIX (PER-IX) > +0.01                              
122000             ADD WS-JUSTERA  TO WS-MSGI-SIMIX (PER-IX)                    
122100                                WS-SIMIX-TOT                              
122200           END-IF                                                         
122300           ADD +1            TO PER-IX                                    
122400         END-PERFORM                                                      
122500       END-PERFORM                                                        
122600                                                                          
122700                                                                          
122800     ELSE                                                                 
122900*      (SIM-INDEX-JA)                                                     
123000                                                                          
123100       MOVE +1               TO PER-IX                                    
123200                                                                          
123300       PERFORM UNTIL PER-IX > +12                                         
123400                                                                          
123500         COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                        
123600                 WS-MSGI-SIMIX (PER-IX) * WS-KVPB-SIM                     
123700                                                                          
123800         ADD +1              TO PER-IX                                    
123900       END-PERFORM                                                        
124000                                                                          
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400                                                                          
124500                                                                          
124600 F-LAES-VISA-INFO SECTION.                                                
124700                                                                          
124800     MOVE NEJ                TO WDK611-SW                                 
124900                                                                          
125000     PERFORM FA-LAES-GRUNDDATA                                            
125100                                                                          
125200     IF SEGMENT-SAKNAS                                                    
125300        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
125400        CALL WMEDKONV USING MED-WMEDAREA                                  
125500        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
125600        PERFORM MFS-RENSA-FAELT-UT                                        
125700        PERFORM MFS-RENSA-FAELT-IN                                        
125800     ELSE                                                                 
125900        PERFORM FC-ARTIKELDATA                                            
126000        PERFORM FD-ORDERINGONG                                            
126100        PERFORM FE-LAES-BENAMNING-WDD3                                    
126200        PERFORM FF-PBTOTAL-BER                                            
126300        MOVE WS-MEDDELANDE TO MOD-TEMFSINF                                
126400     END-IF                                                               
126500     .                                                                    
126600     EJECT                                                                
126700                                                                          
126800                                                                          
126900 FA-LAES-GRUNDDATA SECTION.                                               
127000                                                                          
127100     PERFORM IMS-GET-WDK601                                               
127200     .                                                                    
127300     EJECT                                                                
127400                                                                          
127500                                                                          
127600 FC-ARTIKELDATA   SECTION.                                                
127700                                                                          
127800     MOVE  NEJ  TO SW-SEASON                                              
127900     MOVE  NEJ  TO SW-ERSATT                                              
128000     MOVE ART-TIFINLV TO SPARAD-TIFINLV                                   
128100     MOVE ART-KDERS-UTG TO SPARAD-KDERS                                   
128200     IF ART-KDERS-UTG > 0                                                 
128300       MOVE UTGONGEN TO WS-MEDDELANDE                                     
128400     ELSE                                                                 
128500       PERFORM IMS-GET-WDK611                                             
128600       IF SEGMENT-FINNS                                                   
128700         MOVE JA             TO WDK611-SW                                 
128800         IF CLAG-DAPBPLAN > WS-DAGENS-DATUM                               
128900         OR CLAG-DAPBPLAN = WS-DAGENS-DATUM                               
129000           MOVE CLAG-DAPBPLAN (3:6)                                       
129100                             TO MOD-TIPBPLAN-MAN                          
129200           MOVE CLAG-KVPB-PLAN                                            
129300                             TO MOD-KVPB-MAN                              
129400                                WS-KVPB-SIM                               
129500                                                                          
129600         ELSE                                                             
129700           MOVE MFS-RENSA-FAELT                                           
129800                             TO MOD-TIPBPLAN-MAN                          
129900                                MOD-KVPB-MAN                              
130000         END-IF                                                           
130100         IF CLAG-DASEASON > WS-DAGENS-DATUM                               
130200         OR CLAG-DASEASON = WS-DAGENS-DATUM                               
130300           MOVE CLAG-DASEASON (3:6)                                       
130400                             TO MOD-TISEASON-MAN                          
130500                                                                          
130600           MOVE +1           TO PER-IX                                    
130700           PERFORM UNTIL PER-IX > +12                                     
130800             MOVE CLAG-RESEASON-PLAN (PER-IX)                             
130900                             TO WS-RED-RESEASON                           
131000             MOVE WS-RED-RESEASON                                         
131100                             TO MOD-RESEASON-MAN (PER-IX)                 
131200             ADD +1          TO PER-IX                                    
131300           END-PERFORM                                                    
131400         ELSE                                                             
131500           MOVE MFS-RENSA-FAELT                                           
131600                             TO MOD-TISEASON-MAN                          
131700                                                                          
131800           MOVE +1           TO PER-IX                                    
131900           PERFORM UNTIL PER-IX > +12                                     
132000             MOVE MFS-RENSA-FAELT                                         
132100                             TO MOD-RESEASON-MAN (PER-IX)                 
132200             ADD +1          TO PER-IX                                    
132300           END-PERFORM                                                    
132400         END-IF                                                           
132500         IF CLAG-IDDC-REF NOT = SPACE                                     
132600            IF NOT MFS-UPDATE                                             
132700               MOVE ERR-REFILL-PART TO MED-IDMFSFEL                       
132800               CALL WMEDKONV USING MED-WMEDAREA                           
132900               MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                       
133000            END-IF                                                        
133100         END-IF                                                           
133200       ELSE                                                               
133300         MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                              
133400         CALL WMEDKONV USING MED-WMEDAREA                                 
133500         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
133600         PERFORM MFS-RENSA-FAELT-UT                                       
133700         PERFORM MFS-RENSA-FAELT-IN                                       
133800       END-IF                                                             
133900                                                                          
134000       MOVE CLAG-KDERS TO SPARAD-KDERS                                    
134100       IF CLAG-KDERS > 0                                                  
134200         MOVE ERSATT TO WS-MEDDELANDE                                     
134300         MOVE JA     TO SW-ERSATT                                         
134400       END-IF                                                             
134500     END-IF                                                               
134600     .                                                                    
134700     EJECT                                                                
134800                                                                          
134900                                                                          
135000 FD-ORDERINGONG     SECTION.                                              
135100                                                                          
135200     MOVE +1                 TO PER-IX                                    
135300                                                                          
135400     PERFORM UNTIL PER-IX > +12                                           
135500                                                                          
135600       MOVE ZERO             TO WS-KVOI-CDC-SUM (PER-IX)                  
135700                                WS-KVOI-DC-REF-SUM (PER-IX)               
135800                                WS-KVOI-DC-KUND-SUM (PER-IX)              
135900       ADD +1                TO PER-IX                                    
136000     END-PERFORM                                                          
136100                                                                          
136200     MOVE +1                 TO PER-IX                                    
136300                                                                          
136400************* START ÅR ************                                       
136500     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 2                              
136600     PERFORM IMS-GET-WDL811                                               
136700                                                                          
136800     IF SEGMENT-FINNS                                                     
136900                                                                          
137000       MOVE DAGENS-PERIOD    TO PER-IX                                    
137100       MOVE WS-PERTAB-START-VV-2 (PER-IX)                                 
137200                             TO VECKA-IX                                  
137300                                                                          
137400       PERFORM UNTIL VECKA-IX > +52                                       
137500                                                                          
137600         MOVE ZERO           TO WS-KVOI-PROG                              
137700                                WS-KVOI-REFILL                            
137800                                WS-KVOI-LEDTID                            
137900         PERFORM UNTIL VECKA-IX >                                         
138000                       WS-PERTAB-SLUT-VV-2 (PER-IX)                       
138100                                                                          
138200           ADD AAR-KVOI-PROG (VECKA-IX)                                   
138300                             TO WS-KVOI-PROG                              
138400           ADD AAR-KVOI-REFILL (VECKA-IX)                                 
138500                             TO WS-KVOI-REFILL                            
138600           ADD AAR-KVOI-LEDTID (VECKA-IX)                                 
138700                             TO WS-KVOI-LEDTID                            
138800           ADD +1            TO VECKA-IX                                  
138900         END-PERFORM                                                      
139000                                                                          
139100         ADD WS-KVOI-PROG    TO WS-KVOI-CDC-SUM (PER-IX)                  
139200         ADD WS-KVOI-REFILL  TO WS-KVOI-DC-REF-SUM (PER-IX)               
139300         ADD WS-KVOI-LEDTID  TO WS-KVOI-DC-KUND-SUM (PER-IX)              
139400         ADD +1              TO PER-IX                                    
139500                                                                          
139600       END-PERFORM                                                        
139700     END-IF                                                               
139800************* FÖREGÅENDE ÅR ************                                  
139900     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 1                              
140000     PERFORM IMS-GET-WDL811                                               
140100                                                                          
140200     IF SEGMENT-FINNS                                                     
140300                                                                          
140400       MOVE +1               TO PER-IX                                    
140500       MOVE WS-PERTAB-START-VV-1 (PER-IX)                                 
140600                             TO VECKA-IX                                  
140700                                                                          
140800       PERFORM UNTIL VECKA-IX > +52                                       
140900                                                                          
141000         MOVE ZERO           TO WS-KVOI-PROG                              
141100                                WS-KVOI-REFILL                            
141200                                WS-KVOI-LEDTID                            
141300         PERFORM UNTIL VECKA-IX >                                         
141400                       WS-PERTAB-SLUT-VV-1 (PER-IX)                       
141500                                                                          
141600           ADD AAR-KVOI-PROG (VECKA-IX)                                   
141700                             TO WS-KVOI-PROG                              
141800           ADD AAR-KVOI-REFILL (VECKA-IX)                                 
141900                             TO WS-KVOI-REFILL                            
142000           ADD AAR-KVOI-LEDTID (VECKA-IX)                                 
142100                             TO WS-KVOI-LEDTID                            
142200           ADD +1            TO VECKA-IX                                  
142300         END-PERFORM                                                      
142400                                                                          
142500         ADD WS-KVOI-PROG    TO WS-KVOI-CDC-SUM (PER-IX)                  
142600         ADD WS-KVOI-REFILL  TO WS-KVOI-DC-REF-SUM (PER-IX)               
142700         ADD WS-KVOI-LEDTID  TO WS-KVOI-DC-KUND-SUM (PER-IX)              
142800         ADD +1              TO PER-IX                                    
142900                                                                          
143000       END-PERFORM                                                        
143100     END-IF                                                               
143200************* I ÅR ************                                           
143300     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA                                  
143400     PERFORM IMS-GET-WDL811                                               
143500                                                                          
143600     IF SEGMENT-FINNS                                                     
143700                                                                          
143800       MOVE +1               TO PER-IX                                    
143900       MOVE WS-PERTAB-START-VV-0 (PER-IX)                                 
144000                             TO VECKA-IX                                  
144100                                                                          
144200      IF DAGENS-PERIOD > 1                                                
144300       PERFORM UNTIL VECKA-IX >                                           
144400                         WS-PERTAB-SLUT-VV-0 (DAGENS-PERIOD - 1)          
144500                                                                          
144600         MOVE ZERO           TO WS-KVOI-PROG                              
144700                                WS-KVOI-REFILL                            
144800                                WS-KVOI-LEDTID                            
144900         PERFORM UNTIL VECKA-IX >                                         
145000                       WS-PERTAB-SLUT-VV-0 (PER-IX)                       
145100                                                                          
145200           ADD AAR-KVOI-PROG (VECKA-IX)                                   
145300                             TO WS-KVOI-PROG                              
145400           ADD AAR-KVOI-REFILL (VECKA-IX)                                 
145500                             TO WS-KVOI-REFILL                            
145600           ADD AAR-KVOI-LEDTID (VECKA-IX)                                 
145700                             TO WS-KVOI-LEDTID                            
145800           ADD +1            TO VECKA-IX                                  
145900         END-PERFORM                                                      
146000                                                                          
146100         ADD WS-KVOI-PROG    TO WS-KVOI-CDC-SUM (PER-IX)                  
146200         ADD WS-KVOI-REFILL  TO WS-KVOI-DC-REF-SUM (PER-IX)               
146300         ADD WS-KVOI-LEDTID  TO WS-KVOI-DC-KUND-SUM (PER-IX)              
146400         ADD +1              TO PER-IX                                    
146500                                                                          
146600       END-PERFORM                                                        
146700      END-IF                                                              
146800     END-IF                                                               
146900                                                                          
147000     MOVE +1                 TO PER-IX                                    
147100                                                                          
147200     MOVE ZERO               TO WS-KVOI-CDC-TOT                           
147300                                WS-KVOI-DC-REF-TOT                        
147400                                WS-KVOI-DC-KUND-TOT                       
147500                                                                          
147600     PERFORM UNTIL PER-IX > +12                                           
147700                                                                          
147800       COMPUTE WS-KVOI-CDC-SNITT ROUNDED =                                
147900                  WS-KVOI-CDC-SUM (PER-IX) / 2                            
148000       MOVE WS-KVOI-CDC-SNITT                                             
148100                             TO MOD-KVOI-CDC (PER-IX)                     
148200       ADD WS-KVOI-CDC-SNITT TO WS-KVOI-CDC-TOT                           
148300                                                                          
148400       COMPUTE WS-KVOI-DC-REF-SNITT ROUNDED =                             
148500                  WS-KVOI-DC-REF-SUM (PER-IX) / 2                         
148600       MOVE WS-KVOI-DC-REF-SNITT                                          
148700                             TO MOD-KVOI-DC-REF (PER-IX)                  
148800       ADD WS-KVOI-DC-REF-SNITT                                           
148900                             TO WS-KVOI-DC-REF-TOT                        
149000                                                                          
149100       COMPUTE WS-KVOI-DC-KUND-SNITT ROUNDED =                            
149200                  WS-KVOI-DC-KUND-SUM (PER-IX) / 2                        
149300       MOVE WS-KVOI-DC-KUND-SNITT                                         
149400                             TO MOD-KVOI-DC-KUND (PER-IX)                 
149500       ADD WS-KVOI-DC-KUND-SNITT                                          
149600                             TO WS-KVOI-DC-KUND-TOT                       
149700                                                                          
149800       ADD +1                TO PER-IX                                    
149900                                                                          
150000     END-PERFORM                                                          
150100                                                                          
150200     MOVE WS-KVOI-CDC-TOT    TO MOD-KVOI-TOT-12-CDC                       
150300     MOVE WS-KVOI-DC-REF-TOT TO MOD-KVOI-TOT-12-DC-REF                    
150400     MOVE WS-KVOI-DC-KUND-TOT                                             
150500                             TO MOD-KVOI-TOT-12-DC-KUND                   
150600                                                                          
150700     MOVE +1                 TO PER-IX                                    
150800     PERFORM UNTIL PER-IX > +12                                           
150900       COMPUTE WS-INDEX-DEC ROUNDED =                                     
151000                  ((WS-KVOI-CDC-SUM (PER-IX) +                            
151100                    WS-KVOI-DC-REF-SUM (PER-IX)) / 2)                     
151200                             / ((WS-KVOI-CDC-TOT +                        
151300                                  WS-KVOI-DC-REF-TOT) / 12)               
151400                  ON SIZE ERROR                                           
151500                      MOVE ZERO   TO WS-INDEX-DEC                         
151600       END-COMPUTE                                                        
151700       COMPUTE WS-RESEASON-HIST (PER-IX) ROUNDED =                        
151800               WS-INDEX-DEC * 1                                           
151900       ADD +1                TO PER-IX                                    
152000     END-PERFORM                                                          
152100                                                                          
152200     MOVE ZERO               TO WS-SIMIX-TOT                              
152300     MOVE +1                 TO PER-IX                                    
152400     PERFORM UNTIL PER-IX > +12                                           
152500       ADD WS-RESEASON-HIST (PER-IX)                                      
152600                             TO WS-SIMIX-TOT                              
152700       ADD +1                TO PER-IX                                    
152800     END-PERFORM                                                          
152900                                                                          
153000     IF WS-SIMIX-TOT < +10.00                                             
153100     OR WS-SIMIX-TOT > +14.00                                             
153200       MOVE +1.00            TO WS-RESEASON-HIST (1)                      
153300                                WS-RESEASON-HIST (2)                      
153400                                WS-RESEASON-HIST (3)                      
153500                                WS-RESEASON-HIST (4)                      
153600                                WS-RESEASON-HIST (5)                      
153700                                WS-RESEASON-HIST (6)                      
153800                                WS-RESEASON-HIST (7)                      
153900                                WS-RESEASON-HIST (8)                      
154000                                WS-RESEASON-HIST (9)                      
154100                                WS-RESEASON-HIST (10)                     
154200                                WS-RESEASON-HIST (11)                     
154300                                WS-RESEASON-HIST (12)                     
154400       MOVE +12.00           TO WS-SIMIX-TOT                              
154500     END-IF                                                               
154600                                                                          
154700****                                                                      
154800****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 1.00               
154900****                                                                      
155000                                                                          
155100     IF WS-SIMIX-TOT > +12.00                                             
155200       MOVE -0.01            TO WS-JUSTERA                                
155300     ELSE                                                                 
155400       MOVE +0.01            TO WS-JUSTERA                                
155500     END-IF                                                               
155600                                                                          
155700     PERFORM UNTIL WS-SIMIX-TOT = +12.00                                  
155800                                                                          
155900       MOVE +1               TO PER-IX                                    
156000       PERFORM UNTIL WS-SIMIX-TOT = +12.00                                
156100       OR PER-IX > +12                                                    
156200         IF WS-RESEASON-HIST (PER-IX) > +0.01                             
156300           ADD WS-JUSTERA    TO WS-RESEASON-HIST (PER-IX)                 
156400                                WS-SIMIX-TOT                              
156500         END-IF                                                           
156600         ADD +1              TO PER-IX                                    
156700       END-PERFORM                                                        
156800     END-PERFORM                                                          
156900                                                                          
157000     MOVE +1                 TO PER-IX                                    
157100     PERFORM UNTIL PER-IX > +12                                           
157200       MOVE WS-RESEASON-HIST (PER-IX)                                     
157300                             TO MOD-RESEASON-HIST (PER-IX)                
157400       ADD +1                TO PER-IX                                    
157500     END-PERFORM                                                          
157600     .                                                                    
157700     EJECT                                                                
157800                                                                          
157900                                                                          
158000 FE-LAES-BENAMNING-WDD3    SECTION.                                       
158100                                                                          
158200     PERFORM IMS-GET-BENA11-BSEQ                                          
158300     MOVE TEXT-BEART         TO MOD-BEART                                 
158400     .                                                                    
158500     EJECT                                                                
158600                                                                          
158700                                                                          
158800 FF-PBTOTAL-BER SECTION.                                                  
158900                                                                          
159000     IF CLAG-IDDC-REF NOT = SPACE                                         
159100       MOVE CLAG-IDDC-REF      TO W-IDDC-REF                              
159200       PERFORM IMS-GU-WDK629                                              
159300       IF SEGMENT-FINNS                                                   
159400         MOVE CREF-KVPB-PLAN   TO MOD-KVPB-MASK                           
159500         IF CLAG-DAPBPLAN > WS-DAGENS-DATUM                               
159600         OR CLAG-DAPBPLAN = WS-DAGENS-DATUM                               
159700           CONTINUE                                                       
159800         ELSE                                                             
159900           MOVE CREF-KVPB-PLAN TO WS-KVPB-SIM                             
160000         END-IF                                                           
160100                                                                          
160200         MOVE +1               TO PER-IX                                  
160300         PERFORM UNTIL PER-IX > +12                                       
160400           MOVE CREF-RESEASON-PLAN (PER-IX)                               
160500                               TO WS-RED-RESEASON                         
160600           MOVE WS-RED-RESEASON                                           
160700                               TO MOD-RESEASON-MASK (PER-IX)              
160800           COMPUTE WS-ANTAL ROUNDED =                                     
160900                     CREF-RESEASON-PLAN (PER-IX)                          
161000                                 *   CREF-KVPB-PLAN                       
161100           MOVE WS-ANTAL       TO MOD-KVPB-PER-MASK (PER-IX)              
161200           ADD +1              TO PER-IX                                  
161300         END-PERFORM                                                      
161400       ELSE                                                               
161500         MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                          
161600         CALL WMEDKONV USING MED-WMEDAREA                                 
161700         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
161800         PERFORM MFS-RENSA-FAELT-IN                                       
161900         PERFORM MFS-RENSA-FAELT-UT                                       
162000       END-IF                                                             
162100     ELSE                                                                 
162200       MOVE W-IDARTNR          TO PBTO-IDARTNR                            
162300       CALL W222PBTO USING PBTO-W222PBTO  PBTO-WDK6-PCB                   
162400                           PBTO-WDK7-PCB  PBTO-ARTM-PCB                   
162500                           PBTO-2501-PCB  PBTO-WDB6R-PCB                  
162600                           PBTO-WDK7R-PCB PBTO-WDB6-PCB                   
162700                           PBTO-WDD7-PCB  PBTO-WDK7E-PCB                  
162800                           PBTO-W222-UTIL-WDK6-PCB                        
162900                           PBTO-W222-UTIL-WDK7-PCB                        
163000                           PBTO-W222-UTIL-WDB6-PCB                        
163100                           PBTO-W222-UTUP-WDK7-PCB                        
163200                           PBTO-W222-UTUP-WDB6-PCB                        
163300                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
163400                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
163500                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
163600                                                                          
163700       IF PBTO-KDSVAR = JA                                                
163800         MOVE PBTO-KVPB-PLAN   TO MOD-KVPB-MASK                           
163900         IF CLAG-DAPBPLAN > WS-DAGENS-DATUM                               
164000         OR CLAG-DAPBPLAN = WS-DAGENS-DATUM                               
164100           CONTINUE                                                       
164200         ELSE                                                             
164300           MOVE PBTO-KVPB-PLAN TO WS-KVPB-SIM                             
164400         END-IF                                                           
164500                                                                          
164600         MOVE +1               TO PER-IX                                  
164700         PERFORM UNTIL PER-IX > +12                                       
164800           MOVE PBTO-RESEASON-PLAN (PER-IX)                               
164900                               TO WS-RED-RESEASON                         
165000           MOVE WS-RED-RESEASON                                           
165100                               TO MOD-RESEASON-MASK (PER-IX)              
165200           COMPUTE WS-ANTAL ROUNDED =                                     
165300                     PBTO-RESEASON-PLAN (PER-IX)                          
165400                                 *   PBTO-KVPB-PLAN                       
165500           MOVE WS-ANTAL       TO MOD-KVPB-PER-MASK (PER-IX)              
165600           ADD +1              TO PER-IX                                  
165700         END-PERFORM                                                      
165800       ELSE                                                               
165900         MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                          
166000         CALL WMEDKONV USING MED-WMEDAREA                                 
166100         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
166200         PERFORM MFS-RENSA-FAELT-IN                                       
166300         PERFORM MFS-RENSA-FAELT-UT                                       
166400       END-IF                                                             
166500     END-IF                                                               
166600     .                                                                    
166700     EJECT                                                                
166800                                                                          
166900                                                                          
167000 G-KOLLA-INPUT SECTION.                                                   
167100                                                                          
167200     MOVE JA  TO INDATA-SW                                                
167300     PERFORM GB-DATABAS-KONTROLL                                          
167400     IF INDATA-OK                                                         
167500       PERFORM I-KTRL-INPUT                                               
167600     ELSE                                                                 
167700        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
167800        CALL WMEDKONV USING MED-WMEDAREA                                  
167900        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
168000     END-IF                                                               
168100     IF INDATA-FEL                                                        
168200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
168300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
168400     END-IF                                                               
168500     .                                                                    
168600     EJECT                                                                
168700                                                                          
168800                                                                          
168900 GB-DATABAS-KONTROLL SECTION.                                             
169000                                                                          
169100     PERFORM IMS-GET-WDK601                                               
169200     IF SEGMENT-FINNS                                                     
169300                                                                          
169400       PERFORM IMS-GET-WDK611                                             
169500       IF SEGMENT-FINNS                                                   
169600         CONTINUE                                                         
169700       ELSE                                                               
169800          MOVE NEJ TO INDATA-SW                                           
169900          MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                             
170000          CALL WMEDKONV USING MED-WMEDAREA                                
170100          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
170200       END-IF                                                             
170300     ELSE                                                                 
170400        MOVE NEJ TO INDATA-SW                                             
170500        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
170600        CALL WMEDKONV USING MED-WMEDAREA                                  
170700        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
170800     END-IF                                                               
170900     .                                                                    
171000     EJECT                                                                
171100                                                                          
171200                                                                          
171300                                                                          
171400                                                                          
171500 H-UPPDATERA SECTION.                                                     
171600                                                                          
171700     PERFORM IMS-GHU-WDK611                                               
171800                                                                          
171900     IF WS-TISEASON > WS-DAGENS-DATUM                                     
172000     OR WS-TISEASON = WS-DAGENS-DATUM                                     
172100       MOVE WS-TISEASON      TO CLAG-DASEASON                             
172200                                                                          
172300       MOVE +1               TO PER-IX                                    
172400       PERFORM UNTIL PER-IX > +12                                         
172500         MOVE WS-MSGI-SIMIX (PER-IX)                                      
172600                             TO CLAG-RESEASON-PLAN (PER-IX)               
172700         ADD +1              TO PER-IX                                    
172800       END-PERFORM                                                        
172900       PERFORM IMS-REPL-WDK6-11                                           
173000                                                                          
173100       IF CLAG-IDDC-REF NOT = SPACE                                       
173200         PERFORM IMS-GHNP-WDK629                                          
173300         IF SEGMENT-FINNS                                                 
173400           IF CREF-FLREFNYO = JA                                          
173500             MOVE NEJ  TO CREF-FLREFNYO                                   
173600             PERFORM IMS-REPL-WDK629                                      
173700           END-IF                                                         
173800         END-IF                                                           
173900       END-IF                                                             
174000       PERFORM HA-UPPDATERA-SIM                                           
174100     ELSE                                                                 
174200       MOVE ZERO             TO CLAG-DASEASON                             
174300                                                                          
174400       MOVE +1               TO PER-IX                                    
174500       PERFORM UNTIL PER-IX > +12                                         
174600         MOVE 1.00           TO CLAG-RESEASON-PLAN (PER-IX)               
174700         ADD +1              TO PER-IX                                    
174800       END-PERFORM                                                        
174900       PERFORM IMS-REPL-WDK6-11                                           
175000       PERFORM HA-UPPDATERA-SIM                                           
175100     END-IF                                                               
175200                                                                          
175300     PERFORM MFS-FORM-ATTR                                                
175400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
175500     CALL WMEDKONV USING MED-WMEDAREA                                     
175600     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
175700     .                                                                    
175800     EJECT                                                                
175900                                                                          
176000                                                                          
176100 HA-UPPDATERA-SIM SECTION.                                                
176200                                                                          
176300****************************************************************          
176400*    SIMULERINGSRADEN SKA UPPDATERAS MED DEFAULTVÄRDEN                    
176500****************************************************************          
176600                                                                          
176700     IF CLAG-IDDC-REF NOT = SPACE                                         
176800       MOVE CLAG-IDDC-REF      TO W-IDDC-REF                              
176900       PERFORM IMS-GU-WDK629                                              
177000       IF SEGMENT-FINNS                                                   
177100         IF CLAG-DAPBPLAN > WS-DAGENS-DATUM                               
177200         OR CLAG-DAPBPLAN = WS-DAGENS-DATUM                               
177300           MOVE CLAG-KVPB-PLAN TO WS-KVPB-SIM                             
177400         ELSE                                                             
177500           MOVE CREF-KVPB-PLAN TO WS-KVPB-SIM                             
177600         END-IF                                                           
177700                                                                          
177800         IF CLAG-DASEASON > ZERO                                          
177900           MOVE CLAG-DASEASON (3:6)                                       
178000                               TO MOD-TISEASON-SIM                        
178100                                  WS-MSGI-TISEASON                        
178200                                                                          
178300           MOVE +1             TO PER-IX                                  
178400           PERFORM UNTIL PER-IX > +12                                     
178500             MOVE CLAG-RESEASON-PLAN (PER-IX)                             
178600                               TO WS-MSGI-SIMIX (PER-IX)                  
178700             COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                    
178800                     CLAG-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM            
178900             ADD +1            TO PER-IX                                  
179000           END-PERFORM                                                    
179100                                                                          
179200         ELSE                                                             
179300           MOVE SPACE          TO WS-MSGI-TISEASON                        
179400                                                                          
179500           MOVE +1             TO PER-IX                                  
179600           PERFORM UNTIL PER-IX > +12                                     
179700             MOVE CREF-RESEASON-PLAN (PER-IX)                             
179800                               TO WS-MSGI-SIMIX (PER-IX)                  
179900             COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                    
180000                     CREF-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM            
180100             ADD +1            TO PER-IX                                  
180200           END-PERFORM                                                    
180300         END-IF                                                           
180400       END-IF                                                             
180500     ELSE                                                                 
180600       MOVE W-IDARTNR          TO PBTO-IDARTNR                            
180700       CALL W222PBTO USING PBTO-W222PBTO  PBTO-WDK6-PCB                   
180800                           PBTO-WDK7-PCB  PBTO-ARTM-PCB                   
180900                           PBTO-2501-PCB  PBTO-WDB6R-PCB                  
181000                           PBTO-WDK7R-PCB PBTO-WDB6-PCB                   
181100                           PBTO-WDD7-PCB  PBTO-WDK7E-PCB                  
181200                           PBTO-W222-UTIL-WDK6-PCB                        
181300                           PBTO-W222-UTIL-WDK7-PCB                        
181400                           PBTO-W222-UTIL-WDB6-PCB                        
181500                           PBTO-W222-UTUP-WDK7-PCB                        
181600                           PBTO-W222-UTUP-WDB6-PCB                        
181700                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
181800                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
181900                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
182000                                                                          
182100       IF PBTO-KDSVAR = JA                                                
182200         IF CLAG-DAPBPLAN > WS-DAGENS-DATUM                               
182300         OR CLAG-DAPBPLAN = WS-DAGENS-DATUM                               
182400           MOVE CLAG-KVPB-PLAN TO WS-KVPB-SIM                             
182500         ELSE                                                             
182600           MOVE PBTO-KVPB-PLAN TO WS-KVPB-SIM                             
182700         END-IF                                                           
182800                                                                          
182900         IF CLAG-DASEASON > ZERO                                          
183000           MOVE CLAG-DASEASON (3:6)                                       
183100                               TO MOD-TISEASON-SIM                        
183200                                  WS-MSGI-TISEASON                        
183300                                                                          
183400           MOVE +1             TO PER-IX                                  
183500           PERFORM UNTIL PER-IX > +12                                     
183600             MOVE CLAG-RESEASON-PLAN (PER-IX)                             
183700                               TO WS-MSGI-SIMIX (PER-IX)                  
183800             COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                    
183900                     CLAG-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM            
184000             ADD +1            TO PER-IX                                  
184100           END-PERFORM                                                    
184200                                                                          
184300         ELSE                                                             
184400           MOVE SPACE          TO WS-MSGI-TISEASON                        
184500                                                                          
184600           MOVE +1             TO PER-IX                                  
184700           PERFORM UNTIL PER-IX > +12                                     
184800             MOVE PBTO-RESEASON-PLAN (PER-IX)                             
184900                               TO WS-MSGI-SIMIX (PER-IX)                  
185000             COMPUTE WS-MSGI-SIMANT (PER-IX) ROUNDED =                    
185100                     PBTO-RESEASON-PLAN (PER-IX) * WS-KVPB-SIM            
185200             ADD +1            TO PER-IX                                  
185300           END-PERFORM                                                    
185400         END-IF                                                           
185500                                                                          
185600       ELSE                                                               
185700         STRING ' FEL FRÅN RUTIN W222PBTO '                               
185800         DELIMITED BY SIZE INTO FELTEXT                                   
185900         CALL FELLOG                                                      
186000       END-IF                                                             
186100     END-IF                                                               
186200     .                                                                    
186300     EJECT                                                                
186400                                                                          
186500                                                                          
186600 I-KTRL-INPUT SECTION.                                                    
186700                                                                          
186800     IF CLAG-KDERS > 10                                                   
186900        MOVE NEJ TO INDATA-SW                                             
187000        MOVE KDERS-OVER-10   TO MOD-TEMFSFEL                              
187100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
187200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
187300     END-IF                                                               
187400                                                                          
187500     MOVE MFS-ADD-LAES-IN-FAELT                                           
187600                             TO MOD-TISEASON-SIM-ATTR                     
187700     IF MID-TISEASON-SIM = ALL '+'                                        
187800       MOVE 'AAMMDD'         TO DAT-KDDATFORM                             
187900       MOVE WS-MSGI-TISEASON TO DAT-I-TIDATUM                             
188000                                WS-TISEASON (3:6)                         
188100                                                                          
188200     ELSE                                                                 
188300       MOVE 'AAMMDD'         TO DAT-KDDATFORM                             
188400       MOVE MID-TISEASON-SIM                                              
188500                             TO DAT-I-TIDATUM                             
188600                                WS-TISEASON (3:6)                         
188700                                WS-MSGI-TISEASON                          
188800     END-IF                                                               
188900                                                                          
189000     IF MFS-UPDATE                                                        
189100                                                                          
189200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
189300                           DAT-O-TIDATUM DAT-KDSVAR                       
189400                                                                          
189500       IF DAT-KDSVAR-OK                                                   
189600                                                                          
189700***   LÄGG TILL SEKEL                                                     
189800         IF WS-TISEASON (3:2) > 50                                        
189900           MOVE 19           TO WS-TISEASON (1:2)                         
190000         ELSE                                                             
190100           MOVE 20           TO WS-TISEASON (1:2)                         
190200         END-IF                                                           
190300                                                                          
190400         MOVE WS-TISEASON (3:6)                                           
190500                             TO MOD-TISEASON-SIM                          
190600       ELSE                                                               
190700         MOVE NEJ            TO INDATA-SW                                 
190800         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
190900                             TO MOD-TISEASON-SIM-ATTR                     
191000         MOVE MED-4          TO MOD-TEMFSFEL                              
191100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
191200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
191300       END-IF                                                             
191400     ELSE                                                                 
191500       IF WS-TISEASON (3:6) NOT = SPACE                                   
191600                                                                          
191700         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
191800                             DAT-O-TIDATUM DAT-KDSVAR                     
191900                                                                          
192000         IF DAT-KDSVAR-OK                                                 
192100                                                                          
192200***     LÄGG TILL SEKEL                                                   
192300           IF WS-TISEASON (3:2) > 50                                      
192400             MOVE 19         TO WS-TISEASON (1:2)                         
192500           ELSE                                                           
192600             MOVE 20         TO WS-TISEASON (1:2)                         
192700           END-IF                                                         
192800                                                                          
192900           MOVE WS-TISEASON (3:6)                                         
193000                               TO MOD-TISEASON-SIM                        
193100         ELSE                                                             
193200           MOVE NEJ          TO INDATA-SW                                 
193300           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
193400                               TO MOD-TISEASON-SIM-ATTR                   
193500           MOVE MED-4        TO MOD-TEMFSFEL                              
193600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
193700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
193800         END-IF                                                           
193900       END-IF                                                             
194000     END-IF                                                               
194100                                                                          
194200     MOVE +1                 TO PER-IX                                    
194300     PERFORM UNTIL PER-IX > +12                                           
194400       IF MID-RESEASON-SIM (PER-IX) NOT = ALL '+'                         
194500         MOVE JA             TO SIM-INDEX-SW                              
194600         MOVE MID-RESEASON-SIM (PER-IX)                                   
194700                             TO DEC-IDFRIDATA                             
194800         MOVE 1              TO DEC-KVHELTAL                              
194900         MOVE 2              TO DEC-KVDECIMAL                             
195000         CALL WDECEDIT USING DEC-WDECAREA                                 
195100         IF DEC-KDSVAR-OK                                                 
195200           MOVE MFS-ADD-LAES-IN-FAELT                                     
195300                             TO MOD-RESEASON-SIM-ATTR (PER-IX)            
195400           MOVE DEC-IDEDITDATA                                            
195500                             TO WS-RED-RESEASON                           
195600                                WS-MSGI-SIMIX (PER-IX)                    
195700           MOVE WS-RED-RESEASON                                           
195800                             TO MOD-RESEASON-SIM (PER-IX)                 
195900         ELSE                                                             
196000           MOVE MED-3        TO MOD-TEMFSFEL                              
196100*          MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
196200           MOVE MFS-ALFA-FAELT-FEL                                        
196300                             TO MOD-RESEASON-SIM-ATTR (PER-IX)            
196400           MOVE NEJ          TO INDATA-SW                                 
196500           MOVE ZERO         TO WS-MSGI-SIMIX (PER-IX)                    
196600         END-IF                                                           
196700       END-IF                                                             
196800       IF MID-KVPB-PER-SIM (PER-IX) NOT = ALL '+'                         
196900         MOVE JA             TO SIM-ANTAL-SW                              
197000         IF MID-KVPB-PER-SIM (PER-IX) NUMERIC                             
197100           MOVE MID-KVPB-PER-SIM (PER-IX)                                 
197200                             TO WS-MSGI-SIMANT (PER-IX)                   
197300         ELSE                                                             
197400           MOVE ERR-CORR-HILITE-FLDS                                      
197500                             TO MED-IDMFSFEL                              
197600           CALL WMEDKONV USING MED-WMEDAREA                               
197700           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
197800           MOVE MFS-ALFA-FAELT-FEL                                        
197900                             TO MOD-KVPB-PER-SIM-ATTR (PER-IX)            
198000           MOVE NEJ          TO INDATA-SW                                 
198100         END-IF                                                           
198200       END-IF                                                             
198300       ADD +1                TO PER-IX                                    
198400     END-PERFORM                                                          
198500                                                                          
198600     IF INDATA-FEL                                                        
198700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
198800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
198900     END-IF                                                               
199000                                                                          
199100     IF INDATA-OK                                                         
199200       IF  SIM-ANTAL-JA                                                   
199300       AND SIM-INDEX-JA                                                   
199400****   INTE MÖJLIGT ATT SIMULERA MED BÅDE ANTAL OCH INDEX                 
199500*****  SAMTIDIGT                                                          
199600           MOVE NEJ TO INDATA-SW                                          
199700           MOVE CONFLICT  TO MED-IDMFSFEL                                 
199800           CALL WMEDKONV USING MED-WMEDAREA                               
199900           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
200000           MOVE +1           TO PER-IX                                    
200100           PERFORM UNTIL PER-IX > +12                                     
200200             IF MID-RESEASON-SIM (PER-IX) NOT = ALL '+'                   
200300               MOVE MFS-ALFA-FAELT-FEL                                    
200400                             TO MOD-RESEASON-SIM-ATTR (PER-IX)            
200500             END-IF                                                       
200600             IF MID-KVPB-PER-SIM (PER-IX) NOT = ALL '+'                   
200700               MOVE MFS-ALFA-FAELT-FEL                                    
200800                             TO MOD-KVPB-PER-SIM-ATTR (PER-IX)            
200900             END-IF                                                       
201000             ADD +1          TO PER-IX                                    
201100           END-PERFORM                                                    
201200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
201300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
201400                                                                          
201500       ELSE                                                               
201600         IF SIM-INDEX-JA                                                  
201700******** KOLLA ATT SUMMAN BLIR 12.00                                      
201800           MOVE ZERO           TO WS-SIMIX-SUM                            
201900           MOVE +1             TO PER-IX                                  
202000           PERFORM UNTIL PER-IX > +12                                     
202100                                                                          
202200             ADD WS-MSGI-SIMIX (PER-IX)                                   
202300                             TO WS-SIMIX-SUM                              
202400             ADD +1          TO PER-IX                                    
202500           END-PERFORM                                                    
202600                                                                          
202700           IF WS-SIMIX-SUM NOT = 12.00                                    
202800               MOVE NEJ TO INDATA-SW                                      
202900               MOVE WS-SIMIX-SUM                                          
203000                             TO MED-2-INDEX                               
203100               MOVE MED-2      TO MOD-TEMFSINF                            
203200               MOVE +1               TO PER-IX                            
203300               PERFORM UNTIL PER-IX > +12                                 
203400                 IF MID-RESEASON-SIM (PER-IX) NOT = ALL '+'               
203500                   MOVE MFS-ALFA-FAELT-FEL                                
203600                              TO MOD-RESEASON-SIM-ATTR (PER-IX)           
203700                 END-IF                                                   
203800                 ADD +1              TO PER-IX                            
203900               END-PERFORM                                                
204000               PERFORM MFS-ROER-EJ-FAELT-IN                               
204100               PERFORM MFS-ROER-EJ-FAELT-UT                               
204200           END-IF                                                         
204300         END-IF                                                           
204400                                                                          
204500       END-IF                                                             
204600     END-IF                                                               
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000                                                                          
205100 MFS-FORM-ATTR SECTION.                                                   
205200                                                                          
205300*    --- ALLA INDATA-FÄLT                                                 
205400     MOVE MFS-FORMATETS-ATTR TO MOD-TISEASON-SIM-ATTR                     
205500                                                                          
205600     MOVE +1 TO INDX                                                      
205700     PERFORM UNTIL INDX > 12                                              
205800       MOVE MFS-FORMATETS-ATTR                                            
205900                             TO MOD-KVPB-PER-SIM-ATTR (INDX)              
206000                                MOD-RESEASON-SIM-ATTR (INDX)              
206100        ADD +1 TO INDX                                                    
206200     END-PERFORM                                                          
206300     .                                                                    
206400     EJECT                                                                
206500                                                                          
206600                                                                          
206700 MFS-RENSA-FAELT-UT SECTION.                                              
206800                                                                          
206900*    --- ALLA UTDATA-FÄLT                                                 
207000     MOVE MFS-RENSA-FAELT         TO MOD-BEART                            
207100                                                                          
207200     MOVE +1 TO INDX                                                      
207300     PERFORM UNTIL INDX > 12                                              
207400        MOVE MFS-RENSA-FAELT      TO MOD-TIPP(INDX)                       
207500                                     MOD-KVOI-CDC(INDX)                   
207600                                     MOD-KVOI-DC-REF(INDX)                
207700                                     MOD-KVOI-DC-KUND(INDX)               
207800                                     MOD-RESEASON-HIST(INDX)              
207900                                     MOD-KVPB-PER-MASK(INDX)              
208000                                     MOD-RESEASON-MASK(INDX)              
208100                                     MOD-RESEASON-MAN(INDX)               
208200        ADD +1 TO INDX                                                    
208300     END-PERFORM                                                          
208400                                                                          
208500     MOVE MFS-RENSA-FAELT         TO MOD-KVOI-TOT-12-CDC                  
208600                                     MOD-KVOI-TOT-12-DC-REF               
208700                                     MOD-KVOI-TOT-12-DC-KUND              
208800                                     MOD-KVPB-MASK                        
208900                                     MOD-KVPB-MAN                         
209000                                     MOD-TIPBPLAN-MAN                     
209100                                     MOD-TISEASON-MAN                     
209200     .                                                                    
209300     EJECT                                                                
209400                                                                          
209500                                                                          
209600 MFS-RENSA-FAELT-IN SECTION.                                              
209700                                                                          
209800*    --- ALLA INDATA-FÄLT                                                 
209900     MOVE MFS-RENSA-FAELT         TO MOD-TISEASON-SIM                     
210000                                                                          
210100     MOVE +1 TO INDX                                                      
210200     PERFORM UNTIL INDX > 12                                              
210300        MOVE MFS-RENSA-FAELT      TO MOD-KVPB-PER-SIM(INDX)               
210400                                     MOD-RESEASON-SIM(INDX)               
210500        ADD +1 TO INDX                                                    
210600     END-PERFORM                                                          
210700     .                                                                    
210800     EJECT                                                                
210900                                                                          
211000                                                                          
211100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
211200                                                                          
211300*    --- ALLA INDATA-FÄLT                                                 
211400     MOVE MFS-ROER-EJ-FAELT  TO MOD-TISEASON-SIM                          
211500                                                                          
211600     MOVE +1                 TO PER-IX                                    
211700     PERFORM UNTIL PER-IX > +12                                           
211800       MOVE MFS-ROER-EJ-FAELT                                             
211900                             TO MOD-KVPB-PER-SIM (PER-IX)                 
212000                                MOD-RESEASON-SIM (PER-IX)                 
212100       ADD +1                TO PER-IX                                    
212200     END-PERFORM                                                          
212300     .                                                                    
212400     EJECT                                                                
212500                                                                          
212600                                                                          
212700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
212800                                                                          
212900*    --- ALLA UTDATA-FÄLT                                                 
213000                                                                          
213100     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                                 
213200                                MOD-KVOI-TOT-12-CDC                       
213300                                MOD-KVOI-TOT-12-DC-REF                    
213400                                MOD-KVOI-TOT-12-DC-KUND                   
213500                                MOD-KVPB-MASK                             
213600                                MOD-KVPB-MAN                              
213700                                MOD-TIPBPLAN-MAN                          
213800                                                                          
213900     MOVE 1                  TO INDX                                      
214000     PERFORM UNTIL INDX > 12                                              
214100       MOVE MFS-ROER-EJ-FAELT  TO MOD-TIPP(INDX)                          
214200                                  MOD-KVOI-CDC(INDX)                      
214300                                  MOD-KVOI-DC-REF(INDX)                   
214400                                  MOD-KVOI-DC-KUND(INDX)                  
214500                                  MOD-RESEASON-HIST(INDX)                 
214600                                  MOD-KVPB-PER-MASK(INDX)                 
214700                                  MOD-RESEASON-MASK(INDX)                 
214800                                  MOD-RESEASON-MAN(INDX)                  
214900       ADD 1                 TO INDX                                      
215000     END-PERFORM                                                          
215100     .                                                                    
215200     EJECT                                                                
215300                                                                          
215400                                                                          
215500* --- IMS SEKTIONER ---                                                   
215600                                                                          
215700                                                                          
215800                                                                          
215900 IMS-GET-MSG SECTION.                                                     
216000                                                                          
216100     MOVE '  QC' TO GODK-STATUSKODER                                      
216200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
216300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     EJECT                                                                
216700                                                                          
216800                                                                          
216900 IMS-INSERT-MSG SECTION.                                                  
217000                                                                          
217100     IF MSGI-IDLAND-SPR = 'GB'                                            
217200       MOVE 'N' TO MFS-KDHUVOMR                                           
217300     END-IF                                                               
217400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
217500     MOVE SPACE TO GODK-STATUSKODER                                       
217600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
217700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
217800     PERFORM IMS-STATUSKONTROLL                                           
217900     .                                                                    
218000     EJECT                                                                
218100                                                                          
218200                                                                          
218300 IMS-GET-WDK601 SECTION.                                                  
218400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
218500          DELIMITED BY SIZE INTO SSA1                                     
218600     MOVE '  GE' TO GODK-STATUSKODER                                      
218700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
218800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
218900     PERFORM IMS-STATUSKONTROLL                                           
219000     .                                                                    
219100     EJECT                                                                
219200                                                                          
219300                                                                          
219400 IMS-GET-WDK611 SECTION.                                                  
219500     MOVE 'WDK611     ' TO SSA1                                           
219600     MOVE '  GE' TO GODK-STATUSKODER                                      
219700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
219800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
219900     PERFORM IMS-STATUSKONTROLL                                           
220000     .                                                                    
220100     SKIP3                                                                
220200                                                                          
220300 IMS-GU-WDK629 SECTION.                                                   
220400                                                                          
220500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
220600          DELIMITED BY SIZE INTO SSA1                                     
220700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
220800          DELIMITED BY SIZE INTO SSA2                                     
220900     STRING 'WDK629  (IDDCREF  =' W-IDDCREF-X ')'                         
221000          DELIMITED BY SIZE INTO SSA3                                     
221100     MOVE '  GE' TO GODK-STATUSKODER                                      
221200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK629 SSA1               
221300                                                       SSA2               
221400                                                       SSA3               
221500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
221600     PERFORM IMS-STATUSKONTROLL                                           
221700     .                                                                    
221800                                                                          
221900 IMS-GHU-WDK611 SECTION.                                                  
222000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
222100          DELIMITED BY SIZE INTO SSA1                                     
222200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
222300          DELIMITED BY SIZE INTO SSA2                                     
222400     MOVE '  ' TO GODK-STATUSKODER                                        
222500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
222600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
222700     PERFORM IMS-STATUSKONTROLL                                           
222800     .                                                                    
222900     EJECT                                                                
223000                                                                          
223100                                                                          
223200 IMS-REPL-WDK6-11 SECTION.                                                
223300                                                                          
223400     MOVE '  ' TO GODK-STATUSKODER                                        
223500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
223600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
223700     PERFORM IMS-STATUSKONTROLL                                           
223800     .                                                                    
223900     EJECT                                                                
224000 IMS-GHNP-WDK629 SECTION.                                                 
224100                                                                          
224200     MOVE 'WDK629     ' TO SSA1                                           
224300     MOVE '  GE' TO GODK-STATUSKODER                                      
224400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA-WDK629 SSA1             
224500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
224600     PERFORM IMS-STATUSKONTROLL                                           
224700     .                                                                    
224800     SKIP3                                                                
224900 IMS-REPL-WDK629  SECTION.                                                
225000                                                                          
225100     MOVE '  ' TO GODK-STATUSKODER                                        
225200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK629                  
225300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
225400     PERFORM IMS-STATUSKONTROLL                                           
225500     .                                                                    
225600     EJECT                                                                
225700                                                                          
225800 IMS-GET-WDL811  SECTION.                                                 
225900                                                                          
226000     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
226100          DELIMITED BY SIZE INTO SSA1                                     
226200     STRING 'WLOIGB11(TIAAAA   =' W-TIAAAA-X ')'                          
226300          DELIMITED BY SIZE INTO SSA2                                     
226400     MOVE '  GE' TO GODK-STATUSKODER                                      
226500     CALL CBLTDLI USING GHU OIGB-PCB                                      
226600                                 DLI-IO-AREA-OIGB11 SSA1 SSA2             
226700     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
226800     PERFORM IMS-STATUSKONTROLL                                           
226900     .                                                                    
227000                                                                          
227100     EJECT                                                                
227200                                                                          
227300                                                                          
227400 IMS-GET-BENA11-BSEQ SECTION.                                             
227500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
227600          DELIMITED BY SIZE INTO SSA1                                     
227700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
227800          DELIMITED BY SIZE INTO SSA2                                     
227900     MOVE '  GE' TO GODK-STATUSKODER                                      
228000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA11 SSA1 SSA2          
228100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
228200     PERFORM IMS-STATUSKONTROLL                                           
228300     .                                                                    
228400     EJECT                                                                
228500                                                                          
228600                                                                          
228700 IMS-STATUSKONTROLL SECTION.                                              
228800                                                                          
228900     SET STATUS-IX TO 1                                                   
229000     SEARCH GODK-STATUS                                                   
229100       AT END                                                             
229200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
229300         DELIMITED BY SIZE INTO FELTEXT                                   
229400         CALL FELLOG                                                      
229500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
229600         CONTINUE                                                         
229700     END-SEARCH                                                           
229800     .                                                                    
229900     EJECT                                                                
