000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2013600.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   AUGUSTI 1997.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        TP-PROGRAM ANSKAFFNING - ORDERINGÅNG                             
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001400*        PROGRAMMET LÄSER      WLOIGB (WDL8)                              
001500*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T136                                              
001900*        MID:         W2I13601                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O13601                                            
002300*                                                                         
002400*   ÄNDRINGAR:                                                            
002500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002900*                                                                         
003000*        21-JUL-14  STORY 2223966/BLOCK 2136 FROM UPD FOR REFILL          
003100*                                 PARTS.                                  
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W2                                                       
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W2013600'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004900                                                                          
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005200                                                                          
005300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005400     88  INDATA-OK                           VALUE 'J'.                   
005500     88  INDATA-FEL                          VALUE 'N'.                   
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006100 77  C2-SW                       PIC X       VALUE 'J'.                   
006200     88  C2-SEG-FINNS                        VALUE 'J'.                   
006300     88  C2-SEG-SAKNAS                       VALUE 'N'.                   
006400                                                                          
006500 77  WDK626-SW                   PIC X       VALUE 'N'.                   
006600     88  TA-BORT-WDK626                      VALUE 'J'.                   
006700                                                                          
006800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006900     88  EGEN-MID                            VALUE '2136'.                
007000     88  GODK-MID                            VALUE '2101' '2102'          
007100                                                   '2103' '2104'          
007200                                                   '2105' '2106'          
007300                                                   '2107' '2108'          
007400                                                   '2109'.                
007500     88  HELP-MID                            VALUE '0551'.                
007600                                                                          
007700 77  CL-IX               PIC S9(9)   COMP SYNC.                           
007800 77  INDX                PIC S9(9)   COMP SYNC.                           
007900 77  INDX2               PIC S9(9)   COMP SYNC.                           
008000 77  INDX-AAR            PIC S9(9)   COMP SYNC.                           
008100 77  INDX-PERIOD         PIC S9(9)   COMP SYNC.                           
008200 77  TABW200-INDEX       PIC S9(4)   COMP SYNC.                           
008300 77  VECKA-IX            PIC S9(9)   COMP SYNC.                           
008400 01  FILLER              PIC X(13)  VALUE 'PER-IX       '.                
008500 77  PER-IX              PIC S9(9)  VALUE ZERO COMP-3.                    
008600 77  MOD-IX              PIC S9(9)   COMP SYNC.                           
008700 77  SPARAD-KDERS        PIC S9(3)   COMP-3.                              
008800 77  SPARAD-KDERS-UTG    PIC S9(3)   COMP-3.                              
008900 77  SPARAD-TIFINLV      PIC S9(5)   COMP-3.                              
009000 77  SEASON-SW-C1        PIC  X(1) VALUE SPACE.                           
009100 77  SEASON-SW-C2        PIC  X(1) VALUE SPACE.                           
009200 77  SW-SEASON           PIC  X(1) VALUE SPACE.                           
009300 77  SW-ERSATT           PIC  X(1) VALUE SPACE.                           
009400 77  WS-AVVIKELSE        PIC  S9(4)V999 VALUE ZERO COMP-3.                
009500 77  W-CLAG-KVPB-PLAN    PIC S9(6)V9(1)      COMP-3.                      
009600 77  W-CLAG-DAPBPLAN     PIC 9(8).                                        
009700 77  W-CLAG-KVMAD-TOT    PIC S9(6)V9(1)      COMP-3.                      
009800 77  W-KVPB-PLAN-JUST1   PIC S9(6)V9(1)      COMP-3.                      
009900 77  W-TIPBPLAN-JUST1-FOM PIC 9(6).                                       
010000 77  W-TIPBPLAN-JUST1-TOM PIC 9(6).                                       
010100 77  W-KVPB-PLAN-JUST2    PIC S9(6)V9(1)      COMP-3.                     
010200 77  W-TIPBPLAN-JUST2-FOM PIC 9(6).                                       
010300 77  W-TIPBPLAN-JUST2-TOM PIC 9(6).                                       
010400     SKIP2                                                                
010500                                                                          
010600 01  WS-FALT.                                                             
010700                                                                          
010800     03  FILLER                  PIC X(12)  VALUE 'CURRENT-DATE'.         
010900     03  WS-CURRENT-DATE.                                                 
011000         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
011100         05  FILLER              PIC 9(4)   VALUE ZERO.                   
011200         05  FILLER              PIC 9(6)   VALUE ZERO.                   
011300                                                                          
011400     03  FILLER REDEFINES WS-CURRENT-DATE.                                
011500*-----   INKLUSIVE SEKEL                                                  
011600         05  WS-DAGENS-DATUM     PIC 9(8).                                
011700         05  WS-DAGENS-TID.                                               
011800             07 WS-DAGENS-TIMME  PIC 9(2).                                
011900             07 WS-DAGENS-MINUT  PIC 9(2).                                
012000             07 WS-DAGENS-SEKUND PIC 9(2).                                
012100     03  WS-DAGENS-TIAAAA-1      PIC 9(4).                                
012200     03  WS-DAGENS-TIAAAA-2      PIC 9(4).                                
012300     03  FILLER                  PIC X(13)  VALUE 'WS-DAGENS-PER'.        
012400     03 WS-DAGENS-PER            PIC 9(4)   VALUE ZERO.                   
012500     03 WS-DAG-PER REDEFINES WS-DAGENS-PER.                               
012600             05 WS-DAGENS-AA     PIC 9(2).                                
012700             05 WS-DAGENS-PP     PIC 9(2).                                
012800     03 WS-DAGENS-AAR            PIC 9(4)    VALUE ZERO.                  
012900     03 WS-DAGENS-VECKA          PIC 9(2)    VALUE ZERO.                  
013000     03  FILLER                  PIC X(13)  VALUE 'WS-TIAAPP    '.        
013100     03 WS-TIAAPP                PIC 9(4)   VALUE ZERO.                   
013200     03 FILLER REDEFINES WS-TIAAPP.                                       
013300             05 WS-TIAAPP-AA     PIC 9(2).                                
013400             05 WS-TIAAPP-PP     PIC 9(2).                                
013500     03 WS-TIAAVV                PIC 9(4)   VALUE ZERO.                   
013600     03 FILLER REDEFINES WS-TIAAVV.                                       
013700             05 WS-TIAAVV-AA     PIC 9(2).                                
013800             05 WS-TIAAVV-VV     PIC 9(2).                                
013900     03 WS-PER                   PIC 9(3)   VALUE ZERO.                   
014000     03 FILLER REDEFINES WS-PER.                                          
014100             05 WS-PER-A         PIC 9.                                   
014200             05 WS-PER-PP        PIC 9(2).                                
014300     03  FILLER                  PIC X(13)  VALUE 'WS-PERTAB-AR2'.        
014400     03 WS-DISPL-TABELL.                                                  
014500     04 WS-PERIODTABELL-AR-2     OCCURS 12.                               
014600        05 WS-PERTAB-TIAAPP-2    PIC 9(4).                                
014700        05 WS-PERTAB-START-VV-2  PIC 9(2).                                
014800        05 WS-PERTAB-SLUT-VV-2   PIC 9(2).                                
014900     03  FILLER                  PIC X(13)  VALUE 'WS-PERTAB-AR1'.        
015000     03 WS-PERIODTABELL-AR-1     OCCURS 12.                               
015100        05 WS-PERTAB-TIAAPP-1    PIC 9(4).                                
015200        05 WS-PERTAB-START-VV-1  PIC 9(2).                                
015300        05 WS-PERTAB-SLUT-VV-1   PIC 9(2).                                
015400     03  FILLER                  PIC X(13)  VALUE 'WS-PERTAB-AR0'.        
015500     03 WS-PERIODTABELL-AR-0     OCCURS 12.                               
015600        05 WS-PERTAB-TIAAPP-0    PIC 9(4).                                
015700        05 WS-PERTAB-START-VV-0  PIC 9(2).                                
015800        05 WS-PERTAB-SLUT-VV-0   PIC 9(2).                                
015900     03 WS-SLUT-VV               PIC 9(2)    VALUE ZERO.                  
016000     03 WS-TEST.                                                          
016100        05 WS-A                PIC S9(3)      VALUE ZERO.                 
016200        05 FILLER              PIC X          VALUE '/'.                  
016300        05 WS-B                PIC S9(3)      VALUE ZERO.                 
016400        05 FILLER              PIC X          VALUE '/'.                  
016500        05 WS-C                PIC S9(3)      VALUE ZERO.                 
016600        05 FILLER              PIC X          VALUE '/'.                  
016700        05 WS-D                PIC S9(3)      VALUE ZERO.                 
016800        05 FILLER              PIC X          VALUE '/'.                  
016900        05 WS-E                PIC S9(3)      VALUE ZERO.                 
017000        05 FILLER              PIC X          VALUE '/'.                  
017100        05 WS-F                PIC S9(3)      VALUE ZERO.                 
017200        05 FILLER              PIC X          VALUE '/'.                  
017300        05 WS-G                PIC S9(3)      VALUE ZERO.                 
017400        05 FILLER              PIC X          VALUE '/'.                  
017500        05 WS-H                PIC S9(3)      VALUE ZERO.                 
017600        05 FILLER              PIC X          VALUE '/'.                  
017700        05 WS-I                PIC S9(3)      VALUE ZERO.                 
017800        05 FILLER              PIC X          VALUE '/'.                  
017900        05 WS-J                PIC S9(3)      VALUE ZERO.                 
018000        05 FILLER              PIC X          VALUE '/'.                  
018100        05 WS-K                PIC S9(3)      VALUE ZERO.                 
018200        05 FILLER              PIC X          VALUE '/'.                  
018300        05 WS-L                PIC S9(3)      VALUE ZERO.                 
018400        05 FILLER              PIC X          VALUE '/'.                  
018500                                                                          
018600                                                                          
018700                                                                          
018800                                                                          
018900     03 WS-IDLEVNR             PIC X(5)       VALUE SPACE.                
019000     03 WS-IDLEVNR-8           PIC X(8)       VALUE SPACE.                
019100     03 WS-IDLKTO              PIC S9(7)      VALUE ZERO COMP-3.          
019200     03 WS-KDUART              PIC  X(1)      VALUE 'X'.                  
019300     03 WS-KDLTK               PIC S9(3)      VALUE ZERO COMP-3.          
019400     03 WS-TILTK               PIC S9(5)      VALUE ZERO COMP-3.          
019500     03 WS-KVLS-C2             PIC S9(7)      VALUE ZERO COMP-3.          
019600     03 WS-KVRESS-C2           PIC S9(7)      VALUE ZERO COMP-3.          
019700     03 W-KVPB-REF             PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
019800     03 W-KVPBREOI             PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
019900     03 WS-KVOI-PER            PIC S9(7)      VALUE ZERO COMP-3.          
020000     03 WS-CLAG-FALT           OCCURS 2.                                  
020100       05 WS-KDERS             PIC S9(3)      COMP-3.                     
020200       05 WS-KVPB-SEP          PIC S9(6)V9(1) COMP-3.                     
020300       05 WS-TIPBDAT           PIC S9(5)      COMP-3.                     
020400       05 WS-RVPROFEL          PIC S9(3)      COMP-3.                     
020500       05 WS-RVPROURS          PIC S9(3)      COMP-3.                     
020600       05 WS-KVUTJFEL          PIC S9(6)V9(1) COMP-3.                     
020700       05 WS-KVPB-SATS         PIC S9(6)V9(1) COMP-3.                     
020800       05 WS-KVMAD-SEP         PIC S9(6)V9(1) COMP-3.                     
020900       05 WS-KVMAD-TOT         PIC S9(6)V9(1) COMP-3.                     
021000       05 FL-TA-BORT-TREND     PIC X(1).                                  
021100     03 WS-START-AAR-SEKEL     PIC 9(4)       VALUE ZERO.                 
021200     03 WS-OIREG-AAR-SEKEL     PIC 9(4)       VALUE ZERO.                 
021300     03 WS-KVOI-CDC-SUM        PIC S9(9)      VALUE ZERO.                 
021400     03 WS-KVOI-DC-REF-SUM     PIC S9(9)      VALUE ZERO.                 
021500     03 WS-KVOI-DC-KUND-SUM    PIC S9(9)      VALUE ZERO.                 
021600     03 WS-KVOI-CDC-RULL-12    PIC S9(9)      VALUE ZERO.                 
021700     03 WS-KVOI-DC-REF-RULL-12 PIC S9(9)      VALUE ZERO.                 
021800     03 WS-KVOI-DC-KUND-RULL-12                                           
021900                               PIC S9(9)     VALUE ZERO.                  
022000     03 WS-KVOI-SNITT-12-CDC   PIC S9(9)      VALUE ZERO.                 
022100     03 WS-KVOI-SNITT-12-DC-REF                                           
022200                               PIC S9(9)     VALUE ZERO.                  
022300     03 WS-KVOI-PROG           PIC S9(9)      VALUE ZERO.                 
022400     03 WS-KVOI-REFILL         PIC S9(9)      VALUE ZERO.                 
022500     03 WS-KVOI-LEDTID         PIC S9(9)      VALUE ZERO.                 
022600     03 WS-RED-KVPB-PLAN       PIC Z(5)9.9    VALUE ZERO.                 
022700     03 WS-KVPB-PLAN           PIC S9(6)V9(1) VALUE ZERO.                 
022800     03 WS-DAPBPLAN            PIC X(8)       VALUE SPACE.                
022900     03 WS-TEMFSINF.                                                      
023000      05 WS-TEMFSINF1          PIC 9(6)V9  VALUE ZERO.                    
023100      05 FILLER                PIC X       VALUE '/'.                     
023200      05 WS-TEMFSINF2          PIC X(6)    VALUE SPACE.                   
023300*    -COPY W221PERT                                                       
023400                                                                          
023500 01  IN-FALT.                                                             
023600     03 IN-KVPB-SEP OCCURS 2.                                             
023700       05 WS-IN-KVPB-SEP       PIC S9(6)V9(1) COMP-3.                     
023800     03 WS-IN-FLMPB-C1         PIC X(1)      VALUE SPACE.                 
023900     03 WS-IN-FLMPB-C2         PIC X(1)      VALUE SPACE.                 
024000                                                                          
024100 01  KVPB-X.                                                              
024200     03  WS-HELTAL       PIC 9(6).                                        
024300     03  WS-PUNKT        PIC X.                                           
024400     03  WS-DECIMAL      PIC 9.                                           
024500     SKIP2                                                                
024600 01  WS-KVPB.                                                             
024700     03  KVPB-HELTAL     PIC 9(6).                                        
024800     03  KVPB-DECIMAL    PIC 9.                                           
024900     SKIP2                                                                
025000 01  WS-KVPB-SEP-NUM      PIC 9(6)V9 VALUE ZERO.                          
025100     SKIP2                                                                
025200 01  WS-MEDDELANDE       PIC X(34)    VALUE SPACE.                        
025300     SKIP2                                                                
025400 01  TRANS-SW            PIC X.                                           
025500     88  TRANS-EJ-AKTUELL  VALUE 'N'.                                     
025600     88  TRANS-AKTUELL     VALUE 'N'.                                     
025700     EJECT                                                                
025800 01  FL-SEASON           PIC X   VALUE 'N'.                               
025900     SKIP2                                                                
026000 01  KONSTANTER.                                                          
026100     03  VIP             PIC X       VALUE 'V'.                           
026200     03  C1-PROGNOS      PIC S9(3) COMP-3 VALUE +11.                      
026300     03  C1-DIVERSE      PIC S9(3) COMP-3 VALUE +13.                      
026400     03  C1-SATS         PIC S9(3) COMP-3 VALUE +14.                      
026500     03  C2-PROGNOS      PIC S9(3) COMP-3 VALUE +21.                      
026600     03  C2-DIVERSE      PIC S9(3) COMP-3 VALUE +23.                      
026700     03  C2-SATS         PIC S9(3) COMP-3 VALUE +24.                      
026800     EJECT                                                                
026900***************************                                               
027000******  DATUMAREOR  *******                                               
027100***************************                                               
027200     SKIP2                                                                
027300 01  DAGENS-DATUM-AAVVD.                                                  
027400     03  DAGENS-AAR          PIC 99.                                      
027500     03  DAGENS-VECKA        PIC 99.                                      
027600     03  DAGENS-DAG          PIC 9.                                       
027700     SKIP2                                                                
027800 01  DAGENS-DATUM            PIC S9(5).                                   
027900 01  DAGENS-PERIOD           PIC 9(2).                                    
028000     SKIP2                                                                
028100 01  START-DATUM.                                                         
028200     03  START-AAR           PIC 99.                                      
028300     03  START-PERIOD        PIC 9.                                       
028400     SKIP2                                                                
028500 01  START-TIFINLV-AAR       PIC 99.                                      
028600     SKIP2                                                                
028700 01  AKTUELLT-AAR            PIC 99.                                      
028800     SKIP2                                                                
028900 01  OIREG-DATUM             PIC 9(3).                                    
029000 01  FILLER REDEFINES OIREG-DATUM.                                        
029100     03  OIREG-AAR           PIC 99.                                      
029200     03  OIREG-PERIOD        PIC 9.                                       
029300     EJECT                                                                
029400*01  -COPY W200W001C0                                                     
029500     SKIP2                                                                
029600 01  HALV-REAAR              PIC 99V9.                                    
029700     EJECT                                                                
029800***********************************                                       
029900******   ACKUMULATORER   **********                                       
030000***********************************                                       
030100     SKIP2                                                                
030200*    *** ÅRS-ACKAR ***                                                    
030300 01  AARS-ACK.                                                            
030400   02  FILLER OCCURS 3.                                                   
030500     03  ACK-AAR-C1          PIC S9(9) COMP-3.                            
030600     03  ACK-AAR-DC-REF      PIC S9(9) COMP-3.                            
030700                                                                          
030800*    *** RULLANDE 8 PERIODER ***                                          
030900 01  FILLER.                                                              
031000     03  ACK-RULL-12-C1      PIC S9(9) COMP-3.                            
031100     03  ACK-RULL-12-DC-REF  PIC S9(9) COMP-3.                            
031200                                                                          
031300*    *** PERIODBEHOV ***                                                  
031400 01  FILLER.                                                              
031500     03  ACK-KVPB-SEP        PIC S9(8)V9 COMP-3.                          
031600     03  ACK-KVPB-SATS       PIC S9(8)V9 COMP-3.                          
031700                                                                          
031800*    *** ORDERINGÅNG C1 ***                                               
031900 01  KVOI-C1.                                                             
032000   02  FILLER OCCURS 16.                                                  
032100     03  WS-KVOI-C1          PIC S9(9) COMP-3.                            
032200                                                                          
032300*    *** ORDERINGÅNG C2 ***                                               
032400 01  KVOI-C2.                                                             
032500   02  FILLER OCCURS 16.                                                  
032600     03  WS-KVOI-C2          PIC S9(9) COMP-3.                            
032700                                                                          
032800     SKIP2                                                                
032900 01  FILLER.                                                              
033000     03  KVOI-SEASON-C1      PIC S9(9) COMP-3 OCCURS 8.                   
033100     03  KVOI-SEASON-C2      PIC S9(9) COMP-3 OCCURS 8.                   
033200     SKIP2                                                                
033300 01  FILLER.                                                              
033400     03  WS-KVOI-MED-C1      PIC S9(9) COMP-3.                            
033500     03  WS-KVOI-MED-C2      PIC S9(9) COMP-3.                            
033600     SKIP2                                                                
033700 01  SEASON-INDX.                                                         
033800     03  SEASON-INDX-C1      PIC S9(3) COMP-3 OCCURS 16.                  
033900     03  SEASON-INDX-C2      PIC S9(3) COMP-3 OCCURS 16.                  
034000     SKIP2                                                                
034100 01  FILLER.                                                              
034200     03  SUM-KVOI-C1         PIC S9(9) COMP-3.                            
034300     03  SUM-KVOI-C2         PIC S9(9) COMP-3.                            
034400     03  WS-AVVIK-C1         PIC S9(9) COMP-3.                            
034500     03  WS-AVVIK-C2         PIC S9(9) COMP-3.                            
034600     03  AVVIK-1-C1          PIC S9(9) COMP-3.                            
034700     03  AVVIK-1-C2          PIC S9(9) COMP-3.                            
034800     03  AVVIK-2-C1          PIC S9(9) COMP-3.                            
034900     03  AVVIK-2-C2          PIC S9(9) COMP-3.                            
035000     SKIP2                                                                
035100                                                                          
035200 01  MEDDELANDE.                                                          
035300                                                                          
035400    03  MESSAGE-CODES.                                                    
035500      05  ERR-CORR-HILITE-FLDS  PIC X(3)    VALUE '001'.                  
035600      05  INF-PRESS-PF11        PIC X(3)    VALUE '003'.                  
035700      05  ERR-PF11-AND-NO-DATA  PIC X(3)    VALUE '011'.                  
035800      05  ERR-PART-MISSING      PIC X(3)    VALUE '017'.                  
035900      05  ERR-WRONG-KEY         PIC X(3)    VALUE '401'.                  
036000      05  INF-UPDATE-DONE       PIC X(3)    VALUE '101'.                  
036100      05  ERR-NOT-AUTHORIZED    PIC X(3)    VALUE '405'.                  
036200      05  ERR-REFILL-PART       PIC X(3)    VALUE '434'.                  
036300      05  ERR-PART-EXPIRED      PIC X(3)    VALUE '018'.                  
036400                                                                          
036500   03    ERSATT          PIC X(15)   VALUE 'ARTIKELN ERSATT'.             
036600   03    ORDERING-SAKNAS PIC X(18)   VALUE 'ORDERINGÅNG SAKNAS'.          
036700                                                                          
036800   03    KDERS-OVER-10   PIC X(23)   VALUE                                
036900     'ARTIKELN ERSATT        '.                                           
037000     EJECT                                                                
037100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
037200 01  GENERELLA-SUBPROGRAM.                                                
037300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
037400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
037500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
037600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
037700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
037800     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
037900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
038000     EJECT                                                                
038100*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
038200*01  -COPY WDECAREA                                                       
038300     EJECT                                                                
038400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
038500*01 -COPY WMEDAREA                                                        
038600     SKIP3                                                                
038700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
038800*01 -COPY WDATAREA                                                        
038900     SKIP3                                                                
039000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
039100*01 -COPY WMSGINIT                                                        
039200     SKIP3                                                                
039300     EJECT                                                                
039400*    ---KONSTANTER FÖR ALLA VALIDA DC                                     
039500*01  -COPY WWDCKONS                                                       
039600     SKIP3                                                                
039700     EJECT                                                                
039800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
039900*                                                                         
040000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
040100     SKIP3                                                                
040200*01  MID -COPY W2I13601                                                   
040300     EJECT                                                                
040400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
040500     SKIP3                                                                
040600*01  -COPY WMSGAREA                                                       
040700     EJECT                                                                
040800     03  MOD REDEFINES MSG-AREA.                                          
040900*      05  -COPY W2O13601                                                 
041000     EJECT                                                                
041100*    --- PARAMETRAR TILL SUBPROGRAM W222PBTO                              
041200*                                                                         
041300 01  FILLER                      PIC X(16)   VALUE 'W222PBTO'.            
041400     SKIP3                                                                
041500*01 -COPY W222PBTO                                                        
041600     SKIP3                                                                
041700     EJECT                                                                
041800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
041900     SKIP3                                                                
042000*01  -COPY WMFSAREA                                                       
042100     EJECT                                                                
042200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042300*                                                                         
042400     EJECT                                                                
042500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
042600     SKIP3                                                                
042700 01  NYCKLAR-TILL-DLI.                                                    
042800     03  W-IDARTNR-X.                                                     
042900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
043000     03  W-KDSGEKEY-X.                                                    
043100         05  W-KDSGEKEY          PIC X(1)    VALUE SPACE.                 
043200     03  W-KDCLAGER-X.                                                    
043300         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
043400     03  W-IDSKYLT-X.                                                     
043500         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
043600     03  W-2202KEY-X.                                                     
043700         05  W-2202              PIC X(4)    VALUE '2202'.                
043800         05  W-NYCKEL-VALFRI     PIC X(26)   VALUE LOW-VALUE.             
043900     03  W-2227KEY-X.                                                     
044000         05  W-2227              PIC X(4)    VALUE '2227'.                
044100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
044200     03  W-TIAAAA-X.                                                      
044300         05  W-TIAAAA        PIC 9(4)   VALUE ZERO.                       
044400                                                                          
044500     03  W-IDDC-REF-X.                                                    
044600         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
044700                                                                          
044800     SKIP2                                                                
044900*    --- STATUS-KOD FRÅN IMS                                              
045000 01  STATUS-WS                   PIC XX.                                  
045100     88  SEGMENT-FINNS                       VALUE '  '.                  
045200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
045300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
045400     SKIP2                                                                
045500 01  GODK-STATUSKODER.                                                    
045600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
045700     SKIP3                                                                
045800 01  SSA1                        PIC X(64).                               
045900 01  SSA2                        PIC X(64).                               
046000 01  SSA3                        PIC X(64).                               
046100 01  SSA4                        PIC X(64).                               
046200     EJECT                                                                
046300*    --- IMS FUNKTIONSKODER                                               
046400*01  -COPY W0003                                                          
046500     EJECT                                                                
046600*    ---  DLI INPUT-OUTPUT AREA                                           
046700     EJECT                                                                
046800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
046900     SKIP2                                                                
047000 01  DLI-IO-AREA-WDK601.                                                  
047100*    03  -COPY WDK601                                                     
047200     EJECT                                                                
047300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
047400     SKIP2                                                                
047500 01  DLI-IO-AREA-WDK611.                                                  
047600*    03  -COPY WDK611                                                     
047700     EJECT                                                                
047800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK629'.         
047900     SKIP2                                                                
048000 01  DLI-IO-AREA-WDK629.                                                  
048100*    03  -COPY WDK629                                                     
048200     EJECT                                                                
048300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS01'.         
048400     SKIP2                                                                
048500 01  DLI-IO-AREA-ARTS01.                                                  
048600*    03  -COPY WDK701                                                     
048700     EJECT                                                                
048800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS11'.         
048900     SKIP2                                                                
049000 01  DLI-IO-AREA-ARTS11.                                                  
049100*    03  -COPY WDK711                                                     
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD311'.         
049400     SKIP2                                                                
049500 01  DLI-IO-AREA-WDD311.                                                  
049600*    03  -COPY WDD311                                                     
049700     EJECT                                                                
049800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-OIGB01'.         
049900     SKIP2                                                                
050000 01  DLI-IO-AREA-OIGB01.                                                  
050100*    03  -COPY WDL801                                                     
050200     EJECT                                                                
050300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-OIGB11'.         
050400     SKIP2                                                                
050500 01  DLI-IO-AREA-OIGB11.                                                  
050600*    03  -COPY WDL811                                                     
050700     EJECT                                                                
050800 LINKAGE SECTION.                                                         
050900     SKIP2                                                                
051000*01  -COPY W0009     -PRE MSG-                                            
051100     EJECT                                                                
051200*01  -COPY W0008     -PRE USEA-.                                          
051300         05  FILLER                PIC X.                                 
051400     EJECT                                                                
051500*01  -COPY W0008     -PRE WDK6-.                                          
051600         05  FILLER                PIC X.                                 
051700     EJECT                                                                
051800*01  -COPY W0008     -PRE WDK7-.                                          
051900         05  FILLER                PIC X.                                 
052000     EJECT                                                                
052100*01  -COPY W0008     -PRE WDD3-                                           
052200         05  FILLER                PIC X.                                 
052300     EJECT                                                                
052400*01  -COPY W0008     -PRE OIGB-                                           
052500     05  FILLER                    PIC X.                                 
052600     EJECT                                                                
052700 01  PBTO-WDK6-PCB                 PIC X.                                 
052800 01  PBTO-WDK7-PCB                 PIC X.                                 
052900 01  PBTO-WDK9-PCB                 PIC X.                                 
053000 01  PBTO-2501-PCB                 PIC X.                                 
053100 01  PBTO-WDB6R-PCB                PIC X.                                 
053200 01  PBTO-WDK7R-PCB                PIC X.                                 
053300 01  PBTO-WDB6-PCB                 PIC X.                                 
053400 01  PBTO-WDD7-PCB                 PIC X.                                 
053500 01  PBTO-WDK7E-PCB                PIC X.                                 
053600 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
053700 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
053800 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
053900 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
054000 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
054100 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
054200 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
054300 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
054400     EJECT                                                                
054500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
054600                 WDK6-PCB WDK7-PCB WDD3-PCB OIGB-PCB                      
054700                 PBTO-WDK6-PCB  PBTO-WDK7-PCB                             
054800                 PBTO-WDK9-PCB  PBTO-2501-PCB                             
054900                 PBTO-WDB6R-PCB PBTO-WDK7R-PCB                            
055000                 PBTO-WDB6-PCB  PBTO-WDD7-PCB PBTO-WDK7E-PCB              
055100                 PBTO-W222-UTIL-WDK6-PCB                                  
055200                 PBTO-W222-UTIL-WDK7-PCB                                  
055300                 PBTO-W222-UTIL-WDB6-PCB                                  
055400                 PBTO-W222-UTUP-WDK7-PCB                                  
055500                 PBTO-W222-UTUP-WDB6-PCB                                  
055600                 PBTO-W222-UTUP-UTIL-WDK6-PCB                             
055700                 PBTO-W222-UTUP-UTIL-WDK7-PCB                             
055800                 PBTO-W222-UTUP-UTIL-WDB6-PCB                             
055900                 .                                                        
056000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
056100                 WDK6-PCB WDK7-PCB WDD3-PCB OIGB-PCB                      
056200                 PBTO-WDK6-PCB  PBTO-WDK7-PCB                             
056300                 PBTO-WDK9-PCB  PBTO-2501-PCB                             
056400                 PBTO-WDB6R-PCB PBTO-WDK7R-PCB                            
056500                 PBTO-WDB6-PCB  PBTO-WDD7-PCB PBTO-WDK7E-PCB              
056600                 PBTO-W222-UTIL-WDK6-PCB                                  
056700                 PBTO-W222-UTIL-WDK7-PCB                                  
056800                 PBTO-W222-UTIL-WDB6-PCB                                  
056900                 PBTO-W222-UTUP-WDK7-PCB                                  
057000                 PBTO-W222-UTUP-WDB6-PCB                                  
057100                 PBTO-W222-UTUP-UTIL-WDK6-PCB                             
057200                 PBTO-W222-UTUP-UTIL-WDK7-PCB                             
057300                 PBTO-W222-UTUP-UTIL-WDB6-PCB                             
057400                 .                                                        
057500                                                                          
057600     PERFORM IMS-GET-MSG                                                  
057700     IF SEGMENT-FINNS                                                     
057800       PERFORM A-INIT                                                     
057900*      -- ACCESS CHECK PERFORMED IN B-KOLLA-NYCKLAR                       
058000       PERFORM B-KOLLA-NYCKLAR                                            
058100                                                                          
058200       IF NYCKLAR-OK                                                      
058300         IF MFS-UPDATE                                                    
058400           PERFORM G-KOLLA-INPUT                                          
058500           IF INDATA-OK                                                   
058600             PERFORM H-UPPDATERA                                          
058700           END-IF                                                         
058800         ELSE                                                             
058900           IF MFS-FIRST                                                   
059000             PERFORM C-FOERSTA-SIDA                                       
059100           ELSE                                                           
059200             PERFORM E-SAMMA-SIDA                                         
059300           END-IF                                                         
059400         END-IF                                                           
059500         IF INDATA-OK                                                     
059600           PERFORM F-LAES-VISA-INFO                                       
059700         END-IF                                                           
059800       END-IF                                                             
059900*      MOVE WS-TEMFSINF    TO MOD-TEMFSINF                                
060000*      MOVE MID-W2I13601   TO MOD-TEMFSINF                                
060100*      MOVE WS-TEST        TO MOD-TEMFSINF                                
060200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O13601 + 4                      
060300       PERFORM IMS-INSERT-MSG                                             
060400     END-IF                                                               
060500                                                                          
060600     MOVE ZERO TO RETURN-CODE                                             
060700     GOBACK                                                               
060800     .                                                                    
060900     EJECT                                                                
061000                                                                          
061100                                                                          
061200 A-INIT SECTION.                                                          
061300                                                                          
061400     IF MSG-DUBBLA-TRANSKODER                                             
061500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13601                 
061600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
061700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
061800     ELSE                                                                 
061900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13601                  
062000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
062100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
062200     END-IF                                                               
062300                                                                          
062400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
062500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
062600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
062700                                                                          
062800     MOVE LOW-VALUE TO MSG-AREA                                           
062900     MOVE 'W2O13601' TO MFS-IDMOD                                         
063000     MOVE '2136' TO MOD-IDTRANS                                           
063100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
063200                                                                          
063300     MOVE SPACE  TO MED-IDMFSINF                                          
063400     MOVE SPACE  TO MED-IDMFSFEL                                          
063500                                                                          
063600     IF EGEN-MID OR HELP-MID                                              
063700       CONTINUE                                                           
063800     ELSE                                                                 
063900       MOVE SPACE TO MFS-KDTRTYP                                          
064000       MOVE '7' TO MFS-IDPFK                                              
064100     END-IF                                                               
064200                                                                          
064300     MOVE FUNCTION CURRENT-DATE                                           
064400                             TO WS-CURRENT-DATE                           
064500                                                                          
064600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
064700     MOVE WS-DAGENS-DATUM (3:6)                                           
064800                             TO DAT-I-TIDATUM                             
064900                                                                          
065000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
065100                     DAT-O-TIDATUM DAT-KDSVAR                             
065200                                                                          
065300     IF DAT-KDSVAR-OK                                                     
065400****             HÄMTA SEKELSIFFROR                                       
065500                                                                          
065600       MOVE DAT-TIAARP       TO WS-DAGENS-PER                             
065700       MOVE DAT-TIVV         TO WS-DAGENS-VECKA                           
065800       MOVE DAT-TIAA        TO DAGENS-AAR                                 
065900       MOVE DAT-TIVV        TO DAGENS-VECKA                               
066000       MOVE DAT-TID         TO DAGENS-DAG                                 
066100       MOVE DAT-TIRP        TO DAGENS-PERIOD                              
066200       MOVE DAGENS-DATUM-AAVVD                                            
066300                            TO DAGENS-DATUM                               
066400                                                                          
066500     ELSE                                                                 
066600         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
066700         DELIMITED BY SIZE INTO FELTEXT                                   
066800         CALL FELLOG                                                      
066900     END-IF                                                               
067000                                                                          
067100     PERFORM AA-INITERA-PERIODTABELLER                                    
067200                                                                          
067300     MOVE WS-DAGENS-TIAAAA-2 (4:1)                                        
067400                             TO WS-PER-A                                  
067500     MOVE WS-DAGENS-PP       TO WS-PER-PP                                 
067600     MOVE +1                 TO MOD-IX                                    
067700                                                                          
067800     PERFORM UNTIL MOD-IX > +24                                           
067900       MOVE WS-PER           TO MOD-TIPER (MOD-IX)                        
068000       ADD +1                TO MOD-IX                                    
068100                                WS-PER-PP                                 
068200       IF WS-PER-PP > +12                                                 
068300         MOVE +1             TO WS-PER-PP                                 
068400         IF WS-PER-A = 9                                                  
068500           MOVE ZERO         TO WS-PER-A                                  
068600         ELSE                                                             
068700           ADD 1             TO WS-PER-A                                  
068800         END-IF                                                           
068900       END-IF                                                             
069000     END-PERFORM                                                          
069100     .                                                                    
069200     EJECT                                                                
069300                                                                          
069400                                                                          
069500 AA-INITERA-PERIODTABELLER SECTION.                                       
069600*--------------------------------------------------------------*          
069700* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
069800* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
069900* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
070000* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
070100*--------------------------------------------------------------*          
070200                                                                          
070300     COMPUTE WS-DAGENS-TIAAAA-1 = WS-DAGENS-TIAAAA - 1                    
070400                                                                          
070500     COMPUTE WS-DAGENS-TIAAAA-2 = WS-DAGENS-TIAAAA - 2                    
070600                                                                          
070700     PERFORM AAA-INIT-PER-TAB-AR-2                                        
070800     PERFORM AAB-INIT-PER-TAB-AR-1                                        
070900     PERFORM AAC-INIT-PER-TAB-AKTUELLT-AR                                 
071000     .                                                                    
071100     EJECT                                                                
071200                                                                          
071300                                                                          
071400 AAA-INIT-PER-TAB-AR-2 SECTION.                                           
071500*--------------------------------------------------------------*          
071600* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
071700* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
071800* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
071900* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
072000*--------------------------------------------------------------*          
072100                                                                          
072200     MOVE WS-DAGENS-TIAAAA-2 (3:2)                                        
072300                             TO WS-TIAAPP-AA                              
072400     MOVE 1                  TO WS-TIAAPP-PP                              
072500                                                                          
072600     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
072700                                                                          
072800       MOVE 'AARP'           TO DAT-KDDATFORM                             
072900       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
073000                                                                          
073100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
073200                           DAT-O-TIDATUM DAT-KDSVAR                       
073300                                                                          
073400       IF DAT-KDSVAR-OK                                                   
073500*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
073600         IF DAT-TIVV = +52 OR +53                                         
073700            MOVE 1           TO WS-PERTAB-START-VV-2(WS-TIAAPP-PP)        
073800         ELSE                                                             
073900            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-2(WS-TIAAPP-PP)        
074000         END-IF                                                           
074100                                                                          
074200       ELSE                                                               
074300           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-2'                    
074400           DELIMITED BY SIZE INTO FELTEXT                                 
074500           CALL FELLOG                                                    
074600       END-IF                                                             
074700                                                                          
074800       ADD 1                 TO WS-TIAAPP                                 
074900     END-PERFORM                                                          
075000                                                                          
075100     MOVE +1                 TO PER-IX                                    
075200                                                                          
075300     PERFORM UNTIL PER-IX > +11                                           
075400                                                                          
075500       COMPUTE WS-PERTAB-SLUT-VV-2 (PER-IX) =                             
075600               WS-PERTAB-START-VV-2 (PER-IX + 1) - 1                      
075700                                                                          
075800       ADD +1                TO PER-IX                                    
075900     END-PERFORM                                                          
076000                                                                          
076100     MOVE WS-TIAAPP-AA       TO WS-TIAAVV-AA                              
076200     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
076300     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-2 (12)                  
076400     .                                                                    
076500     EJECT                                                                
076600                                                                          
076700                                                                          
076800 AAB-INIT-PER-TAB-AR-1 SECTION.                                           
076900*--------------------------------------------------------------*          
077000* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
077100* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
077200* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
077300* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
077400*--------------------------------------------------------------*          
077500                                                                          
077600     MOVE WS-DAGENS-TIAAAA-1 (3:2)                                        
077700                             TO WS-TIAAPP-AA                              
077800     MOVE 1                  TO WS-TIAAPP-PP                              
077900                                                                          
078000     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
078100                                                                          
078200       MOVE 'AARP'           TO DAT-KDDATFORM                             
078300       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
078400                                                                          
078500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
078600                           DAT-O-TIDATUM DAT-KDSVAR                       
078700                                                                          
078800       IF DAT-KDSVAR-OK                                                   
078900*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
079000         IF DAT-TIVV = +52 OR +53                                         
079100            MOVE 1           TO WS-PERTAB-START-VV-1(WS-TIAAPP-PP)        
079200         ELSE                                                             
079300            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-1(WS-TIAAPP-PP)        
079400         END-IF                                                           
079500                                                                          
079600       ELSE                                                               
079700           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-1'                    
079800           DELIMITED BY SIZE INTO FELTEXT                                 
079900           CALL FELLOG                                                    
080000       END-IF                                                             
080100                                                                          
080200       ADD 1                 TO WS-TIAAPP                                 
080300     END-PERFORM                                                          
080400                                                                          
080500     MOVE +1                 TO PER-IX                                    
080600                                                                          
080700     PERFORM UNTIL PER-IX > +11                                           
080800                                                                          
080900       COMPUTE WS-PERTAB-SLUT-VV-1 (PER-IX) =                             
081000               WS-PERTAB-START-VV-1 (PER-IX + 1) - 1                      
081100                                                                          
081200       ADD +1                TO PER-IX                                    
081300     END-PERFORM                                                          
081400                                                                          
081500     MOVE WS-TIAAPP-AA       TO WS-TIAAVV-AA                              
081600     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
081700     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-1 (12)                  
081800     .                                                                    
081900     EJECT                                                                
082000                                                                          
082100                                                                          
082200 AAC-INIT-PER-TAB-AKTUELLT-AR SECTION.                                    
082300*--------------------------------------------------------------*          
082400* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
082500* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
082600* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
082700* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
082800*--------------------------------------------------------------*          
082900                                                                          
083000     MOVE WS-DAGENS-TIAAAA (3:2)                                          
083100                             TO WS-TIAAPP-AA                              
083200     MOVE 1                  TO WS-TIAAPP-PP                              
083300                                                                          
083400     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
083500                                                                          
083600       MOVE 'AARP'           TO DAT-KDDATFORM                             
083700       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
083800                                                                          
083900       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
084000                           DAT-O-TIDATUM DAT-KDSVAR                       
084100                                                                          
084200       IF DAT-KDSVAR-OK                                                   
084300*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
084400         IF DAT-TIVV = +52 OR +53                                         
084500            MOVE 1           TO WS-PERTAB-START-VV-0(WS-TIAAPP-PP)        
084600         ELSE                                                             
084700            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-0(WS-TIAAPP-PP)        
084800         END-IF                                                           
084900                                                                          
085000       ELSE                                                               
085100           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-0'                    
085200           DELIMITED BY SIZE INTO FELTEXT                                 
085300           CALL FELLOG                                                    
085400       END-IF                                                             
085500                                                                          
085600       ADD 1                 TO WS-TIAAPP                                 
085700     END-PERFORM                                                          
085800                                                                          
085900     MOVE +1                 TO PER-IX                                    
086000                                                                          
086100     PERFORM UNTIL PER-IX > +11                                           
086200                                                                          
086300       COMPUTE WS-PERTAB-SLUT-VV-0 (PER-IX) =                             
086400               WS-PERTAB-START-VV-0 (PER-IX + 1) - 1                      
086500                                                                          
086600       ADD +1                TO PER-IX                                    
086700     END-PERFORM                                                          
086800                                                                          
086900     MOVE WS-TIAAPP-AA       TO WS-TIAAVV-AA                              
087000     PERFORM AB-KOLLA-ANTAL-VECKOR                                        
087100     MOVE WS-SLUT-VV         TO WS-PERTAB-SLUT-VV-0 (12)                  
087200     .                                                                    
087300     EJECT                                                                
087400                                                                          
087500                                                                          
087600 AB-KOLLA-ANTAL-VECKOR SECTION.                                           
087700                                                                          
087800* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
087900                                                                          
088000     MOVE 53        TO WS-TIAAVV-VV                                       
088100     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
088200     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
088300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
088400                         DAT-O-TIDATUM DAT-KDSVAR                         
088500     IF DAT-KDSVAR-OK                                                     
088600       MOVE 53 TO WS-SLUT-VV                                              
088700     ELSE                                                                 
088800       MOVE 52 TO WS-SLUT-VV                                              
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300                                                                          
089400 B-KOLLA-NYCKLAR SECTION.                                                 
089500                                                                          
089600     MOVE JA TO NYCKLAR-SW                                                
089700                                                                          
089800*    -- KONTROLL AV IDARTNR                                               
089900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
090000                                                                          
090100     IF MID-IDARTNR-IN NOT = ALL '+'                                      
090200       MOVE '7'         TO MFS-IDPFK                                      
090300       MOVE SPACE       TO MFS-KDTRTYP                                    
090400     END-IF                                                               
090500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
090600     MOVE '001'             TO MSGI-KDCALL                                
090700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
090800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
090900     MOVE '2136'            TO MSGI-IDTRANS                               
091000     IF MFS-IDTRANS = '2136'                                              
091100     OR (MID-IDARTNR-IN NUMERIC                                           
091200     AND MID-IDARTNR-IN > ZERO)                                           
091300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
091400     END-IF                                                               
091500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
091600                                                                          
091700     IF MSGI-IDLAND-SPR = 'GB'                                            
091800       MOVE +2 TO SPRAK-IX                                                
091900       MOVE 'GB ' TO MED-IDSKYLT                                          
092000                     W-IDSKYLT                                            
092100     ELSE                                                                 
092200       MOVE +1 TO SPRAK-IX                                                
092300       MOVE 'S  ' TO MED-IDSKYLT                                          
092400                     W-IDSKYLT                                            
092500     END-IF                                                               
092600     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
092700     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
092800     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
092900       MOVE WS-IDARTNR TO W-IDARTNR                                       
093000     ELSE                                                                 
093100       MOVE NEJ TO NYCKLAR-SW                                             
093200     END-IF                                                               
093300                                                                          
093400     IF GODK-MID OR NYCKLAR-OK                                            
093500       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
093600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
093700     ELSE                                                                 
093800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
093900     END-IF                                                               
094000                                                                          
094100     IF NYCKLAR-OK                                                        
094200*      --- ACCESS CHECK ---                                               
094300       PERFORM IMS-GET-WDK601                                             
094400       IF SEGMENT-FINNS                                                   
094500          MOVE ART-IDLEVNR TO WS-IDLEVNR-8                                
094600*         --- CHECK IF LIMITATIONS APPLIES FOR THE USER                   
094700          IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                       
094800          OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                 
094900*            --- NO LIMITATIONS ON SUPPLIER CODE                          
095000             CONTINUE                                                     
095100          ELSE                                                            
095200             MOVE NEJ TO NYCKLAR-SW                                       
095300                         INDATA-SW                                        
095400             MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                      
095500             CALL WMEDKONV USING MED-WMEDAREA                             
095600             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
095700             PERFORM MFS-RENSA-FAELT-IN                                   
095800             PERFORM MFS-RENSA-FAELT-UT                                   
095900          END-IF                                                          
096000       ELSE                                                               
096100*         --- THIS CHECK IS DONE ELSEWHERE                                
096200          CONTINUE                                                        
096300       END-IF                                                             
096400     ELSE                                                                 
096500*      --- WRONG KEYS ---                                                 
096600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
096700       CALL WMEDKONV USING MED-WMEDAREA                                   
096800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
096900       PERFORM MFS-RENSA-FAELT-IN                                         
097000       PERFORM MFS-RENSA-FAELT-UT                                         
097100     END-IF                                                               
097200     .                                                                    
097300     EJECT                                                                
097400 C-FOERSTA-SIDA SECTION.                                                  
097500                                                                          
097600     PERFORM MFS-RENSA-FAELT-IN                                           
097700     .                                                                    
097800     EJECT                                                                
097900                                                                          
098000                                                                          
098100 E-SAMMA-SIDA SECTION.                                                    
098200                                                                          
098300     IF MID-INPUT = ALL '+'                                               
098400       PERFORM MFS-RENSA-FAELT-IN                                         
098500     ELSE                                                                 
098600       IF EGEN-MID OR HELP-MID                                            
098700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
098800         CALL WMEDKONV USING MED-WMEDAREA                                 
098900         MOVE MED-TEMFSINF   TO MOD-TEMFSINF                              
099000         PERFORM MFS-LAES-IN-IGEN                                         
099100                                                                          
099200         PERFORM EA-MID-INDATA-TILL-MOD                                   
099300       ELSE                                                               
099400         PERFORM MFS-RENSA-FAELT-IN                                       
099500       END-IF                                                             
099600     END-IF                                                               
099700     .                                                                    
099800 EA-MID-INDATA-TILL-MOD SECTION.                                          
099900* * * * * FÖR VARJE MID-FÄLT                                              
100000* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
100100* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
100200                                                                          
100300     IF MID-KVPB-PLAN = ALL '+'                                           
100400       MOVE MFS-RENSA-FAELT  TO MOD-KVPB-PLAN-IN                          
100500     ELSE                                                                 
100600       MOVE MID-KVPB-PLAN TO MOD-KVPB-PLAN-IN                             
100700       INSPECT MOD-KVPB-PLAN-IN REPLACING LEADING ZERO BY SPACE           
100800     END-IF                                                               
100900                                                                          
101000     IF MID-TIPBPLAN = ALL '+'                                            
101100       MOVE MFS-RENSA-FAELT  TO MOD-TIPBPLAN-IN                           
101200     ELSE                                                                 
101300       MOVE MID-TIPBPLAN     TO MOD-TIPBPLAN-IN                           
101400     END-IF                                                               
101500                                                                          
101600     IF MID-KVPB-PLAN-JUST1 = ALL '+'                                     
101700       MOVE MFS-RENSA-FAELT  TO MOD-KVPB-PLAN-JUST1-IN                    
101800     ELSE                                                                 
101900       MOVE MID-KVPB-PLAN-JUST1 TO MOD-KVPB-PLAN-JUST1-IN                 
102000       INSPECT MOD-KVPB-PLAN-JUST1-IN                                     
102100               REPLACING LEADING ZERO BY SPACE                            
102200     END-IF                                                               
102300                                                                          
102400     IF MID-TIPBPLAN-JUST1-FOM = ALL '+'                                  
102500       MOVE MFS-RENSA-FAELT  TO MOD-TIPBPLAN-JUST1-FOM-IN                 
102600     ELSE                                                                 
102700       MOVE MID-TIPBPLAN-JUST1-FOM TO MOD-TIPBPLAN-JUST1-FOM-IN           
102800     END-IF                                                               
102900                                                                          
103000     IF MID-TIPBPLAN-JUST1-TOM = ALL '+'                                  
103100       MOVE MFS-RENSA-FAELT  TO MOD-TIPBPLAN-JUST1-TOM-IN                 
103200     ELSE                                                                 
103300       MOVE MID-TIPBPLAN-JUST1-TOM TO MOD-TIPBPLAN-JUST1-TOM-IN           
103400     END-IF                                                               
103500                                                                          
103600     IF MID-KVPB-PLAN-JUST2 = ALL '+'                                     
103700       MOVE MFS-RENSA-FAELT  TO MOD-KVPB-PLAN-JUST2-IN                    
103800     ELSE                                                                 
103900       MOVE MID-KVPB-PLAN-JUST2 TO MOD-KVPB-PLAN-JUST2-IN                 
104000       INSPECT MOD-KVPB-PLAN-JUST2-IN                                     
104100               REPLACING LEADING ZERO BY SPACE                            
104200     END-IF                                                               
104300                                                                          
104400     IF MID-TIPBPLAN-JUST2-FOM = ALL '+'                                  
104500       MOVE MFS-RENSA-FAELT  TO MOD-TIPBPLAN-JUST2-FOM-IN                 
104600     ELSE                                                                 
104700       MOVE MID-TIPBPLAN-JUST2-FOM  TO MOD-TIPBPLAN-JUST2-FOM-IN          
104800     END-IF                                                               
104900                                                                          
105000     IF MID-TIPBPLAN-JUST2-TOM = ALL '+'                                  
105100       MOVE MFS-RENSA-FAELT  TO MOD-TIPBPLAN-JUST2-TOM-IN                 
105200     ELSE                                                                 
105300       MOVE MID-TIPBPLAN-JUST2-TOM TO MOD-TIPBPLAN-JUST2-TOM-IN           
105400     END-IF                                                               
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800                                                                          
105900 F-LAES-VISA-INFO SECTION.                                                
106000                                                                          
106100     PERFORM IMS-GET-WDK601                                               
106200     IF SEGMENT-SAKNAS                                                    
106300        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
106400        CALL WMEDKONV USING MED-WMEDAREA                                  
106500        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
106600        PERFORM MFS-RENSA-FAELT-UT                                        
106700        PERFORM MFS-RENSA-FAELT-IN                                        
106800     ELSE                                                                 
106900        PERFORM FC-ARTIKELDATA                                            
107000        PERFORM FD-ORDERINGONG                                            
107100        PERFORM FE-LAES-BENAMNING-WDD3                                    
107200        IF MED-IDMFSINF = SPACE                                           
107300          MOVE WS-MEDDELANDE TO MOD-TEMFSINF                              
107400        END-IF                                                            
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800                                                                          
107900                                                                          
108000 FC-ARTIKELDATA   SECTION.                                                
108100                                                                          
108200     MOVE  NEJ  TO SW-SEASON                                              
108300     MOVE  NEJ  TO SW-ERSATT                                              
108400     MOVE ART-TIFINLV TO SPARAD-TIFINLV                                   
108500     MOVE ART-KDERS-UTG TO SPARAD-KDERS                                   
108600     IF ART-KDERS-UTG > 0                                                 
108700       MOVE ERR-PART-EXPIRED  TO MED-IDMFSFEL                             
108800       CALL WMEDKONV USING MED-WMEDAREA                                   
108900       MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                             
109000     ELSE                                                                 
109100       PERFORM IMS-GET-WDK611                                             
109200       IF SEGMENT-FINNS                                                   
109300         MOVE CLAG-KVPB-SEP  TO MOD-KVPB-CDC                              
109400                                                                          
109500         IF CLAG-DAPBPLAN > WS-DAGENS-DATUM                               
109600         OR CLAG-DAPBPLAN = WS-DAGENS-DATUM                               
109700           MOVE CLAG-DAPBPLAN (3:6)                                       
109800                             TO MOD-TIPBPLAN-UT                           
109900           MOVE CLAG-KVPB-PLAN                                            
110000                             TO WS-RED-KVPB-PLAN                          
110100           MOVE WS-RED-KVPB-PLAN                                          
110200                             TO MOD-KVPB-PLAN-UT                          
110300           INSPECT MOD-KVPB-PLAN-UT                                       
110400                             REPLACING LEADING ZERO BY SPACE              
110500         ELSE                                                             
110600           MOVE MFS-RENSA-FAELT                                           
110700                             TO MOD-KVPB-PLAN-UT                          
110800                                MOD-TIPBPLAN-UT                           
110900         END-IF                                                           
111000                                                                          
111100         MOVE CLAG-KVPB-PLAN-JUST1    TO WS-RED-KVPB-PLAN                 
111200         MOVE WS-RED-KVPB-PLAN        TO MOD-KVPB-PLAN-JUST1-UT           
111300                                                                          
111400         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
111500         MOVE CLAG-TIPBPLAN-JUST1-FOM TO DAT-I-TIDATUM                    
111600         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
111700                             DAT-O-TIDATUM DAT-KDSVAR                     
111800         IF DAT-KDSVAR-OK                                                 
111900            MOVE DAT-TIAAVV-GRP       TO MOD-TIPBPLAN-JUST1-FOM-UT        
112000         ELSE                                                             
112100            MOVE SPACE                TO MOD-TIPBPLAN-JUST1-FOM-UT        
112200         END-IF                                                           
112300                                                                          
112400         MOVE CLAG-TIPBPLAN-JUST1-TOM TO MOD-TIPBPLAN-JUST1-TOM-UT        
112500         INSPECT MOD-TIPBPLAN-JUST1-TOM-UT                                
112600                 REPLACING LEADING ZERO BY SPACE                          
112700         MOVE CLAG-KVPB-PLAN-JUST2    TO WS-RED-KVPB-PLAN                 
112800         MOVE WS-RED-KVPB-PLAN        TO MOD-KVPB-PLAN-JUST2-UT           
112900                                                                          
113000         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
113100         MOVE CLAG-TIPBPLAN-JUST2-FOM TO DAT-I-TIDATUM                    
113200         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
113300                             DAT-O-TIDATUM DAT-KDSVAR                     
113400         IF DAT-KDSVAR-OK                                                 
113500            MOVE DAT-TIAAVV-GRP       TO MOD-TIPBPLAN-JUST2-FOM-UT        
113600         ELSE                                                             
113700            MOVE SPACE                TO MOD-TIPBPLAN-JUST2-FOM-UT        
113800         END-IF                                                           
113900                                                                          
114000         MOVE CLAG-TIPBPLAN-JUST2-TOM TO MOD-TIPBPLAN-JUST2-TOM-UT        
114100         INSPECT MOD-TIPBPLAN-JUST2-TOM-UT                                
114200                 REPLACING LEADING ZERO BY SPACE                          
114300                                                                          
114400         IF CLAG-IDDC-REF NOT = SPACE                                     
114500            PERFORM IMS-GET-WDK629                                        
114600                                                                          
114700            IF SEGMENT-FINNS                                              
114800               MOVE CREF-KVPB-PLAN  TO MOD-KVPB-MASK                      
114900            ELSE                                                          
115000               MOVE MFS-RENSA-FAELT TO MOD-KVPB-MASK                      
115100            END-IF                                                        
115200                                                                          
115300            IF NOT MFS-UPDATE                                             
115400               MOVE ERR-REFILL-PART TO MED-IDMFSFEL                       
115500               CALL WMEDKONV USING MED-WMEDAREA                           
115600               MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                       
115700               PERFORM MFS-LOCK-IN-REFILL-PARTS                           
115800            END-IF                                                        
115900         ELSE                                                             
116000            MOVE W-IDARTNR          TO PBTO-IDARTNR                       
116100            CALL W222PBTO USING  PBTO-W222PBTO                            
116200                                 PBTO-WDK6-PCB                            
116300                                 PBTO-WDK7-PCB                            
116400                                 PBTO-WDK9-PCB                            
116500                                 PBTO-2501-PCB                            
116600                                 PBTO-WDB6R-PCB                           
116700                                 PBTO-WDK7R-PCB                           
116800                                 PBTO-WDB6-PCB                            
116900                                 PBTO-WDD7-PCB                            
117000                                 PBTO-WDK7E-PCB                           
117100                                 PBTO-W222-UTIL-WDK6-PCB                  
117200                                 PBTO-W222-UTIL-WDK7-PCB                  
117300                                 PBTO-W222-UTIL-WDB6-PCB                  
117400                                 PBTO-W222-UTUP-WDK7-PCB                  
117500                                 PBTO-W222-UTUP-WDB6-PCB                  
117600                                 PBTO-W222-UTUP-UTIL-WDK6-PCB             
117700                                 PBTO-W222-UTUP-UTIL-WDK7-PCB             
117800                                 PBTO-W222-UTUP-UTIL-WDB6-PCB             
117900                                                                          
118000            IF PBTO-KDSVAR = JA                                           
118100              MOVE PBTO-KVPB-PLAN   TO MOD-KVPB-MASK                      
118200            ELSE                                                          
118300              MOVE MFS-RENSA-FAELT  TO MOD-KVPB-MASK                      
118400            END-IF                                                        
118500         END-IF                                                           
118600                                                                          
118700         PERFORM IMS-GET-SART                                             
118800         IF SEGMENT-FINNS                                                 
118900           MOVE ZERO TO W-KVPB-REF                                        
119000           MOVE WC-CDC-SE TO W-IDDC-REF                                   
119100           PERFORM IMS-GET-SLAG-REF                                       
119200           PERFORM UNTIL SEGMENT-SAKNAS                                   
119300              COMPUTE W-KVPB-REF = W-KVPB-REF    +                        
119400                                   SLAG-KVPB-REF +                        
119500                                   SLAG-KVPBREOI                          
119600              ADD SLAG-KVPBREOI TO W-KVPBREOI                             
119700              PERFORM IMS-GET-SLAG-REF                                    
119800           END-PERFORM                                                    
119900           MOVE W-KVPB-REF   TO MOD-KVPB-DC                               
120000         ELSE                                                             
120100           MOVE MFS-RENSA-FAELT                                           
120200                             TO MOD-KVPB-DC                               
120300         END-IF                                                           
120400                                                                          
120500         MOVE CLAG-KDERS TO SPARAD-KDERS                                  
120600         IF CLAG-KDERS > 0                                                
120700           MOVE ERSATT TO WS-MEDDELANDE                                   
120800           MOVE JA     TO SW-ERSATT                                       
120900         END-IF                                                           
121000       END-IF                                                             
121100     END-IF                                                               
121200     .                                                                    
121300     EJECT                                                                
121400                                                                          
121500                                                                          
121600 FD-ORDERINGONG     SECTION.                                              
121700                                                                          
121800     MOVE +1                 TO MOD-IX                                    
121900     MOVE ZERO               TO WS-KVOI-CDC-SUM                           
122000                                WS-KVOI-DC-REF-SUM                        
122100                                WS-KVOI-DC-KUND-SUM                       
122200                                WS-KVOI-CDC-RULL-12                       
122300                                WS-KVOI-DC-REF-RULL-12                    
122400                                WS-KVOI-DC-KUND-RULL-12                   
122500                                                                          
122600************* START ÅR ************                                       
122700     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 2                              
122800     MOVE W-TIAAAA (3:2)     TO MOD-AAR (1)                               
122900     PERFORM IMS-GET-WDL811                                               
123000                                                                          
123100     IF SEGMENT-FINNS                                                     
123200                                                                          
123300       MOVE +1               TO PER-IX                                    
123400       MOVE WS-PERTAB-START-VV-2 (PER-IX)                                 
123500                             TO VECKA-IX                                  
123600                                                                          
123700       PERFORM UNTIL VECKA-IX NOT <                                       
123800                     WS-PERTAB-START-VV-2 (DAGENS-PERIOD)                 
123900                                                                          
124000         ADD AAR-KVOI-PROG (VECKA-IX)                                     
124100                           TO WS-KVOI-CDC-SUM                             
124200         ADD AAR-KVOI-REFILL (VECKA-IX)                                   
124300                           TO WS-KVOI-DC-REF-SUM                          
124400         ADD AAR-KVOI-LEDTID (VECKA-IX)                                   
124500                           TO WS-KVOI-DC-KUND-SUM                         
124600         ADD +1              TO VECKA-IX                                  
124700       END-PERFORM                                                        
124800                                                                          
124900       MOVE DAGENS-PERIOD    TO PER-IX                                    
125000       MOVE WS-PERTAB-START-VV-2 (PER-IX)                                 
125100                             TO VECKA-IX                                  
125200                                                                          
125300       PERFORM UNTIL VECKA-IX > +52                                       
125400                                                                          
125500         MOVE ZERO           TO WS-KVOI-PROG                              
125600                                WS-KVOI-REFILL                            
125700                                WS-KVOI-LEDTID                            
125800         PERFORM UNTIL VECKA-IX >                                         
125900                       WS-PERTAB-SLUT-VV-2 (PER-IX)                       
126000                                                                          
126100           ADD AAR-KVOI-PROG (VECKA-IX)                                   
126200                             TO WS-KVOI-PROG                              
126300                                WS-KVOI-CDC-SUM                           
126400           ADD AAR-KVOI-REFILL (VECKA-IX)                                 
126500                             TO WS-KVOI-REFILL                            
126600                                WS-KVOI-DC-REF-SUM                        
126700           ADD AAR-KVOI-LEDTID (VECKA-IX)                                 
126800                             TO WS-KVOI-LEDTID                            
126900                                WS-KVOI-DC-KUND-SUM                       
127000           ADD +1            TO VECKA-IX                                  
127100         END-PERFORM                                                      
127200                                                                          
127300         MOVE WS-KVOI-PROG   TO MOD-KVOI-CDC     (MOD-IX)                 
127400         MOVE WS-KVOI-REFILL TO MOD-KVOI-DC-REF  (MOD-IX)                 
127500         MOVE WS-KVOI-LEDTID TO MOD-KVOI-DC-KUND (MOD-IX)                 
127600         ADD +1              TO MOD-IX                                    
127700                                PER-IX                                    
127800                                                                          
127900       END-PERFORM                                                        
128000       MOVE WS-KVOI-CDC-SUM  TO MOD-AAR-CDC (1)                           
128100       MOVE WS-KVOI-DC-REF-SUM                                            
128200                             TO MOD-AAR-DC-REF (1)                        
128300       MOVE WS-KVOI-DC-KUND-SUM                                           
128400                             TO MOD-AAR-DC-KUND (1)                       
128500     ELSE                                                                 
128600        ADD +12              TO MOD-IX                                    
128700        SUBTRACT DAGENS-PERIOD                                            
128800                             FROM MOD-IX                                  
128900        ADD +1               TO MOD-IX                                    
129000     END-IF                                                               
129100************* FÖREGÅENDE ÅR ************                                  
129200     MOVE ZERO               TO WS-KVOI-CDC-SUM                           
129300                                WS-KVOI-DC-REF-SUM                        
129400                                WS-KVOI-DC-KUND-SUM                       
129500     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 1                              
129600     MOVE W-TIAAAA (3:2)     TO MOD-AAR (2)                               
129700     PERFORM IMS-GET-WDL811                                               
129800                                                                          
129900     IF SEGMENT-FINNS                                                     
130000                                                                          
130100       MOVE +1               TO PER-IX                                    
130200       MOVE WS-PERTAB-START-VV-1 (PER-IX)                                 
130300                             TO VECKA-IX                                  
130400                                                                          
130500       PERFORM UNTIL VECKA-IX > +52                                       
130600                                                                          
130700         MOVE ZERO           TO WS-KVOI-PROG                              
130800                                WS-KVOI-REFILL                            
130900                                WS-KVOI-LEDTID                            
131000         PERFORM UNTIL VECKA-IX >                                         
131100                       WS-PERTAB-SLUT-VV-1 (PER-IX)                       
131200                                                                          
131300           ADD AAR-KVOI-PROG (VECKA-IX)                                   
131400                             TO WS-KVOI-PROG                              
131500                                WS-KVOI-CDC-SUM                           
131600           ADD AAR-KVOI-REFILL (VECKA-IX)                                 
131700                             TO WS-KVOI-REFILL                            
131800                                WS-KVOI-DC-REF-SUM                        
131900           ADD AAR-KVOI-LEDTID (VECKA-IX)                                 
132000                             TO WS-KVOI-LEDTID                            
132100                                WS-KVOI-DC-KUND-SUM                       
132200           ADD +1            TO VECKA-IX                                  
132300         END-PERFORM                                                      
132400                                                                          
132500         MOVE WS-KVOI-PROG   TO MOD-KVOI-CDC     (MOD-IX)                 
132600         MOVE WS-KVOI-REFILL TO MOD-KVOI-DC-REF  (MOD-IX)                 
132700         MOVE WS-KVOI-LEDTID TO MOD-KVOI-DC-KUND (MOD-IX)                 
132800                                                                          
132900         IF MOD-IX > +12                                                  
133000           ADD WS-KVOI-PROG  TO WS-KVOI-CDC-RULL-12                       
133100           ADD WS-KVOI-REFILL                                             
133200                             TO WS-KVOI-DC-REF-RULL-12                    
133300           ADD WS-KVOI-LEDTID                                             
133400                             TO WS-KVOI-DC-KUND-RULL-12                   
133500         END-IF                                                           
133600         ADD +1              TO MOD-IX                                    
133700                                PER-IX                                    
133800                                                                          
133900       END-PERFORM                                                        
134000       MOVE WS-KVOI-CDC-SUM  TO MOD-AAR-CDC (2)                           
134100       MOVE WS-KVOI-DC-REF-SUM                                            
134200                             TO MOD-AAR-DC-REF (2)                        
134300       MOVE WS-KVOI-DC-KUND-SUM                                           
134400                             TO MOD-AAR-DC-KUND (2)                       
134500     ELSE                                                                 
134600       ADD +12               TO MOD-IX                                    
134700     END-IF                                                               
134800************* I ÅR ************                                           
134900     MOVE ZERO               TO WS-KVOI-CDC-SUM                           
135000                                WS-KVOI-DC-REF-SUM                        
135100                                WS-KVOI-DC-KUND-SUM                       
135200     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA                                  
135300     MOVE W-TIAAAA (3:2)     TO MOD-AAR (3)                               
135400     PERFORM IMS-GET-WDL811                                               
135500                                                                          
135600     IF SEGMENT-FINNS                                                     
135700                                                                          
135800       MOVE +1               TO PER-IX                                    
135900       MOVE WS-PERTAB-START-VV-0 (PER-IX)                                 
136000                             TO VECKA-IX                                  
136100                                                                          
136200       IF DAGENS-PERIOD > 1                                               
136300                                                                          
136400         PERFORM UNTIL VECKA-IX >                                         
136500                         WS-PERTAB-SLUT-VV-0 (DAGENS-PERIOD - 1)          
136600                                                                          
136700           MOVE ZERO         TO WS-KVOI-PROG                              
136800                                WS-KVOI-REFILL                            
136900                                WS-KVOI-LEDTID                            
137000           PERFORM UNTIL VECKA-IX >                                       
137100                         WS-PERTAB-SLUT-VV-0 (PER-IX)                     
137200                                                                          
137300             ADD AAR-KVOI-PROG (VECKA-IX)                                 
137400                             TO WS-KVOI-PROG                              
137500                                WS-KVOI-CDC-SUM                           
137600             ADD AAR-KVOI-REFILL (VECKA-IX)                               
137700                             TO WS-KVOI-REFILL                            
137800                                WS-KVOI-DC-REF-SUM                        
137900             ADD AAR-KVOI-LEDTID (VECKA-IX)                               
138000                             TO WS-KVOI-LEDTID                            
138100                                WS-KVOI-DC-KUND-SUM                       
138200             ADD +1          TO VECKA-IX                                  
138300           END-PERFORM                                                    
138400                                                                          
138500           MOVE WS-KVOI-PROG TO MOD-KVOI-CDC (MOD-IX)                     
138600           MOVE WS-KVOI-REFILL                                            
138700                             TO MOD-KVOI-DC-REF (MOD-IX)                  
138800           MOVE WS-KVOI-LEDTID                                            
138900                             TO MOD-KVOI-DC-KUND (MOD-IX)                 
139000           ADD WS-KVOI-PROG  TO WS-KVOI-CDC-RULL-12                       
139100           ADD WS-KVOI-REFILL                                             
139200                             TO WS-KVOI-DC-REF-RULL-12                    
139300           ADD WS-KVOI-LEDTID                                             
139400                             TO WS-KVOI-DC-KUND-RULL-12                   
139500           ADD +1            TO MOD-IX                                    
139600                                PER-IX                                    
139700                                                                          
139800         END-PERFORM                                                      
139900       END-IF                                                             
140000                                                                          
140100       MOVE ZERO             TO WS-KVOI-PROG                              
140200                                WS-KVOI-REFILL                            
140300                                WS-KVOI-LEDTID                            
140400       PERFORM UNTIL VECKA-IX >                                           
140500                     WS-PERTAB-SLUT-VV-0 (PER-IX)                         
140600                                                                          
140700         ADD AAR-KVOI-PROG (VECKA-IX)                                     
140800                             TO WS-KVOI-PROG                              
140900                                WS-KVOI-CDC-SUM                           
141000         ADD AAR-KVOI-REFILL (VECKA-IX)                                   
141100                             TO WS-KVOI-REFILL                            
141200                                WS-KVOI-DC-REF-SUM                        
141300         ADD AAR-KVOI-LEDTID (VECKA-IX)                                   
141400                             TO WS-KVOI-LEDTID                            
141500                                WS-KVOI-DC-KUND-SUM                       
141600         ADD +1              TO VECKA-IX                                  
141700       END-PERFORM                                                        
141800                                                                          
141900       MOVE WS-KVOI-PROG     TO MOD-KVOI-INNEV-CDC                        
142000       MOVE WS-KVOI-REFILL   TO MOD-KVOI-INNEV-DC-REF                     
142100       MOVE WS-KVOI-LEDTID   TO MOD-KVOI-INNEV-DC-KUND                    
142200       MOVE WS-KVOI-CDC-SUM  TO MOD-AAR-CDC (3)                           
142300       MOVE WS-KVOI-DC-REF-SUM                                            
142400                             TO MOD-AAR-DC-REF (3)                        
142500       MOVE WS-KVOI-DC-KUND-SUM                                           
142600                             TO MOD-AAR-DC-KUND (3)                       
142700     END-IF                                                               
142800     MOVE WS-KVOI-CDC-RULL-12                                             
142900                             TO MOD-KVOI-RULL-12-CDC                      
143000     MOVE WS-KVOI-DC-REF-RULL-12                                          
143100                             TO MOD-KVOI-RULL-12-DC-REF                   
143200     MOVE WS-KVOI-DC-KUND-RULL-12                                         
143300                             TO MOD-KVOI-RULL-12-DC-KUND                  
143400     COMPUTE WS-KVOI-SNITT-12-CDC ROUNDED =                               
143500             WS-KVOI-CDC-RULL-12 / 12                                     
143600     COMPUTE WS-KVOI-SNITT-12-DC-REF ROUNDED =                            
143700            (WS-KVOI-DC-REF-RULL-12 + W-KVPBREOI) / 12                    
143800     MOVE WS-KVOI-SNITT-12-CDC                                            
143900                             TO MOD-KVOI-SNITT-12-CDC                     
144000     MOVE WS-KVOI-SNITT-12-DC-REF                                         
144100                             TO MOD-KVOI-SNITT-12-DC-REF                  
144200     .                                                                    
144300     EJECT                                                                
144400                                                                          
144500                                                                          
144600 FE-LAES-BENAMNING-WDD3    SECTION.                                       
144700                                                                          
144800     PERFORM IMS-GET-WDD311-BSEQ                                          
144900     MOVE TEXT-BEART         TO MOD-BEART                                 
145000     .                                                                    
145100     EJECT                                                                
145200                                                                          
145300                                                                          
145400 FF-PBTOTAL-BER SECTION.                                                  
145500                                                                          
145600     MOVE W-IDARTNR          TO PBTO-IDARTNR                              
145700     CALL W222PBTO USING  PBTO-W222PBTO                                   
145800                          PBTO-WDK6-PCB                                   
145900                          PBTO-WDK7-PCB                                   
146000                          PBTO-WDK9-PCB                                   
146100                          PBTO-2501-PCB                                   
146200                          PBTO-WDB6R-PCB                                  
146300                          PBTO-WDK7R-PCB                                  
146400                          PBTO-WDB6-PCB                                   
146500                          PBTO-WDD7-PCB                                   
146600                          PBTO-WDK7E-PCB                                  
146700                          PBTO-W222-UTIL-WDK6-PCB                         
146800                          PBTO-W222-UTIL-WDK7-PCB                         
146900                          PBTO-W222-UTIL-WDB6-PCB                         
147000                          PBTO-W222-UTUP-WDK7-PCB                         
147100                          PBTO-W222-UTUP-WDB6-PCB                         
147200                          PBTO-W222-UTUP-UTIL-WDK6-PCB                    
147300                          PBTO-W222-UTUP-UTIL-WDK7-PCB                    
147400                          PBTO-W222-UTUP-UTIL-WDB6-PCB                    
147500                                                                          
147600     IF PBTO-KDSVAR = JA                                                  
147700       MOVE PBTO-KVPB-PLAN   TO MOD-KVPB-MASK                             
147800     ELSE                                                                 
147900       MOVE MFS-RENSA-FAELT  TO MOD-KVPB-MASK                             
148000     END-IF                                                               
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400                                                                          
148500 G-KOLLA-INPUT SECTION.                                                   
148600                                                                          
148700     MOVE JA  TO INDATA-SW                                                
148800     IF MID-INPUT = ALL '+'                                               
148900        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
149000        CALL WMEDKONV USING MED-WMEDAREA                                  
149100        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
149200        PERFORM MFS-RENSA-FAELT-IN                                        
149300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
149400        MOVE NEJ TO INDATA-SW                                             
149500     ELSE                                                                 
149600        PERFORM GA-DATABAS-KONTROLL                                       
149700        IF INDATA-OK                                                      
149800          PERFORM GB-KTRL-INDATA                                          
149900        END-IF                                                            
150000        IF INDATA-FEL                                                     
150100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
150200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
150300        END-IF                                                            
150400     END-IF                                                               
150500     .                                                                    
150600     EJECT                                                                
150700                                                                          
150800                                                                          
150900 GA-DATABAS-KONTROLL SECTION.                                             
151000                                                                          
151100     PERFORM IMS-GET-WDK601                                               
151200     IF SEGMENT-FINNS                                                     
151300       PERFORM IMS-GET-WDK611                                             
151400       IF SEGMENT-FINNS                                                   
151500         MOVE CLAG-KVPB-PLAN-JUST1    TO W-KVPB-PLAN-JUST1                
151600         MOVE CLAG-TIPBPLAN-JUST1-FOM TO W-TIPBPLAN-JUST1-FOM             
151700         MOVE CLAG-TIPBPLAN-JUST1-TOM TO W-TIPBPLAN-JUST1-TOM             
151800         MOVE CLAG-KVPB-PLAN-JUST2    TO W-KVPB-PLAN-JUST2                
151900         MOVE CLAG-TIPBPLAN-JUST2-FOM TO W-TIPBPLAN-JUST2-FOM             
152000         MOVE CLAG-TIPBPLAN-JUST2-TOM TO W-TIPBPLAN-JUST2-TOM             
152100         CONTINUE                                                         
152200       ELSE                                                               
152300          MOVE NEJ TO INDATA-SW                                           
152400          MOVE ERR-PART-MISSING TO MED-IDMFSFEL                           
152500          CALL WMEDKONV USING MED-WMEDAREA                                
152600          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
152700       END-IF                                                             
152800     ELSE                                                                 
152900        MOVE NEJ TO INDATA-SW                                             
153000        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
153100        CALL WMEDKONV USING MED-WMEDAREA                                  
153200        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
153300     END-IF                                                               
153400     .                                                                    
153500     EJECT                                                                
153600                                                                          
153700                                                                          
153800 GB-KTRL-INDATA SECTION.                                                  
153900                                                                          
154000     IF CLAG-KDERS > 10                                                   
154100        MOVE NEJ               TO INDATA-SW                               
154200        MOVE KDERS-OVER-10     TO MOD-TEMFSFEL                            
154300     END-IF                                                               
154400                                                                          
154500* KVPB-PLAN                                                               
154600     MOVE ZERO                 TO WS-KVPB-PLAN                            
154700     IF MID-KVPB-PLAN NOT = ALL '+'                                       
154800       MOVE MID-KVPB-PLAN TO DEC-IDFRIDATA                                
154900                                                                          
155000       MOVE 6                  TO DEC-KVHELTAL                            
155100       MOVE 1                  TO DEC-KVDECIMAL                           
155200       CALL WDECEDIT USING DEC-WDECAREA                                   
155300       IF DEC-KDSVAR-OK                                                   
155400         MOVE DEC-IDEDITDATA   TO WS-RED-KVPB-PLAN                        
155500                                  WS-KVPB-PLAN                            
155600         MOVE WS-RED-KVPB-PLAN TO MOD-KVPB-PLAN-UT                        
155700         INSPECT MOD-KVPB-PLAN-UT REPLACING LEADING ZERO BY SPACE         
155800       ELSE                                                               
155900         MOVE NEJ              TO INDATA-SW                               
156000         MOVE MFS-NUM-FIELD-WRONG                                         
156100                               TO MOD-KVPB-PLAN-ATTR                      
156200       END-IF                                                             
156300     END-IF                                                               
156400                                                                          
156500* DAPBPLAN                                                                
156600     MOVE SPACE                TO WS-DAPBPLAN                             
156700     IF MID-TIPBPLAN NOT = ALL '+'                                        
156800       MOVE MID-TIPBPLAN       TO WS-DAPBPLAN (3:6)                       
156900                                  WS-TEMFSINF1                            
157000       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
157100       MOVE WS-DAPBPLAN (3:6)  TO DAT-I-TIDATUM                           
157200                                                                          
157300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
157400                           DAT-O-TIDATUM DAT-KDSVAR                       
157500                                                                          
157600       IF DAT-KDSVAR-OK                                                   
157700         MOVE MFS-NUM-FIELD-OK TO MOD-TIPBPLAN-ATTR                       
157800***  LÄGG TILL SEKEL                                                      
157900         IF WS-DAPBPLAN (3:2) > 50                                        
158000           MOVE 19             TO WS-DAPBPLAN (1:2)                       
158100         ELSE                                                             
158200           MOVE 20             TO WS-DAPBPLAN (1:2)                       
158300         END-IF                                                           
158400       ELSE                                                               
158500         MOVE NEJ              TO INDATA-SW                               
158600         MOVE MFS-NUM-FIELD-WRONG                                         
158700                               TO MOD-TIPBPLAN-ATTR                       
158800       END-IF                                                             
158900     END-IF                                                               
159000                                                                          
159100* KVPB-PLAN-JUST1                                                         
159200     IF MID-KVPB-PLAN-JUST1 = ZERO                                        
159300       MOVE MID-KVPB-PLAN-JUST1   TO W-KVPB-PLAN-JUST1                    
159400     ELSE                                                                 
159500       IF MID-KVPB-PLAN-JUST1 NOT = ALL '+'                               
159600         MOVE MID-KVPB-PLAN-JUST1 TO DEC-IDFRIDATA                        
159700         MOVE 6                   TO DEC-KVHELTAL                         
159800         MOVE 1                   TO DEC-KVDECIMAL                        
159900         CALL WDECEDIT USING DEC-WDECAREA                                 
160000         IF DEC-KDSVAR-OK                                                 
160100           MOVE DEC-IDEDITDATA    TO W-KVPB-PLAN-JUST1                    
160200           MOVE MFS-NUM-FIELD-OK  TO MOD-KVPB-PLAN-JUST1-IN-ATTR          
160300                                                                          
160400         ELSE                                                             
160500           MOVE MFS-NUM-FIELD-WRONG                                       
160600                                  TO MOD-KVPB-PLAN-JUST1-IN-ATTR          
160700           MOVE NEJ               TO INDATA-SW                            
160800         END-IF                                                           
160900       END-IF                                                             
161000     END-IF                                                               
161100                                                                          
161200* TIPBPLAN-JUST1-FOM AND KVPB-TIPBPLAN-JUST1-TOM                          
161300     IF MID-TIPBPLAN-JUST1-FOM = ZERO                                     
161400       MOVE MID-TIPBPLAN-JUST1-FOM TO W-TIPBPLAN-JUST1-FOM                
161500     ELSE                                                                 
161600       IF MID-TIPBPLAN-JUST1-FOM NOT = ALL '+'                            
161700         IF MID-TIPBPLAN-JUST1-FOM NOT NUMERIC                            
161800           MOVE NEJ              TO INDATA-SW                             
161900           MOVE MFS-NUM-FIELD-WRONG                                       
162000                                 TO MOD-TIPBPLAN-JUST1-FOM-IN-ATTR        
162100         ELSE                                                             
162200           MOVE 'AAVV'             TO DAT-KDDATFORM                       
162300           MOVE MID-TIPBPLAN-JUST1-FOM TO DAT-I-TIDATUM                   
162400           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
162500                               DAT-O-TIDATUM DAT-KDSVAR                   
162600           IF DAT-KDSVAR-OK                                               
162700             MOVE MFS-NUM-FIELD-OK TO                                     
162800                                    MOD-TIPBPLAN-JUST1-FOM-IN-ATTR        
162900             MOVE DAT-TIAAMMDD     TO W-TIPBPLAN-JUST1-FOM                
163000           ELSE                                                           
163100             MOVE NEJ              TO INDATA-SW                           
163200             MOVE MFS-NUM-FIELD-WRONG                                     
163300                                 TO MOD-TIPBPLAN-JUST1-FOM-IN-ATTR        
163400           END-IF                                                         
163500         END-IF                                                           
163600       END-IF                                                             
163700                                                                          
163800       IF MID-TIPBPLAN-JUST2-FOM = ZERO                                   
163900         MOVE MID-TIPBPLAN-JUST2-FOM TO W-TIPBPLAN-JUST2-FOM              
164000       ELSE                                                               
164100         IF MID-TIPBPLAN-JUST1-TOM NOT = ALL '+'                          
164200           IF MID-TIPBPLAN-JUST1-TOM NOT NUMERIC                          
164300             MOVE NEJ              TO INDATA-SW                           
164400             MOVE MFS-NUM-FIELD-WRONG                                     
164500                                 TO MOD-TIPBPLAN-JUST1-TOM-IN-ATTR        
164600           ELSE                                                           
164700             MOVE MID-TIPBPLAN-JUST1-TOM                                  
164800                                     TO DAT-I-TIDATUM                     
164900             MOVE 'AAMMDD'           TO DAT-KDDATFORM                     
165000             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
165100                                 DAT-O-TIDATUM DAT-KDSVAR                 
165200             IF DAT-KDSVAR-OK                                             
165300               MOVE MFS-NUM-FIELD-OK                                      
165400                                 TO MOD-TIPBPLAN-JUST1-TOM-IN-ATTR        
165500               MOVE MID-TIPBPLAN-JUST1-TOM TO W-TIPBPLAN-JUST1-TOM        
165600             ELSE                                                         
165700               MOVE NEJ              TO INDATA-SW                         
165800               MOVE MFS-NUM-FIELD-WRONG                                   
165900                                 TO MOD-TIPBPLAN-JUST1-TOM-IN-ATTR        
166000             END-IF                                                       
166100           END-IF                                                         
166200         END-IF                                                           
166300       END-IF                                                             
166400     END-IF                                                               
166500                                                                          
166600* KVPB-PLAN-JUST2                                                         
166700     IF MID-KVPB-PLAN-JUST2 = ZERO                                        
166800       MOVE MID-KVPB-PLAN-JUST2     TO W-KVPB-PLAN-JUST2                  
166900     ELSE                                                                 
167000       IF MID-KVPB-PLAN-JUST2 NOT = ALL '+'                               
167100         MOVE MID-KVPB-PLAN-JUST2   TO DEC-IDFRIDATA                      
167200         MOVE 6                     TO DEC-KVHELTAL                       
167300         MOVE 1                     TO DEC-KVDECIMAL                      
167400         CALL WDECEDIT USING DEC-WDECAREA                                 
167500         IF DEC-KDSVAR-OK                                                 
167600           MOVE DEC-IDEDITDATA      TO W-KVPB-PLAN-JUST2                  
167700           MOVE MFS-NUM-FIELD-OK    TO MOD-KVPB-PLAN-JUST2-IN-ATTR        
167800         ELSE                                                             
167900           MOVE MFS-NUM-FIELD-WRONG                                       
168000                                    TO MOD-KVPB-PLAN-JUST2-IN-ATTR        
168100           MOVE NEJ                 TO INDATA-SW                          
168200         END-IF                                                           
168300       END-IF                                                             
168400     END-IF                                                               
168500                                                                          
168600* TIPBPLAN-JUST2-FOM AND KVPB-TIPBPLAN-JUST2-TOM                          
168700     IF MID-TIPBPLAN-JUST2-FOM = ZERO                                     
168800        MOVE MID-TIPBPLAN-JUST2-FOM TO W-TIPBPLAN-JUST2-FOM               
168900     ELSE                                                                 
169000       IF MID-TIPBPLAN-JUST2-FOM NOT = ALL '+'                            
169100         MOVE 'AAVV'             TO DAT-KDDATFORM                         
169200         MOVE MID-TIPBPLAN-JUST2-FOM TO DAT-I-TIDATUM                     
169300                                                                          
169400         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
169500                             DAT-O-TIDATUM DAT-KDSVAR                     
169600                                                                          
169700         IF DAT-KDSVAR-OK                                                 
169800           MOVE MFS-NUM-FIELD-OK TO MOD-TIPBPLAN-JUST2-FOM-IN-ATTR        
169900           MOVE DAT-TIAAMMDD     TO W-TIPBPLAN-JUST2-FOM                  
170000         ELSE                                                             
170100           MOVE NEJ              TO INDATA-SW                             
170200           MOVE MFS-NUM-FIELD-WRONG                                       
170300                                 TO MOD-TIPBPLAN-JUST2-FOM-IN-ATTR        
170400         END-IF                                                           
170500       END-IF                                                             
170600                                                                          
170700       IF MID-TIPBPLAN-JUST2-TOM = ZERO                                   
170800          MOVE MID-TIPBPLAN-JUST2-TOM TO W-TIPBPLAN-JUST2-TOM             
170900       ELSE                                                               
171000         IF MID-TIPBPLAN-JUST2-TOM NOT = ALL '+'                          
171100           MOVE MID-TIPBPLAN-JUST2-TOM                                    
171200                                   TO DAT-I-TIDATUM                       
171300           MOVE 'AAMMDD'           TO DAT-KDDATFORM                       
171400           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
171500                               DAT-O-TIDATUM DAT-KDSVAR                   
171600           IF DAT-KDSVAR-OK                                               
171700             MOVE MFS-NUM-FIELD-OK TO                                     
171800                                   MOD-TIPBPLAN-JUST2-TOM-IN-ATTR         
171900             MOVE MID-TIPBPLAN-JUST2-TOM TO W-TIPBPLAN-JUST2-TOM          
172000           ELSE                                                           
172100             MOVE NEJ              TO INDATA-SW                           
172200             MOVE MFS-NUM-FIELD-WRONG TO                                  
172300                                   MOD-TIPBPLAN-JUST2-TOM-IN-ATTR         
172400           END-IF                                                         
172500         END-IF                                                           
172600       END-IF                                                             
172700     END-IF                                                               
172800                                                                          
172900     IF INDATA-OK                                                         
173000       IF W-TIPBPLAN-JUST1-FOM > W-TIPBPLAN-JUST1-TOM                     
173100          IF MID-TIPBPLAN-JUST1-FOM NOT = ALL '+'                         
173200             MOVE MFS-NUM-FIELD-WRONG                                     
173300                        TO MOD-TIPBPLAN-JUST1-FOM-IN-ATTR                 
173400          END-IF                                                          
173500          IF MID-TIPBPLAN-JUST1-TOM NOT = ALL '+'                         
173600             MOVE MFS-NUM-FIELD-WRONG                                     
173700                        TO MOD-TIPBPLAN-JUST1-TOM-IN-ATTR                 
173800          END-IF                                                          
173900          MOVE NEJ      TO INDATA-SW                                      
174000       END-IF                                                             
174100                                                                          
174200       IF W-TIPBPLAN-JUST2-FOM > W-TIPBPLAN-JUST2-TOM                     
174300          IF MID-TIPBPLAN-JUST2-FOM NOT = ALL '+'                         
174400             MOVE MFS-NUM-FIELD-WRONG                                     
174500                        TO MOD-TIPBPLAN-JUST2-FOM-IN-ATTR                 
174600          END-IF                                                          
174700          IF MID-TIPBPLAN-JUST2-TOM NOT = ALL '+'                         
174800             MOVE MFS-NUM-FIELD-WRONG                                     
174900                        TO MOD-TIPBPLAN-JUST2-TOM-IN-ATTR                 
175000          END-IF                                                          
175100          MOVE NEJ      TO INDATA-SW                                      
175200       END-IF                                                             
175300     END-IF                                                               
175400                                                                          
175500     IF INDATA-OK                                                         
175600       IF ((W-TIPBPLAN-JUST1-FOM = ZERO AND                               
175700            W-TIPBPLAN-JUST2-FOM > ZERO)                                  
175800       OR  (W-TIPBPLAN-JUST1-TOM = ZERO AND                               
175900            W-TIPBPLAN-JUST2-TOM > ZERO))                                 
176000       OR  (W-KVPB-PLAN-JUST1 > ZERO AND                                  
176100           (W-TIPBPLAN-JUST1-FOM = ZERO OR                                
176200            W-TIPBPLAN-JUST1-TOM = ZERO))                                 
176300       OR  (W-KVPB-PLAN-JUST2 > ZERO AND                                  
176400           (W-TIPBPLAN-JUST2-FOM = ZERO OR                                
176500            W-TIPBPLAN-JUST2-TOM = ZERO))                                 
176600           IF MID-KVPB-PLAN-JUST1 NOT = ALL '+'                           
176700              MOVE MFS-NUM-FIELD-WRONG                                    
176800                         TO MOD-KVPB-PLAN-JUST1-IN-ATTR                   
176900           END-IF                                                         
177000           IF MID-KVPB-PLAN-JUST2 NOT = ALL '+'                           
177100              MOVE MFS-NUM-FIELD-WRONG                                    
177200                         TO MOD-KVPB-PLAN-JUST2-IN-ATTR                   
177300           END-IF                                                         
177400           IF MID-TIPBPLAN-JUST1-FOM NOT = ALL '+'                        
177500              MOVE MFS-NUM-FIELD-WRONG                                    
177600                         TO MOD-TIPBPLAN-JUST1-FOM-IN-ATTR                
177700           END-IF                                                         
177800           IF MID-TIPBPLAN-JUST1-TOM NOT = ALL '+'                        
177900              MOVE MFS-NUM-FIELD-WRONG                                    
178000                         TO MOD-TIPBPLAN-JUST1-TOM-IN-ATTR                
178100           END-IF                                                         
178200           IF MID-TIPBPLAN-JUST2-FOM NOT = ALL '+'                        
178300              MOVE MFS-NUM-FIELD-WRONG                                    
178400                         TO MOD-TIPBPLAN-JUST2-FOM-IN-ATTR                
178500           END-IF                                                         
178600           IF MID-TIPBPLAN-JUST2-TOM NOT = ALL '+'                        
178700              MOVE MFS-NUM-FIELD-WRONG                                    
178800                         TO MOD-TIPBPLAN-JUST2-TOM-IN-ATTR                
178900           END-IF                                                         
179000           MOVE NEJ      TO INDATA-SW                                     
179100       END-IF                                                             
179200     END-IF                                                               
179300                                                                          
179400     IF INDATA-OK                                                         
179500       IF (W-TIPBPLAN-JUST1-FOM > 0 AND W-TIPBPLAN-JUST2-FOM > 0)         
179600      AND  W-TIPBPLAN-JUST1-FOM >= W-TIPBPLAN-JUST2-FOM                   
179700          IF MID-TIPBPLAN-JUST1-FOM NOT = ALL '+'                         
179800             MOVE MFS-NUM-FIELD-WRONG                                     
179900                        TO MOD-TIPBPLAN-JUST1-FOM-IN-ATTR                 
180000          END-IF                                                          
180100          IF MID-TIPBPLAN-JUST2-FOM NOT = ALL '+'                         
180200             MOVE MFS-NUM-FIELD-WRONG                                     
180300                        TO MOD-TIPBPLAN-JUST2-FOM-IN-ATTR                 
180400          END-IF                                                          
180500          MOVE NEJ      TO INDATA-SW                                      
180600       END-IF                                                             
180700     END-IF                                                               
180800                                                                          
180900     IF INDATA-FEL                                                        
181000       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
181100       CALL WMEDKONV          USING MED-WMEDAREA                          
181200       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
181300     END-IF                                                               
181400     .                                                                    
181500     EJECT                                                                
181600                                                                          
181700                                                                          
181800 H-UPPDATERA SECTION.                                                     
181900                                                                          
182000     PERFORM IMS-GU-WDK611                                                
182100     IF MID-KVPB-PLAN = ALL '+'                                           
182200        MOVE CLAG-KVPB-PLAN  TO WS-KVPB-PLAN                              
182300     END-IF                                                               
182400                                                                          
182500     IF MID-TIPBPLAN = ALL '+'                                            
182600        MOVE CLAG-DAPBPLAN   TO WS-DAPBPLAN                               
182700     END-IF                                                               
182800                                                                          
182900     MOVE CLAG-KVMAD-TOT     TO W-CLAG-KVMAD-TOT                          
183000                                                                          
183100     IF WS-DAPBPLAN > WS-DAGENS-DATUM                                     
183200     OR WS-DAPBPLAN = WS-DAGENS-DATUM                                     
183300                                                                          
183400       IF WS-KVPB-PLAN NOT = CLAG-KVPB-PLAN                               
183500       AND WS-KVPB-PLAN > ZERO                                            
183600         IF WS-KVPB-PLAN < CLAG-KVPB-PLAN                                 
183700           COMPUTE W-CLAG-KVMAD-TOT ROUNDED =                             
183800             CLAG-KVMAD-TOT * (WS-KVPB-PLAN / CLAG-KVPB-PLAN)             
183900         END-IF                                                           
184000       END-IF                                                             
184100                                                                          
184200       MOVE WS-KVPB-PLAN     TO W-CLAG-KVPB-PLAN                          
184300       MOVE WS-DAPBPLAN      TO W-CLAG-DAPBPLAN                           
184400                                                                          
184500     ELSE                                                                 
184600       IF CLAG-IDDC-REF NOT = SPACE                                       
184700         PERFORM IMS-GET-WDK629                                           
184800         IF SEGMENT-FINNS                                                 
184900           IF CREF-KVPB-PLAN > ZERO                                       
185000             IF CREF-KVPB-PLAN < CLAG-KVPB-PLAN                           
185100               COMPUTE W-CLAG-KVMAD-TOT ROUNDED =                         
185200               CLAG-KVMAD-TOT * (CREF-KVPB-PLAN / CLAG-KVPB-PLAN)         
185300             END-IF                                                       
185400           END-IF                                                         
185500           MOVE CREF-KVPB-PLAN  TO W-CLAG-KVPB-PLAN                       
185600           MOVE ZERO            TO W-CLAG-DAPBPLAN                        
185700         ELSE                                                             
185800           MOVE ZERO            TO W-CLAG-KVPB-PLAN                       
185900                                   W-CLAG-DAPBPLAN                        
186000         END-IF                                                           
186100       ELSE                                                               
186200         MOVE W-IDARTNR         TO PBTO-IDARTNR                           
186300         CALL W222PBTO USING PBTO-W222PBTO                                
186400                             PBTO-WDK6-PCB                                
186500                             PBTO-WDK7-PCB                                
186600                             PBTO-WDK9-PCB                                
186700                             PBTO-2501-PCB                                
186800                             PBTO-WDB6R-PCB                               
186900                             PBTO-WDK7R-PCB                               
187000                             PBTO-WDB6-PCB                                
187100                             PBTO-WDD7-PCB                                
187200                             PBTO-WDK7E-PCB                               
187300                             PBTO-W222-UTIL-WDK6-PCB                      
187400                             PBTO-W222-UTIL-WDK7-PCB                      
187500                             PBTO-W222-UTIL-WDB6-PCB                      
187600                             PBTO-W222-UTUP-WDK7-PCB                      
187700                             PBTO-W222-UTUP-WDB6-PCB                      
187800                             PBTO-W222-UTUP-UTIL-WDK6-PCB                 
187900                             PBTO-W222-UTUP-UTIL-WDK7-PCB                 
188000                             PBTO-W222-UTUP-UTIL-WDB6-PCB                 
188100                                                                          
188200         IF PBTO-KDSVAR = JA                                              
188300           IF PBTO-KVPB-PLAN > ZERO                                       
188400           IF PBTO-KVPB-PLAN < CLAG-KVPB-PLAN                             
188500             COMPUTE W-CLAG-KVMAD-TOT ROUNDED =                           
188600               CLAG-KVMAD-TOT * (PBTO-KVPB-PLAN / CLAG-KVPB-PLAN)         
188700           END-IF                                                         
188800           END-IF                                                         
188900           MOVE PBTO-KVPB-PLAN  TO W-CLAG-KVPB-PLAN                       
189000           MOVE ZERO            TO W-CLAG-DAPBPLAN                        
189100         ELSE                                                             
189200           MOVE ZERO            TO W-CLAG-KVPB-PLAN                       
189300                                   W-CLAG-DAPBPLAN                        
189400         END-IF                                                           
189500       END-IF                                                             
189600     END-IF                                                               
189700                                                                          
189800     PERFORM IMS-GHU-WDK611                                               
189900                                                                          
190000     IF MID-KVPB-PLAN-JUST1 NOT = ALL '+'                                 
190100        MOVE W-KVPB-PLAN-JUST1     TO CLAG-KVPB-PLAN-JUST1                
190200     END-IF                                                               
190300     IF MID-TIPBPLAN-JUST1-FOM NOT = ALL '+'                              
190400       MOVE W-TIPBPLAN-JUST1-FOM   TO CLAG-TIPBPLAN-JUST1-FOM             
190500     END-IF                                                               
190600     IF MID-TIPBPLAN-JUST1-TOM NOT = ALL '+'                              
190700       MOVE MID-TIPBPLAN-JUST1-TOM TO CLAG-TIPBPLAN-JUST1-TOM             
190800     END-IF                                                               
190900                                                                          
191000     IF MID-KVPB-PLAN-JUST2 NOT = ALL '+'                                 
191100       MOVE W-KVPB-PLAN-JUST2      TO CLAG-KVPB-PLAN-JUST2                
191200     END-IF                                                               
191300     IF MID-TIPBPLAN-JUST2-FOM NOT = ALL '+'                              
191400       MOVE W-TIPBPLAN-JUST2-FOM   TO CLAG-TIPBPLAN-JUST2-FOM             
191500     END-IF                                                               
191600     IF MID-TIPBPLAN-JUST2-TOM NOT = ALL '+'                              
191700       MOVE MID-TIPBPLAN-JUST2-TOM TO CLAG-TIPBPLAN-JUST2-TOM             
191800     END-IF                                                               
191900                                                                          
192000     MOVE W-CLAG-KVPB-PLAN      TO CLAG-KVPB-PLAN                         
192100     MOVE W-CLAG-DAPBPLAN       TO CLAG-DAPBPLAN                          
192200     MOVE W-CLAG-KVMAD-TOT      TO CLAG-KVMAD-TOT                         
192300     PERFORM IMS-REPL-WDK6-11                                             
192400                                                                          
192500     IF CLAG-IDDC-REF NOT = SPACE                                         
192600       PERFORM IMS-GHNP-WDK629                                            
192700       IF SEGMENT-FINNS                                                   
192800         IF CREF-FLREFNYO = JA                                            
192900           MOVE NEJ  TO CREF-FLREFNYO                                     
193000           PERFORM IMS-REPL-WDK629                                        
193100         END-IF                                                           
193200       END-IF                                                             
193300     END-IF                                                               
193400                                                                          
193500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
193600     CALL WMEDKONV USING MED-WMEDAREA                                     
193700     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
193800     PERFORM MFS-RENSA-FAELT-IN                                           
193900     .                                                                    
194000     EJECT                                                                
194100                                                                          
194200                                                                          
194300 MFS-RENSA-FAELT-UT SECTION.                                              
194400                                                                          
194500*    --- ALLA UTDATA-FÄLT                                                 
194600     MOVE MFS-RENSA-FAELT         TO MOD-BEART                            
194700                                     MOD-KVPB-MASK                        
194800                                     MOD-KVOI-INNEV-CDC                   
194900                                     MOD-KVOI-INNEV-DC-REF                
195000                                     MOD-KVOI-INNEV-DC-KUND               
195100                                     MOD-KVOI-RULL-12-CDC                 
195200                                     MOD-KVOI-RULL-12-DC-REF              
195300                                     MOD-KVOI-RULL-12-DC-KUND             
195400                                     MOD-KVPB-CDC                         
195500                                     MOD-KVPB-DC                          
195600                                     MOD-KVOI-SNITT-12-CDC                
195700                                     MOD-KVOI-SNITT-12-DC-REF             
195800                                                                          
195900     MOVE +1 TO INDX                                                      
196000     PERFORM UNTIL INDX > 24                                              
196100        MOVE MFS-RENSA-FAELT      TO MOD-TIPER(INDX)                      
196200        ADD +1 TO INDX                                                    
196300     END-PERFORM                                                          
196400                                                                          
196500     MOVE +1 TO INDX                                                      
196600     PERFORM UNTIL INDX > 24                                              
196700        MOVE MFS-RENSA-FAELT      TO MOD-KVOI-CDC(INDX)                   
196800                                     MOD-KVOI-DC-REF(INDX)                
196900                                     MOD-KVOI-DC-KUND(INDX)               
197000        ADD +1 TO INDX                                                    
197100     END-PERFORM                                                          
197200                                                                          
197300     MOVE +1 TO INDX                                                      
197400     PERFORM UNTIL INDX > 3                                               
197500        MOVE MFS-RENSA-FAELT      TO MOD-AAR(INDX)                        
197600                                     MOD-AAR-CDC(INDX)                    
197700                                     MOD-AAR-DC-REF(INDX)                 
197800                                     MOD-AAR-DC-KUND(INDX)                
197900        ADD +1 TO INDX                                                    
198000     END-PERFORM                                                          
198100     .                                                                    
198200     EJECT                                                                
198300                                                                          
198400                                                                          
198500 MFS-RENSA-FAELT-IN SECTION.                                              
198600                                                                          
198700*    --- ALLA INDATA-FÄLT                                                 
198800     MOVE MFS-RENSA-FAELT    TO MOD-KVPB-PLAN-IN                          
198900                                MOD-TIPBPLAN-IN                           
199000                                MOD-KVPB-PLAN-JUST1-IN                    
199100                                MOD-TIPBPLAN-JUST1-FOM-IN                 
199200                                MOD-TIPBPLAN-JUST1-TOM-IN                 
199300                                MOD-KVPB-PLAN-JUST2-IN                    
199400                                MOD-TIPBPLAN-JUST2-FOM-IN                 
199500                                MOD-TIPBPLAN-JUST2-TOM-IN                 
199600     .                                                                    
199700     EJECT                                                                
199800                                                                          
199900                                                                          
200000 MFS-LAES-IN-IGEN SECTION.                                                
200100                                                                          
200200*    --- ALLA INDATA-FÄLT                                                 
200300     MOVE MFS-ADD-LAES-IN-FAELT                                           
200400                             TO MOD-KVPB-PLAN-ATTR                        
200500                                MOD-TIPBPLAN-ATTR                         
200600                                MOD-KVPB-PLAN-JUST1-IN-ATTR               
200700                                MOD-TIPBPLAN-JUST1-FOM-IN-ATTR            
200800                                MOD-TIPBPLAN-JUST1-TOM-IN-ATTR            
200900                                MOD-KVPB-PLAN-JUST2-IN-ATTR               
201000                                MOD-TIPBPLAN-JUST2-FOM-IN-ATTR            
201100                                MOD-TIPBPLAN-JUST2-TOM-IN-ATTR            
201200     .                                                                    
201300     EJECT                                                                
201400                                                                          
201500                                                                          
201600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
201700                                                                          
201800*    --- ALLA INDATA-FÄLT                                                 
201900     MOVE MFS-ROER-EJ-FAELT  TO MOD-KVPB-PLAN-IN                          
202000                                MOD-TIPBPLAN-IN                           
202100                                MOD-KVPB-PLAN-JUST1-IN                    
202200                                MOD-TIPBPLAN-JUST1-FOM-IN                 
202300                                MOD-TIPBPLAN-JUST1-TOM-IN                 
202400                                MOD-KVPB-PLAN-JUST2-IN                    
202500                                MOD-TIPBPLAN-JUST2-FOM-IN                 
202600                                MOD-TIPBPLAN-JUST2-TOM-IN                 
202700     .                                                                    
202800     EJECT                                                                
202900                                                                          
203000                                                                          
203100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
203200                                                                          
203300*    --- ALLA UTDATA-FÄLT                                                 
203400                                                                          
203500     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                                 
203600                                MOD-KVOI-INNEV-CDC                        
203700                                MOD-KVOI-INNEV-DC-REF                     
203800                                MOD-KVOI-INNEV-DC-KUND                    
203900                                MOD-KVOI-RULL-12-CDC                      
204000                                MOD-KVOI-RULL-12-DC-REF                   
204100                                MOD-KVOI-RULL-12-DC-KUND                  
204200                                MOD-KVPB-MASK                             
204300                                MOD-KVPB-PLAN-UT                          
204400                                MOD-TIPBPLAN-UT                           
204500                                MOD-KVPB-PLAN-JUST1-UT                    
204600                                MOD-KVPB-PLAN-JUST1-UT                    
204700                                MOD-TIPBPLAN-JUST1-FOM-UT                 
204800                                MOD-TIPBPLAN-JUST1-TOM-UT                 
204900                                MOD-KVPB-PLAN-JUST2-UT                    
205000                                MOD-TIPBPLAN-JUST2-FOM-UT                 
205100                                MOD-TIPBPLAN-JUST2-TOM-UT                 
205200                                MOD-KVPB-CDC                              
205300                                MOD-KVPB-DC                               
205400                                MOD-KVOI-SNITT-12-CDC                     
205500                                MOD-KVOI-SNITT-12-DC-REF                  
205600                                                                          
205700     MOVE 1                  TO INDX                                      
205800     PERFORM UNTIL INDX > 24                                              
205900       MOVE MFS-ROER-EJ-FAELT                                             
206000                             TO MOD-TIPER(INDX)                           
206100       ADD 1                 TO INDX                                      
206200     END-PERFORM                                                          
206300                                                                          
206400     MOVE 1                  TO INDX                                      
206500     PERFORM UNTIL INDX > 24                                              
206600       MOVE MFS-ROER-EJ-FAELT  TO MOD-KVOI-CDC(INDX)                      
206700                                  MOD-KVOI-DC-REF(INDX)                   
206800                                  MOD-KVOI-DC-KUND(INDX)                  
206900       ADD 1                 TO INDX                                      
207000     END-PERFORM                                                          
207100                                                                          
207200     MOVE 1                  TO INDX                                      
207300     PERFORM UNTIL INDX > 3                                               
207400       MOVE MFS-ROER-EJ-FAELT  TO MOD-AAR(INDX)                           
207500                                  MOD-AAR-CDC(INDX)                       
207600                                  MOD-AAR-DC-REF(INDX)                    
207700                                  MOD-AAR-DC-KUND(INDX)                   
207800       ADD 1                 TO INDX                                      
207900     END-PERFORM                                                          
208000                                                                          
208100     .                                                                    
208200     EJECT                                                                
208300                                                                          
208400                                                                          
208500 MFS-LOCK-IN-REFILL-PARTS  SECTION.                                       
208600                                                                          
208700     MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-KVPB-PLAN-ATTR                     
208800                                   MOD-TIPBPLAN-ATTR                      
208900                                   MOD-KVPB-PLAN-JUST1-IN-ATTR            
209000                                   MOD-TIPBPLAN-JUST1-FOM-IN-ATTR         
209100                                   MOD-TIPBPLAN-JUST1-TOM-IN-ATTR         
209200                                   MOD-KVPB-PLAN-JUST2-IN-ATTR            
209300                                   MOD-TIPBPLAN-JUST2-FOM-IN-ATTR         
209400                                   MOD-TIPBPLAN-JUST2-TOM-IN-ATTR         
209500                                                                          
209600                                                                          
209700     .                                                                    
209800     EJECT                                                                
209900                                                                          
210000                                                                          
210100* --- IMS SEKTIONER ---                                                   
210200                                                                          
210300                                                                          
210400                                                                          
210500 IMS-GET-MSG SECTION.                                                     
210600                                                                          
210700     MOVE '  QC' TO GODK-STATUSKODER                                      
210800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
210900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
211000     PERFORM IMS-STATUSKONTROLL                                           
211100     .                                                                    
211200     EJECT                                                                
211300                                                                          
211400                                                                          
211500 IMS-INSERT-MSG SECTION.                                                  
211600                                                                          
211700     IF MSGI-IDLAND-SPR = 'GB'                                            
211800       MOVE 'N' TO MFS-KDHUVOMR                                           
211900     END-IF                                                               
212000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
212100     MOVE SPACE TO GODK-STATUSKODER                                       
212200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
212300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
212400     PERFORM IMS-STATUSKONTROLL                                           
212500     .                                                                    
212600     EJECT                                                                
212700                                                                          
212800                                                                          
212900 IMS-GET-WDK601 SECTION.                                                  
213000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
213100          DELIMITED BY SIZE INTO SSA1                                     
213200     MOVE '  GE' TO GODK-STATUSKODER                                      
213300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
213400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
213500     PERFORM IMS-STATUSKONTROLL                                           
213600     .                                                                    
213700     EJECT                                                                
213800                                                                          
213900                                                                          
214000 IMS-GET-WDK611 SECTION.                                                  
214100     MOVE 'WDK611     ' TO SSA1                                           
214200     MOVE '  GE' TO GODK-STATUSKODER                                      
214300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
214400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700     SKIP3                                                                
214800                                                                          
214900 IMS-GET-WDK629 SECTION.                                                  
215000     MOVE 'WDK629     ' TO SSA1                                           
215100     MOVE '  GE' TO GODK-STATUSKODER                                      
215200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK629 SSA1              
215300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     SKIP3                                                                
215700                                                                          
215800                                                                          
215900 IMS-GHNP-WDK629 SECTION.                                                 
216000                                                                          
216100     MOVE 'WDK629     ' TO SSA1                                           
216200     MOVE '  GE' TO GODK-STATUSKODER                                      
216300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA-WDK629 SSA1             
216400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
216500     PERFORM IMS-STATUSKONTROLL                                           
216600     .                                                                    
216700     SKIP3                                                                
216800                                                                          
216900                                                                          
217000 IMS-REPL-WDK629 SECTION.                                                 
217100                                                                          
217200     MOVE '  ' TO GODK-STATUSKODER                                        
217300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK629                  
217400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
217500     PERFORM IMS-STATUSKONTROLL                                           
217600     .                                                                    
217700     SKIP3                                                                
217800                                                                          
217900                                                                          
218000 IMS-GU-WDK611 SECTION.                                                   
218100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
218200          DELIMITED BY SIZE INTO SSA1                                     
218300     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
218400     MOVE '  ' TO GODK-STATUSKODER                                        
218500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
218600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
218700     PERFORM IMS-STATUSKONTROLL                                           
218800     .                                                                    
218900                                                                          
219000 IMS-GHU-WDK611 SECTION.                                                  
219100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
219200          DELIMITED BY SIZE INTO SSA1                                     
219300     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
219400     MOVE '  ' TO GODK-STATUSKODER                                        
219500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
219600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
219700     PERFORM IMS-STATUSKONTROLL                                           
219800     .                                                                    
219900     EJECT                                                                
220000                                                                          
220100                                                                          
220200 IMS-GET-SART SECTION.                                                    
220300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
220400          DELIMITED BY SIZE INTO SSA1                                     
220500     MOVE '  GE' TO GODK-STATUSKODER                                      
220600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-ARTS01 SSA1               
220700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
220800     PERFORM IMS-STATUSKONTROLL                                           
220900     .                                                                    
221000     SKIP3                                                                
221100                                                                          
221200                                                                          
221300 IMS-GET-SLAG-REF   SECTION.                                              
221400                                                                          
221500     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
221600          DELIMITED BY SIZE INTO SSA1                                     
221700     MOVE '  GE' TO GODK-STATUSKODER                                      
221800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-ARTS11 SSA1              
221900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
222000     PERFORM IMS-STATUSKONTROLL                                           
222100     .                                                                    
222200     EJECT                                                                
222300                                                                          
222400                                                                          
222500 IMS-REPL-WDK6-11 SECTION.                                                
222600                                                                          
222700     MOVE '  ' TO GODK-STATUSKODER                                        
222800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
222900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
223000     PERFORM IMS-STATUSKONTROLL                                           
223100     .                                                                    
223200     EJECT                                                                
223300                                                                          
223400                                                                          
223500 IMS-GET-WDL811  SECTION.                                                 
223600                                                                          
223700     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
223800          DELIMITED BY SIZE INTO SSA1                                     
223900     STRING 'WLOIGB11(TIAAAA   =' W-TIAAAA-X ')'                          
224000          DELIMITED BY SIZE INTO SSA2                                     
224100     MOVE '  GE' TO GODK-STATUSKODER                                      
224200     CALL CBLTDLI USING GHU OIGB-PCB                                      
224300                                 DLI-IO-AREA-OIGB11 SSA1 SSA2             
224400     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
224500     PERFORM IMS-STATUSKONTROLL                                           
224600     .                                                                    
224700                                                                          
224800     EJECT                                                                
224900                                                                          
225000                                                                          
225100 IMS-GET-WDD311-BSEQ SECTION.                                             
225200     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
225300          DELIMITED BY SIZE INTO SSA1                                     
225400     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
225500          DELIMITED BY SIZE INTO SSA2                                     
225600     MOVE '  GE'              TO GODK-STATUSKODER                         
225700     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD311 SSA1 SSA2          
225800     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
225900     PERFORM IMS-STATUSKONTROLL                                           
226000     .                                                                    
226100     EJECT                                                                
226200                                                                          
226300                                                                          
226400 IMS-STATUSKONTROLL SECTION.                                              
226500                                                                          
226600     SET STATUS-IX TO 1                                                   
226700     SEARCH GODK-STATUS                                                   
226800       AT END                                                             
226900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
227000         DELIMITED BY SIZE INTO FELTEXT                                   
227100         CALL FELLOG                                                      
227200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
227300         CONTINUE                                                         
227400     END-SEARCH                                                           
227500     .                                                                    
227600     EJECT                                                                
227700*    -COPY WY2000P2                                                       
