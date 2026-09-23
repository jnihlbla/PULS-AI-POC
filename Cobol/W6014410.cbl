000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014410.                                                
000400*AUTHOR.         UMESH JAIN.                                              
000500*DATE-WRITTEN.   NOV 2011.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RAPPORTERA PÅ PARTI. INLAGT, AVVIKELSE                           
001100*                                                                         
001200*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
001300*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001400*        PROGRAMMET          LÄSER      WLARTD (WDD8)                     
001500*        PROGRAMMET          LÄSER      WLARTS (WDK7)                     
001600*        PROGRAMMET          LÄSER      W6UPFA (W6L1)                     
001700*        PROGRAMMET          LÄSER      WLARTC (WDK6)                     
001800*        PROGRAMMET          LÄSER      WDB6   (WDB6)                     
001900*    SUB PROGRAMMET W611PMRK UPPDATERAR W6INLA (W6D1)                     
002000*                            LÄSER      W6PLAA (W6G1)                     
002100*    SUB PROGRAMMET W611STYR LÄSER      W6HANA (W6G1)                     
002200*                                       W6PLAA (W6G1)                     
002300*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W6T144                                              
002700*        MID:         W6I14401                                            
002800*        WEB REQU.    W60144I1                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W6O14401                                            
003200*        WEB RESP.    W60144O1                                            
003300                                                                          
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W6014410'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  TABINDX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
005200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  6191-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  MAX-6191-IX                 PIC S9(9)  VALUE +24   COMP SYNC.        
005500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +707  COMP SYNC.        
005600 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005700                                                                          
005800*    --- TABELL FÖR HANTERING AV JÄMFÖRELSE KOLLINR - VOR                 
005900 01  FILLER                      PIC X(16)  VALUE 'VOR-TABELL'.           
006000                                                                          
006100 01  VOR-TABELL.                                                          
006200     03  VOR-TABELL-RAD OCCURS 50.                                        
006300         05  VOR-TAB-IDLEVNR-KOLLI    PIC  X(5) VALUE SPACE.              
006400         05  VOR-TAB-IDOKOLLI         PIC  9(9).                          
006500         05  VOR-TAB-KVINLART         PIC S9(7) COMP-3.                   
006600                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800                                                                          
006900 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
007000 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
007100 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
007200                                                                          
007300 01  WS-IDDC-LOCAL.                                                       
007400     03  FILLER                  PIC X(5)    VALUE 'WIDDC'.               
007500     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
007600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
007700                                                                          
007800 77  WS-IDDC                      PIC X(2)    VALUE SPACE.                
007900 77  W-ADBUFFOMR                 PIC X(4)    VALUE SPACE.                 
008000 77  W-ADLAGOMR                  PIC X(4)    VALUE SPACE.                 
008100 77  W-KVRAPP                    PIC S9(7)   VALUE ZERO COMP-3.           
008200 77  W-KVINLART-UPD              PIC S9(7)   VALUE ZERO COMP-3.           
008300 77  W-KVINLART-TOTALT           PIC S9(7)   VALUE ZERO COMP-3.           
008400 77  W-KVINLART-DIFF             PIC S9(7)   VALUE ZERO COMP-3.           
008500 77  W-KDCMDVAL-RAEKN            PIC S9(7)   VALUE ZERO COMP-3.           
008600 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008700 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008800 77  WS-BELOPP                   PIC S9(13)  VALUE ZERO COMP-3.           
008900 77  WS-PRAVCOST                 PIC S9(7)   VALUE ZERO COMP-3.           
009000 77  W-KVAVIS                    PIC S9(7)   VALUE ZERO COMP-3.           
009100                                                                          
009200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009300     88  INDATA-OK                           VALUE 'J'.                   
009400     88  INDATA-FEL                          VALUE 'N'.                   
009500                                                                          
009600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009700     88  NYCKLAR-OK                          VALUE 'J'.                   
009800     88  NYCKLAR-FEL                         VALUE 'N'.                   
009900                                                                          
010000 77  INLA11-SW                   PIC X       VALUE 'J'.                   
010100     88  INLA11-FINNS                        VALUE 'J'.                   
010200     88  INLA11-SAKNAS                       VALUE 'N'.                   
010300                                                                          
010400 77  KOLLI-NYCKEL-SW             PIC X       VALUE 'J'.                   
010500     88  KOLLI-NYCKEL                        VALUE 'J'.                   
010600                                                                          
010700 77  PARTI-NYCKEL-SW             PIC X       VALUE 'J'.                   
010800     88  PARTI-NYCKEL                        VALUE 'J'.                   
010900                                                                          
011000 77  VOR-SW                      PIC X       VALUE 'J'.                   
011100     88  VOR-TRAEFF                          VALUE 'J'.                   
011200                                                                          
011300 77  RAD-SW                      PIC X       VALUE 'J'.                   
011400     88  RAD-SAKNAS                          VALUE 'N'.                   
011500                                                                          
011600 77  DIVKLI-SW                   PIC X       VALUE 'N'.                   
011700     88  DIVKLI                              VALUE 'J'.                   
011800                                                                          
011900 77  FINN-RAD-SW                 PIC X       VALUE 'J'.                   
012000     88  FOERSTA-EJ-FUNNEN                   VALUE 'N'.                   
012100     88  LAES-NAESTA                         VALUE 'J'.                   
012200                                                                          
012300 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
012400     88  FOERSTA-6191                        VALUE 'J'.                   
012500                                                                          
012600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012700     88  EGEN-MID                            VALUE '6144'.                
012800     88  GODK-MID                            VALUE '6143' '6144'          
012900                                                   '6145'.                
013000     88  HELP-MID                            VALUE '0551'.                
013100     EJECT                                                                
013200*      --- VALID IDDC CODES                                               
013300                                                                          
013400*01  -COPY WWDC99                                                         
013500                                                                          
013600       EJECT                                                              
013700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013800 01  GENERELLA-SUBPROGRAM.                                                
013900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014200     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
014300     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
014400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014500     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
014600     EJECT                                                                
014700*01 -COPY WL01TIDZ                                                        
014800     SKIP3                                                                
014900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015000*01 -COPY WMEDAREA                                                        
015100     SKIP3                                                                
015200 01  MESSAGE-CODES.                                                       
015300     03  INF-MORE-INFO-EXISTS    PIC X(3)  VALUE '011'.                   
015400     03  INF-PRESS-PF11          PIC X(3)  VALUE '013'.                   
015500     03  INFO-DEV-VALUE-TOO-HIGH PIC X(3)  VALUE '359'.                   
015600     03  INF-UPDATE-DONE         PIC X(3)  VALUE '001'.                   
015700     03  INF-FIRST-PAGE          PIC X(3)  VALUE '010'.                   
015800     03  INF-NO-MORE-LINES       PIC X(3)  VALUE '316'.                   
015900     03  ERR-WRONG-KEY           PIC X(3)  VALUE '022'.                   
016000     03  ERR-CORR-HILITE-FLDS    PIC X(3)  VALUE '020'.                   
016100     03  ERR-DIVERSE             PIC X(3)  VALUE '354'.                   
016200     03  ERR-PF11-AND-NO-DATA    PIC X(3)  VALUE '014'.                   
016300     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)  VALUE '007'.                   
016400     03  ERR-MISSING             PIC X(3)  VALUE '027'.                   
016500     03  ERR-INVALID-FIELD       PIC X(3)  VALUE '023'.                   
016600     03  ERR-QUALITY             PIC X(3)  VALUE '357'.                   
016700     03  ERR-CHECK-QUAL-FIRST    PIC X(3)  VALUE '355'.                   
016800     03  ERR-CONTROL-NOT-COMPL   PIC X(3)  VALUE '356'.                   
016900     03  ERR-PLACE-MISSING       PIC X(3)  VALUE '313'.                   
017000     03  ERR-PLACE-MISSING-SVS   PIC X(3)  VALUE '314'.                   
017100     03  ERR-WEIGHT-MISSING      PIC X(3)  VALUE '310'.                   
017200     03  ERR-VOLUME-MISSING      PIC X(3)  VALUE '311'.                   
017300     03  ERR-ORIGIN-MISSING      PIC X(3)  VALUE '312'.                   
017400                                                                          
017500     EJECT                                                                
017600*    --- PARAMETRAR TILL SUBPROGRAM W611PMRK                              
017700*01 -COPY W611PMRK                                                        
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM W611STYR                              
018000*01 -COPY W611STYR                                                        
018100     EJECT                                                                
018200*    --- SPAR AREA  FÖR INLA21                                            
018300*01  -COPY W6D121   -PRE SPAR-                                            
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
018600     SKIP3                                                                
018700 *01  -COPY WMFSAREA                                                      
018800      EJECT                                                               
018900 01  KOM-MSG-IO-AREA.                                                     
019000*03  -COPY WMSGKOM                                                        
019100     EJECT                                                                
019200 01  FILLER             PIC X(16)  VALUE 'MSG/KOM-AREA'.                  
019300     SKIP3                                                                
019400*01  -COPY WMSGSNUF     -PRE P-TO-P-                                      
019500     EJECT                                                                
019600 01      FILLER                  PIC X(24)   VALUE                        
019700                                 'MOD6191-MID-W6I19101'.                  
019800     SKIP2                                                                
019900     -COPY W6I19101 -PRE MOD6191-                                         
020000     EJECT                                                                
020100 01      FILLER                  PIC X(24)   VALUE                        
020200                                 'MOD6193-MID-W6I19301'.                  
020300     SKIP2                                                                
020400     -COPY W6I19301 -PRE MOD6193-                                         
020500     EJECT                                                                
020600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020700*                                                                         
020800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020900     SKIP3                                                                
021000 01  NYCKLAR-TILL-DLI.                                                    
021100     SKIP2                                                                
021200     03  W-IDLOPNRM-X.                                                    
021300         05  W-IDLOPNRM              PIC S9(9) COMP-3 VALUE ZERO.         
021400     03  W-W6D1CSEQ-X.                                                    
021500        05  W-D1CSEQ-IDLEVNR-KOLLI   PIC  X(5) VALUE SPACE.               
021600        05  W-D1CSEQ-IDOKOLLI        PIC  9(9) VALUE ZERO.                
021700                                                                          
021800     03  W-W6D1BSEQ-X.                                                    
021900         05  W-D1BSEQ-IDLOPNRM   PIC S9(9)    COMP-3 VALUE ZERO.          
022000                                                                          
022100     03  W-IDRADNR-X.                                                     
022200         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
022300                                                                          
022400     03  W-IDARTNR-X.                                                     
022500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022600                                                                          
022700     03  W-IDLEVNR-KOLLI-X.                                               
022800        05  W-IDLEVNR-KOLLI          PIC  X(5) VALUE SPACE.               
022900                                                                          
023000     03  W-IDOKOLLI-X.                                                    
023100        05  W-IDOKOLLI               PIC  9(9) VALUE ZERO.                
023200                                                                          
023300     03  W-W6GXKEY-6005-X.                                                
023400         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
023500         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
023600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
023700                                                                          
023800     03  W-W6GXKEY-6006-X.                                                
023900         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
024000         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
024100                                                                          
024200     03  W-IDDC-X.                                                        
024300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024400                                                                          
024500     03  W-IDDC-B6-X.                                                     
024600         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
024700                                                                          
024800     EJECT                                                                
024900*    --- STATUS-KOD FRÅN IMS                                              
025000 01  STATUS-WS                   PIC XX.                                  
025100     88  SEGMENT-FINNS                       VALUE '  '.                  
025200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025400     SKIP2                                                                
025500 01  GODK-STATUSKODER.                                                    
025600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025700     SKIP3                                                                
025800 01  SSA1                        PIC X(96).                               
025900 01  SSA2                        PIC X(64).                               
026000     EJECT                                                                
026100*    --- IMS FUNKTIONSKODER                                               
026200*01  -COPY W0003                                                          
026300     EJECT                                                                
026400*    ---  DLI INPUT-OUTPUT AREA                                           
026500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026600     SKIP3                                                                
026700 01  DLI-IO-AREA1.                                                        
026800     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
026900     SKIP3                                                                
027000     03  W6INLA21 REDEFINES IO-AREA1.                                     
027100*        05  -COPY W6D111                                                 
027200     EJECT                                                                
027300     EJECT                                                                
027400 01  DLI-IO-AREA2.                                                        
027500     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
027600     SKIP3                                                                
027700     03  W6INLA21 REDEFINES IO-AREA2.                                     
027800*        05  -COPY W6D121                                                 
027900     EJECT                                                                
028000 01  DLI-IO-AREA3.                                                        
028100     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
028200     SKIP3                                                                
028300     03  W6PLAA11 REDEFINES IO-AREA3.                                     
028400*        05  -COPY W6GX6006 -PRE PLAA-                                    
028500     EJECT                                                                
028600     03  W6INLC01 REDEFINES IO-AREA3.                                     
028700*        05  -COPY W6D1B1                                                 
028800     EJECT                                                                
028900     03  WLARTD11 REDEFINES IO-AREA3.                                     
029000*        05  -COPY WDD811                                                 
029100     EJECT                                                                
029200 01  FILLER                      PIC X(16)   VALUE                        
029300     'DLI-IO-AREA-WDK7'.                                                  
029400     SKIP3                                                                
029500 01  DLI-IO-AREA-WDK7.                                                    
029600     03  IO-AREA-WDK7            PIC X(300)  VALUE SPACE.                 
029700     SKIP3                                                                
029800     03  WDK711   REDEFINES IO-AREA-WDK7.                                 
029900        05 -COPY WDK711   -PRE WLARTS-                                    
030000     EJECT                                                                
030100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-UPFA'.         
030200     SKIP3                                                                
030300 01  DLI-IO-UPFA.                                                         
030400     03  IO-UPFA                PIC X(100)  VALUE SPACE.                  
030500     03  W6UPFA01 REDEFINES IO-UPFA.                                      
030600*        05  -COPY W6L101                                                 
030700     EJECT                                                                
030800 01  DLI-IO-AREA-UPFA11.                                                  
030900     03  W6UPFA11.                                                        
031000*        05  -COPY W6L111                                                 
031100     SKIP3                                                                
031200 01  DLI-IO-AREA-UPFA12.                                                  
031300     03  W6UPFA12.                                                        
031400*        05  -COPY W6L112                                                 
031500     EJECT                                                                
031600 01  DLI-IO-ARTC.                                                         
031700     03  IO-ARTC                 PIC X(900)  VALUE SPACE.                 
031800     SKIP3                                                                
031900     03  WLARTC01 REDEFINES IO-ARTC.                                      
032000*        05  -COPY WDK601  -PRE ARTC-                                     
032100     EJECT                                                                
032200     03  WLARTC11 REDEFINES IO-ARTC.                                      
032300*        05  -COPY WDK611  -PRE ARTC-                                     
032400     EJECT                                                                
032500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032600 01   DLI-IO-AREA-B601.                                                   
032700*    03  -COPY WDB601                                                     
032800     EJECT                                                                
032900 LINKAGE SECTION.                                                         
033000                                                                          
033100 01  REQU-AREA.                                                           
033200*    03 -COPY WZ01REQU                                                    
033300*    03 -COPY W60144I1                                                    
033400     EJECT                                                                
033500 01  RESP-AREA.                                                           
033600*    03 -COPY WZ01RESP                                                    
033700*    03 -COPY W60144O1                                                    
033800     EJECT                                                                
033900                                                                          
034000 01  MAX-KVRADER                 PIC S9(4)  COMP.                         
034100                                                                          
034200*01  -COPY W0009   -PRE MSG-                                              
034300     EJECT                                                                
034400*01  -COPY W0009   -PRE ALT1-                                             
034500     EJECT                                                                
034600*01  -COPY W0009   -PRE DISP-                                             
034700     EJECT                                                                
034800*01  -COPY W0008  -PRE USEA-                                              
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE INLA1-                                             
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE INLA2-                                             
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE INLC-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE PLAA-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE ARTD-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008   -PRE ARTS-                                             
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008  -PRE UPFA-                                              
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008  -PRE ARTC-                                              
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008  -PRE WDB6-                                              
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800**  PCB'ER FÖR SUBPGM                                                     
037900 01  PMRK-INLB-PCB               PIC X.                                   
038000                                                                          
038100 01  PMRK-INLC-PCB               PIC X.                                   
038200                                                                          
038300 01  PMRK-PLAA-PCB               PIC X.                                   
038400                                                                          
038500 01  STYR-HANA-PCB               PIC X.                                   
038600                                                                          
038700 01  STYR-PLAA-PCB               PIC X.                                   
038800                                                                          
038900 01  KOM-KOMA-PCB                PIC X.                                   
039000                                                                          
039100                                                                          
039200 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
039300                           MSG-PCB ALT1-PCB DISP-PCB                      
039400                           INLA1-PCB                                      
039500                           INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB           
039600                           ARTS-PCB                                       
039700                           UPFA-PCB ARTC-PCB WDB6-PCB                     
039800                           PMRK-INLB-PCB PMRK-INLC-PCB                    
039900                           PMRK-PLAA-PCB                                  
040000                           STYR-HANA-PCB                                  
040100                           STYR-PLAA-PCB                                  
040200                           KOM-KOMA-PCB.                                  
040300                                                                          
040400     PERFORM A-INIT                                                       
040500     PERFORM B-KOLLA-NYCKLAR                                              
040600     IF NYCKLAR-OK                                                        
040700       IF REQU-UPDATE OR REQU-UPD-V                                       
040800         MOVE REQU-KVRADER TO RESP-KVRADER                                
040900         PERFORM G-KOLLA-INPUT                                            
041000         IF INDATA-OK                                                     
041100           PERFORM H-UPPDATERA                                            
041200         END-IF                                                           
041300       ELSE                                                               
041400         IF REQU-NEXT                                                     
041500           PERFORM D-NAESTA-SIDA                                          
041600         ELSE                                                             
041700           IF REQU-FIRST                                                  
041800             MOVE REQU-IDRADNR-START TO W-IDRADNR                         
041900             PERFORM MFS-RENSA-FAELT-UT                                   
042000           ELSE                                                           
042100             IF REQU-QUERY                                                
042200               PERFORM E-SAMMA-SIDA                                       
042300             END-IF                                                       
042400           END-IF                                                         
042500         END-IF                                                           
042600       END-IF                                                             
042700       IF INDATA-OK                                                       
042800         PERFORM F-LAES-VISA-INFO                                         
042900       END-IF                                                             
043000     END-IF                                                               
043100                                                                          
043200     MOVE ZERO TO RETURN-CODE                                             
043300     GOBACK                                                               
043400     .                                                                    
043500     EJECT                                                                
043600 A-INIT SECTION.                                                          
043700     IF REQU-KVRADER NOT NUMERIC                                          
043800       MOVE ZERO      TO REQU-KVRADER                                     
043900     END-IF                                                               
044000                                                                          
044100     MOVE ALL '+'     TO RESP-W60144O1-OUT                                
044200     MOVE 001         TO RESP-IDMSGVER                                    
044300     MOVE MAX-KVRADER TO RESP-KVRADER                                     
044400     MOVE SPACE       TO RESP-IDMSG-ERROR                                 
044500                         RESP-IDMSG-INFO                                  
044600                         RESP-IDELMT-ERROR                                
044700                                                                          
044800     MOVE JA          TO INDATA-SW                                        
044900                         FOERSTA-6191-SW                                  
045000     ACCEPT DAGENS-DATUM FROM DATE                                        
045100     MOVE DAGENS-DATUM     TO MSGI-TILOKDAT                               
045200     MOVE DAGENS-TID(1:4)  TO MSGI-TILOKTID                               
           MOVE REQU-IDDC-KEY    TO MSGI-IDDC                                   
045300     CALL  WL01TIDZ USING MSGI-WL01TIDZ                                   
045400     .                                                                    
045500     EJECT                                                                
045600 B-KOLLA-NYCKLAR SECTION.                                                 
045700*    VALIDATE IDDC KEY                                                    
045800     MOVE REQU-IDDC-KEY  TO W-IDDC-B6                                     
045900     PERFORM IMS-GU-WDB601                                                
046000     IF DCS-CDC OR DCS-NDC                                                
046100       MOVE DCS-IDDC  TO WS-IDDC                                          
046200                         W-IDDC                                           
046300     ELSE                                                                 
046400       MOVE NEJ            TO NYCKLAR-SW                                  
046500       MOVE ERR-WRONG-KEY  TO RESP-IDMSG-ERROR                            
046600       MOVE 'IDDC'         TO RESP-IDELMT-ERROR                           
046700       MOVE ZERO           TO RESP-KVRADER                                
046800     END-IF                                                               
046900                                                                          
047000     MOVE REQU-IDLOPNRM-KEY      TO WS-IDLOPNRM                           
047100     MOVE REQU-IDLEVNR-KOLLI-KEY TO WS-IDLEVNR-KOLLI                      
047200     MOVE REQU-IDOKOLLI-KEY      TO WS-IDOKOLLI                           
047300     MOVE NEJ                    TO PARTI-NYCKEL-SW                       
047400                                    KOLLI-NYCKEL-SW                       
047500     IF WS-IDLOPNRM          NUMERIC      OR                              
047600        (WS-IDLEVNR-KOLLI    NOT = SPACE  AND                             
047700        WS-IDOKOLLI          NUMERIC AND                                  
047800        WS-IDOKOLLI          NOT = ZEROS)                                 
047900       IF WS-IDLOPNRM > ZEROES                                            
048000         MOVE JA             TO PARTI-NYCKEL-SW                           
048100       ELSE                                                               
048200         MOVE JA             TO KOLLI-NYCKEL-SW                           
048300       END-IF                                                             
048400     END-IF                                                               
048500     IF PARTI-NYCKEL OR KOLLI-NYCKEL                                      
048600       CONTINUE                                                           
048700     ELSE                                                                 
048800       MOVE NEJ              TO NYCKLAR-SW                                
048900       MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                          
049000       MOVE 'IDLOPNRM'       TO RESP-IDELMT-ERROR                         
049100       MOVE ZERO             TO RESP-KVRADER                              
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 D-NAESTA-SIDA SECTION.                                                   
049600                                                                          
049700     MOVE REQU-IDRADNR-START   TO W-IDRADNR                               
049800                                                                          
049900     MOVE SPACES               TO RESP-IDANSTNR                           
050000     MOVE +1                   TO INDX                                    
050100     PERFORM UNTIL INDX        >  MAX-KVRADER                             
050200         MOVE SPACES           TO RESP-KDCMDVAL-RAD    (INDX)             
050300                                  RESP-KVINLART-UPD    (INDX)             
050400                                  RESP-ADINLOMR-NXT-UPD(INDX)             
050500         ADD +1                TO INDX                                    
050600     END-PERFORM                                                          
050700     .                                                                    
050800     EJECT                                                                
050900 E-SAMMA-SIDA SECTION.                                                    
051000     IF REQU-INPUT-UPD        = ALL '+'                                   
051100       MOVE REQU-IDRADNR-START   TO W-IDRADNR                             
051200     ELSE                                                                 
051300       PERFORM EA-REQU-INDATA-TILL-RESP                                   
051400       MOVE NEJ TO INDATA-SW                                              
051500       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 EA-REQU-INDATA-TILL-RESP SECTION.                                        
052000     IF REQU-IDANSTNR          NOT =  ALL '+'                             
052100       MOVE REQU-IDANSTNR    TO RESP-IDANSTNR                             
052200     END-IF                                                               
052300                                                                          
052400     MOVE +1                        TO INDX                               
052500     PERFORM UNTIL INDX             >  MAX-KVRADER                        
052600       IF REQU-KDCMDVAL-LINE(INDX) NOT = ALL '+'                          
052700         MOVE REQU-KDCMDVAL-LINE(INDX)                                    
052800                                TO RESP-KDCMDVAL-RAD(INDX)                
052900       END-IF                                                             
053000                                                                          
053100       IF REQU-KVINLART-UPD-LINE(INDX) NOT = ALL '+'                      
053200         MOVE REQU-KVINLART-UPD-LINE(INDX)                                
053300                                TO RESP-KVINLART-UPD(INDX)                
053400       END-IF                                                             
053500                                                                          
053600       IF REQU-ADINLOMR-NXT-UPD-LINE(INDX) NOT =  ALL '+'                 
053700         MOVE REQU-ADINLOMR-NXT-UPD-LINE(INDX)                            
053800                               TO RESP-ADINLOMR-NXT-UPD(INDX)             
053900       END-IF                                                             
054000                                                                          
054100       ADD +1                  TO INDX                                    
054200     END-PERFORM                                                          
054300     .                                                                    
054400     EJECT                                                                
054500 F-LAES-VISA-INFO SECTION.                                                
054600     PERFORM FA-LAES-GRUNDDATA                                            
054700                                                                          
054800     IF INLA11-SAKNAS OR DIVKLI                                           
054900       IF DIVKLI                                                          
055000         MOVE ERR-DIVERSE          TO RESP-IDMSG-ERROR                    
055100       ELSE                                                               
055200         MOVE ERR-MISSING          TO RESP-IDMSG-ERROR                    
055300       END-IF                                                             
055400     ELSE                                                                 
055500       MOVE NEJ TO FINN-RAD-SW                                            
055600       PERFORM S07-INITIERA-VOR-TABELL                                    
055700       MOVE +1               TO INDX                                      
055800       MOVE +0               TO RESP-KVRADER                              
055900       PERFORM FB-LAES-RADDATA                                            
056000       IF SEGMENT-FINNS                                                   
056100         MOVE RAD-IDRADNR  TO RESP-IDRADNR-START                          
056200       ELSE                                                               
056300         MOVE ZERO         TO RESP-IDRADNR-START                          
056400       END-IF                                                             
056500                                                                          
056600       PERFORM UNTIL INDX    >  MAX-KVRADER OR SEGMENT-SAKNAS             
056700           IF REQU-UPDATE OR REQU-FIRST OR REQU-UPD-V                     
056800               MOVE SPACES            TO RESP-IDANSTNR                    
056900               MOVE SPACES            TO RESP-KDCMDVAL-RAD(INDX)          
057000                                         RESP-KVINLART-UPD(INDX)          
057100                                     RESP-ADINLOMR-NXT-UPD(INDX)          
057200           END-IF                                                         
057300           IF (REQU-UPDATE OR REQU-UPD-V) AND INDX = +1                   
057400             CONTINUE                                                     
057500           ELSE                                                           
057600             MOVE MFS-ADD-SET-CURSOR   TO                                 
057700                                    RESP-KDCMDVAL-RAD-ATTR(INDX)          
057800           END-IF                                                         
057900           IF SEGMENT-FINNS                                               
058000             MOVE JA  TO FINN-RAD-SW                                      
058100             IF RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'                    
058200                MOVE RAD-IDRADNR  TO RESP-IDRADNR-RAD (INDX)              
058300                MOVE RAD-KVINLART TO RESP-KVINLART-RAD (INDX)             
058400                MOVE RAD-ADINLOMR TO RESP-ADINLOMR-RAD (INDX)             
058500                MOVE RAD-KDINLSTA TO RESP-KDINLSTA-RAD (INDX)             
058600                IF REQU-IDSPRAK = 'EN'                                    
058700                  EVALUATE RESP-KDINLSTA-RAD(INDX)                        
058800                    WHEN 'FPK'                                            
058900                       MOVE 'PP '   TO RESP-KDINLSTA-RAD(INDX)            
059000                    WHEN 'INL'                                            
059100                       MOVE 'BIN'   TO RESP-KDINLSTA-RAD(INDX)            
059200                    WHEN 'SAK'                                            
059300                       MOVE 'MIS'   TO RESP-KDINLSTA-RAD(INDX)            
059400                    WHEN 'AVV'                                            
059500                       MOVE 'DEV'   TO RESP-KDINLSTA-RAD(INDX)            
059600                    WHEN 'ANT'                                            
059700                       MOVE 'DEV'   TO RESP-KDINLSTA-RAD(INDX)            
059800                    WHEN 'KVA'                                            
059900                       MOVE 'Q-D'   TO RESP-KDINLSTA-RAD(INDX)            
060000                    WHEN 'RET'                                            
060100                       MOVE 'RET'   TO RESP-KDINLSTA-RAD(INDX)            
060200                    WHEN 'FRD'                                            
060300                       MOVE 'TRP'   TO RESP-KDINLSTA-RAD(INDX)            
060400                    WHEN 'MAK'                                            
060500                       MOVE 'CAN'   TO RESP-KDINLSTA-RAD(INDX)            
060600                  END-EVALUATE                                            
060700                END-IF                                                    
060800                MOVE RAD-IDLEVNR-KOLLI TO                                 
060900                                 RESP-IDLEVNR-KOLLI-RAD(INDX)             
061000                                 VOR-TAB-IDLEVNR-KOLLI(INDX)              
061100                MOVE RAD-IDOKOLLI TO RESP-IDOKOLLI-RAD (INDX)             
061200                                     VOR-TAB-IDOKOLLI (INDX)              
061300                ADD +1            TO INDX                                 
061400                ADD +1            TO RESP-KVRADER                         
061500             ELSE                                                         
061600                IF INDX > +1                                              
061700                   PERFORM FC-KOLLA-VOR                                   
061800                END-IF                                                    
061900             END-IF                                                       
062000             PERFORM FB-LAES-RADDATA                                      
062100           END-IF                                                         
062200         END-PERFORM                                                      
062300                                                                          
062400         IF SEGMENT-FINNS                                                 
062500            IF RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'                     
062600               MOVE RAD-IDRADNR       TO  RESP-IDRADNR-NEXT               
062700               IF RESP-IDMSG-INFO     =   SPACE                           
062800                 MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO             
062900               END-IF                                                     
063000            ELSE                                                          
063100               MOVE ZERO              TO RESP-IDRADNR-NEXT                
063200               IF RAD-KDINLSTA = 'VOR'                                    
063300                  PERFORM FC-KOLLA-VOR                                    
063400               END-IF                                                     
063500            END-IF                                                        
063600            PERFORM IMS-GNP-INLA2-INLA21                                  
063700            PERFORM UNTIL SEGMENT-SAKNAS                                  
063800               IF RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'                  
063900                  IF RESP-IDRADNR-NEXT = ZERO                             
064000                     MOVE RAD-IDRADNR TO RESP-IDRADNR-NEXT                
064100                  ELSE                                                    
064200                     CONTINUE                                             
064300                  END-IF                                                  
064400               ELSE                                                       
064500                  IF RAD-KDINLSTA = 'VOR'                                 
064600                     PERFORM FC-KOLLA-VOR                                 
064700                  ELSE                                                    
064800                     CONTINUE                                             
064900                  END-IF                                                  
065000               END-IF                                                     
065100               PERFORM IMS-GNP-INLA2-INLA21                               
065200            END-PERFORM                                                   
065300         ELSE                                                             
065400            MOVE RESP-IDRADNR-START TO RESP-IDRADNR-NEXT                  
065500**          MOVE INF-NO-MORE-LINES  TO RESP-IDMSG-INFO                    
065600         END-IF                                                           
065700                                                                          
065800         PERFORM FD-FLYTTA-VOR-TILL-BILD                                  
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 FA-LAES-GRUNDDATA SECTION.                                               
066300     MOVE NEJ                  TO INLA11-SW                               
066400                                                                          
066500     IF PARTI-NYCKEL                                                      
066600       MOVE WS-IDLOPNRM        TO W-D1BSEQ-IDLOPNRM                       
066700                                  RESP-IDLOPNRM                           
066800     ELSE                                                                 
066900       IF KOLLI-NYCKEL                                                    
067000         PERFORM FAA-LAES-KOLLI-INDEX                                     
067100       END-IF                                                             
067200     END-IF                                                               
067300     PERFORM IMS-GU-INLA2-INLA11                                          
067400     IF SEGMENT-FINNS                                                     
067500       MOVE JA           TO INLA11-SW                                     
067600       PERFORM FAB-TA-FRAM-KVRAPP                                         
067700       PERFORM FAC-TA-FRAM-BUFFERT-ADRESS                                 
067800       IF ART-FLKVAFEL      = JA  OR                                      
067900          ART-FLKVAKAR      = JA                                          
068000          PERFORM FAD-TA-FRAM-ADINLOMR-NXT                                
068100       ELSE                                                               
068200          MOVE SPACES          TO RESP-ADINLOMR-NXT                       
068300       END-IF                                                             
068400                                                                          
068500       MOVE ART-IDARTNR  TO RESP-IDARTNR                                  
068600       MOVE ART-KVAVIS   TO RESP-KVAVIS                                   
068700       MOVE ART-BEART    TO RESP-BEART                                    
068800       MOVE ART-KDSORT   TO RESP-KDSORT                                   
068900       MOVE ART-BEFT     TO RESP-BEFT                                     
069000       MOVE ART-ADLAGOMR TO RESP-ADLAGOMR                                 
069100       MOVE ART-ADGANG   TO RESP-ADGANG                                   
069200       MOVE ART-ADPLATS  TO RESP-ADPLATS                                  
069300       EVALUATE ART-KDFARLIG                                              
069400         WHEN 4                                                           
069500           IF REQU-IDSPRAK = 'EN'                                         
069600             MOVE 'JA'         TO RESP-BEFARLIG-TEXT                      
069700           ELSE                                                           
069800             MOVE 'YES'        TO RESP-BEFARLIG-TEXT                      
069900           END-IF                                                         
070000         WHEN 5                                                           
070100           MOVE 'ASBEST'       TO RESP-BEFARLIG-TEXT                      
070200         WHEN 6                                                           
070300           IF REQU-IDSPRAK = 'EN'                                         
070400             MOVE 'KEMIKALIER' TO RESP-BEFARLIG-TEXT                      
070500           ELSE                                                           
070600             MOVE 'CHEMICALS ' TO RESP-BEFARLIG-TEXT                      
070700           END-IF                                                         
070800         WHEN 7                                                           
070900           IF REQU-IDSPRAK = 'EN'                                         
071000             MOVE 'JA'     TO RESP-BEFARLIG-TEXT                          
071100           ELSE                                                           
071200             MOVE 'YES'    TO RESP-BEFARLIG-TEXT                          
071300           END-IF                                                         
071400       END-EVALUATE                                                       
071500     END-IF                                                               
071600     .                                                                    
071700     EJECT                                                                
071800 FAA-LAES-KOLLI-INDEX  SECTION.                                           
071900     MOVE WS-IDLEVNR-KOLLI     TO W-D1CSEQ-IDLEVNR-KOLLI                  
072000                                  W-IDLEVNR-KOLLI                         
072100     MOVE WS-IDOKOLLI          TO W-D1CSEQ-IDOKOLLI                       
072200                                  W-IDOKOLLI                              
072300     PERFORM IMS-GU-INLA1-INLA11                                          
072400     IF SEGMENT-FINNS                                                     
072500       MOVE ART-IDLOPNRM TO WS-IDLOPNRM                                   
072600                            W-D1BSEQ-IDLOPNRM                             
072700       MOVE WS-IDLOPNRM  TO RESP-IDLOPNRM                                 
072800                                                                          
072900       PERFORM IMS-GNP-INLA1-INLA21                                       
073000       IF RAD-FLDIVKLI = JA                                               
073100         MOVE JA TO DIVKLI-SW                                             
073200       END-IF                                                             
073300     ELSE                                                                 
073400       MOVE ZERO             TO W-D1BSEQ-IDLOPNRM                         
073500       MOVE SPACE            TO RESP-IDLOPNRM                             
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 FAB-TA-FRAM-KVRAPP    SECTION.                                           
074000     MOVE ZERO                 TO W-KVRAPP                                
074100     PERFORM IMS-GNP-INLA2-INLA21                                         
074200     PERFORM UNTIL SEGMENT-SAKNAS                                         
074300       IF (RAD-KDINLSTA      = 'INL' OR 'VOR' OR 'FRD') OR                
074400          (RAD-FLSATS        = JA AND ART-ADLAGOMR NOT = 30)              
074500           COMPUTE W-KVRAPP  = W-KVRAPP + RAD-KVINLART                    
074600       END-IF                                                             
074700       PERFORM IMS-GNP-INLA2-INLA21                                       
074800     END-PERFORM                                                          
074900     MOVE W-KVRAPP             TO RESP-KVRAPP                             
075000     .                                                                    
075100     EJECT                                                                
075200 FAC-TA-FRAM-BUFFERT-ADRESS SECTION.                                      
075300     MOVE 1                        TO INDX                                
075400     MOVE ART-IDARTNR              TO W-IDARTNR                           
075500     PERFORM IMS-GU-ARTD-ARTD11                                           
075600     IF SEGMENT-FINNS                                                     
075700       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 3                           
075800         IF SALDO-ADBUFFOMR        =  ART-ADLAGOMR                        
075900             PERFORM IMS-GNP-ARTD-ARTD11                                  
076000          ELSE                                                            
076100             MOVE SALDO-ADBUFFOMR  TO RESP-ADBUFFOMR (INDX)               
076200             MOVE SALDO-ADBUFFGANG TO RESP-ADBUFFGANG (INDX)              
076300             MOVE SALDO-ADBUFFPL   TO RESP-ADBUFFPL  (INDX)               
076400             PERFORM IMS-GNP-ARTD-ARTD11                                  
076500             ADD +1                TO INDX                                
076600         END-IF                                                           
076700       END-PERFORM                                                        
076800       IF INDX               = 1                                          
076900         PERFORM IMS-GNP-ARTD-ARTD11-FIRST                                
077000         IF SEGMENT-FINNS                                                 
077100             MOVE SALDO-ADBUFFOMR  TO RESP-ADBUFFOMR (INDX)               
077200             MOVE SALDO-ADBUFFGANG TO RESP-ADBUFFGANG(INDX)               
077300             MOVE SALDO-ADBUFFPL   TO RESP-ADBUFFPL (INDX)                
077400         END-IF                                                           
077500       END-IF                                                             
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900 FAD-TA-FRAM-ADINLOMR-NXT  SECTION.                                       
078000     MOVE ART-IDLOPNRM          TO W-IDLOPNRM                             
078100     PERFORM IMS-GU-INLC-INLC01                                           
078200                                                                          
078300     MOVE ART-IDDC              TO STYR-IDDC                              
078400     MOVE ART-IDARTNR           TO STYR-IDARTNR                           
078500     MOVE ART-IDFKNGRP          TO STYR-IDFKNGRP                          
078600     MOVE SEQB-IDLEVNR          TO STYR-IDLEVNR                           
078700     MOVE ART-BEFT              TO STYR-BEFT                              
078800     CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                      
078900                                       STYR-PLAA-PCB                      
079000     MOVE STYR-ADINLOMR-FB      TO RESP-ADINLOMR-NXT                      
079100     MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-ADINLOMR-NXT-ATTR                 
079200     .                                                                    
079300     EJECT                                                                
079400 FB-LAES-RADDATA         SECTION.                                         
079500     IF ((REQU-QUERY OR REQU-NEXT OR REQU-UPDATE OR REQU-UPD-V)           
079600         AND INDX = 1 AND FOERSTA-EJ-FUNNEN)                              
079700       PERFORM IMS-GNP-INLA2-INLA21-KVAL                                  
079800     ELSE                                                                 
079900       IF (INDX = 1              AND                                      
080000          FOERSTA-EJ-FUNNEN)                                              
080100         PERFORM IMS-GNP-INLA2-INLA21-FIRST                               
080200       ELSE                                                               
080300         PERFORM IMS-GNP-INLA2-INLA21                                     
080400       END-IF                                                             
080500     END-IF                                                               
080600                                                                          
080700     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
080800          (RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK' OR 'VOR') AND           
080900          (RAD-FLSATS   = NEJ OR ART-ADLAGOMR = 30)                       
081000       PERFORM IMS-GNP-INLA2-INLA21                                       
081100     END-PERFORM                                                          
081200     .                                                                    
081300     EJECT                                                                
081400 FC-KOLLA-VOR        SECTION.                                             
081500     MOVE +1 TO TABINDX                                                   
081600     MOVE NEJ TO VOR-SW                                                   
081700                                                                          
081800     PERFORM UNTIL TABINDX > MAX-KVRADER OR                               
081900       (VOR-TAB-IDLEVNR-KOLLI(TABINDX) = '99999' AND                      
082000        VOR-TAB-IDOKOLLI     (TABINDX) = 999999999) OR                    
082100        VOR-TRAEFF                                                        
082200                                                                          
082300       IF (RAD-IDLEVNR-KOLLI = VOR-TAB-IDLEVNR-KOLLI(TABINDX) AND         
082400          RAD-IDOKOLLI      = VOR-TAB-IDOKOLLI     (TABINDX))             
082500         ADD RAD-KVINLART TO VOR-TAB-KVINLART (TABINDX)                   
082600         MOVE JA          TO VOR-SW                                       
082700       ELSE                                                               
082800         ADD +1 TO TABINDX                                                
082900       END-IF                                                             
083000     END-PERFORM                                                          
083100     .                                                                    
083200     EJECT                                                                
083300 FD-FLYTTA-VOR-TILL-BILD  SECTION.                                        
083400     MOVE +1 TO TABINDX                                                   
083500                                                                          
083600     PERFORM UNTIL TABINDX > MAX-KVRADER OR                               
083700       (VOR-TAB-IDLEVNR-KOLLI(TABINDX) = '99999' AND                      
083800        VOR-TAB-IDOKOLLI     (TABINDX) = 999999999)                       
083900                                                                          
084000        IF VOR-TAB-KVINLART(TABINDX)      >  0                            
084100           MOVE VOR-TAB-KVINLART(TABINDX) TO                              
084200                         RESP-KVINLART-VOR-RAD(TABINDX)                   
084300        ELSE                                                              
084400           CONTINUE                                                       
084500        END-IF                                                            
084600                                                                          
084700        ADD +1 TO TABINDX                                                 
084800     END-PERFORM                                                          
084900     .                                                                    
085000     EJECT                                                                
085100 G-KOLLA-INPUT SECTION.                                                   
085200     MOVE SPACE                   TO RESP-IDMSG-ERROR                     
085300     IF REQU-INPUT-UPD =  ALL '+'                                         
085400       MOVE ERR-PF11-AND-NO-DATA  TO RESP-IDMSG-ERROR                     
085500       MOVE NEJ                   TO INDATA-SW                            
085600     ELSE                                                                 
085700       PERFORM GA-KOLLA-RADBEHANDLING                                     
085800       IF INDATA-FEL                                                      
085900         MOVE REQU-IDRADNR-START   TO RESP-IDRADNR-START                  
086000         IF RESP-IDMSG-ERROR =  SPACE                                     
086100           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
086200         END-IF                                                           
086300       ELSE                                                               
086400         IF W-KDCMDVAL-RAEKN = ZERO                                       
086500           MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                 
086600           MOVE NEJ                    TO INDATA-SW                       
086700         END-IF                                                           
086800       END-IF                                                             
086900     END-IF                                                               
087000                                                                          
087100     IF INDATA-OK AND NDC                                                 
087200       IF REQU-UPDATE AND REQU-IDMSGVER NOT = '001'                       
087300         PERFORM GB-KOLLA-BELOPP                                          
087400       END-IF                                                             
087500     END-IF                                                               
087600     .                                                                    
087700     EJECT                                                                
087800 GA-KOLLA-RADBEHANDLING SECTION.                                          
087900     MOVE ZERO                 TO W-KDCMDVAL-RAEKN                        
088000     MOVE MFS-NUM-FAELT-RAETT  TO RESP-IDANSTNR-ATTR                      
088100     MOVE +1                   TO INDX                                    
088200     PERFORM UNTIL INDX        >  REQU-KVRADER                            
088300         IF (REQU-KDCMDVAL-LINE(INDX)     = ALL '+' OR SPACE) AND         
088400            (REQU-KVINLART-UPD-LINE(INDX) = ALL '+' OR SPACE) AND         
088500            (REQU-ADINLOMR-NXT-UPD-LINE(INDX)  = ALL '+' OR SPACE)        
088600           MOVE MFS-ALFA-FAELT-RAETT TO                                   
088700                               RESP-KDCMDVAL-RAD-ATTR(INDX)               
088800                               RESP-ADINLOMR-NXT-UPD-ATTR(INDX)           
088900           MOVE MFS-NUM-FAELT-RAETT  TO                                   
089000                               RESP-KVINLART-UPD-ATTR(INDX)               
089100         ELSE                                                             
089200           ADD +1 TO W-KDCMDVAL-RAEKN                                     
089300           PERFORM GAA-KOLLA-ATT-RAD-FINNS                                
089400           IF SEGMENT-FINNS                                               
089500             PERFORM GAB-KOLLA-KDCMDVAL                                   
089600             PERFORM GAC-KOLLA-KVINLART-UPD                               
089700             PERFORM GAD-KOLLA-ADINLOMR-NXT-UPD                           
089800             IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'DEV'                 
089900               PERFORM GAE-KOLLA-IDANSTNR                                 
090000             END-IF                                                       
090100             IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'INL' OR 'I'          
090200                                               OR 'DIN' OR 'DI'           
090300                                               OR 'DEV' OR 'PB'           
090400                                               OR 'BIN'                   
090500               PERFORM GAF-KOLLA-KVAL-SPAERR                              
090600               IF NOT CDC-TR                                              
090700                 PERFORM GAG-KOLLA-PLATS                                  
090800               END-IF                                                     
090900               IF CDC-SE OR NDC-CN OR NDC-US                              
091000                 PERFORM GAH-KOLLA-VIKT-VOLYM-URSPR                       
091100               END-IF                                                     
091200             END-IF                                                       
091300           END-IF                                                         
091400         END-IF                                                           
091500         ADD +1                TO INDX                                    
091600     END-PERFORM                                                          
091700     .                                                                    
091800     EJECT                                                                
091900 GAA-KOLLA-ATT-RAD-FINNS SECTION.                                         
092000     INSPECT REQU-IDRADNR-LINE(INDX) REPLACING                            
092100                                       LEADING SPACE BY ZERO              
092200     MOVE WS-IDLOPNRM              TO W-D1BSEQ-IDLOPNRM                   
092201                                                                          
092210     IF REQU-IDRADNR-LINE(INDX) NOT = ALL '+'                             
092300        MOVE REQU-IDRADNR-LINE(INDX)   TO W-IDRADNR                       
092301     ELSE                                                                 
092302        MOVE ZERO                      TO W-IDRADNR                       
092310     END-IF                                                               
092320                                                                          
092400     PERFORM IMS-GU-INLA2-INLA11-2                                        
092500     IF SEGMENT-FINNS                                                     
092600       MOVE ART-IDARTNR           TO W-IDARTNR                            
092700       PERFORM IMS-GU-ARTC11                                              
092800       PERFORM IMS-GNP-INLA2-INLA21-2                                     
092900     END-IF                                                               
093000                                                                          
093100     IF SEGMENT-FINNS AND RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'          
093200       MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDCMDVAL-RAD-ATTR(INDX)          
093300       MOVE JA                 TO RAD-SW                                  
093400     ELSE                                                                 
093500       MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-RAD-ATTR(INDX)            
093600       MOVE NEJ                TO INDATA-SW                               
093700       MOVE NEJ                TO RAD-SW                                  
093800       MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                        
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 GAB-KOLLA-KDCMDVAL   SECTION.                                            
094300     IF REQU-KDCMDVAL-LINE(INDX)   = ALL '+' OR SPACE OR                  
094400                                     'AVV' OR 'I' OR 'INL' OR             
094500                                     'DIN' OR 'DI' OR                     
094600                                     'DEV' OR 'PB' OR 'BIN'               
094700         CONTINUE                                                         
094800      ELSE                                                                
094900         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-RAD-ATTR(INDX)          
095000         MOVE NEJ                TO INDATA-SW                             
095100     END-IF                                                               
095200     .                                                                    
095300     EJECT                                                                
095400 GAC-KOLLA-KVINLART-UPD   SECTION.                                        
095500     IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'DIN' OR 'DI' OR              
095600                                  'DEV' OR 'PB '                          
095700       IF REQU-KVINLART-UPD-LINE(INDX) NUMERIC                            
095800           MOVE REQU-KVINLART-UPD-LINE(INDX) TO W-KVINLART-UPD            
095900           MOVE MFS-NUM-FAELT-RAETT                                       
096000                             TO RESP-KVINLART-UPD-ATTR(INDX)              
096100           IF W-KVINLART-UPD = RAD-KVINLART                               
096200               MOVE NEJ      TO INDATA-SW                                 
096300               MOVE REQU-KVINLART-UPD-LINE(INDX)                          
096400                             TO RESP-KVINLART-UPD(INDX)                   
096500               MOVE MFS-NUM-FAELT-FEL                                     
096600                             TO RESP-KVINLART-UPD-ATTR(INDX)              
096700           END-IF                                                         
096800                                                                          
096900           IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'DEV'                   
097000             MOVE WS-IDLOPNRM  TO W-D1BSEQ-IDLOPNRM                       
097100             PERFORM IMS-GU-INLA2-INLA11                                  
097200             IF SEGMENT-FINNS                                             
097300* SPARAR UNDAN ARTIKELNR OCH KVAVIS FÖR GB-BELOPP KOLL                    
097400               IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'DEV'               
097500                  MOVE ART-IDARTNR  TO W-IDARTNR                          
097600                  MOVE ART-KVAVIS   TO W-KVAVIS                           
097700               END-IF                                                     
097800               IF ART-KDRT = +3                                           
097900                  MOVE NEJ   TO INDATA-SW                                 
098000                  MOVE MFS-ALFA-FAELT-FEL                                 
098100                                TO RESP-KDCMDVAL-RAD-ATTR(INDX)           
098200                  MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR              
098300                  MOVE 'KDRT'            TO RESP-IDELMT-ERROR             
098400               ELSE                                                       
098500                  CONTINUE                                                
098600               END-IF                                                     
098700             ELSE                                                         
098800               MOVE NEJ TO INDATA-SW                                      
098900               MOVE MFS-ALFA-FAELT-FEL                                    
099000                                TO RESP-KDCMDVAL-RAD-ATTR(INDX)           
099100               MOVE ERR-MISSING TO RESP-IDMSG-ERROR                       
099200             END-IF                                                       
099300           ELSE                                                           
099400             IF W-KVINLART-UPD =  0 OR                                    
099500               W-KVINLART-UPD >  RAD-KVINLART                             
099600               MOVE NEJ       TO INDATA-SW                                
099700               MOVE REQU-KVINLART-UPD-LINE(INDX)                          
099800                              TO RESP-KVINLART-UPD(INDX)                  
099900               MOVE MFS-NUM-FAELT-FEL                                     
100000                              TO RESP-KVINLART-UPD-ATTR(INDX)             
100100               MOVE ERR-UPDATE-NOT-ALLOWED                                
100200                              TO RESP-IDMSG-ERROR                         
100300             END-IF                                                       
100400           END-IF                                                         
100500         ELSE                                                             
100600           MOVE ZERO         TO W-KVINLART-UPD                            
100700           MOVE NEJ          TO INDATA-SW                                 
100800           MOVE REQU-KVINLART-UPD-LINE(INDX)                              
100900                             TO RESP-KVINLART-UPD(INDX)                   
101000           MOVE MFS-NUM-FAELT-FEL                                         
101100                             TO RESP-KVINLART-UPD-ATTR(INDX)              
101200         END-IF                                                           
101300       ELSE                                                               
101400         IF REQU-KVINLART-UPD-LINE(INDX) = ALL '+' OR SPACE               
101500           MOVE MFS-NUM-FAELT-RAETT                                       
101600                             TO RESP-KVINLART-UPD-ATTR(INDX)              
101700          ELSE                                                            
101800           MOVE REQU-KVINLART-UPD-LINE(INDX)                              
101900                             TO RESP-KVINLART-UPD(INDX)                   
102000           MOVE MFS-NUM-FAELT-FEL                                         
102100                             TO RESP-KVINLART-UPD-ATTR(INDX)              
102200           MOVE NEJ          TO INDATA-SW                                 
102300       END-IF                                                             
102400     END-IF                                                               
102500     .                                                                    
102600     EJECT                                                                
102700 GAD-KOLLA-ADINLOMR-NXT-UPD  SECTION.                                     
102800     IF REQU-ADINLOMR-NXT-UPD-LINE(INDX) = ALL '+' OR SPACE               
102900       MOVE MFS-ALFA-FAELT-RAETT                                          
103000                            TO RESP-ADINLOMR-NXT-UPD-ATTR(INDX)           
103100      ELSE                                                                
103200       IF REQU-KDCMDVAL-LINE(INDX) = 'INL' OR 'I' OR 'AVV' OR             
103300                                   'DIN' OR 'DI' OR                       
103400                                   'PB ' OR 'DEV' OR 'BIN'                
103500          MOVE REQU-ADINLOMR-NXT-UPD-LINE(INDX)                           
103600                          TO RESP-ADINLOMR-NXT-UPD(INDX)                  
103700          MOVE MFS-ALFA-FAELT-FEL                                         
103800                          TO RESP-ADINLOMR-NXT-UPD-ATTR(INDX)             
103900          MOVE NEJ        TO INDATA-SW                                    
104000        ELSE                                                              
104100          PERFORM GADA-KOLLA-PLAA                                         
104200          PERFORM GADB-KOLLA-ADINLOMR                                     
104300       END-IF                                                             
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 GADA-KOLLA-PLAA              SECTION.                                    
104800     MOVE REQU-ADINLOMR-NXT-UPD-LINE(INDX) TO W-6006-ADINLOMR             
104900     PERFORM IMS-GU-PLAA-PLAA11                                           
105000     IF SEGMENT-FINNS                                                     
105100       MOVE MFS-ALFA-FAELT-RAETT                                          
105200                           TO RESP-ADINLOMR-NXT-UPD-ATTR(INDX)            
105300      ELSE                                                                
105400       MOVE REQU-ADINLOMR-NXT-UPD-LINE(INDX)                              
105500                           TO RESP-ADINLOMR-NXT-UPD(INDX)                 
105600       MOVE MFS-ALFA-FAELT-FEL                                            
105700                           TO RESP-ADINLOMR-NXT-UPD-ATTR(INDX)            
105800       MOVE NEJ            TO INDATA-SW                                   
105900     END-IF                                                               
106000     .                                                                    
106100     EJECT                                                                
106200 GADB-KOLLA-ADINLOMR          SECTION.                                    
106300     IF RAD-SAKNAS OR                                                     
106400        RAD-ADINLOMR = REQU-ADINLOMR-NXT-UPD-LINE(INDX)                   
106500       MOVE REQU-ADINLOMR-NXT-UPD-LINE(INDX)                              
106600                           TO RESP-ADINLOMR-NXT-UPD(INDX)                 
106700       MOVE MFS-ALFA-FAELT-FEL                                            
106800                           TO RESP-ADINLOMR-NXT-UPD-ATTR(INDX)            
106900       MOVE NEJ            TO INDATA-SW                                   
107000     END-IF                                                               
107100     .                                                                    
107200     EJECT                                                                
107300 GAE-KOLLA-IDANSTNR          SECTION.                                     
107400     INSPECT REQU-IDANSTNR REPLACING LEADING SPACE BY ZERO                
107500     IF REQU-IDANSTNR              NUMERIC                                
107600       MOVE MFS-NUM-FAELT-RAETT  TO RESP-IDANSTNR-ATTR                    
107700      ELSE                                                                
107800       MOVE REQU-IDANSTNR        TO RESP-IDANSTNR                         
107900       MOVE MFS-NUM-FAELT-FEL    TO RESP-IDANSTNR-ATTR                    
108000       MOVE NEJ                  TO INDATA-SW                             
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 GAF-KOLLA-KVAL-SPAERR       SECTION.                                     
108500     MOVE WS-IDLOPNRM            TO W-D1BSEQ-IDLOPNRM                     
108600     PERFORM IMS-GU-INLA2-INLA11                                          
108700     IF ART-FLKVAFEL             =  JA                                    
108800       MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-RAD-ATTR(INDX)            
108900       MOVE NEJ                TO INDATA-SW                               
109000       MOVE ERR-QUALITY        TO RESP-IDMSG-ERROR                        
109100     END-IF                                                               
109200                                                                          
109300     IF ART-FLKVAKAR =  JA                                                
109400       MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMDVAL-RAD-ATTR(INDX)          
109500       MOVE NEJ                  TO INDATA-SW                             
109600       MOVE ERR-CHECK-QUAL-FIRST TO RESP-IDMSG-ERROR                      
109700     END-IF                                                               
109800                                                                          
109900** KOLLAR OM KONTROLLERAD PÅ 6139                                         
110000     IF INDATA-OK                                                         
110100        MOVE WS-IDLOPNRM     TO W-IDLOPNRM                                
110200        PERFORM IMS-GU-UPFA-01                                            
110300        IF SEGMENT-FINNS                                                  
110400          IF UPPF-KVKVAPRIM > 0                                           
110500** ARTIKEL UTTAGEN FÖR PRIMÄRKONTROLL                                     
110600            IF UPPF-KDKVASTA-PRI = '2' OR '3'                             
110700** PRIMÄRKONTROLL SATT SOM JA/NEJ. (OM NEJ HAR KR SKAPATS).               
110800              CONTINUE                                                    
110900            ELSE                                                          
111000              MOVE ERR-CONTROL-NOT-COMPL                                  
111100                             TO RESP-IDMSG-ERROR                          
111200              MOVE NEJ       TO INDATA-SW                                 
111300            END-IF                                                        
111400          END-IF                                                          
111500          IF UPPF-KVKVASEK > 0                                            
111600            IF UPPF-KDKVASTA-SEK = '2' OR '3'                             
111700              CONTINUE                                                    
111800            ELSE                                                          
111900              MOVE ERR-CONTROL-NOT-COMPL                                  
112000                             TO RESP-IDMSG-ERROR                          
112100              MOVE NEJ       TO INDATA-SW                                 
112200            END-IF                                                        
112300          END-IF                                                          
112400        END-IF                                                            
112500     END-IF                                                               
112600                                                                          
112700** KOLLAR ATT EVENTUELLT GAMLA KR BLIVIT BEDÖMDA                          
112800     IF INDATA-OK                                                         
112900       PERFORM IMS-GU-UPFA-01                                             
113000       IF SEGMENT-FINNS                                                   
113100         PERFORM IMS-GNP-UPFA11                                           
113200         PERFORM UNTIL SEGMENT-SAKNAS                                     
113300           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
113400             CONTINUE                                                     
113500           ELSE                                                           
113600             MOVE ERR-CONTROL-NOT-COMPL                                   
113700                            TO RESP-IDMSG-ERROR                           
113800             MOVE NEJ       TO INDATA-SW                                  
113900           END-IF                                                         
114000           PERFORM IMS-GNP-UPFA11                                         
114100         END-PERFORM                                                      
114200       END-IF                                                             
114300     END-IF                                                               
114400                                                                          
114500** KOLLAR ATT EVENTUELL SPECIALKONTROLL ÄR GJORD                          
114600     IF INDATA-OK                                                         
114700       PERFORM IMS-GU-UPFA-01                                             
114800       IF SEGMENT-FINNS                                                   
114900         PERFORM IMS-GNP-UPFA12                                           
115000         PERFORM UNTIL SEGMENT-SAKNAS                                     
115100           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
115200             CONTINUE                                                     
115300           ELSE                                                           
115400             MOVE ERR-CONTROL-NOT-COMPL                                   
115500                            TO RESP-IDMSG-ERROR                           
115600             MOVE NEJ       TO INDATA-SW                                  
115700           END-IF                                                         
115800           PERFORM IMS-GNP-UPFA12                                         
115900         END-PERFORM                                                      
116000       END-IF                                                             
116100     END-IF                                                               
116200     .                                                                    
116300     EJECT                                                                
116400 GAG-KOLLA-PLATS   SECTION.                                               
116500     IF  CDC-SE                                                           
116600     AND REQU-UPD-V                                                       
116700       IF ARTC-CLAG-ADLAGOMR-SVS = ZERO                                   
116800           MOVE NEJ                TO INDATA-SW                           
116900           MOVE MFS-ALFA-FAELT-FEL TO                                     
117000                                   RESP-KDCMDVAL-RAD-ATTR(INDX)           
117100           MOVE ERR-PLACE-MISSING-SVS TO RESP-IDMSG-ERROR                 
117200       END-IF                                                             
117300                                                                          
117400     ELSE                                                                 
117500       IF ART-ADLAGOMR = 90 OR ART-ADTRDEST(1:2) = 'CD'                   
117600         CONTINUE                                                         
117700       ELSE                                                               
117800         IF ART-ADPLATS = ZERO                                            
117900           MOVE NEJ                TO INDATA-SW                           
118000           MOVE MFS-ALFA-FAELT-FEL TO                                     
118100                                   RESP-KDCMDVAL-RAD-ATTR(INDX)           
118200           MOVE ERR-PLACE-MISSING  TO RESP-IDMSG-ERROR                    
118300         END-IF                                                           
118400       END-IF                                                             
118500     END-IF                                                               
118600     .                                                                    
118700     EJECT                                                                
118800 GAH-KOLLA-VIKT-VOLYM-URSPR   SECTION.                                    
118900     IF ART-VKART = ZERO                                                  
119000       MOVE NEJ                TO INDATA-SW                               
119100       MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-RAD-ATTR(INDX)            
119200       MOVE ERR-WEIGHT-MISSING TO RESP-IDMSG-ERROR                        
119300     END-IF                                                               
119400                                                                          
119500     IF ART-VLARTNTO = ZERO                                               
119600       MOVE NEJ                TO INDATA-SW                               
119700       MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-RAD-ATTR(INDX)            
119800       MOVE ERR-VOLUME-MISSING TO RESP-IDMSG-ERROR                        
119900     END-IF                                                               
120000                                                                          
120100     IF ART-KDARTURS = SPACE                                              
120200       MOVE NEJ                TO INDATA-SW                               
120300       MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-RAD-ATTR(INDX)            
120400       MOVE ERR-ORIGIN-MISSING TO RESP-IDMSG-ERROR                        
120500     END-IF                                                               
120600     .                                                                    
120700     EJECT                                                                
120800 GB-KOLLA-BELOPP   SECTION.                                               
120900     PERFORM IMS-GU-ARTS11                                                
121000     IF SEGMENT-FINNS                                                     
121100       MOVE +1 TO INDX                                                    
121200       PERFORM UNTIL INDX > REQU-KVRADER OR INDATA-FEL                    
121300         IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'DEV'                     
121400           INSPECT REQU-KVINLART-UPD-LINE(INDX)                           
121500                              REPLACING LEADING SPACE BY ZERO             
121600           IF REQU-KVINLART-UPD-LINE(INDX) NUMERIC                        
121700             MOVE REQU-KVINLART-UPD-LINE(INDX)                            
121800                                 TO W-KVINLART-TOTALT                     
121900             COMPUTE W-KVINLART-DIFF = W-KVINLART-TOTALT -                
122000                                                          W-KVAVIS        
122100             IF NDC-JP                                                    
122200               COMPUTE WS-BELOPP = ARTC-CLAG-PRARTSTD *                   
122300                                                   W-KVINLART-DIFF        
122400             ELSE                                                         
122500               COMPUTE WS-BELOPP = WLARTS-SLAG-PRAVCOST *                 
122600                                                   W-KVINLART-DIFF        
122700             END-IF                                                       
122800           END-IF                                                         
122900                                                                          
123000           IF NDC-US                                                      
123100             IF WS-BELOPP > +1000 OR WS-BELOPP < -1000                    
123200               MOVE NEJ                TO INDATA-SW                       
123300               MOVE INFO-DEV-VALUE-TOO-HIGH                               
123400                                       TO RESP-IDMSG-INFO                 
123500             END-IF                                                       
123600           END-IF                                                         
123700           IF NDC-CA                                                      
123800             IF WS-BELOPP > +5000 OR WS-BELOPP < -5000                    
123900               MOVE NEJ                TO INDATA-SW                       
124000               MOVE INFO-DEV-VALUE-TOO-HIGH                               
124100                                       TO RESP-IDMSG-INFO                 
124200             END-IF                                                       
124300           END-IF                                                         
124400           IF NDC-JP                                                      
124500             IF WS-BELOPP > +1000 OR WS-BELOPP < -1000                    
124600               MOVE NEJ                TO INDATA-SW                       
124700               MOVE INFO-DEV-VALUE-TOO-HIGH                               
124800                                       TO RESP-IDMSG-INFO                 
124900             END-IF                                                       
125000           END-IF                                                         
125100         END-IF                                                           
125200         ADD +1 TO INDX                                                   
125300       END-PERFORM                                                        
125400     END-IF                                                               
125500     .                                                                    
125600     EJECT                                                                
125700 H-UPPDATERA SECTION.                                                     
125800                                                                          
125900     MOVE +1                   TO 6191-IX                                 
126000     PERFORM HA-UPPDATERA-RADER                                           
126100                                                                          
126200     IF 6191-IX                > 1                                        
126300       PERFORM S03-STARTA-W6T191                                          
126400     END-IF                                                               
126500                                                                          
126600     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
126700     .                                                                    
126800     EJECT                                                                
126900 HA-UPPDATERA-RADER SECTION.                                              
127000     MOVE +1                       TO INDX                                
127100     PERFORM UNTIL INDX            >  REQU-KVRADER                        
127200         IF REQU-KDCMDVAL-LINE(INDX) = 'AVV' OR 'DEV'                     
127300           PERFORM HAA-UPPDATERA-AVVIKELSE                                
127400         END-IF                                                           
127500                                                                          
127600         IF REQU-ADINLOMR-NXT-UPD-LINE(INDX) = ALL '+' OR SPACE           
127700           CONTINUE                                                       
127800         ELSE                                                             
127900           PERFORM HAB-UPPDATERA-RETUR                                    
128000         END-IF                                                           
128100                                                                          
128200         IF REQU-KDCMDVAL-LINE(INDX) = 'INL' OR 'I' OR 'BIN'              
128300           PERFORM HAC-UPPDATERA-INLAEGGNING                              
128400         END-IF                                                           
128500                                                                          
128600         IF REQU-KDCMDVAL-LINE(INDX) = 'DIN' OR 'DI' OR 'PB '             
128700           PERFORM HAD-UPPDATERA-DELINLAEGGNING                           
128800         END-IF                                                           
128900         ADD +1                    TO INDX                                
129000     END-PERFORM                                                          
129100     .                                                                    
129200     EJECT                                                                
129300 HAA-UPPDATERA-AVVIKELSE SECTION.                                         
129400     PERFORM S01-LAES-INLA21                                              
129500     IF REQU-KVINLART-UPD-LINE(INDX) =  ZERO AND RAD-IDRADNR > +1         
129600       MOVE 'AVV'                  TO RAD-KDINLSTA                        
129700       MOVE REQU-IDANSTNR          TO RAD-IDANSTNR                        
129800       MOVE SPACE                  TO RAD-ADINLOMR                        
129900                                      RAD-ADINLOMR-NXT                    
130000       MOVE ZERO                   TO RAD-IDINLVGN                        
130100                                      RAD-IDILIST                         
130200                                      RAD-IDILIRAD                        
130300                                      RAD-TIUPPDAT                        
130400       IF CDC-SE AND REQU-UPD-V                                           
130500         MOVE JA                   TO RAD-FLSVSLS                         
130600       ELSE                                                               
130700         MOVE NEJ                  TO RAD-FLSVSLS                         
130800       END-IF                                                             
130900       PERFORM IMS-REPL-INLA2-INLA21                                      
131000       PERFORM S02-SKAPA-6191-MID                                         
131100       PERFORM S04-SKAPA-6193-MID                                         
131200     ELSE                                                                 
131300       IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                       
131400          IF RAD-IDLEVNR-KOLLI = SPACE                                    
131500             PERFORM S09-CALL-W611PMRK-PARTI                              
131600          ELSE                                                            
131700             PERFORM S08-CALL-W611PMRK-KOLLI                              
131800          END-IF                                                          
131900          PERFORM S01-LAES-INLA21                                         
132000       END-IF                                                             
132100       IF RAD-IDRADNR        =  1                                         
132200                                                                          
132300** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
132400** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
132500                                                                          
132600         PERFORM IMS-DLET-INLA2-INLA21                                    
132700         MOVE REQU-KVINLART-UPD-LINE(INDX) TO W-KVINLART-UPD              
132800         PERFORM S06-SKAPA-6191-REQU-BORT-RAD                             
132900         IF W-KVINLART-UPD > +0                                           
133000           PERFORM IMS-GNP-INLA2-INLA21-LAST                              
133100           IF SEGMENT-FINNS                                               
133200              COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                  
133300           ELSE                                                           
133400              MOVE +2          TO SPAR-RAD-IDRADNR                        
133500           END-IF                                                         
133600           MOVE SPAR-RAD-W6D121        TO RAD-W6D121                      
133700           MOVE REQU-KVINLART-UPD-LINE(INDX) TO RAD-KVINLART              
133800           MOVE 'INL'                  TO RAD-KDINLSTA                    
133900           MOVE SPACE                  TO RAD-ADINLOMR                    
134000                                          RAD-ADINLOMR-NXT                
134100           MOVE ZERO                   TO RAD-IDINLVGN                    
134200                                          RAD-IDILIST                     
134300                                          RAD-IDILIRAD                    
134400           MOVE MSGI-TILOKDAT          TO RAD-TIUPPDAT                    
134500           IF  CDC-SE AND REQU-UPD-V                                      
134600             MOVE JA                   TO RAD-FLSVSLS                     
134700           ELSE                                                           
134800             MOVE NEJ                  TO RAD-FLSVSLS                     
134900           END-IF                                                         
135000           PERFORM IMS-ISRT-INLA2-INLA21                                  
135100           PERFORM S05-SKAPA-6191-REQU-NY-RAD                             
135200         END-IF                                                           
135300       ELSE                                                               
135400         MOVE REQU-KVINLART-UPD-LINE(INDX) TO RAD-KVINLART                
135500                                         W-KVINLART-UPD                   
135600         MOVE 'INL'                   TO RAD-KDINLSTA                     
135700         MOVE SPACE                   TO RAD-ADINLOMR                     
135800                                         RAD-ADINLOMR-NXT                 
135900         MOVE ZERO                    TO RAD-IDINLVGN                     
136000                                         RAD-IDILIST                      
136100                                         RAD-IDILIRAD                     
136200         MOVE MSGI-TILOKDAT          TO RAD-TIUPPDAT                      
136300         IF CDC-SE AND REQU-UPD-V                                         
136400           MOVE JA                   TO RAD-FLSVSLS                       
136500         ELSE                                                             
136600           MOVE NEJ                  TO RAD-FLSVSLS                       
136700         END-IF                                                           
136800         PERFORM IMS-REPL-INLA2-INLA21                                    
136900         PERFORM S02-SKAPA-6191-MID                                       
137000       END-IF                                                             
137100        IF W-KVINLART-UPD = 0                                             
137200          PERFORM HAAA-SKAPA-NY-AVV-RAD                                   
137300          PERFORM S04-SKAPA-6193-MID                                      
137400        ELSE                                                              
137500          PERFORM S04-SKAPA-6193-MID                                      
137600          PERFORM HAAA-SKAPA-NY-AVV-RAD                                   
137700        END-IF                                                            
137800        PERFORM S05-SKAPA-6191-REQU-NY-RAD                                
137900     END-IF                                                               
138000                                                                          
138100     IF (RAD-FLPRIO         = 'J'  AND                                    
138200         RAD-KVINLART       > ZERO AND                                    
138300         RAD-KDINLSTA       = 'AVV')                                      
138400        IF RAD-IDLEVNR-KOLLI = SPACE                                      
138500          PERFORM S09-CALL-W611PMRK-PARTI                                 
138600        ELSE                                                              
138700          PERFORM S08-CALL-W611PMRK-KOLLI                                 
138800        END-IF                                                            
138900     END-IF                                                               
139000     .                                                                    
139100     EJECT                                                                
139200 HAAA-SKAPA-NY-AVV-RAD SECTION.                                           
139300** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN -LAST OCH ISRT              
139400** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
139500                                                                          
139600     PERFORM IMS-GNP-INLA2-INLA21-LAST                                    
139700     IF SEGMENT-FINNS                                                     
139800         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
139900      ELSE                                                                
140000         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
140100     END-IF                                                               
140200     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
140300     MOVE SPACE                  TO RAD-ADINLOMR                          
140400                                    RAD-ADINLOMR-NXT                      
140500     MOVE REQU-IDANSTNR          TO RAD-IDANSTNR                          
140600     MOVE ZERO                   TO RAD-IDILIRAD                          
140700                                    RAD-IDILIST                           
140800                                    RAD-IDINLVGN                          
140900                                    RAD-TIUPPDAT                          
141000     MOVE 'AVV'                  TO RAD-KDINLSTA                          
141100     MOVE REQU-KVINLART-UPD-LINE(INDX) TO W-KVINLART-UPD                  
141200     COMPUTE RAD-KVINLART        =  SPAR-RAD-KVINLART -                   
141300                                    W-KVINLART-UPD                        
141400     IF  CDC-SE AND REQU-UPD-V                                            
141500       MOVE JA                   TO RAD-FLSVSLS                           
141600     ELSE                                                                 
141700       MOVE NEJ                  TO RAD-FLSVSLS                           
141800     END-IF                                                               
141900                                                                          
142000     PERFORM IMS-ISRT-INLA2-INLA21                                        
142100     .                                                                    
142200     EJECT                                                                
142300 HAB-UPPDATERA-RETUR     SECTION.                                         
142400     PERFORM S01-LAES-INLA21                                              
142500     MOVE REQU-ADINLOMR-NXT-UPD-LINE(INDX) TO RAD-ADINLOMR-NXT            
142600     PERFORM IMS-REPL-INLA2-INLA21                                        
142700     PERFORM S02-SKAPA-6191-MID                                           
142800     .                                                                    
142900     EJECT                                                                
143000 HAC-UPPDATERA-INLAEGGNING  SECTION.                                      
143100     PERFORM S01-LAES-INLA21                                              
143200     IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                         
143300        IF RAD-IDLEVNR-KOLLI = SPACE                                      
143400           PERFORM S09-CALL-W611PMRK-PARTI                                
143500        ELSE                                                              
143600           PERFORM S08-CALL-W611PMRK-KOLLI                                
143700        END-IF                                                            
143800        PERFORM S01-LAES-INLA21                                           
143900     END-IF                                                               
144000                                                                          
144100     IF RAD-IDRADNR            =  1                                       
144200                                                                          
144300** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
144400** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
144500                                                                          
144600        PERFORM IMS-DLET-INLA2-INLA21                                     
144700        PERFORM S06-SKAPA-6191-REQU-BORT-RAD                              
144800        PERFORM IMS-GNP-INLA2-INLA21-LAST                                 
144900        IF SEGMENT-FINNS                                                  
145000           COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                     
145100        ELSE                                                              
145200           MOVE +2           TO SPAR-RAD-IDRADNR                          
145300        END-IF                                                            
145400        MOVE SPAR-RAD-W6D121  TO RAD-W6D121                               
145500        MOVE 'INL'            TO RAD-KDINLSTA                             
145600        MOVE SPACE            TO RAD-ADINLOMR                             
145700                                 RAD-ADINLOMR-NXT                         
145800        MOVE ZERO             TO RAD-IDINLVGN                             
145900                                 RAD-IDILIST                              
146000                                 RAD-IDILIRAD                             
146100        MOVE MSGI-TILOKDAT    TO RAD-TIUPPDAT                             
146200        IF  CDC-SE AND REQU-UPD-V                                         
146300          MOVE JA                   TO RAD-FLSVSLS                        
146400        ELSE                                                              
146500          MOVE NEJ                  TO RAD-FLSVSLS                        
146600        END-IF                                                            
146700        PERFORM IMS-ISRT-INLA2-INLA21                                     
146800        PERFORM S05-SKAPA-6191-REQU-NY-RAD                                
146900     ELSE                                                                 
147000        MOVE 'INL'            TO RAD-KDINLSTA                             
147100        MOVE SPACE            TO RAD-ADINLOMR                             
147200                                 RAD-ADINLOMR-NXT                         
147300        MOVE ZERO             TO RAD-IDINLVGN                             
147400                                 RAD-IDILIST                              
147500                                 RAD-IDILIRAD                             
147600        MOVE MSGI-TILOKDAT    TO RAD-TIUPPDAT                             
147700        IF  CDC-SE AND REQU-UPD-V                                         
147800          MOVE JA                   TO RAD-FLSVSLS                        
147900        ELSE                                                              
148000          MOVE NEJ                  TO RAD-FLSVSLS                        
148100        END-IF                                                            
148200        PERFORM IMS-REPL-INLA2-INLA21                                     
148300        PERFORM S02-SKAPA-6191-MID                                        
148400     END-IF                                                               
148500                                                                          
148600     PERFORM S04-SKAPA-6193-MID                                           
148700     .                                                                    
148800     EJECT                                                                
148900 HAD-UPPDATERA-DELINLAEGGNING  SECTION.                                   
149000                                                                          
149100     PERFORM S01-LAES-INLA21                                              
149200     MOVE REQU-KVINLART-UPD-LINE(INDX) TO W-KVINLART-UPD                  
149300     COMPUTE RAD-KVINLART         = RAD-KVINLART - W-KVINLART-UPD         
149400     IF  CDC-SE AND REQU-UPD-V                                            
149500       MOVE JA                   TO RAD-FLSVSLS                           
149600     ELSE                                                                 
149700       MOVE NEJ                  TO RAD-FLSVSLS                           
149800     END-IF                                                               
149900     PERFORM IMS-REPL-INLA2-INLA21                                        
150000     PERFORM S02-SKAPA-6191-MID                                           
150100     PERFORM HADA-SKAPA-NY-RAD                                            
150200     PERFORM S05-SKAPA-6191-REQU-NY-RAD                                   
150300     PERFORM S04-SKAPA-6193-MID                                           
150400     .                                                                    
150500     EJECT                                                                
150600 HADA-SKAPA-NY-RAD SECTION.                                               
150700                                                                          
150800** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
150900** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
151000                                                                          
151100     PERFORM IMS-GNP-INLA2-INLA21-LAST                                    
151200     IF SEGMENT-FINNS                                                     
151300        COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                        
151400     ELSE                                                                 
151500        COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                   
151600     END-IF                                                               
151700     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
151800     MOVE SPACE                  TO RAD-ADINLOMR                          
151900                                    RAD-ADINLOMR-NXT                      
152000                                    RAD-KDINLSTA                          
152100     MOVE ZERO                   TO RAD-IDILIRAD                          
152200                                    RAD-IDILIST                           
152300                                    RAD-IDINLVGN                          
152400     MOVE MSGI-TILOKDAT          TO RAD-TIUPPDAT                          
152500     MOVE REQU-KVINLART-UPD-LINE(INDX) TO RAD-KVINLART                    
152600                                                                          
152700     IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                         
152800        PERFORM IMS-ISRT-INLA2-INLA21                                     
152900                                                                          
153000        IF RAD-IDLEVNR-KOLLI = SPACE                                      
153100           PERFORM S09-CALL-W611PMRK-PARTI                                
153200        ELSE                                                              
153300           PERFORM S08-CALL-W611PMRK-KOLLI                                
153400        END-IF                                                            
153500                                                                          
153600        MOVE SPAR-RAD-IDRADNR    TO W-IDRADNR                             
153700        PERFORM IMS-GU-INLA2-INLA11                                       
153800        PERFORM IMS-GHNP-INLA2-INLA21                                     
153900        MOVE 'INL'               TO RAD-KDINLSTA                          
154000        MOVE MSGI-TILOKDAT          TO RAD-TIUPPDAT                       
154100        IF  CDC-SE AND REQU-UPD-V                                         
154200          MOVE JA                   TO RAD-FLSVSLS                        
154300        ELSE                                                              
154400          MOVE NEJ                  TO RAD-FLSVSLS                        
154500        END-IF                                                            
154600        PERFORM IMS-REPL-INLA2-INLA21                                     
154700     ELSE                                                                 
154800        MOVE MSGI-TILOKDAT          TO RAD-TIUPPDAT                       
154900        MOVE 'INL'               TO RAD-KDINLSTA                          
155000        IF  CDC-SE AND REQU-UPD-V                                         
155100          MOVE JA                   TO RAD-FLSVSLS                        
155200        ELSE                                                              
155300          MOVE NEJ                  TO RAD-FLSVSLS                        
155400        END-IF                                                            
155500        PERFORM IMS-ISRT-INLA2-INLA21                                     
155600     END-IF                                                               
155700     .                                                                    
155800     EJECT                                                                
155900 S01-LAES-INLA21         SECTION.                                         
156000                                                                          
156100     INSPECT REQU-IDRADNR-LINE(INDX) REPLACING                            
156200                                    LEADING SPACE BY ZERO                 
156300     MOVE WS-IDLOPNRM            TO W-D1BSEQ-IDLOPNRM                     
156400     PERFORM IMS-GU-INLA2-INLA11                                          
156401                                                                          
156410     IF REQU-IDRADNR-LINE(INDX) NOT = ALL '+'                             
156500        MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                         
156501     ELSE                                                                 
156502        MOVE ZERO                    TO W-IDRADNR                         
156510     END-IF                                                               
156520                                                                          
156600     PERFORM IMS-GHNP-INLA2-INLA21                                        
156610                                                                          
156611     IF REQU-IDANSTNR NUMERIC                                             
156620       MOVE REQU-IDANSTNR        TO RAD-IDANSTNR                          
156630     END-IF                                                               
156700     MOVE RAD-W6D121             TO SPAR-RAD-W6D121                       
156800     .                                                                    
156900     EJECT                                                                
157000 S02-SKAPA-6191-MID   SECTION.                                            
157100     MOVE 'W6014410'           TO MOD6191-MID-IDPGM                       
157200     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
157300     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
157400     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR (6191-IX)           
157500     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
157600     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
157700     MOVE +0                   TO MOD6191-MID-KVKOLLI (6191-IX)           
157800     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
157900                                                                          
158000     MOVE SPAR-RAD-ADINLOMR    TO MOD6191-MID-ADINLOMR-OLD                
158100                                                   (6191-IX)              
158200     MOVE SPAR-RAD-ADINLOMR-NXT                                           
158300                               TO MOD6191-MID-ADINLOMR-NXT-OLD            
158400                                                 (6191-IX)                
158500     MOVE SPAR-RAD-KDINLSTA    TO MOD6191-MID-KDINLSTA-OLD                
158600                                                 (6191-IX)                
158700     MOVE SPAR-RAD-KVINLART    TO MOD6191-MID-KVINLART-OLD                
158800                                                   (6191-IX)              
158900                                                                          
159000     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
159100                                                   (6191-IX)              
159200     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
159300                                                   (6191-IX)              
159400     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
159500                                                   (6191-IX)              
159600     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
159700                                                   (6191-IX)              
159800     ADD +1                    TO 6191-IX                                 
159900     IF 6191-IX                > MAX-6191-IX                              
160000         PERFORM S03-STARTA-W6T191                                        
160100     END-IF                                                               
160200     .                                                                    
160300     EJECT                                                                
160400 S03-STARTA-W6T191         SECTION.                                       
160500                                                                          
160600     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
160700     COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                    
160800                                  17 + (MOD6191-MID-KVPOST * 64)          
160900     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
161000     MOVE '6144'               TO P-TO-P-MSG-IDTRANS                      
161100     MOVE '1'                  TO P-TO-P-MSG-KDMFSFOR                     
161200                                                                          
161300     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
161400                                                                          
161500     IF FOERSTA-6191                                                      
161600         PERFORM IMS-ISRT-ALT1-MSG-6191                                   
161700         MOVE NEJ               TO FOERSTA-6191-SW                        
161800      ELSE                                                                
161900         PERFORM IMS-PURG-ALT1-MSG-6191                                   
162000     END-IF                                                               
162100     MOVE +1                   TO 6191-IX                                 
162200     .                                                                    
162300     EJECT                                                                
162400 S04-SKAPA-6193-MID   SECTION.                                            
162500                                                                          
162600     ACCEPT DAGENS-DATUM       FROM DATE                                  
162700     ACCEPT DAGENS-TID         FROM TIME                                  
162800                                                                          
162900     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
163000     MOVE +53                  TO MSG-KOM-KVLL                            
163100     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
163200     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
163300     MOVE SPACE                TO MSG-KOM-KDTRANS                         
163400     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
163500     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
163600     MOVE 'W6014410'           TO MSG-KOM-IDSNDJOB                        
163700     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
163800     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
163900     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
164000                                                                          
164100     MOVE ART-IDLOPNRM         TO MOD6193-MID-IDLOPNRM                    
164200     MOVE RAD-IDRADNR          TO MOD6193-MID-IDRADNR                     
164300                                                                          
164400     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX + 12                  
164500     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
164600     MOVE '6144'               TO P-TO-P-MSG-IDTRANS                      
164700     MOVE '1'                  TO P-TO-P-MSG-KDMFSFOR                     
164800                                                                          
164900     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
165000                                                                          
165100     CALL W006KOM USING MSG-PCB                                           
165200                        DISP-PCB                                          
165300                        KOM-KOMA-PCB                                      
165400                        MSG-KOM-WMSGKOM                                   
165500                        P-TO-P-MSG-IO-AREA-SNUF                           
165600     .                                                                    
165700     EJECT                                                                
165800 S05-SKAPA-6191-REQU-NY-RAD  SECTION.                                     
165900     MOVE 'W6014410'           TO MOD6191-MID-IDPGM                       
166000     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
166100     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
166200     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR (6191-IX)           
166300     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
166400     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
166500     MOVE +0                   TO MOD6191-MID-KVKOLLI (6191-IX)           
166600     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
166700                                                                          
166800     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
166900                                                   (6191-IX)              
167000                                  MOD6191-MID-ADINLOMR-NXT-OLD            
167100                                                   (6191-IX)              
167200                                  MOD6191-MID-KDINLSTA-OLD                
167300                                                   (6191-IX)              
167400     MOVE ZERO                 TO MOD6191-MID-KVINLART-OLD                
167500                                                   (6191-IX)              
167600                                                                          
167700     MOVE SPAR-RAD-ADINLOMR    TO MOD6191-MID-ADINLOMR-NEW                
167800                                                   (6191-IX)              
167900     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
168000                                                   (6191-IX)              
168100     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
168200                                                   (6191-IX)              
168300     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
168400                                                   (6191-IX)              
168500     ADD +1                    TO 6191-IX                                 
168600     IF 6191-IX                > MAX-6191-IX                              
168700         PERFORM S03-STARTA-W6T191                                        
168800     END-IF                                                               
168900     .                                                                    
169000     EJECT                                                                
169100 S06-SKAPA-6191-REQU-BORT-RAD  SECTION.                                   
169200                                                                          
169300     MOVE 'W6014410'           TO MOD6191-MID-IDPGM                       
169400     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
169500     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
169600     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR (6191-IX)           
169700     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
169800     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
169900     MOVE +0                   TO MOD6191-MID-KVKOLLI (6191-IX)           
170000     MOVE 'J'                  TO MOD6191-MID-FLINLI   (6191-IX)          
170100                                                                          
170200     MOVE SPAR-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-OLD              
170300                                                   (6191-IX)              
170400     MOVE SPAR-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-OLD          
170500                                                   (6191-IX)              
170600     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD              
170700                                                   (6191-IX)              
170800     MOVE SPAR-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
170900                                                   (6191-IX)              
171000                                                                          
171100     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
171200                                                   (6191-IX)              
171300                                  MOD6191-MID-ADINLOMR-NXT-NEW            
171400                                                   (6191-IX)              
171500                                  MOD6191-MID-KDINLSTA-NEW                
171600                                                   (6191-IX)              
171700     MOVE ZERO                 TO MOD6191-MID-KVINLART-NEW                
171800                                                   (6191-IX)              
171900     ADD +1                    TO 6191-IX                                 
172000     IF 6191-IX                > MAX-6191-IX                              
172100         PERFORM S03-STARTA-W6T191                                        
172200     END-IF                                                               
172300     .                                                                    
172400     EJECT                                                                
172500 S07-INITIERA-VOR-TABELL SECTION.                                         
172600                                                                          
172700     MOVE +1 TO TABINDX                                                   
172800                                                                          
172900     PERFORM UNTIL TABINDX > MAX-KVRADER                                  
173000        MOVE '99999'    TO VOR-TAB-IDLEVNR-KOLLI(TABINDX)                 
173100        MOVE 999999999  TO VOR-TAB-IDOKOLLI     (TABINDX)                 
173200        MOVE 0          TO VOR-TAB-KVINLART     (TABINDX)                 
173300        ADD +1 TO TABINDX                                                 
173400                                                                          
173500     END-PERFORM                                                          
173600                                                                          
173700     MOVE +1 TO TABINDX                                                   
173800     .                                                                    
173900     EJECT                                                                
174000 S08-CALL-W611PMRK-KOLLI SECTION.                                         
174100                                                                          
174200     MOVE RAD-IDLEVNR-KOLLI    TO PMRK-IDLEVNR                            
174300     MOVE RAD-IDOKOLLI         TO PMRK-IDOKOLLI                           
174400     MOVE ZERO                 TO PMRK-IDLOPNRM                           
174500                                  PMRK-IDRADNR                            
174600     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
174700                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
174800     .                                                                    
174900     EJECT                                                                
175000 S09-CALL-W611PMRK-PARTI SECTION.                                         
175100     MOVE SPACE                TO PMRK-IDLEVNR                            
175200     MOVE ZERO                 TO PMRK-IDOKOLLI                           
175300     MOVE ART-IDLOPNRM         TO PMRK-IDLOPNRM                           
175400     MOVE RAD-IDRADNR          TO PMRK-IDRADNR                            
175500     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
175600                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
175700     .                                                                    
175800 MFS-RENSA-FAELT-UT  SECTION.                                             
175900     MOVE SPACES                 TO RESP-W60144O1-OUT                     
176000     .                                                                    
176100* --- IMS SEKTIONER ---                                                   
176200 IMS-ISRT-ALT1-MSG-6191  SECTION.                                         
176300     MOVE SPACE TO GODK-STATUSKODER                                       
176400     CALL  CBLTDLI  USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
176500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
176600     PERFORM IMS-STATUSKONTROLL                                           
176700     .                                                                    
176800     SKIP3                                                                
176900 IMS-PURG-ALT1-MSG-6191  SECTION.                                         
177000     MOVE SPACE TO GODK-STATUSKODER                                       
177100     CALL  CBLTDLI  USING PURG ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
177200     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
177300     PERFORM IMS-STATUSKONTROLL                                           
177400     .                                                                    
177500     EJECT                                                                
177600 IMS-GU-INLA1-INLA11 SECTION.                                             
177700     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
177800          DELIMITED BY SIZE INTO SSA1                                     
177900     MOVE '  GE' TO GODK-STATUSKODER                                      
178000     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA1 SSA1                    
178100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
178200     PERFORM IMS-STATUSKONTROLL                                           
178300     .                                                                    
178400     SKIP3                                                                
178500 IMS-GNP-INLA1-INLA21 SECTION.                                            
178600     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-KOLLI-X                       
178700                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
178800          DELIMITED BY SIZE INTO SSA1                                     
178900     MOVE '  GE' TO GODK-STATUSKODER                                      
179000     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA2 SSA1                   
179100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
179200     PERFORM IMS-STATUSKONTROLL                                           
179300     .                                                                    
179400     EJECT                                                                
179500 IMS-GU-INLA2-INLA11 SECTION.                                             
179600     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
179700                    '&IDDC     =' W-IDDC-X ')'                            
179800          DELIMITED BY SIZE INTO SSA1                                     
179900     MOVE '  GE' TO GODK-STATUSKODER                                      
180000     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA1 SSA1                    
180100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
180200     PERFORM IMS-STATUSKONTROLL                                           
180300     .                                                                    
180400     SKIP3                                                                
180500 IMS-GNP-INLA2-INLA21-FIRST SECTION.                                      
180600     MOVE 'W6INLA21*F'          TO SSA1                                   
180700     MOVE '  GE' TO GODK-STATUSKODER                                      
180800     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
180900     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
181000     PERFORM IMS-STATUSKONTROLL                                           
181100     .                                                                    
181200     SKIP3                                                                
181300 IMS-GNP-INLA2-INLA21 SECTION.                                            
181400     MOVE 'W6INLA21'          TO SSA1                                     
181500     MOVE '  GE' TO GODK-STATUSKODER                                      
181600     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
181700     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     EJECT                                                                
182100 IMS-GNP-INLA2-INLA21-KVAL SECTION.                                       
182200     STRING 'W6INLA21*F(IDRADNR >=' W-IDRADNR-X ')'                       
182300          DELIMITED BY SIZE INTO SSA1                                     
182400     MOVE '  GE' TO GODK-STATUSKODER                                      
182500     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
182600     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
182700     PERFORM IMS-STATUSKONTROLL                                           
182800     .                                                                    
182900     SKIP3                                                                
183000 IMS-GU-INLA2-INLA21 SECTION.                                             
183100     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
183200          DELIMITED BY SIZE INTO SSA1                                     
183300     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
183400          DELIMITED BY SIZE INTO SSA2                                     
183500     MOVE '  GE' TO GODK-STATUSKODER                                      
183600     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA2 SSA1 SSA2               
183700     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
183800     PERFORM IMS-STATUSKONTROLL                                           
183900     .                                                                    
184000     EJECT                                                                
184100 IMS-GU-INLA2-INLA11-2 SECTION.                                           
184200     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
184300          DELIMITED BY SIZE INTO SSA1                                     
184400     MOVE '  GE' TO GODK-STATUSKODER                                      
184500     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA1 SSA1                    
184600     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
184700     PERFORM IMS-STATUSKONTROLL                                           
184800     .                                                                    
184900     EJECT                                                                
185000 IMS-GNP-INLA2-INLA21-2 SECTION.                                          
185100     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
185200          DELIMITED BY SIZE INTO SSA1                                     
185300     MOVE '  GE' TO GODK-STATUSKODER                                      
185400     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
185500     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
185600     PERFORM IMS-STATUSKONTROLL                                           
185700     .                                                                    
185800     EJECT                                                                
185900 IMS-GNP-INLA2-INLA21-LAST SECTION.                                       
186000     MOVE 'W6INLA21*L' TO SSA1                                            
186100     MOVE '  GE' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
186300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600     SKIP3                                                                
186700 IMS-GHNP-INLA2-INLA21 SECTION.                                           
186800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
186900          DELIMITED BY SIZE INTO SSA1                                     
187000     MOVE '    ' TO GODK-STATUSKODER                                      
187100     CALL CBLTDLI USING GHNP INLA2-PCB DLI-IO-AREA2 SSA1                  
187200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500     EJECT                                                                
187600 IMS-DLET-INLA2-INLA21 SECTION.                                           
187700     MOVE '    ' TO GODK-STATUSKODER                                      
187800     CALL CBLTDLI USING DLET INLA2-PCB DLI-IO-AREA2                       
187900     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
188000     PERFORM IMS-STATUSKONTROLL                                           
188100     .                                                                    
188200     SKIP3                                                                
188300 IMS-REPL-INLA2-INLA21 SECTION.                                           
188400     MOVE '    ' TO GODK-STATUSKODER                                      
188500     CALL CBLTDLI USING REPL INLA2-PCB DLI-IO-AREA2                       
188600     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     EJECT                                                                
189000 IMS-ISRT-INLA2-INLA21 SECTION.                                           
189100     MOVE 'W6INLA21' TO SSA1                                              
189200     MOVE '    ' TO GODK-STATUSKODER                                      
189300     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-AREA2 SSA1                  
189400     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
189500     PERFORM IMS-STATUSKONTROLL                                           
189600     .                                                                    
189700     SKIP3                                                                
189800 IMS-GU-INLC-INLC01 SECTION.                                              
189900     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
190000          DELIMITED BY SIZE INTO SSA1                                     
190100     MOVE '  GE' TO GODK-STATUSKODER                                      
190200     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA3 SSA1                     
190300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
190400     PERFORM IMS-STATUSKONTROLL                                           
190500     .                                                                    
190600     EJECT                                                                
190700 IMS-GU-PLAA-PLAA11 SECTION.                                              
190800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
190900          DELIMITED BY SIZE INTO SSA1                                     
191000     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
191100          DELIMITED BY SIZE INTO SSA2                                     
191200     MOVE '  GE' TO GODK-STATUSKODER                                      
191300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA3 SSA1 SSA2                
191400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
191500     PERFORM IMS-STATUSKONTROLL                                           
191600     .                                                                    
191700     EJECT                                                                
191800 IMS-GU-ARTD-ARTD11 SECTION.                                              
191900     STRING 'WLARTD01*P(IDARTNR  =' W-IDARTNR-X ')'                       
192000          DELIMITED BY SIZE INTO SSA1                                     
192100     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
192200          DELIMITED BY SIZE INTO SSA2                                     
192300     MOVE '  GE' TO GODK-STATUSKODER                                      
192400     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA3 SSA1 SSA2                
192500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
192600     PERFORM IMS-STATUSKONTROLL                                           
192700     .                                                                    
192800     SKIP3                                                                
192900 IMS-GNP-ARTD-ARTD11 SECTION.                                             
193000     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
193100          DELIMITED BY SIZE INTO SSA1                                     
193200     MOVE '  GE' TO GODK-STATUSKODER                                      
193300     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA3 SSA1                    
193400     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     .                                                                    
193700     SKIP3                                                                
193800 IMS-GNP-ARTD-ARTD11-FIRST SECTION.                                       
193900     STRING 'WLARTD11*F(IDDC     =' W-IDDC-X ')'                          
194000          DELIMITED BY SIZE INTO SSA1                                     
194100     MOVE '  GE' TO GODK-STATUSKODER                                      
194200     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA3 SSA1                    
194300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
194400     PERFORM IMS-STATUSKONTROLL                                           
194500     .                                                                    
194600     EJECT                                                                
194700 IMS-GU-ARTS11 SECTION.                                                   
194800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
195100          DELIMITED BY SIZE INTO SSA2                                     
195200     MOVE '  GE' TO GODK-STATUSKODER                                      
195300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
195400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     SKIP3                                                                
195800 IMS-GU-UPFA-01  SECTION.                                                 
195900     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
196000          DELIMITED BY SIZE INTO SSA1                                     
196100     MOVE '  GE' TO GODK-STATUSKODER                                      
196200     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-UPFA SSA1                      
196300     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     SKIP2                                                                
196700 IMS-GNP-UPFA11 SECTION.                                                  
196800     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
196900          DELIMITED BY SIZE INTO SSA1                                     
197000     MOVE 'W6UPFA11 ' TO SSA2                                             
197100     MOVE '  GE' TO GODK-STATUSKODER                                      
197200     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
197300     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
197400     PERFORM IMS-STATUSKONTROLL                                           
197500     .                                                                    
197600     SKIP3                                                                
197700 IMS-GNP-UPFA12 SECTION.                                                  
197800     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
197900          DELIMITED BY SIZE INTO SSA1                                     
198000     MOVE 'W6UPFA12 ' TO SSA2                                             
198100     MOVE '  GE' TO GODK-STATUSKODER                                      
198200     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
198300     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
198400     PERFORM IMS-STATUSKONTROLL                                           
198500     .                                                                    
198600     SKIP3                                                                
198700 IMS-GU-ARTC11 SECTION.                                                   
198800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
198900          DELIMITED BY SIZE INTO SSA1                                     
199000     MOVE 'WLARTC11'       TO SSA2                                        
199100     MOVE '  ' TO GODK-STATUSKODER                                        
199200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC SSA1 SSA2                 
199300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
199400     PERFORM IMS-STATUSKONTROLL                                           
199500     .                                                                    
199600     SKIP3                                                                
199700 IMS-GU-WDB601 SECTION.                                                   
199800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
199900          DELIMITED BY SIZE INTO SSA1                                     
200000     MOVE '  GE' TO GODK-STATUSKODER                                      
200100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
200200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     IF SEGMENT-SAKNAS                                                    
200500        MOVE SPACE TO DCS-KDDC                                            
200600     END-IF                                                               
200700     .                                                                    
200800     SKIP3                                                                
200900 IMS-STATUSKONTROLL SECTION.                                              
201000     SET STATUS-IX TO 1                                                   
201100     SEARCH GODK-STATUS                                                   
201200       AT END                                                             
201300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
201400         DELIMITED BY SIZE INTO FELTEXT                                   
201500         CALL FELLOG                                                      
201600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
201700         CONTINUE                                                         
201800     END-SEARCH                                                           
201900     .                                                                    
