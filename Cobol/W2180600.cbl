000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2180600.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   92/09/03.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BERÄKNAR DISPONIBELDATUM "TIDISPIN" FÖR R31-OR                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*        PROGRAMMET UPPDATERAR WLXXCT (WDR5)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     EJECT                                                                
003100                                                                          
003200*---------------------------------------------------------------          
003300*   WORKING STORAGE                                                       
003400*--------------------------------------------------------------           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900 01  FILLER                  PIC X(16) VALUE 'WORKING STORAGE'.           
004000                                                                          
004100     SKIP2                                                                
004200 77  IDPGM                       PIC X(8)    VALUE 'W2180600'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  AKTIV                       PIC X       VALUE 'A'.                   
004600 77  PASSIV                      PIC X       VALUE 'P'.                   
004700                                                                          
004800 01  ARBETSFALT.                                                          
004900                                                                          
005000     03 WS-TIFINLV-AAMMDD        PIC  9(6)   VALUE ZERO.                  
005100     03 WS-KVDAGAR-INLEV         PIC S9(3)   VALUE ZERO COMP-3.           
005200     03 WS-KVPB-TOT         PIC S9(9)V9(1)   VALUE ZERO COMP-3.           
005300     03 WS-KVAKS-TOT        PIC S9(9)V9(1)   VALUE ZERO COMP-3.           
005400     03 WS-VECKOBEHOV       PIC S9(9)V9(1)   VALUE ZERO COMP-3.           
005500     03 WS-DD-PLUS-INLEV         PIC S9(7)   VALUE ZERO COMP-3.           
005600     03 WS-BEFINTLIG-TIDISPIN    PIC S9(7)   VALUE ZERO COMP-3.           
005700     03 WS-UPPNADD-TIDISPIN      PIC S9(7)   VALUE ZERO COMP-3.           
005800                                                                          
005900     03 WS-MOTTAGANDE-LAGER     PIC S9(1)   VALUE ZERO COMP-3.            
006000                                                                          
006100     03 WS-PERIOD-TIP            PIC  9(1)   VALUE ZERO.                  
006200                                                                          
006300     03 WS-IN-DATUM              PIC  9(6)   VALUE ZERO.                  
006400     03 WS-UT-DATUM              PIC  9(6)   VALUE ZERO.                  
006500     03 WS-ANTAL-PLUSVECKOR      PIC  9(3)   VALUE ZERO.                  
006600                                                                          
006700     03 WS-TMPX-YYMMDD      PIC S9(7) PACKED-DECIMAL VALUE ZERO.          
006800     03 WS-TMPY-YYMMDD      PIC S9(7) PACKED-DECIMAL VALUE ZERO.          
006900     03 ANTAL-ARTIKEL-SAKNAS     PIC S9(9)   VALUE ZERO COMP-3.           
007000     03 ANTAL-KDERS-UTG-OVER-0   PIC S9(9)   VALUE ZERO COMP-3.           
007100     03 ANTAL-KDERS-OVER-0       PIC S9(9)   VALUE ZERO COMP-3.           
007200     03 ANTAL-UPPDAT             PIC S9(9)   VALUE ZERO COMP-3.           
007300     03 DAGENS-DATUM-PACK        PIC S9(7)   VALUE ZERO COMP-3.           
007400                                                                          
007500 01  CHKP-VAR.                                                            
007600     03  CHKP-MSG-IO-AREA-LENGTH PIC S9(9)   VALUE +32 COMP SYNC.         
007700     03  CHKP-MSG-IO-AREA        PIC X(32)   VALUE SPACE.                 
007800     03  CHKP-AREA-LENGTH        PIC S9(9)   VALUE +32 COMP SYNC.         
007900     03  CHKP-AREA               PIC X(32)   VALUE SPACE.                 
008000     03  CHKP-ANT                PIC S9(3)   VALUE +0.                    
008100     03  CHKP-MAX                PIC S9(3)   VALUE +050.                  
008200                                                                          
008300     SKIP2                                                                
008400 01  FELTEXT.                                                             
008500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008700     EJECT                                                                
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400                                                                          
009500                                                                          
009600 01  DATUM.                                                               
009700     03  WS-TIAAVVD-GRP.                                                  
009800         05  WS-TIAAVV-GRP.                                               
009900            07 WS-TIAA-VECKA         PIC 9(2).                            
010000            07 WS-TIIVV              PIC 9(2).                            
010100         05  WS-TIAAVV               REDEFINES WS-TIAAVV-GRP              
010200                                     PIC 9(4).                            
010300         05  WS-TID                  PIC 9.                               
010400     03 WS-TIAAVVD-GRP-R             REDEFINES WS-TIAAVVD-GRP             
010500                                     PIC 9(5).                            
010600*-------------------------------------------------------------            
010700*      SWICHAR                                                            
010800*-------------------------------------------------------------            
010900                                                                          
011000 01  FILLER                     PIC X(16) VALUE 'SWITCHAR'.               
011100                                                                          
011200 01  SWITCHAR.                                                            
011300     03 BERAKNA-TIDISPIN-SW     PIC X VALUE ' '.                          
011400        88  BERAKNA-TIDISPIN          VALUE 'J'.                          
011500        88  BERAKNA-INTE-TIDISPIN     VALUE 'N'.                          
011600                                                                          
011700     03 VECKOBEHOV-TACKT-SW      PIC X VALUE ' '.                         
011800        88  VECKOBEHOV-TACKT          VALUE 'J'.                          
011900        88  VECKOBEHOV-EJ-TACKT       VALUE 'N'.                          
012000     EJECT                                                                
012100*      --- VALID IDDC CODES                                               
012200*                                                                         
012300*01    -COPY WWDCKONS                                                     
012400       EJECT                                                              
012500                                                                          
012600*--------------------------------------------------------------           
012700*      DYNAMISKA SUBPROGRAM                                               
012800*--------------------------------------------------------------           
012900 01  FILLER                      PIC X(16) VALUE 'SUBPROGRAM'.            
013000                                                                          
013100 01  DYNAMISKA-SUBPROGRAM.                                                
013200*                                                                         
013300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013600     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
013700     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
014000                                                                          
014100*01  -COPY WDATAREA.                                                      
014200                                                                          
014300 01  FILLER                      PIC X(16)   VALUE 'WORKDAY'.             
014400                                                                          
014500*01  -COPY WORKAREA.                                                      
014600                                                                          
014700 01  FILLER                      PIC X(16)   VALUE 'W009VADD'.            
014800                                                                          
014900 01  W009VADD-AREA.                                                       
015000     03 DATUM-AAVV               PIC S9(5) COMP-3.                        
015100     03 ANTAL                    PIC S9(3) COMP-3.                        
015200                                                                          
015300     EJECT                                                                
015400                                                                          
015500*--------------------------------------------------------------           
015600*      IMS                                                                
015700*--------------------------------------------------------------           
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100                                                                          
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400                                                                          
016500     03  W-KDSEGKEY-X.                                                    
016600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016700                                                                          
016800     03  W-IDDC-X.                                                        
016900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017000                                                                          
017100     03  W-2241KEY-X.                                                     
017200         05  W-IDHTYP            PIC X(4)     VALUE '2241'.               
017300         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
017400                                                                          
017500     03  W-2242KEY-X.                                                     
017600         05  W-2242-IDARTNR      PIC S9(9)    COMP-3.                     
017700         05  W-LOW-VALUE         PIC X(5)     VALUE LOW-VALUE.            
017800     SKIP2                                                                
017900*    --- STATUS-KOD FRÅN IMS                                              
018000 01  STATUS-WS                   PIC XX.                                  
018100     88  SEGMENT-FINNS                       VALUE '  '.                  
018200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018500     88  IMS-EJ-OK                           VALUE 'XD'.                  
018600     SKIP2                                                                
018700 01  GODK-STATUSKODER.                                                    
018800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018900     SKIP3                                                                
019000 01  SSA1                        PIC X(64).                               
019100 01  SSA2                        PIC X(64).                               
019200 01  SSA3                        PIC X(64).                               
019300     EJECT                                                                
019400*    --- IMS FUNKTIONSKODER                                               
019500*01  -COPY W0003                                                          
019600     EJECT                                                                
019700                                                                          
019800*--------------------------------------------------------------           
019900*         DLI  AREOR                                                      
020000*--------------------------------------------------------------           
020100*                                                                         
020200*    ---  DLI INPUT-OUTPUT AREA                                           
020300                                                                          
020400 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
020500 01  DLI-IO-ARTC01.                                                       
020600*    03  -COPY WDK601                                                     
020700     EJECT                                                                
020800                                                                          
020900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
021000 01  DLI-IO-ARTC11.                                                       
021100*    03  -COPY WDK611                                                     
021200     EJECT                                                                
021300                                                                          
021400 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
021500 01  DLI-IO-ARTS01.                                                       
021600*    03  -COPY WDK701                                                     
021700     EJECT                                                                
021800                                                                          
021900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
022000 01  DLI-IO-ARTS11.                                                       
022100*    03  -COPY WDK711                                                     
022200     EJECT                                                                
022300                                                                          
022400 01  FILLER         PIC X(24) VALUE 'DLI-IO-GX2241'.                      
022500 01  DLI-IO-GX2241.                                                       
022600     03  IO-GX2241      PIC X(30) VALUE SPACE.                            
022700     EJECT                                                                
022800                                                                          
022900 01  FILLER         PIC X(24) VALUE 'DLI-IO-GX2242'.                      
023000 01  DLI-IO-GX2242.                                                       
023100*    03  -COPY WDGX2242                                                   
023200     EJECT                                                                
023300                                                                          
023400*---------------------------------------------------------------          
023500*        LINKAGE SECTION                                                  
023600*---------------------------------------------------------------          
023700*                                                                         
023800* 01  FILLER                   PIC X(16) VALUE 'LINKAGE SECTION'          
023900                                                                          
024000 LINKAGE SECTION.                                                         
024100                                                                          
024200*01  -COPY W0009   -PRE MSG-                                              
024300     EJECT                                                                
024400*01  -COPY W0008  -PRE ARTC-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE ARTS-                                              
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000*01  -COPY W0008  -PRE XXCT-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300                                                                          
025400*------------------------------------------------------------             
025500*   PROCDURE DIVISION                                                     
025600*-----------------------------------------------------------              
025700                                                                          
025800 PROCEDURE DIVISION  USING MSG-PCB                                        
025900                           ARTC-PCB ARTS-PCB                              
026000                           XXCT-PCB.                                      
026100     ENTRY 'DLITCBL' USING MSG-PCB                                        
026200                           ARTC-PCB ARTS-PCB                              
026300                           XXCT-PCB.                                      
026400                                                                          
026500     SKIP2                                                                
026600     PERFORM A-INIT                                                       
026700     PERFORM IMS-GET-2241                                                 
026800     IF SEGMENT-FINNS                                                     
026900        PERFORM IMS-GET-2242                                              
027000        IF SEGMENT-FINNS                                                  
027100           PERFORM UNTIL SEGMENT-SAKNAS                                   
027200              PERFORM IMS-DLET-2242                                       
027300              PERFORM AA-NOLLSTALL                                        
027400              PERFORM B-SAMLA-DATA                                        
027500              PERFORM C-SKALL-TIDISPIN-BERAKNAS                           
027600              IF BERAKNA-TIDISPIN                                         
027700                 PERFORM D-TACKER-INLEV-VECKOBEHOV                        
027800                 IF VECKOBEHOV-TACKT                                      
027900                    PERFORM E-BERAKNA-UPPDATERA-TIDISPIN                  
028000                 END-IF                                                   
028100              END-IF                                                      
028200              ADD +1 TO CHKP-ANT                                          
028300              IF CHKP-ANT >= CHKP-MAX                                     
028400                 PERFORM IMS-CHECKPOINT                                   
028500                 PERFORM IMS-GET-2241                                     
028600                 MOVE +1 TO CHKP-ANT                                      
028700              END-IF                                                      
028800              PERFORM AA-NOLLSTALL                                        
028900              PERFORM IMS-GET-2242                                        
029000           END-PERFORM                                                    
029100        END-IF                                                            
029200     END-IF                                                               
029300     PERFORM Z-FINIT                                                      
029400                                                                          
029500     MOVE ZERO TO RETURN-CODE                                             
029600     GOBACK                                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 A-INIT SECTION.                                                          
030100     SKIP2                                                                
030200                                                                          
030300     PERFORM IMS-RESTART                                                  
030400     MOVE +1 TO CHKP-ANT                                                  
030500                                                                          
030600     ACCEPT DAGENS-DATUM       FROM DATE                                  
030700     MOVE DAGENS-DATUM TO DAGENS-DATUM-PACK                               
030800                                                                          
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 AA-NOLLSTALL SECTION.                                                    
031300     SKIP2                                                                
031400                                                                          
031500     MOVE JA             TO BERAKNA-TIDISPIN-SW                           
031600                                                                          
031700     MOVE NEJ            TO VECKOBEHOV-TACKT-SW                           
031800                                                                          
031900     MOVE ZERO           TO WS-KVPB-TOT                                   
032000                            WS-KVAKS-TOT                                  
032100                            WS-KVDAGAR-INLEV                              
032200                            WS-DD-PLUS-INLEV                              
032300                            WS-BEFINTLIG-TIDISPIN                         
032400                            WS-UPPNADD-TIDISPIN                           
032500     .                                                                    
032600     EJECT                                                                
032700                                                                          
032800                                                                          
032900 B-SAMLA-DATA SECTION.                                                    
033000                                                                          
033100                                                                          
033200* LÄSA WDK601                                                             
033300                                                                          
033400     MOVE 2242-IDARTNR TO W-IDARTNR                                       
033500     PERFORM IMS-GU-ARTC01                                                
033600     IF SEGMENT-FINNS                                                     
033700        IF ART-KDERS-UTG > 0                                              
033800           ADD +1                   TO ANTAL-KDERS-OVER-0                 
033900        END-IF                                                            
034000     ELSE                                                                 
034100        MOVE NEJ                    TO BERAKNA-TIDISPIN-SW                
034200        ADD +1                      TO ANTAL-ARTIKEL-SAKNAS               
034300     END-IF                                                               
034400                                                                          
034500     IF SEGMENT-FINNS AND ART-KDERS-UTG = 0                               
034600                                                                          
034700*LÄSA WDK611                                                              
034800                                                                          
034900        PERFORM IMS-GHNP-ARTC11                                           
035000        MOVE CLAG-TIDISPIN     TO WS-BEFINTLIG-TIDISPIN                   
035100     END-IF                                                               
035200                                                                          
035300     .                                                                    
035400     EJECT                                                                
035500                                                                          
035600                                                                          
035700 C-SKALL-TIDISPIN-BERAKNAS SECTION.                                       
035800                                                                          
035900     IF BERAKNA-TIDISPIN                                                  
036000                                                                          
036100        IF ART-KDERS-UTG > 0                                              
036200           MOVE NEJ                    TO BERAKNA-TIDISPIN-SW             
036300           ADD +1                      TO ANTAL-KDERS-UTG-OVER-0          
036400        END-IF                                                            
036500                                                                          
036600        IF CLAG-KDERS > 10                                                
036700           MOVE NEJ                    TO BERAKNA-TIDISPIN-SW             
036800           ADD +1                      TO ANTAL-KDERS-OVER-0              
036900        END-IF                                                            
037000                                                                          
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500                                                                          
037600                                                                          
037700 D-TACKER-INLEV-VECKOBEHOV SECTION.                                       
037800                                                                          
037900* SUMMERA PB                                                              
038000     PERFORM S08-SUMMERA-PB                                               
038100                                                                          
038200* VECKOBEHOV                                                              
038300     COMPUTE WS-VECKOBEHOV = WS-KVPB-TOT / 4.33                           
038400                                                                          
038500* VOLYMVÄRDESKLASS                                                        
038600     IF CLAG-KDVVKL = 5                                                   
038700        COMPUTE WS-VECKOBEHOV = WS-VECKOBEHOV * 0.5                       
038800     END-IF                                                               
038900                                                                          
039000*SUMMERA AKS                                                              
039100     COMPUTE WS-KVAKS-TOT =                                               
039200                    CLAG-KVAKS-CDC +                                      
039300                    CLAG-KVAKS-T   +                                      
039400                    CLAG-KVAKS-PAV                                        
039500                                                                          
039600* VECKOBEHOV UPPNÅTT?                                                     
039700     IF WS-KVAKS-TOT > 0                                                  
039800        IF WS-KVAKS-TOT >= WS-VECKOBEHOV                                  
039900           MOVE JA            TO VECKOBEHOV-TACKT-SW                      
040000        END-IF                                                            
040100     END-IF                                                               
040200                                                                          
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600                                                                          
040700 E-BERAKNA-UPPDATERA-TIDISPIN SECTION.                                    
040800                                                                          
040900     PERFORM EA-BERAKNA-INLEVTID                                          
041000     PERFORM EB-KOLLA-MOTTAGANDE-LAGER                                    
041100     PERFORM EC-BERAKNA-ANDRA-LAGRET                                      
041200     PERFORM ED-JAMFOR-MED-BEF-UPPDATERA                                  
041300     .                                                                    
041400     EJECT                                                                
041500                                                                          
041600                                                                          
041700 EA-BERAKNA-INLEVTID SECTION.                                             
041800                                                                          
041900     COMPUTE WS-KVDAGAR-INLEV = CLAG-KVDAGAR-INLEV + 1                    
042000     MOVE 2                    TO WORK-KDCALL                             
042100     MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-FOM                       
042200     MOVE WS-KVDAGAR-INLEV     TO WORK-KVWORKD                            
042300     MOVE WC-CDC-SE            TO WORK-IDDC                               
042400     CALL WORKDAY USING WORK-KDCALL                                       
042500                        WORK-DATE-AREA                                    
042600                        WORK-KDSVAR                                       
042700     IF WORK-KDSVAR-OK                                                    
042800        MOVE WORK-TIAAMMDD-TOM TO WS-DD-PLUS-INLEV                        
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200                                                                          
043300 EB-KOLLA-MOTTAGANDE-LAGER SECTION.                                       
043400                                                                          
043500     IF CLAG-KDGK = +1 OR CLAG-KDGK = +2                                  
043600        IF CLAG-KDGK = +1                                                 
043700           MOVE +1          TO WS-MOTTAGANDE-LAGER                        
043800        ELSE                                                              
043900           IF CLAG-KDGK = +2                                              
044000                 MOVE +2    TO WS-MOTTAGANDE-LAGER                        
044100           END-IF                                                         
044200        END-IF                                                            
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600                                                                          
044700 EC-BERAKNA-ANDRA-LAGRET SECTION.                                         
044800                                                                          
044900     IF CLAG-KDGK = 1 OR CLAG-KDGK = 2                                    
045000        IF WS-MOTTAGANDE-LAGER = 1                                        
045100*C1                                                                       
045200           MOVE WS-DD-PLUS-INLEV   TO WS-UPPNADD-TIDISPIN                 
045300        ELSE                                                              
045400           IF WS-MOTTAGANDE-LAGER = 2                                     
045500              IF CLAG-KDLTK = 1                                           
045600*C1                                                                       
045700                 MOVE WS-DD-PLUS-INLEV   TO WS-IN-DATUM                   
045800                 MOVE 1                  TO WS-ANTAL-PLUSVECKOR           
045900                 PERFORM S01-BERAKNA-PLUSVECKOR                           
046000                 MOVE WS-UT-DATUM        TO WS-UPPNADD-TIDISPIN           
046100              END-IF                                                      
046200           END-IF                                                         
046300        END-IF                                                            
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700                                                                          
046800 ED-JAMFOR-MED-BEF-UPPDATERA SECTION.                                     
046900                                                                          
047000     MOVE WS-BEFINTLIG-TIDISPIN   TO TMP1-YYMMDD                          
047100     MOVE DAGENS-DATUM-PACK       TO TMP2-YYMMDD                          
047200     PERFORM WY2000P1                                                     
047300     MOVE TMP1-YYMMDD             TO WS-TMPX-YYMMDD                       
047400     MOVE TMP2-YYMMDD             TO WS-TMPY-YYMMDD                       
047500     MOVE WS-UPPNADD-TIDISPIN     TO TMP1-YYMMDD                          
047600     MOVE WS-BEFINTLIG-TIDISPIN   TO TMP2-YYMMDD                          
047700     PERFORM WY2000P1                                                     
047800     IF WS-TMPX-YYMMDD <= WS-TMPY-YYMMDD                                  
047900     OR TMP1-YYMMDD < TMP2-YYMMDD                                         
048000                                                                          
048100        MOVE WS-UPPNADD-TIDISPIN TO CLAG-TIDISPIN                         
048200        PERFORM IMS-REPL-CLAG                                             
048300        ADD +1                   TO ANTAL-UPPDAT                          
048400                                                                          
048500     END-IF                                                               
048600                                                                          
048700     .                                                                    
048800     EJECT                                                                
048900                                                                          
049000                                                                          
049100 Z-FINIT SECTION.                                                         
049200                                                                          
049300     DISPLAY 'ARTIKEL SAKNAS PÅ WDK6 = ' ANTAL-ARTIKEL-SAKNAS             
049400     DISPLAY 'KDERS-UTG ÖVER 0       = ' ANTAL-KDERS-UTG-OVER-0           
049500     DISPLAY 'KDERS ÖVER 0           = ' ANTAL-KDERS-OVER-0               
049600     DISPLAY 'UPPDAT                 = ' ANTAL-UPPDAT                     
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000                                                                          
050100 S01-BERAKNA-PLUSVECKOR SECTION.                                          
050200                                                                          
050300* ANVÄNDS VID OMRÄKNING FRÅN AAMMDD TILL AAVVD                            
050400   SKIP2                                                                  
050500     MOVE WS-IN-DATUM         TO DAT-I-TIDATUM                            
050600     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
050700     CALL WDATKONV       USING DAT-KDDATFORM                              
050800                               DAT-I-TIDATUM                              
050900                               DAT-O-TIDATUM                              
051000                               DAT-KDSVAR                                 
051100     IF DAT-KDSVAR-OK                                                     
051200        MOVE DAT-TIAAVVD      TO WS-TIAAVVD-GRP-R                         
051300        MOVE WS-TIAAVV         TO DATUM-AAVV                              
051400        MOVE WS-ANTAL-PLUSVECKOR TO ANTAL                                 
051500        CALL W009VADD USING DATUM-AAVV ANTAL                              
051600        MOVE DATUM-AAVV            TO WS-TIAAVV                           
051700        MOVE WS-TIAAVVD-GRP-R      TO DAT-I-TIDATUM                       
051800        MOVE 'AAVVD'               TO DAT-KDDATFORM                       
051900        CALL WDATKONV       USING DAT-KDDATFORM                           
052000                                  DAT-I-TIDATUM                           
052100                                  DAT-O-TIDATUM                           
052200                                  DAT-KDSVAR                              
052300        IF DAT-KDSVAR-OK                                                  
052400           MOVE DAT-TIAAMMDD           TO WS-UT-DATUM                     
052500        END-IF                                                            
052600     END-IF                                                               
052700     .                                                                    
052800                                                                          
052900 S08-SUMMERA-PB SECTION.                                                  
053000                                                                          
053100     COMPUTE WS-KVPB-TOT =                                                
053200             CLAG-KVPB-SEP        +                                       
053300             CLAG-KVPB-TPO                                                
053400                                                                          
053500     PERFORM IMS-GU-ARTS01                                                
053600     IF SEGMENT-FINNS                                                     
053700        PERFORM IMS-GNP-ARTS11                                            
053800        PERFORM UNTIL SEGMENT-SAKNAS                                      
053900           IF SLAG-KDREFSTA = AKTIV                                       
054000              MOVE SLAG-IDDC TO W-IDDC                                    
054100              IF SLAG-FLREFILL = JA                                       
054200                 ADD SLAG-KVPB-REF                                        
054300                             TO WS-KVPB-TOT                               
054400              END-IF                                                      
054500           END-IF                                                         
054600           PERFORM IMS-GNP-ARTS11                                         
054700        END-PERFORM                                                       
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200                                                                          
055300* --- IMS SEKTIONER ---                                                   
055400     SKIP3                                                                
055500     EJECT                                                                
055600 IMS-GU-ARTC01 SECTION.                                                   
055700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
055800          DELIMITED BY SIZE INTO SSA1                                     
055900     MOVE '  GE' TO GODK-STATUSKODER                                      
056000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
056100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056200     PERFORM IMS-STATUSKONTROLL                                           
056300     .                                                                    
056400     EJECT                                                                
056500                                                                          
056600                                                                          
056700 IMS-GHNP-ARTC11 SECTION.                                                 
056800     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
056900          DELIMITED BY SIZE INTO SSA1                                     
057000     MOVE '  GE' TO GODK-STATUSKODER                                      
057100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC11  SSA1                 
057200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057300     PERFORM IMS-STATUSKONTROLL                                           
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700                                                                          
057800 IMS-REPL-CLAG SECTION.                                                   
057900                                                                          
058000     MOVE '  ' TO GODK-STATUSKODER                                        
058100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
058200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 IMS-GU-ARTS01 SECTION.                                                   
058800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
058900          DELIMITED BY SIZE INTO SSA1                                     
059000     MOVE '  GE' TO GODK-STATUSKODER                                      
059100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS01 SSA1                    
059200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700                                                                          
059800  IMS-GNP-ARTS11 SECTION.                                                 
059900     MOVE 'WLARTS11  '   TO SSA1                                          
060000     MOVE '  GE' TO GODK-STATUSKODER                                      
060100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11  SSA1                  
060200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     EJECT                                                                
060600                                                                          
060700                                                                          
060800 IMS-GET-2241 SECTION.                                                    
060900     STRING 'WLXXCT01(WDG3KEY  =' W-2241KEY-X ')'                         
061000          DELIMITED BY SIZE INTO SSA1                                     
061100     MOVE '  GE' TO GODK-STATUSKODER                                      
061200     CALL CBLTDLI USING GU XXCT-PCB DLI-IO-GX2241 SSA1                    
061300     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
061400     PERFORM IMS-STATUSKONTROLL                                           
061500     .                                                                    
061600     EJECT                                                                
061700 IMS-GET-2242 SECTION.                                                    
061800     MOVE 'WLXXCT11  '      TO SSA1                                       
061900     MOVE '  GE' TO GODK-STATUSKODER                                      
062000     CALL CBLTDLI USING GHNP XXCT-PCB DLI-IO-GX2242 SSA1                  
062100     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     SKIP3                                                                
062500 IMS-DLET-2242 SECTION.                                                   
062600                                                                          
062700     MOVE '  ' TO GODK-STATUSKODER                                        
062800     CALL CBLTDLI USING DLET XXCT-PCB DLI-IO-GX2242                       
062900     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
063000     PERFORM IMS-STATUSKONTROLL                                           
063100     .                                                                    
063200 IMS-RESTART SECTION.                                                     
063300     SKIP2                                                                
063400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
063500     MOVE '  ' TO GODK-STATUSKODER                                        
063600     CALL CBLTDLI USING XRST MSG-PCB                                      
063700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
063800                        CHKP-AREA-LENGTH CHKP-AREA                        
063900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     EJECT                                                                
064300 IMS-CHECKPOINT SECTION.                                                  
064400     SKIP2                                                                
064500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
064600     MOVE '  XD' TO GODK-STATUSKODER                                      
064700     CALL CBLTDLI USING CHKP MSG-PCB                                      
064800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
064900                        CHKP-AREA-LENGTH CHKP-AREA                        
065000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065100     PERFORM IMS-STATUSKONTROLL                                           
065200                                                                          
065300     IF IMS-EJ-OK                                                         
065400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
065500       DISPLAY FELTEXT                                                    
065600       CALL FELLOG                                                        
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 IMS-STATUSKONTROLL SECTION.                                              
066100     SKIP2                                                                
066200     SET STATUS-IX TO 1                                                   
066300     SEARCH GODK-STATUS                                                   
066400       AT END                                                             
066500         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
066600         DISPLAY FELTEXT                                                  
066700         CALL FELLOG                                                      
066800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066900         CONTINUE                                                         
067000     END-SEARCH                                                           
067100     .                                                                    
067200     EJECT                                                                
067300*    -COPY WY2000P1                                                       
