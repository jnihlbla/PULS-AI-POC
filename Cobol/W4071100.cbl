000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4071100.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   95/04/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        MPP SHOWING ALL DISCREPANCYS FOR A CERTAIN DISTRICT              
001100*        (CUSTOMER), OR DISCREPANCYS AT A CERTAIN STATUS.                 
001200*        THERE IS ALSO POSSIBILITIES TO DELETE A WHOLE DISCREPANCY        
001300*                                                                         
001400*        AFTER THE RETURNPERMIT IS ISSUED YOU CAN DELETE THE              
001500*        RETURNPERMIT AND PUT IT IN STATUS 7.                             
001600*                                                                         
001700*        CHANGE REQUEST FEB. 2003.DISCR.REPORT WITH CODES 60/62           
001800*        IN STATUS 1 - NOW THE ENTIRE REPORT CAN BE APPROVED AND          
001900*        PUT IN STATUS 3.                                                 
002000*                                                                         
002100*        THE PROGRAM READS/UPD   WLKREE (WDA2)                            
002200*        THE PROGRAM READS       WDB6                                     
002300*        THE PROGRAM READS       WDR5 (WDGX6327)                          
002400*        THE PROGRAM READS/UPD   WDR5 (WDGX4103)                          
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: W4T711                                              
002800*        MID:         W4I71101                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        MOD:         W4O71101                                            
003200*        MID:         W4T712                                              
003300*                                                                         
003400*    E-TRACKER 1572353 DATUM 20050415                                     
003500*    E-TRACKER 8635407 DATUM 20091021 RETURN CODES MATRIX                 
003600*    E-TRACKER 10143271 DATUM 20111018 CHINA WAREHOUSE PROJECT-1          
003700*                                                                         
003800                                                                          
003900 ENVIRONMENT DIVISION.                                                    
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400     SKIP3                                                                
004500 77  IDPGM                       PIC X(08)   VALUE 'W4071100'.            
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
004900                                                                          
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  WS-IDDISTR                  PIC 9(4).                                
005300 77  WS-IDKUNDNR                 PIC 9(6).                                
005400 77  WS-IDDISTR-JUMP             PIC X(4).                                
005500 77  WS-IDKUNDNR-JUMP            PIC X(6).                                
005600 77  WS-IDRAPPNR-JUMP            PIC X(7).                                
005700 77  WS-KDLEVANM                 PIC X.                                   
005800 77  WS-KDLEVANM-COMP            PIC 9.                                   
005900 77  SPAR-KDCMDVAL               PIC X(3).                                
006000 77  WS-CODE-COUNT               PIC S9(4)   COMP-3.                      
006100 77  WS-KVRADER-TOT              PIC S9(5)   COMP-3.                      
006200 77  WS-KVRADER-BEH              PIC S9(5)   COMP-3.                      
006300 77  WS-SELECT-COUNT             PIC S9(3)   COMP-3.                      
006400 77  WS-SPAR-IDFTG               PIC 9(2)    VALUE ZERO.                  
006500 77  WS-MATRIX-Q                 PIC X       VALUE 'N'.                   
006600*    --- INDEX FOR SCROLL LINES                                           
006700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006800 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
006900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
007200     88  INDATA-OK                           VALUE 'Y'.                   
007300     88  INDATA-WRONG                        VALUE 'N'.                   
007400                                                                          
007500 77  JUMP-TO-4712-SW             PIC X       VALUE 'N'.                   
007600     88  JUMP-TO-4712                        VALUE 'Y'.                   
007700                                                                          
007800 77  FLSVAR-SW                   PIC X       VALUE 'N'.                   
007900     88  FLSVAR-IFYLLD                       VALUE 'Y'.                   
008000                                                                          
008100 77  ANN-GODKANN-SW              PIC X       VALUE 'N'.                   
008200     88  UPPDAT-IFYLLD                       VALUE 'Y'.                   
008300                                                                          
008400 77  KOD-60-62-SW                PIC X       VALUE 'N'.                   
008500     88  BARA-KOD-60-62                      VALUE 'Y'.                   
008600                                                                          
008700 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
008800     88  KDCMD-FINNS                         VALUE 'Y'.                   
008900     88  KDCMD-SAKNAS                        VALUE 'N'.                   
009000                                                                          
009100 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
009200     88  KEYS-OK                             VALUE 'Y'.                   
009300     88  KEYS-WRONG                          VALUE 'N'.                   
009400                                                                          
009500 77  GODK-ADM-DC-SW              PIC X       VALUE 'Y'.                   
009600     88  GODK-ADM-DC                         VALUE 'Y'.                   
009700     88  EJ-GODK-ADM-DC                      VALUE 'N'.                   
009800                                                                          
009900 77  OK-SEL-SW                   PIC X(3).                                
010000     88  OK-CODE                             VALUE 'A  ' 'ANN'            
010100                                                   'D  ' 'DEL'            
010200                                                   'S  ' 'SEL'            
010300                                                   'V  ' 'VAL'            
010400                                                   'LA ' 'ANM'            
010500                                                   'GOD' 'APP'            
010600                                                   'X  '.                 
010700                                                                          
010800     88  OK-SELECT                           VALUE 'S  ' 'SEL'            
010900                                                   'V  ' 'VAL'            
011000                                                   'LA ' 'ANM'            
011100                                                   'X  '.                 
011200                                                                          
011300     88  OK-DELETE                           VALUE 'A  ' 'ANN'            
011400                                                   'D  ' 'DEL'.           
011500                                                                          
011600     88  OK-GODKANN                          VALUE 'GOD' 'APP'.           
011700                                                                          
011800                                                                          
011900 77  KDLEVANM-10-SW               PIC X.                                  
012000     88  KDLEVANM-10                         VALUE 'N'.                   
012100                                                                          
012200 77  STATUS-KEY-SW               PIC X.                                   
012300     88  STATUS-KEY                          VALUE 'Y'.                   
012400                                                                          
012500 77  DISTR-KEY-SW                PIC X.                                   
012600     88  DISTR-KEY                           VALUE 'Y'.                   
012700                                                                          
012800 77  CUST-KEY-SW                 PIC X.                                   
012900     88  CUST-KEY                            VALUE 'Y'.                   
013000                                                                          
013100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013200     88  OWN-MID                             VALUE '4711'.                
013300     88  GOOD-MID                            VALUE '4711' '4712'          
013400                                                   '4713' '4714'          
013500                                                   '4715' '4716'.         
013600     88  HELP-MID                            VALUE '0551'.                
013700     EJECT                                                                
013800*    --- DATUM                                                            
013900 01  DATE-LAP-TIAAAAMMDD.                                                 
014000     03  DATE-LAP-TISEKEL        PIC 9(2)    VALUE ZERO.                  
014100     03  DATE-LAP-TIAAMMDD       PIC 9(6)    VALUE ZERO.                  
014200*                                                                         
014300 01  DATE-LAP                    PIC 9(5)    VALUE ZERO.                  
014400 01  FILLER REDEFINES DATE-LAP.                                           
014500     03  TIAA-LAP                PIC 9(2).                                
014600     03  TIDDD-LAP               PIC 9(3).                                
014700*                                                                         
014800 01  TIDDD-REST                  PIC 9(3)    VALUE ZERO.                  
014900*                                                                         
015000 01  DATE-TODAY                  PIC 9(5)    VALUE ZERO.                  
015100 01  FILLER REDEFINES DATE-TODAY.                                         
015200     03  TIAA-TODAY              PIC 9(2).                                
015300     03  TIDDD-TODAY             PIC 9(3).                                
015400*                                                                         
015500     EJECT                                                                
015600                                                                          
015700*01  -COPY WWIDFTG                                                        
015800     EJECT                                                                
015900                                                                          
016000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016100 01  GENERAL-SUBPROGRAM.                                                  
016200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
016800     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
016900     EJECT                                                                
017000 01  FILLER                      PIC X(8)    VALUE 'WDATAREA'.            
017100*    --- PARAMETERS FOR SUBPROGRAM WDATKONV                               
017200*01 -COPY WDATAREA                                                        
017300     EJECT                                                                
017400 01  FILLER                      PIC X(8)    VALUE 'WMEDKONV'.            
017500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
017600*01 -COPY WMEDAREA                                                        
017700*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
017800*   -COPY WSECAREA                                                        
017900     EJECT                                                                
018000 01  MESSAGE-CODES.                                                       
018100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018200     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
018300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018400     03  ERR-CANCEL-NOT-POSS     PIC X(3)    VALUE '066'.                 
018500     03  ERR-WRONG-STATUS        PIC X(3)    VALUE '079'.                 
018600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018700     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
018800     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
018900     03  ERR-EJ-BEHORIG-GODK-LA  PIC X(3)    VALUE '604'.                 
019000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019200     03  INF-OK-TO-CANCEL        PIC X(3)    VALUE '083'.                 
019300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
019400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
019500     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
019600     03  INF-LINE-DELETED        PIC X(3)    VALUE '752'.                 
019700     03  INF-LINE-UPDATED        PIC X(3)    VALUE '779'.                 
019800     EJECT                                                                
019900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
020000*                                                                         
020100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
020200*01 -COPY WMSGINIT                                                        
020300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
020400     EJECT                                                                
020500*    ---  LÄNKAREA TILL W418OKOD                                          
020600 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
020700                                                                          
020800*01 -COPY W418OKOD           -PRE OKOD-.                                  
020900     EJECT                                                                
021000*                                                                         
021100 01  FILLER                   PIC X(16) VALUE 'MID-AREA-W4I711 '.         
021200*01  MID -COPY W4I71101                                                   
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021500*01  -COPY WMSGAREA                                                       
021600     EJECT                                                                
021700     03  MOD REDEFINES MSG-AREA.                                          
021800*      05  -COPY W4O71101                                                 
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022100*01  -COPY WMFSAREA                                                       
022200     EJECT                                                                
022300 01  ALT-MSG-IO-AREA.                                                     
022400   03 ALT-LL                     PIC S9(4)  VALUE +55  COMP SYNC.         
022500   03 ALT-Z1                     PIC X.                                   
022600   03 ALT-Z2                     PIC X.                                   
022700   03 ALT-TRANSKOD               PIC X(8)   VALUE 'W4T712  '.             
022800   03 ALT-IDTRANS                PIC X(4)   VALUE '4711'.                 
022900   03 ALT-SPRAK                  PIC X.                                   
023000   03 ALT-IDDISTR-IN             PIC X(4).                                
023100   03 FILLER                     PIC X(4)   VALUE SPACE.                  
023200   03 ALT-IDKUNDNR-IN            PIC X(6).                                
023300   03 FILLER                     PIC X(6)   VALUE SPACE.                  
023400   03 ALT-IDRAPPNR-IN            PIC X(7).                                
023500   03 FILLER                     PIC X(7)   VALUE SPACE.                  
023600     EJECT                                                                
023700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
023800*                                                                         
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024100 01  KEYS-TO-DLI.                                                         
024200   03  W-WDA2A1KY-MIN-X.                                                  
024300     05  W-KDLEVANM-MIN          PIC 9.                                   
024400     05  W-IDFTG-MIN             PIC 9(2).                                
024500     05  W-IDDISTR-MIN           PIC S9(5)   COMP-3.                      
024600     05  W-IDKUNDNR-MIN          PIC S9(7)   COMP-3.                      
024700     05  W-IDRAPPNR-MIN          PIC 9(7).                                
024800                                                                          
024900   03  W-WDA2A1KY-MAX-X.                                                  
025000     05  W-KDLEVANM-MAX          PIC 9.                                   
025100     05  W-IDFTG-MAX             PIC 9(2).                                
025200     05  W-IDDISTR-MAX           PIC S9(5)   COMP-3.                      
025300     05  W-IDKUNDNR-MAX          PIC S9(7)   COMP-3.                      
025400     05  W-IDRAPPNR-MAX          PIC 9(7).                                
025500                                                                          
025600   03  W-IDLEVANM-X.                                                      
025700     05  W-IDDISTR               PIC S9(5)   COMP-3.                      
025800     05  W-IDKUNDNR              PIC S9(7)   COMP-3.                      
025900     05  W-IDRAPPNR              PIC X(7).                                
026000                                                                          
026100   03  W-WDGXKEY-4107-X.                                                  
026200     05  FILLER                  PIC X(4)    VALUE '4107'.                
026300     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
026400                                                                          
026500   03  W-KDSEGKEY-X.                                                      
026600     05  FILLER                  PIC X       VALUE '1'.                   
026700                                                                          
026800   03  W-IDDC-B6-X.                                                       
026900     05  W-IDDC-B6               PIC X(2)    VALUE SPACE.                 
027000                                                                          
027100   03  W-WDA3FSEQ-X.                                                      
027200     05  W-IDDC-A3               PIC  X(2)   VALUE SPACE.                 
027300     05  W-IDDISTR-A3            PIC S9(5)   VALUE ZERO  COMP-3.          
027400     05  W-IDKUNDNR-A3           PIC S9(7)   VALUE ZERO  COMP-3.          
027500     05  W-IDRAPPNR-A3           PIC  9(7).                               
027600                                                                          
027700* TILL WDR5 ATTESTANSVARIGTABELL KREDITNOTOR                              
027800   03  W-WDGXKEY-6327-X.                                                  
027900       05  W-IDHTYP-6327       PIC X(4)    VALUE '6327'.                  
028000       05  W-KDARBTYP-6327     PIC X(8)    VALUE 'DISC    '.              
028100       05  W-IDDC-6327         PIC X(2)    VALUE SPACE.                   
028200       05  FILLER              PIC X(16)   VALUE LOW-VALUE.               
028300                                                                          
028400   03  W-KY6328-MIN-X.                                                    
028500       05  W-SUBEL-6328-MIN    PIC 9(7)    VALUE ZERO.                    
028600       05  W-IDUSER-6328-MIN   PIC X(8)    VALUE LOW-VALUE.               
028700                                                                          
028800   03  W-KY6328-MAX-X.                                                    
028900       05  W-SUBEL-6328-MAX    PIC 9(7)    VALUE 9999999.                 
029000       05  W-IDUSER-6328-MAX   PIC X(8)    VALUE HIGH-VALUE.              
029100                                                                          
029200   03  W-IDUSER-6328-X.                                                   
029300       05  W-IDUSER-GODK-6328  PIC X(8)    VALUE SPACE.                   
029400                                                                          
029500   03  W-WDGXKEY-4103-X.                                                  
029600       05  W-IDHTYP-4103       PIC X(4)  VALUE '4103'.                    
029700       05  W-IDDISTR-4103      PIC S9(5) VALUE ZERO COMP-3.               
029800       05  W-IDKUNDNR-4103     PIC S9(7) VALUE ZERO COMP-3.               
029900       05  W-IDRAPPNR-4103     PIC 9(7)  VALUE ZERO.                      
030000       05  FILLER              PIC X(12) VALUE LOW-VALUE.                 
030100                                                                          
030200*    --- STATUS-KOD FRÅN IMS                                              
030300 01  STATUS-WS                   PIC XX.                                  
030400     88  SEGMENT-FOUND                       VALUE '  '.                  
030500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
030600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
030700     88  SEGMENT-FINISH                      VALUE 'GB'.                  
030800 01  GOOD-STATUSCODES.                                                    
030900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031000 01  SSA1                        PIC X(96).                               
031100 01  SSA2                        PIC X(64).                               
031200     EJECT                                                                
031300*    --- IMS FUNCTION CODES                                               
031400*01  -COPY W0003                                                          
031500     EJECT                                                                
031600*    ---  DLI INPUT-OUTPUT AREA 0                                         
031700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-0'.        
031800 01  DLI-IO-AREA-0.                                                       
031900     03  IO-AREA-0               PIC X(100)  VALUE SPACE.                 
032000     03  WL410711 REDEFINES IO-AREA-0.                                    
032100*        05  -COPY WDGX4108                                               
032200     EJECT                                                                
032300*    ---  DLI INPUT-OUTPUT AREA                                           
032400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032500 01  DLI-IO-AREA.                                                         
032600     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
032700     03  WDA2A1   REDEFINES IO-AREA.                                      
032800*        05  -COPY WDA2A1                                                 
032900     03  WLKREE01 REDEFINES IO-AREA.                                      
033000*        05  -COPY WDA201                                                 
033100     03  WLKREE11 REDEFINES IO-AREA.                                      
033200*        05  -COPY WDA211                                                 
033300     EJECT                                                                
033400 01  FILLER                      PIC X(16)                                
033500                                 VALUE 'DLI-IO-AREA-WDA3'.                
033600 01  DLI-IO-AREA-WDA3.                                                    
033700     03  IO-AREA-WDA3            PIC X(200)  VALUE SPACE.                 
033800     03  WLRETA01 REDEFINES IO-AREA-WDA3.                                 
033900*        05  -COPY WDA301                                                 
034000*                                                                         
034100     EJECT                                                                
034200*                                                                         
034300 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX6327'.             
034400 01  DLI-IO-WDGX6327.                                                     
034500*    03  -COPY WDGX6327                                                   
034600     EJECT                                                                
034700*                                                                         
034800 01  FILLER               PIC X(16) VALUE 'DLI-IO-WDGX6328'.              
034900 01  DLI-IO-WDGX6328.                                                     
035000*    03  -COPY WDGX6328                                                   
035100     EJECT                                                                
035200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
035300 01  DLI-IO-WDGX4103.                                                     
035400*    03  -COPY WDGX4103                                                   
035500     EJECT                                                                
035600                                                                          
035700 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB601'.               
035800 01  DLI-IO-WDB601.                                                       
035900*    03  -COPY WDB601                                                     
036000     EJECT                                                                
036100 LINKAGE SECTION.                                                         
036200                                                                          
036300*01  -COPY W0009  -PRE MSG-                                               
036400*01  -COPY W0009  -PRE ALT-MSG-                                           
036500     EJECT                                                                
036600*01  -COPY W0008  -PRE USEA-                                              
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008  -PRE KREE-                                              
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008  -PRE WDA2A-                                             
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008  -PRE 4107-                                              
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008  -PRE RETA-                                              
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008  -PRE 6327-                                              
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008  -PRE 4103-                                              
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700*01  -COPY W0008  -PRE WDB6-                                              
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000 PROCEDURE DIVISION  USING MSG-PCB                                        
039100                           ALT-MSG-PCB                                    
039200                           USEA-PCB                                       
039300                           KREE-PCB                                       
039400                           WDA2A-PCB                                      
039500                           4107-PCB                                       
039600                           RETA-PCB                                       
039700                           6327-PCB                                       
039800                           4103-PCB                                       
039900                           WDB6-PCB.                                      
040000                                                                          
040100     ENTRY 'DLITCBL' USING MSG-PCB                                        
040200                           ALT-MSG-PCB                                    
040300                           USEA-PCB                                       
040400                           KREE-PCB                                       
040500                           WDA2A-PCB                                      
040600                           4107-PCB                                       
040700                           RETA-PCB                                       
040800                           6327-PCB                                       
040900                           4103-PCB                                       
041000                           WDB6-PCB.                                      
041100                                                                          
041200     PERFORM IMS-GET-MSG                                                  
041300     IF SEGMENT-FOUND                                                     
041400       PERFORM A-INIT                                                     
041500       PERFORM B-CHECK-KEYS                                               
041600       IF KEYS-OK AND OWN-MID                                             
041700         IF MFS-UPDATE                                                    
041800           PERFORM G-CHECK-INPUT                                          
041900           IF INDATA-OK AND FLSVAR-IFYLLD                                 
042000             IF MID-FLSVAR-ANN = 'J' OR 'Y'                               
042100                PERFORM H-UPDATE                                          
042200             ELSE                                                         
042300                PERFORM L-UPDATE-CODE-60-62                               
042400             END-IF                                                       
042500           ELSE                                                           
042600             IF INDATA-OK                                                 
042700               PERFORM I-SHOW-DISCREPANCY                                 
042800             ELSE                                                         
042900               CONTINUE                                                   
043000             END-IF                                                       
043100           END-IF                                                         
043200         ELSE                                                             
043300           IF MFS-FIRST                                                   
043400             PERFORM C-FIRST-PAGE                                         
043500           ELSE                                                           
043600             IF MFS-NEXT                                                  
043700               PERFORM D-NEXT-PAGE                                        
043800             ELSE                                                         
043900               PERFORM E-SAME-PAGE                                        
044000             END-IF                                                       
044100           END-IF                                                         
044200           IF JUMP-TO-4712                                                
044300             PERFORM K-INSERT-4712-MID                                    
044400           ELSE                                                           
044500             PERFORM F-READ-SHOW-INFO                                     
044600           END-IF                                                         
044700         END-IF                                                           
044800       END-IF                                                             
044900       IF JUMP-TO-4712                                                    
045000         CONTINUE                                                         
045100       ELSE                                                               
045200         PERFORM IMS-INSERT-MSG                                           
045300       END-IF                                                             
045400     END-IF                                                               
045500                                                                          
045600     MOVE ZERO TO RETURN-CODE                                             
045700     GOBACK                                                               
045800     .                                                                    
045900     EJECT                                                                
046000 A-INIT SECTION.                                                          
046100                                                                          
046200     IF MSG-DOUBLE-TRANSACTIONS                                           
046300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I71101                 
046400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
046500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
046600     ELSE                                                                 
046700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I71101                  
046800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
046900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
047000     END-IF                                                               
047100                                                                          
047200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
047300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
047400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
047500                                                                          
047600     MOVE LOW-VALUE TO MSG-AREA                                           
047700     MOVE 'W4O71101' TO MFS-IDMOD                                         
047800     MOVE '4711' TO MOD-IDTRANS                                           
047900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
048000                                                                          
048100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
048200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
048300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71101 + 4                        
048400                                                                          
048500     IF OWN-MID OR HELP-MID                                               
048600       CONTINUE                                                           
048700     ELSE                                                                 
048800       MOVE SPACE TO MFS-KDTRTYP                                          
048900       MOVE '7' TO MFS-IDPFK                                              
049000     END-IF                                                               
049100                                                                          
049200     MOVE SPACE                       TO MED-IDMFSINF                     
049300     MOVE SPACE                       TO MED-IDMFSFEL                     
049400                                                                          
049500     .                                                                    
049600     EJECT                                                                
049700 B-CHECK-KEYS SECTION.                                                    
049800                                                                          
049900     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
050000     MOVE '001'                   TO MSGI-KDCALL                          
050100     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
050200     MOVE '4711'                  TO MSGI-IDTRANS                         
050300     MOVE MSG-LTERM-NAME          TO MSGI-IDLTERM-USER                    
050400     IF OWN-MID                                                           
050500         MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                         
050600         MOVE MID-IDKUNDNR-IN     TO MSGI-IDKUNDNR                        
050700     END-IF                                                               
050800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
050900                                                                          
051000     IF MSGI-IDLAND-SPR = 'GB'                                            
051100       MOVE 'GB'                TO MED-IDSKYLT                            
051200     ELSE                                                                 
051300       MOVE 'S '                TO MED-IDSKYLT                            
051400     END-IF                                                               
051500                                                                          
051600     MOVE YES                     TO KEYS-SW                              
051700     MOVE NOO                     TO STATUS-KEY-SW                        
051800                                     DISTR-KEY-SW                         
051900                                     CUST-KEY-SW                          
052000*                                                                         
052100     IF MID-KDLEVANM-IN          NOT = ALL '+' AND OWN-MID                
052200       MOVE MID-KDLEVANM-IN       TO WS-KDLEVANM                          
052300       MOVE '7'                   TO MFS-IDPFK                            
052400       MOVE SPACE                 TO MFS-KDTRTYP                          
052500     ELSE                                                                 
052600       MOVE MID-KDLEVANM-UT       TO WS-KDLEVANM                          
052700     END-IF                                                               
052800*                                                                         
052900     IF MID-IDDISTR-IN           NOT = ALL '+' AND OWN-MID                
053000       MOVE MID-IDDISTR-IN        TO WS-IDDISTR                           
053100       MOVE '7'                   TO MFS-IDPFK                            
053200       MOVE SPACE                 TO MFS-KDTRTYP                          
053300     ELSE                                                                 
053400       MOVE MID-IDDISTR-UT        TO WS-IDDISTR                           
053500       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
053600     END-IF                                                               
053700*                                                                         
053800     IF MID-IDKUNDNR-IN          NOT = ALL '+' AND OWN-MID                
053900       MOVE MID-IDKUNDNR-IN       TO WS-IDKUNDNR                          
054000       MOVE '7'                   TO MFS-IDPFK                            
054100       MOVE SPACE                 TO MFS-KDTRTYP                          
054200     ELSE                                                                 
054300       MOVE MID-IDKUNDNR-UT       TO WS-IDKUNDNR                          
054400       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
054500     END-IF                                                               
054600*                                                                         
054700     IF WS-KDLEVANM NUMERIC                                               
054800       MOVE YES                   TO STATUS-KEY-SW                        
054900     END-IF                                                               
055000*                                                                         
055100     IF WS-IDDISTR NUMERIC                                                
055200        IF WS-IDDISTR > ZERO                                              
055300           MOVE YES               TO DISTR-KEY-SW                         
055400           IF WS-IDKUNDNR NUMERIC                                         
055500              IF WS-IDKUNDNR > ZERO                                       
055600                 MOVE YES         TO CUST-KEY-SW                          
055700              END-IF                                                      
055800           END-IF                                                         
055900        END-IF                                                            
056000     ELSE                                                                 
056100        IF WS-KDLEVANM NOT NUMERIC                                        
056200           MOVE NOO              TO KEYS-SW                               
056300        END-IF                                                            
056400     END-IF                                                               
056500*                                                                         
056600     EVALUATE TRUE                                                        
056700       WHEN WS-KDLEVANM NOT NUMERIC AND                                   
056800            STATUS-KEY                                                    
056900            MOVE NOO              TO KEYS-SW                              
057000       WHEN WS-IDDISTR  NOT NUMERIC AND                                   
057100            DISTR-KEY                                                     
057200            MOVE NOO              TO KEYS-SW                              
057300       WHEN WS-IDKUNDNR NOT NUMERIC AND                                   
057400            CUST-KEY                                                      
057500            MOVE NOO              TO KEYS-SW                              
057600     END-EVALUATE                                                         
057700                                                                          
057800     IF OWN-MID AND KEYS-OK                                               
057900       MOVE WS-KDLEVANM           TO MOD-KDLEVANM-UT                      
058000       MOVE WS-IDDISTR            TO MOD-IDDISTR-UT                       
058100       MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR-UT                      
058200       INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZEROES BY SPACE          
058300       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZEROES BY SPACE          
058400     ELSE                                                                 
058500       MOVE MFS-ERASE-FIELD       TO MOD-KDLEVANM-UT                      
058600                                     MOD-IDDISTR-UT                       
058700                                     MOD-IDKUNDNR-UT                      
058800     END-IF                                                               
058900                                                                          
059000     PERFORM BA-GET-4107                                                  
059100                                                                          
059200     IF KEYS-WRONG                                                        
059300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
059400       CALL WMEDKONV USING MED-WMEDAREA                                   
059500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059600       PERFORM MFS-ERASE-FIELD-OUT                                        
059700       PERFORM MFS-ERASE-INPUT-FIELD                                      
059800     END-IF                                                               
059900                                                                          
060000     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
060100        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
060200        MOVE '4711'               TO SEC-IDTRANS                          
060300        MOVE MSGI-IDDISTR         TO WS-IDDISTR                           
060400        MOVE WS-IDDISTR           TO SEC-IDKEY                            
060500                                                                          
060600        CALL WSECURIT USING SEC-IDUSER                                    
060700                            SEC-IDTRANS                                   
060800                            SEC-IDKEY                                     
060900                            SEC-KDSVAR                                    
061000                                                                          
061100        IF SEC-KDSVAR = 'F'                                               
061200           MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                         
061300           MOVE NOO               TO KEYS-SW                              
061400           CALL WMEDKONV USING MED-WMEDAREA                               
061500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
061600           PERFORM MFS-ERASE-FIELD-OUT                                    
061700           PERFORM MFS-ERASE-INPUT-FIELD                                  
061800        END-IF                                                            
061900     END-IF                                                               
062000     PERFORM MFS-ERASE-FIELD-IN                                           
062100     .                                                                    
062200     EJECT                                                                
062300 BA-GET-4107                    SECTION.                                  
062400                                                                          
062500     PERFORM IMS-GET-410711                                               
062600                                                                          
062700     MOVE 'IDAG  '              TO DAT-KDDATFORM                          
062800                                                                          
062900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
063000                     DAT-O-TIDATUM DAT-KDSVAR                             
063100     IF DAT-KDSVAR-OK                                                     
063200       MOVE DAT-TIAA-DAGNR      TO TIAA-TODAY                             
063300       MOVE DAT-TIDDD           TO TIDDD-TODAY                            
063400                                                                          
063500       IF TIDDD-TODAY > 4108-KVDAGAR-LAP                                  
063600          COMPUTE TIDDD-LAP      = TIDDD-TODAY - 4108-KVDAGAR-LAP         
063700          MOVE TIAA-TODAY       TO TIAA-LAP                               
063800       ELSE                                                               
063900          IF TIAA-TODAY          = 00                                     
064000             MOVE 99             TO TIAA-LAP                              
064100          ELSE                                                            
064200             COMPUTE TIAA-LAP    = TIAA-TODAY - 1                         
064300          END-IF                                                          
064400          COMPUTE TIDDD-REST     = 4108-KVDAGAR-LAP - TIDDD-TODAY         
064500          COMPUTE TIDDD-LAP      = 365 - TIDDD-REST                       
064600       END-IF                                                             
064700                                                                          
064800       MOVE 'AADDD '            TO DAT-KDDATFORM                          
064900       MOVE DATE-LAP            TO DAT-I-TIDATUM                          
065000       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
065100                       DAT-O-TIDATUM DAT-KDSVAR                           
065200                                                                          
065300       IF DAT-KDSVAR-OK                                                   
065400         MOVE DAT-TISEKEL       TO DATE-LAP-TISEKEL                       
065500         MOVE DAT-TIAAMMDD      TO DATE-LAP-TIAAMMDD                      
065600       END-IF                                                             
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 C-FIRST-PAGE SECTION.                                                    
066100                                                                          
066200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
066300     CALL WMEDKONV USING MED-WMEDAREA                                     
066400     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
066500                                                                          
066600     PERFORM MFS-ERASE-INPUT-FIELD                                        
066700                                                                          
066800*    --- BLANK/ZERO OUT SCROLLKEY                                         
066900     MOVE ZERO  TO MOD-IDDISTR-ENTER                                      
067000                   MOD-IDDISTR-NEXT                                       
067100                   MOD-IDKUNDNR-ENTER                                     
067200                   MOD-IDKUNDNR-NEXT                                      
067300                   MOD-IDRAPPNR-ENTER                                     
067400                   MOD-IDRAPPNR-NEXT                                      
067500                   MOD-KDLEVANM-ENTER                                     
067600                   MOD-KDLEVANM-NEXT                                      
067700     MOVE LOW-VALUE               TO W-WDA2A1KY-MIN-X                     
067800     MOVE HIGH-VALUE              TO W-WDA2A1KY-MAX-X                     
067900                                                                          
068000     MOVE MSGI-IDFTG              TO W-IDFTG-MIN                          
068100                                     W-IDFTG-MAX                          
068200                                                                          
068300     IF STATUS-KEY                                                        
068400       MOVE WS-KDLEVANM           TO W-KDLEVANM-MIN                       
068500                                     W-KDLEVANM-MAX                       
068600       IF DISTR-KEY                                                       
068700         MOVE WS-IDDISTR          TO W-IDDISTR-MIN                        
068800                                     W-IDDISTR-MAX                        
068900         IF CUST-KEY                                                      
069000           MOVE WS-IDKUNDNR       TO W-IDKUNDNR-MIN                       
069100                                     W-IDKUNDNR-MAX                       
069200         END-IF                                                           
069300       END-IF                                                             
069400     ELSE                                                                 
069500       MOVE 0                     TO W-KDLEVANM-MIN                       
069600                                     W-KDLEVANM-MAX                       
069700       IF DISTR-KEY                                                       
069800         MOVE WS-IDDISTR          TO W-IDDISTR-MIN                        
069900                                     W-IDDISTR-MAX                        
070000         IF CUST-KEY                                                      
070100           MOVE WS-IDKUNDNR       TO W-IDKUNDNR-MIN                       
070200                                     W-IDKUNDNR-MAX                       
070300         END-IF                                                           
070400       END-IF                                                             
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 D-NEXT-PAGE SECTION.                                                     
070900                                                                          
071000     IF OWN-MID                                                           
071100       MOVE HIGH-VALUE              TO W-WDA2A1KY-MAX-X                   
071200                                                                          
071300       MOVE MSGI-IDFTG              TO W-IDFTG-MIN                        
071400                                       W-IDFTG-MAX                        
071500                                                                          
071600       IF STATUS-KEY                                                      
071700         MOVE MID-KDLEVANM-NEXT     TO W-KDLEVANM-MAX                     
071800         IF DISTR-KEY                                                     
071900           MOVE MID-IDDISTR-NEXT    TO W-IDDISTR-MAX                      
072000           IF CUST-KEY                                                    
072100             MOVE MID-IDKUNDNR-NEXT TO W-IDKUNDNR-MAX                     
072200           END-IF                                                         
072300         END-IF                                                           
072400       ELSE                                                               
072500         IF DISTR-KEY                                                     
072600           MOVE MID-IDDISTR-NEXT    TO W-IDDISTR-MAX                      
072700           IF CUST-KEY                                                    
072800             MOVE MID-IDKUNDNR-NEXT TO W-IDKUNDNR-MAX                     
072900           END-IF                                                         
073000         END-IF                                                           
073100         MOVE MID-KDLEVANM-NEXT     TO W-KDLEVANM-MAX                     
073200       END-IF                                                             
073300       MOVE MID-KDLEVANM-NEXT       TO W-KDLEVANM-MIN                     
073400       MOVE MID-IDDISTR-NEXT        TO W-IDDISTR-MIN                      
073500       MOVE MID-IDKUNDNR-NEXT       TO W-IDKUNDNR-MIN                     
073600       MOVE MID-IDRAPPNR-NEXT       TO W-IDRAPPNR-MIN                     
073700                                                                          
073800       PERFORM MFS-ERASE-INPUT-FIELD                                      
073900     ELSE                                                                 
074000       PERFORM MFS-ERASE-INPUT-FIELD                                      
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 E-SAME-PAGE SECTION.                                                     
074500                                                                          
074600     IF OWN-MID OR HELP-MID                                               
074700       MOVE HIGH-VALUE              TO W-WDA2A1KY-MAX-X                   
074800                                                                          
074900       MOVE MSGI-IDFTG              TO W-IDFTG-MIN                        
075000                                       W-IDFTG-MAX                        
075100                                                                          
075200       IF STATUS-KEY                                                      
075300         MOVE MID-KDLEVANM-ENTER    TO W-KDLEVANM-MAX                     
075400         IF DISTR-KEY                                                     
075500           MOVE MID-IDDISTR-ENTER   TO W-IDDISTR-MAX                      
075600           IF CUST-KEY                                                    
075700             MOVE MID-IDKUNDNR-ENTER TO W-IDKUNDNR-MAX                    
075800           END-IF                                                         
075900         END-IF                                                           
076000       ELSE                                                               
076100         IF DISTR-KEY                                                     
076200           MOVE MID-IDDISTR-ENTER   TO W-IDDISTR-MAX                      
076300           IF CUST-KEY                                                    
076400             MOVE MID-IDKUNDNR-ENTER TO W-IDKUNDNR-MAX                    
076500           END-IF                                                         
076600         END-IF                                                           
076700         MOVE MID-KDLEVANM-ENTER    TO W-KDLEVANM-MAX                     
076800       END-IF                                                             
076900       MOVE MID-KDLEVANM-ENTER      TO W-KDLEVANM-MIN                     
077000       MOVE MID-IDDISTR-ENTER       TO W-IDDISTR-MIN                      
077100       MOVE MID-IDKUNDNR-ENTER      TO W-IDKUNDNR-MIN                     
077200       MOVE MID-IDRAPPNR-ENTER      TO W-IDRAPPNR-MIN                     
077300                                                                          
077400       IF MID-INPUT = ALL '+'                                             
077500         MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR-ANN                         
077600         MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR-ANN                        
077700         MOVE MFS-RENSA-FAELT  TO MOD-IDRAPPNR-ANN                        
077800         MOVE MFS-RENSA-FAELT  TO MOD-FLSVAR-ANN                          
077900         MOVE MFS-RENSA-FAELT  TO MOD-FLSVAR-GODKANN                      
078000         PERFORM EA-MID-KDCMDVAL-TILL-MOD                                 
078100       ELSE                                                               
078200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
078300         CALL WMEDKONV USING MED-WMEDAREA                                 
078400         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
078500         PERFORM EB-MID-INDATA-TILL-MOD                                   
078600       END-IF                                                             
078700     ELSE                                                                 
078800       PERFORM MFS-ERASE-INPUT-FIELD                                      
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200 EA-MID-KDCMDVAL-TILL-MOD SECTION.                                        
079300                                                                          
079400     MOVE YES                         TO INDATA-SW                        
079500     MOVE +0                          TO WS-CODE-COUNT                    
079600     MOVE NOO                         TO ANN-GODKANN-SW                   
079700                                                                          
079800     MOVE +1 TO INDX                                                      
079900     PERFORM UNTIL INDX > MAX-INDX                                        
080000       IF MID-KDCMDVAL (INDX)  =  ALL '+'                                 
080100         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL (INDX)                     
080200       ELSE                                                               
080300         MOVE MID-KDCMDVAL (INDX)     TO OK-SEL-SW                        
080400         IF OK-CODE                                                       
080500           ADD +1                     TO WS-CODE-COUNT                    
080600           IF OK-SELECT                                                   
080700             MOVE MID-IDDISTR (INDX)  TO WS-IDDISTR-JUMP                  
080800             MOVE MID-IDKUNDNR (INDX) TO WS-IDKUNDNR-JUMP                 
080900             MOVE MID-IDRAPPNR (INDX) TO WS-IDRAPPNR-JUMP                 
081000             MOVE YES                 TO JUMP-TO-4712-SW                  
081100             MOVE MAX-INDX TO INDX                                        
081200           ELSE                                                           
081300             MOVE YES                 TO ANN-GODKANN-SW                   
081400             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)          
081500             MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL (INDX)                
081600           END-IF                                                         
081700         ELSE                                                             
081800           MOVE NOO                 TO INDATA-SW                          
081900                                       JUMP-TO-4712-SW                    
082000           MOVE MFS-ALFA-FAELT-FEL TO                                     
082100                               MOD-KDCMD-ATTR (INDX)                      
082200           MOVE MFS-ROER-EJ-FAELT  TO                                     
082300                               MOD-KDCMDVAL   (INDX)                      
082400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
082500           CALL WMEDKONV USING MED-WMEDAREA                               
082600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
082700         END-IF                                                           
082800       END-IF                                                             
082900       ADD +1 TO INDX                                                     
083000     END-PERFORM                                                          
083100                                                                          
083200     IF JUMP-TO-4712                                                      
083300       CONTINUE                                                           
083400     ELSE                                                                 
083500       IF  ( WS-CODE-COUNT   > +0 ) AND UPPDAT-IFYLLD                     
083600                                                                          
083700         MOVE NOO                      TO INDATA-SW                       
083800                                          JUMP-TO-4712-SW                 
083900         MOVE INF-PRESS-PF11           TO MED-IDMFSINF                    
084000         CALL WMEDKONV USING MED-WMEDAREA                                 
084100         MOVE MED-MFSINF               TO MOD-TEMFSFEL                    
084200       END-IF                                                             
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600 EB-MID-INDATA-TILL-MOD SECTION.                                          
084700                                                                          
084800     IF MID-FLSVAR-ANN = '+'                                              
084900       MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-ANN                             
085000     ELSE                                                                 
085100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSVAR-ANN-ATTR                  
085200       MOVE MFS-ROER-EJ-FAELT TO MOD-FLSVAR-ANN                           
085300     END-IF                                                               
085400                                                                          
085500     IF MID-FLSVAR-GODKANN = '+'                                          
085600       MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-GODKANN                         
085700     ELSE                                                                 
085800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSVAR-GODKANN-ATTR              
085900       MOVE MFS-ROER-EJ-FAELT TO MOD-FLSVAR-GODKANN                       
086000     END-IF                                                               
086100                                                                          
086200     .                                                                    
086300     EJECT                                                                
086400 F-READ-SHOW-INFO SECTION.                                                
086500                                                                          
086600     IF STATUS-KEY                                                        
086700        PERFORM FA-READ-SHOW-INFO-STATUS                                  
086800     ELSE                                                                 
086900        PERFORM FB-READ-SHOW-INFO-DISTRICT                                
087000     END-IF                                                               
087100     IF SEGMENT-FOUND AND INDX > +1                                       
087200        MOVE SEQA-IDDISTR              TO MOD-IDDISTR-NEXT                
087300        MOVE SEQA-IDKUNDNR             TO MOD-IDKUNDNR-NEXT               
087400        MOVE SEQA-IDRAPPNR             TO MOD-IDRAPPNR-NEXT               
087500        MOVE SEQA-KDLEVANM             TO MOD-KDLEVANM-NEXT               
087600        IF MFS-FIRST OR MFS-NEXT                                          
087700          MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                    
087800          CALL WMEDKONV USING MED-WMEDAREA                                
087900          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
088000        END-IF                                                            
088100     ELSE                                                                 
088200       IF SEGMENT-MISSING AND (INDX > MAX-INDX) AND                       
088300                        ( KDLEVANM-10-SW = NOO )                          
088400         MOVE W-IDDISTR             TO MOD-IDDISTR-NEXT                   
088500         MOVE W-IDKUNDNR            TO MOD-IDKUNDNR-NEXT                  
088600         MOVE W-IDRAPPNR            TO MOD-IDRAPPNR-NEXT                  
088700         MOVE W-KDLEVANM-MIN        TO MOD-KDLEVANM-NEXT                  
088800                                                                          
088900         IF MFS-FIRST OR MFS-NEXT                                         
089000           MOVE INF-MORE-INFO-EXISTS     TO MED-IDMFSINF                  
089100           CALL WMEDKONV USING MED-WMEDAREA                               
089200           MOVE MED-MFSINF TO MOD-TEMFSINF                                
089300         END-IF                                                           
089400       ELSE                                                               
089500         IF SEGMENT-MISSING AND INDX > +1                                 
089600           IF MED-IDMFSINF = SPACE AND MED-IDMFSFEL = SPACE               
089700             MOVE INF-LAST-PAGE         TO MED-IDMFSINF                   
089800             CALL WMEDKONV USING MED-WMEDAREA                             
089900             MOVE MED-MFSINF            TO MOD-TEMFSINF                   
090000           END-IF                                                         
090100         END-IF                                                           
090200         MOVE ZERO                     TO MOD-IDDISTR-NEXT                
090300                                          MOD-IDKUNDNR-NEXT               
090400                                          MOD-IDRAPPNR-NEXT               
090500                                          MOD-KDLEVANM-NEXT               
090600       END-IF                                                             
090700     END-IF                                                               
090800     MOVE MFS-CLOSE-FIELD              TO MOD-FLSVAR-ANN-ATTR             
090900                                          MOD-FLSVAR-GODKANN-ATTR         
091000     MOVE MFS-ERASE-FIELD              TO MOD-FLSVAR-ANN                  
091100                                          MOD-FLSVAR-GODKANN              
091200     .                                                                    
091300     EJECT                                                                
091400 FA-READ-SHOW-INFO-STATUS   SECTION.                                      
091500                                                                          
091600     PERFORM IMS-GU-WDA2A1                                                
091700     IF SEGMENT-FOUND                                                     
091800        PERFORM S01-KOLLA-BEHORIGHET                                      
091900     END-IF                                                               
092000     MOVE +1                          TO INDX                             
092100     IF SEGMENT-FOUND                                                     
092200        MOVE SEQA-IDDISTR             TO MOD-IDDISTR-ENTER                
092300        MOVE SEQA-IDKUNDNR            TO MOD-IDKUNDNR-ENTER               
092400        MOVE SEQA-IDRAPPNR            TO MOD-IDRAPPNR-ENTER               
092500        MOVE SEQA-KDLEVANM            TO MOD-KDLEVANM-ENTER               
092600        PERFORM UNTIL SEGMENT-MISSING OR                                  
092700                      SEGMENT-FINISH  OR                                  
092800                    (INDX > MAX-INDX)                                     
092900          MOVE SEQA-IDDISTR        TO W-IDDISTR                           
093000          MOVE SEQA-IDKUNDNR       TO W-IDKUNDNR                          
093100          MOVE SEQA-IDRAPPNR       TO W-IDRAPPNR                          
093200          PERFORM IMS-GU-KREE01                                           
093300*                                                                         
093400          MOVE ANM-IDDISTR       TO MOD-IDDISTR  (INDX)                   
093500          MOVE ANM-IDKUNDNR      TO MOD-IDKUNDNR (INDX)                   
093600          MOVE ANM-IDRAPPNR      TO MOD-IDRAPPNR (INDX)                   
093700          MOVE ANM-IDUSER        TO MOD-IDUSER   (INDX)                   
093800          MOVE ANM-KDLEVANM      TO MOD-KDLEVANM (INDX)                   
093900          MOVE ANM-DALEVANM(3:6) TO MOD-TILEVANM (INDX)                   
094000          MOVE ANM-DARETILL(3:6) TO MOD-TIRETILL (INDX)                   
094100                                                                          
094200          IF ANM-KDLEVANM      = '2'                                      
094300            IF ANM-DALEVANM < DATE-LAP-TIAAAAMMDD                         
094400              MOVE MFS-ADD-HILIGHT-FIELD                                  
094500                                TO MOD-IDDISTR-ATTR (INDX)                
094600                                   MOD-IDKUNDNR-ATTR (INDX)               
094700                                   MOD-IDRAPPNR-ATTR (INDX)               
094800            END-IF                                                        
094900          END-IF                                                          
095000*                                                                         
095100          MOVE +0             TO WS-KVRADER-TOT                           
095200                                 WS-KVRADER-BEH                           
095300          PERFORM IMS-GNP-KREE11                                          
095400          PERFORM UNTIL SEGMENT-MISSING                                   
095500            ADD +1            TO WS-KVRADER-TOT                           
095600            IF LEV-KDKREBEH NOT = 'R  ' AND 'RR '                         
095700               ADD +1         TO WS-KVRADER-BEH                           
095800            END-IF                                                        
095900            PERFORM IMS-GNP-KREE11                                        
096000          END-PERFORM                                                     
096100          MOVE WS-KVRADER-TOT TO MOD-KVRADER-TOT (INDX)                   
096200          MOVE WS-KVRADER-BEH TO MOD-KVRADER-BEH (INDX)                   
096300          ADD +1              TO INDX                                     
096400                                                                          
096500          PERFORM IMS-GN-WDA2A1                                           
096600          IF SEGMENT-FOUND                                                
096700             PERFORM S01-KOLLA-BEHORIGHET                                 
096800          END-IF                                                          
096900        END-PERFORM                                                       
097000                                                                          
097100        PERFORM UNTIL INDX > MAX-INDX                                     
097200          PERFORM MFS-ERASE-LINE-FIELD-OUT                                
097300          MOVE MFS-ERASE-FIELD         TO MOD-KDCMDVAL   (INDX)           
097400          MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)           
097500          ADD +1                TO INDX                                   
097600        END-PERFORM                                                       
097700     ELSE                                                                 
097800        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
097900        CALL WMEDKONV USING MED-WMEDAREA                                  
098000        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
098100*                                                                         
098200        PERFORM UNTIL INDX > MAX-INDX                                     
098300          PERFORM MFS-ERASE-LINE-FIELD-OUT                                
098400          MOVE MFS-ERASE-FIELD         TO MOD-KDCMDVAL   (INDX)           
098500          MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)           
098600          ADD +1                   TO INDX                                
098700        END-PERFORM                                                       
098800     END-IF                                                               
098900     .                                                                    
099000     EJECT                                                                
099100 FB-READ-SHOW-INFO-DISTRICT SECTION.                                      
099200                                                                          
099300     MOVE +1                          TO INDX                             
099400     MOVE NOO                         TO KDLEVANM-10-SW                   
099500     PERFORM IMS-GU-WDA2A1                                                
099600     PERFORM UNTIL (INDX > MAX-INDX) OR                                   
099700                   KDLEVANM-10-SW = YES                                   
099800        PERFORM UNTIL SEGMENT-MISSING OR                                  
099900                      SEGMENT-FINISH  OR                                  
100000                      (INDX > MAX-INDX)                                   
100100                                                                          
100200          IF SEGMENT-FOUND           AND INDX = +1                        
100300            MOVE SEQA-IDDISTR        TO MOD-IDDISTR-ENTER                 
100400            MOVE SEQA-IDKUNDNR       TO MOD-IDKUNDNR-ENTER                
100500            MOVE SEQA-IDRAPPNR       TO MOD-IDRAPPNR-ENTER                
100600            MOVE SEQA-KDLEVANM       TO MOD-KDLEVANM-ENTER                
100700          END-IF                                                          
100800          MOVE SEQA-IDDISTR           TO W-IDDISTR                        
100900          MOVE SEQA-IDKUNDNR          TO W-IDKUNDNR                       
101000          MOVE SEQA-IDRAPPNR          TO W-IDRAPPNR                       
101100          PERFORM IMS-GU-KREE01                                           
101200                                                                          
101300          MOVE ANM-IDDISTR          TO MOD-IDDISTR  (INDX)                
101400          MOVE ANM-IDKUNDNR         TO MOD-IDKUNDNR (INDX)                
101500          MOVE ANM-IDRAPPNR         TO MOD-IDRAPPNR (INDX)                
101600          MOVE ANM-IDUSER           TO MOD-IDUSER   (INDX)                
101700          MOVE ANM-KDLEVANM         TO MOD-KDLEVANM (INDX)                
101800          MOVE ANM-DALEVANM(3:6)    TO MOD-TILEVANM (INDX)                
101900          MOVE ANM-DARETILL(3:6)    TO MOD-TIRETILL (INDX)                
102000                                                                          
102100          MOVE +0                   TO WS-KVRADER-TOT                     
102200                                         WS-KVRADER-BEH                   
102300          PERFORM IMS-GNP-KREE11                                          
102400          PERFORM UNTIL SEGMENT-MISSING                                   
102500            ADD +1                  TO WS-KVRADER-TOT                     
102600            IF LEV-KDKREBEH         NOT = 'R  ' AND 'RR '                 
102700               ADD +1               TO WS-KVRADER-BEH                     
102800            END-IF                                                        
102900            PERFORM IMS-GNP-KREE11                                        
103000          END-PERFORM                                                     
103100          MOVE WS-KVRADER-TOT       TO MOD-KVRADER-TOT (INDX)             
103200          MOVE WS-KVRADER-BEH       TO MOD-KVRADER-BEH (INDX)             
103300          ADD +1                    TO INDX                               
103400                                                                          
103500          PERFORM IMS-GN-WDA2A1                                           
103600        END-PERFORM                                                       
103700                                                                          
103800        IF INDX > MAX-INDX                                                
103900          CONTINUE                                                        
104000        ELSE                                                              
104100          IF W-KDLEVANM-MIN = 9                                           
104200            MOVE YES TO KDLEVANM-10-SW                                    
104300          ELSE                                                            
104400            ADD 1                       TO W-KDLEVANM-MIN                 
104500                                           W-KDLEVANM-MAX                 
104600            IF NOT CUST-KEY                                               
104700               MOVE +0                  TO W-IDKUNDNR-MIN                 
104800            END-IF                                                        
104900            MOVE ZERO                   TO W-IDRAPPNR-MIN                 
105000            PERFORM IMS-GN-WDA2A1                                         
105100          END-IF                                                          
105200        END-IF                                                            
105300                                                                          
105400     END-PERFORM                                                          
105500     PERFORM UNTIL INDX > MAX-INDX                                        
105600       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
105700       MOVE MFS-ERASE-FIELD         TO MOD-KDCMDVAL   (INDX)              
105800       MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)              
105900       ADD +1                         TO INDX                             
106000     END-PERFORM                                                          
106100     IF SEGMENT-MISSING              AND INDX = +1                        
106200        MOVE ERR-WRONG-KEY            TO MED-IDMFSFEL                     
106300        CALL WMEDKONV USING MED-WMEDAREA                                  
106400        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
106500        PERFORM UNTIL INDX > MAX-INDX                                     
106600          PERFORM MFS-ERASE-LINE-FIELD-OUT                                
106700          MOVE MFS-ERASE-FIELD         TO MOD-KDCMDVAL   (INDX)           
106800          MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)           
106900          ADD +1                      TO INDX                             
107000        END-PERFORM                                                       
107100     END-IF                                                               
107200     .                                                                    
107300     EJECT                                                                
107400 G-CHECK-INPUT SECTION.                                                   
107500                                                                          
107600     MOVE YES                         TO INDATA-SW                        
107700     MOVE +1                          TO INDX                             
107800     MOVE +0                          TO WS-CODE-COUNT                    
107900     MOVE NOO                         TO FLSVAR-SW                        
108000     MOVE NOO                         TO KDCMD-SW                         
108100                                                                          
108200                                                                          
108300     MOVE +1                           TO INDX                            
108400     PERFORM UNTIL INDX  >  MAX-INDX                                      
108500       IF MID-KDCMDVAL(INDX)  NOT = ALL '+'                               
108600         MOVE YES TO KDCMD-SW                                             
108700       END-IF                                                             
108800       ADD +1                          TO INDX                            
108900     END-PERFORM                                                          
109000                                                                          
109100     IF ( MID-INPUT = ALL '+' ) AND KDCMD-SAKNAS                          
109200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
109300       CALL WMEDKONV USING MED-WMEDAREA                                   
109400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
109500       PERFORM MFS-NOT-TOUCH-FIELD                                        
109600       MOVE NOO TO INDATA-SW                                              
109700                                                                          
109800       IF MID-IDDISTR (1) NOT = SPACE                                     
109900         MOVE MFS-CLOSE-FIELD       TO MOD-FLSVAR-ANN-ATTR                
110000                                       MOD-FLSVAR-GODKANN-ATTR            
110100         MOVE MFS-ERASE-FIELD       TO MOD-FLSVAR-ANN                     
110200                                       MOD-FLSVAR-GODKANN                 
110300       END-IF                                                             
110400     ELSE                                                                 
110500       IF MID-FLSVAR-ANN  = '+' AND MID-FLSVAR-GODKANN = '+'              
110600                                                                          
110700         MOVE +1                           TO INDX                        
110800         PERFORM UNTIL INDX              > MAX-INDX                       
110900           IF MID-KDCMDVAL (INDX) = ALL '+'                               
111000             MOVE MFS-ERASE-FIELD  TO MOD-KDCMDVAL (INDX)                 
111100           ELSE                                                           
111200             MOVE MID-KDCMDVAL (INDX)     TO OK-SEL-SW                    
111300             IF OK-DELETE OR OK-GODKANN                                   
111400                MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)        
111500                ADD +1                    TO WS-CODE-COUNT                
111600                MOVE MID-IDDISTR (INDX)   TO W-IDDISTR                    
111700                MOVE MID-IDKUNDNR (INDX)  TO W-IDKUNDNR                   
111800                MOVE MID-IDRAPPNR (INDX)  TO W-IDRAPPNR                   
111900                INSPECT W-IDRAPPNR REPLACING LEADING SPACE BY ZERO        
112000                                                                          
112100                MOVE MID-KDCMDVAL (INDX)  TO SPAR-KDCMDVAL                
112200             ELSE                                                         
112300                MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)          
112400                MOVE NOO TO INDATA-SW                                     
112500             END-IF                                                       
112600           END-IF                                                         
112700           ADD +1                       TO INDX                           
112800         END-PERFORM                                                      
112900                                                                          
113000         IF INDATA-WRONG                                                  
113100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
113200           CALL WMEDKONV USING MED-WMEDAREA                               
113300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
113400           PERFORM MFS-NOT-TOUCH-FIELD                                    
113500           MOVE MFS-CLOSE-FIELD   TO MOD-FLSVAR-ANN-ATTR                  
113600                                     MOD-FLSVAR-GODKANN-ATTR              
113700           MOVE MFS-ERASE-FIELD   TO MOD-FLSVAR-ANN                       
113800                                     MOD-FLSVAR-GODKANN                   
113900         ELSE                                                             
114000           IF WS-CODE-COUNT               NOT = +1                        
114100                                                                          
114200              MOVE NOO                    TO INDATA-SW                    
114300              MOVE ERR-CONFLICT           TO MED-IDMFSFEL                 
114400              CALL WMEDKONV USING MED-WMEDAREA                            
114500              MOVE MED-MFSFEL             TO MOD-TEMFSFEL                 
114600              PERFORM MFS-NOT-TOUCH-FIELD                                 
114700              MOVE MFS-CLOSE-FIELD  TO MOD-FLSVAR-ANN-ATTR                
114800                                       MOD-FLSVAR-GODKANN-ATTR            
114900              MOVE MFS-ERASE-FIELD  TO MOD-FLSVAR-ANN                     
115000                                       MOD-FLSVAR-GODKANN                 
115100           ELSE                                                           
115200             MOVE MSGI-IDFTG    TO WS-IDFTG                               
115300             IF IDFTG-PV OR IDFTG-NON-VCC                                 
115310                                                                          
115400               MOVE +1                TO INDX                             
115500               PERFORM UNTIL INDX  > MAX-INDX                             
115600                 IF MID-KDCMDVAL (INDX) NOT = ALL '+'                     
115700                   PERFORM J-GODK-ADM-KONTROLL                            
115800                   MOVE MAX-INDX TO INDX                                  
115900                 END-IF                                                   
116000                 ADD +1                       TO INDX                     
116100               END-PERFORM                                                
116200             END-IF                                                       
116210                                                                          
116300           END-IF                                                         
116400         END-IF                                                           
116500       ELSE                                                               
116600         IF (MID-FLSVAR-ANN NOT = ALL '+')  AND                           
116700            (MID-FLSVAR-GODKANN NOT = ALL '+' )                           
116800            MOVE NOO                    TO INDATA-SW                      
116900            MOVE ERR-CONFLICT           TO MED-IDMFSFEL                   
117000            CALL WMEDKONV USING MED-WMEDAREA                              
117100            MOVE MED-MFSFEL             TO MOD-TEMFSFEL                   
117200            MOVE MFS-ROER-EJ-FAELT  TO MOD-FLSVAR-ANN                     
117300                                       MOD-FLSVAR-GODKANN                 
117400                                       MOD-IDDISTR-ANN                    
117500                                       MOD-IDKUNDNR-ANN                   
117600                                       MOD-IDRAPPNR-ANN                   
117700            MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLSVAR-ANN-ATTR             
117800            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-ANN-ATTR            
117900                                          MOD-IDKUNDNR-ANN-ATTR           
118000                                          MOD-IDRAPPNR-ANN-ATTR           
118100         ELSE                                                             
118200           IF MID-FLSVAR-ANN NOT = ALL '+'                                
118300             PERFORM GA-CHECK-INPUT-ANN                                   
118400           ELSE                                                           
118500             PERFORM GB-CHECK-INPUT-GODKANN                               
118600           END-IF                                                         
118700         END-IF                                                           
118800       END-IF                                                             
118900     END-IF                                                               
119000     .                                                                    
119100     EJECT                                                                
119200 GA-CHECK-INPUT-ANN SECTION.                                              
119300                                                                          
119400     IF MID-FLSVAR-ANN = 'J' OR 'Y'                                       
119500       MOVE YES                     TO FLSVAR-SW                          
119600       MOVE MID-IDDISTR-ANN         TO WS-IDDISTR                         
119700       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
119800       IF WS-IDDISTR NUMERIC                                              
119900          MOVE WS-IDDISTR           TO W-IDDISTR                          
120000       ELSE                                                               
120100         MOVE NOO                   TO INDATA-SW                          
120200       END-IF                                                             
120300                                                                          
120400       MOVE MID-IDKUNDNR-ANN        TO WS-IDKUNDNR                        
120500       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
120600       IF WS-IDKUNDNR NUMERIC                                             
120700          MOVE WS-IDKUNDNR          TO W-IDKUNDNR                         
120800       ELSE                                                               
120900         MOVE NOO                   TO INDATA-SW                          
121000       END-IF                                                             
121100                                                                          
121200       MOVE MID-IDRAPPNR-ANN        TO W-IDRAPPNR                         
121300       INSPECT W-IDRAPPNR REPLACING LEADING SPACE BY ZERO                 
121400     ELSE                                                                 
121500       MOVE NOO                     TO INDATA-SW                          
121600       MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                       
121700       CALL WMEDKONV USING MED-WMEDAREA                                   
121800       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
121900       MOVE MFS-ROER-EJ-FAELT       TO MOD-FLSVAR-ANN                     
122000                                       MOD-IDDISTR-ANN                    
122100                                       MOD-IDKUNDNR-ANN                   
122200                                       MOD-IDRAPPNR-ANN                   
122300       MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLSVAR-ANN-ATTR                
122400       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-ANN-ATTR               
122500                                       MOD-IDKUNDNR-ANN-ATTR              
122600                                       MOD-IDRAPPNR-ANN-ATTR              
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000 GB-CHECK-INPUT-GODKANN SECTION.                                          
123100                                                                          
123200     IF MID-FLSVAR-GODKANN = 'J' OR 'Y'                                   
123300       MOVE YES                     TO FLSVAR-SW                          
123400       MOVE MID-IDDISTR-ANN         TO WS-IDDISTR                         
123500       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
123600       IF WS-IDDISTR NUMERIC                                              
123700          MOVE WS-IDDISTR           TO W-IDDISTR                          
123800       ELSE                                                               
123900         MOVE NOO                   TO INDATA-SW                          
124000       END-IF                                                             
124100                                                                          
124200       MOVE MID-IDKUNDNR-ANN        TO WS-IDKUNDNR                        
124300       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
124400       IF WS-IDKUNDNR NUMERIC                                             
124500          MOVE WS-IDKUNDNR          TO W-IDKUNDNR                         
124600       ELSE                                                               
124700         MOVE NOO                   TO INDATA-SW                          
124800       END-IF                                                             
124900                                                                          
125000       MOVE MID-IDRAPPNR-ANN        TO W-IDRAPPNR                         
125100       INSPECT W-IDRAPPNR REPLACING LEADING SPACE BY ZERO                 
125200     ELSE                                                                 
125300       MOVE NOO                     TO INDATA-SW                          
125400       MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                       
125500       CALL WMEDKONV USING MED-WMEDAREA                                   
125600       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
125700       MOVE MFS-ROER-EJ-FAELT       TO MOD-FLSVAR-GODKANN                 
125800                                       MOD-IDDISTR-ANN                    
125900                                       MOD-IDKUNDNR-ANN                   
126000                                       MOD-IDRAPPNR-ANN                   
126100       MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLSVAR-GODKANN-ATTR            
126200       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-ANN-ATTR               
126300                                       MOD-IDKUNDNR-ANN-ATTR              
126400                                       MOD-IDRAPPNR-ANN-ATTR              
126500     END-IF                                                               
126600     .                                                                    
126700     EJECT                                                                
126800 H-UPDATE      SECTION.                                                   
126900                                                                          
127000     MOVE ZERO TO WS-SPAR-IDFTG                                           
127100                                                                          
127200     PERFORM IMS-GHU-KREE01                                               
127300     IF SEGMENT-FOUND                                                     
127400       MOVE ANM-IDDISTR  TO W-IDDISTR-4103                                
127500       MOVE ANM-IDKUNDNR TO W-IDKUNDNR-4103                               
127600       MOVE ANM-IDRAPPNR TO W-IDRAPPNR-4103                               
127700       MOVE ANM-IDFTG    TO WS-SPAR-IDFTG                                 
127800                                                                          
127900       IF ANM-KDLEVANM            < '4'                                   
128000         MOVE '8'                 TO ANM-KDLEVANM                         
128100         MOVE 0                   TO ANM-KDLEVATT                         
128200         PERFORM IMS-REPL-KREE                                            
128300         PERFORM IMS-GHNP-KREE11                                          
128400         PERFORM UNTIL SEGMENT-MISSING                                    
128500           MOVE 'ANN'             TO LEV-KDKREBEH                         
128600           MOVE 'J'               TO LEV-FLANNULL                         
128700           PERFORM IMS-REPL-KREE                                          
128800           PERFORM IMS-GHNP-KREE11                                        
128900         END-PERFORM                                                      
129000                                                                          
129100* - NÄR HELA LA ANNULLERAS SKALL MAN ÄVEN TA BORT DEN FR ATTESTKÖN        
129200* - OBS! SKALL EJ GÄLLA USA/CAN,JAPAN DC61 OCH AUSTRALIEN DC62            
129210         MOVE WS-SPAR-IDFTG    TO WS-IDFTG                                
129300         IF IDFTG-PV OR IDFTG-CN                                          
129400           PERFORM IMS-GHU-WDR501-4103                                    
129500           IF SEGMENT-FOUND                                               
129600             PERFORM IMS-DLET-WDR501-4103                                 
129700           END-IF                                                         
129800         END-IF                                                           
129900                                                                          
130000         MOVE INF-LINE-UPDATED    TO MED-IDMFSINF                         
130100         CALL WMEDKONV USING MED-WMEDAREA                                 
130200         MOVE MED-MFSINF          TO MOD-TEMFSINF                         
130300         PERFORM MFS-ERASE-INPUT-FIELD                                    
130400*                                                                         
130500         PERFORM IMS-GU-KREE01                                            
130600         MOVE +1                  TO INDX                                 
130700         MOVE ANM-IDDISTR         TO MOD-IDDISTR  (INDX)                  
130800                                         MOD-IDDISTR-ENTER                
130900         MOVE ANM-IDKUNDNR        TO MOD-IDKUNDNR (INDX)                  
131000                                         MOD-IDKUNDNR-ENTER               
131100         MOVE ANM-IDRAPPNR        TO MOD-IDRAPPNR (INDX)                  
131200                                         MOD-IDRAPPNR-ENTER               
131300         MOVE ANM-IDUSER          TO MOD-IDUSER   (INDX)                  
131400         MOVE ANM-KDLEVANM        TO MOD-KDLEVANM (INDX)                  
131500                                         MOD-KDLEVANM-ENTER               
131600         MOVE ANM-DALEVANM(3:6)   TO MOD-TILEVANM (INDX)                  
131700         MOVE ANM-DARETILL(3:6)   TO MOD-TIRETILL (INDX)                  
131800                                                                          
131900         MOVE +0                  TO WS-KVRADER-TOT                       
132000                                         WS-KVRADER-BEH                   
132100         PERFORM IMS-GNP-KREE11                                           
132200         PERFORM UNTIL SEGMENT-MISSING                                    
132300           ADD +1                 TO WS-KVRADER-TOT                       
132400           IF LEV-KDKREBEH       NOT = 'R  ' AND 'RR '                    
132500              ADD +1              TO WS-KVRADER-BEH                       
132600           END-IF                                                         
132700           PERFORM IMS-GNP-KREE11                                         
132800         END-PERFORM                                                      
132900         MOVE WS-KVRADER-TOT      TO MOD-KVRADER-TOT (INDX)               
133000         MOVE WS-KVRADER-BEH      TO MOD-KVRADER-BEH (INDX)               
133100                                                                          
133200         ADD +1                   TO INDX                                 
133300         PERFORM UNTIL INDX > MAX-INDX                                    
133400           PERFORM MFS-ERASE-LINE-FIELD-OUT                               
133500           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)          
133600           ADD +1                 TO INDX                                 
133700         END-PERFORM                                                      
133800       ELSE                                                               
133900         IF ANM-KDLEVANM          = '4'                                   
134000           MOVE '7'               TO ANM-KDLEVANM                         
134100           MOVE 0                 TO ANM-KDLEVATT                         
134200           MOVE +0                TO ANM-KVRADER-RT                       
134300                                     ANM-KVRADER-OBEH                     
134400           PERFORM IMS-REPL-KREE                                          
134500           PERFORM IMS-GHNP-KREE11                                        
134600           PERFORM UNTIL SEGMENT-MISSING                                  
134700             IF LEV-KDKREBEH (1:1) = 'Y' OR 'J' OR 'C'                    
134800               MOVE LEV-KDANMORS TO OKOD-KDANMORS                         
134900               CALL W418OKOD USING OKOD-W418OKOD                          
135000               IF OKOD-FL-RETILL = 'J' OR                                 
135100                  OKOD-FL-INTERNUPPACKNING = 'J'                          
135200                                                                          
135300                 MOVE 'ANN'       TO LEV-KDKREBEH                         
135400                 MOVE 'J'         TO LEV-FLANNULL                         
135500                 PERFORM IMS-REPL-KREE                                    
135600               END-IF                                                     
135700             END-IF                                                       
135800             PERFORM IMS-GHNP-KREE11                                      
135900           END-PERFORM                                                    
136000                                                                          
136100* - NÄR HELA LA ANNULLERAS SKALL MAN ÄVEN TA BORT DEN FR ATTESTKÖN        
136200* - OBS! SKALL EJ GÄLLA USA/CAN,JAPAN DC61 OCH AUSTRALIEN DC62            
136300           MOVE WS-SPAR-IDFTG    TO WS-IDFTG                              
136310           IF IDFTG-PV OR IDFTG-CN                                        
136400             PERFORM IMS-GHU-WDR501-4103                                  
136500             IF SEGMENT-FOUND                                             
136600               PERFORM IMS-DLET-WDR501-4103                               
136700             END-IF                                                       
136800           END-IF                                                         
136900                                                                          
137000           MOVE INF-LINE-UPDATED TO MED-IDMFSINF                          
137100           CALL WMEDKONV USING MED-WMEDAREA                               
137200           MOVE MED-MFSINF       TO MOD-TEMFSINF                          
137300           PERFORM MFS-ERASE-INPUT-FIELD                                  
137400*                                                                         
137500           PERFORM IMS-GU-KREE01                                          
137600           MOVE +1                TO INDX                                 
137700           MOVE ANM-IDDISTR       TO MOD-IDDISTR  (INDX)                  
137800                                           MOD-IDDISTR-ENTER              
137900           MOVE ANM-IDKUNDNR      TO MOD-IDKUNDNR (INDX)                  
138000                                           MOD-IDKUNDNR-ENTER             
138100           MOVE ANM-IDRAPPNR      TO MOD-IDRAPPNR (INDX)                  
138200                                           MOD-IDRAPPNR-ENTER             
138300           MOVE ANM-IDUSER        TO MOD-IDUSER   (INDX)                  
138400           MOVE ANM-KDLEVANM      TO MOD-KDLEVANM (INDX)                  
138500                                           MOD-KDLEVANM-ENTER             
138600           MOVE ANM-DALEVANM(3:6) TO MOD-TILEVANM (INDX)                  
138700           MOVE ANM-DARETILL(3:6) TO MOD-TIRETILL (INDX)                  
138800                                                                          
138900           MOVE +0                TO WS-KVRADER-TOT                       
139000                                           WS-KVRADER-BEH                 
139100           PERFORM IMS-GNP-KREE11                                         
139200           PERFORM UNTIL SEGMENT-MISSING                                  
139300             ADD +1               TO WS-KVRADER-TOT                       
139400             IF LEV-KDKREBEH     NOT = 'R  ' AND 'RR '                    
139500                ADD +1            TO WS-KVRADER-BEH                       
139600             END-IF                                                       
139700             PERFORM IMS-GNP-KREE11                                       
139800           END-PERFORM                                                    
139900           MOVE WS-KVRADER-TOT    TO MOD-KVRADER-TOT (INDX)               
140000           MOVE WS-KVRADER-BEH    TO MOD-KVRADER-BEH (INDX)               
140100                                                                          
140200           ADD +1                 TO INDX                                 
140300           PERFORM UNTIL INDX > MAX-INDX                                  
140400             PERFORM MFS-ERASE-LINE-FIELD-OUT                             
140500             MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)        
140600             ADD +1               TO INDX                                 
140700           END-PERFORM                                                    
140800         END-IF                                                           
140900       END-IF                                                             
141000     END-IF                                                               
141100     .                                                                    
141200     EJECT                                                                
141300 I-SHOW-DISCREPANCY SECTION.                                              
141400                                                                          
141500     PERFORM IMS-GU-KREE01                                                
141600                                                                          
141700     MOVE SPAR-KDCMDVAL TO OK-SEL-SW                                      
141800     IF OK-GODKANN                                                        
141900       PERFORM IA-SHOW-DISCREPANCY-60-62                                  
142000     ELSE                                                                 
142100       IF OK-DELETE                                                       
142200         IF SEGMENT-FOUND                                                 
142300           MOVE MSGI-IDDC             TO W-IDDC-A3                        
142400           MOVE ANM-IDDISTR           TO W-IDDISTR-A3                     
142500           MOVE ANM-IDKUNDNR          TO W-IDKUNDNR-A3                    
142600           MOVE ANM-IDRAPPNR          TO W-IDRAPPNR-A3                    
142700           PERFORM IMS-GU-WLRETA01                                        
142800           IF SEGMENT-FOUND                                               
142900               MOVE ERR-CANCEL-NOT-POSS TO MED-IDMFSFEL                   
143000               CALL WMEDKONV USING MED-WMEDAREA                           
143100               MOVE MED-MFSFEL        TO MOD-TEMFSFEL                     
143200               PERFORM MFS-NOT-TOUCH-FIELD                                
143300               MOVE MFS-CLOSE-FIELD   TO MOD-FLSVAR-GODKANN-ATTR          
143400               MOVE MFS-ERASE-FIELD   TO MOD-FLSVAR-GODKANN               
143500           ELSE                                                           
143600             IF ANM-KDLEVANM          < '5'                               
143700               MOVE ANM-IDDISTR       TO MOD-IDDISTR-ANN                  
143800               MOVE ANM-IDKUNDNR      TO MOD-IDKUNDNR-ANN                 
143900               MOVE ANM-IDRAPPNR      TO MOD-IDRAPPNR-ANN                 
144000               MOVE MFS-OPEN-ALPHA-FIELD TO MOD-FLSVAR-ANN-ATTR           
144100               MOVE MFS-ADD-READ-HILIGHT-FIELD                            
144200                                        TO MOD-IDDISTR-ANN-ATTR           
144300                                           MOD-IDKUNDNR-ANN-ATTR          
144400                                           MOD-IDRAPPNR-ANN-ATTR          
144500               MOVE INF-OK-TO-CANCEL  TO MED-IDMFSFEL                     
144600               CALL WMEDKONV USING MED-WMEDAREA                           
144700               MOVE MED-MFSFEL        TO MOD-TEMFSINF                     
144800                                                                          
144900               MOVE MFS-CLOSE-FIELD   TO MOD-FLSVAR-GODKANN-ATTR          
145000               MOVE MFS-ERASE-FIELD   TO MOD-FLSVAR-GODKANN               
145100             ELSE                                                         
145200               MOVE ERR-CANCEL-NOT-POSS TO MED-IDMFSFEL                   
145300               CALL WMEDKONV USING MED-WMEDAREA                           
145400               MOVE MED-MFSFEL        TO MOD-TEMFSFEL                     
145500               PERFORM MFS-NOT-TOUCH-FIELD                                
145600               MOVE MFS-CLOSE-FIELD   TO MOD-FLSVAR-GODKANN-ATTR          
145700               MOVE MFS-ERASE-FIELD   TO MOD-FLSVAR-GODKANN               
145800             END-IF                                                       
145900           END-IF                                                         
146000         END-IF                                                           
146100       END-IF                                                             
146200     END-IF                                                               
146300     .                                                                    
146400     EJECT                                                                
146500 IA-SHOW-DISCREPANCY-60-62 SECTION.                                       
146600                                                                          
146700     MOVE YES TO KOD-60-62-SW                                             
146800     MOVE NOO TO WS-MATRIX-Q                                              
146900                                                                          
147000     IF SEGMENT-FOUND                                                     
147100       IF ANM-KDLEVANM          = '1'                                     
147200         MOVE ANM-IDDISTR       TO MOD-IDDISTR-ANN                        
147300         MOVE ANM-IDKUNDNR      TO MOD-IDKUNDNR-ANN                       
147400         MOVE ANM-IDRAPPNR      TO MOD-IDRAPPNR-ANN                       
147500                                                                          
147600         PERFORM IMS-GNP-KREE11                                           
147700         PERFORM UNTIL SEGMENT-MISSING                                    
147800           IF LEV-KDANMORS = '60' OR '62'                                 
147900             IF LEV-KDKREBEH = 'Q  ' OR 'P  '                             
148000               MOVE YES   TO  WS-MATRIX-Q                                 
148100               MOVE NOO   TO  KOD-60-62-SW                                
148200             END-IF                                                       
148300           ELSE                                                           
148400             MOVE NOO     TO  KOD-60-62-SW                                
148500           END-IF                                                         
148600           PERFORM IMS-GNP-KREE11                                         
148700         END-PERFORM                                                      
148800                                                                          
148900         IF BARA-KOD-60-62                                                
149000           MOVE MFS-OPEN-ALPHA-FIELD TO MOD-FLSVAR-GODKANN-ATTR           
149100           MOVE MFS-ADD-READ-HILIGHT-FIELD                                
149200                                    TO MOD-IDDISTR-ANN-ATTR               
149300                                       MOD-IDKUNDNR-ANN-ATTR              
149400                                       MOD-IDRAPPNR-ANN-ATTR              
149500           MOVE INF-PRESS-PF11    TO MED-IDMFSFEL                         
149600           CALL WMEDKONV USING MED-WMEDAREA                               
149700           MOVE MED-MFSFEL        TO MOD-TEMFSINF                         
149800                                                                          
149900           MOVE MFS-CLOSE-FIELD     TO MOD-FLSVAR-ANN-ATTR                
150000           MOVE MFS-ERASE-FIELD     TO MOD-FLSVAR-ANN                     
150100         ELSE                                                             
150200           IF WS-MATRIX-Q = YES                                           
150300             MOVE SPACE             TO MOD-TEMFSINF                       
150400             MOVE 'MATRIX CONFLICT' TO MOD-TEMFSINF                       
150500           END-IF                                                         
150600                                                                          
150700           MOVE ERR-UPDATE-NOT-OK   TO MED-IDMFSFEL                       
150800           CALL WMEDKONV USING MED-WMEDAREA                               
150900           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
151000           PERFORM MFS-NOT-TOUCH-FIELD                                    
151100           MOVE MFS-CLOSE-FIELD     TO MOD-FLSVAR-ANN-ATTR                
151200                                       MOD-FLSVAR-GODKANN-ATTR            
151300           MOVE MFS-ERASE-FIELD     TO MOD-FLSVAR-ANN                     
151400                                       MOD-FLSVAR-GODKANN                 
151500         END-IF                                                           
151600       ELSE                                                               
151700         MOVE ERR-UPDATE-NOT-OK   TO MED-IDMFSFEL                         
151800         CALL WMEDKONV USING MED-WMEDAREA                                 
151900         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
152000         PERFORM MFS-NOT-TOUCH-FIELD                                      
152100         MOVE MFS-CLOSE-FIELD     TO MOD-FLSVAR-ANN-ATTR                  
152200                                     MOD-FLSVAR-GODKANN-ATTR              
152300         MOVE MFS-ERASE-FIELD     TO MOD-FLSVAR-ANN                       
152400                                     MOD-FLSVAR-GODKANN                   
152500       END-IF                                                             
152600     END-IF                                                               
152700     .                                                                    
152800     EJECT                                                                
152900 J-GODK-ADM-KONTROLL  SECTION.                                            
153000                                                                          
153100     MOVE YES TO GODK-ADM-DC-SW                                           
153200                                                                          
153300     MOVE MID-IDDISTR (INDX)   TO W-IDDISTR                               
153400     MOVE MID-IDKUNDNR (INDX)  TO W-IDKUNDNR                              
153500     MOVE MID-IDRAPPNR (INDX)  TO W-IDRAPPNR                              
153600     INSPECT W-IDRAPPNR REPLACING LEADING SPACE BY ZERO                   
153700                                                                          
153800     PERFORM IMS-GU-KREE01                                                
153900     PERFORM IMS-GNP-KREE11                                               
154000                                                                          
154100     PERFORM UNTIL SEGMENT-MISSING OR EJ-GODK-ADM-DC                      
154200       IF LEV-IDDC NOT = W-IDDC-B6                                        
154300         MOVE LEV-IDDC  TO W-IDDC-B6                                      
154400         PERFORM IMS-GU-WDB601                                            
154500       END-IF                                                             
154600       IF DCS-NDC-PF                                                      
154700         CONTINUE                                                         
154800       ELSE                                                               
154900         MOVE 'DISC'          TO W-KDARBTYP-6327                          
155000         MOVE LEV-IDDC        TO W-IDDC-6327                              
155100         MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                        
155200         MOVE ZERO            TO W-SUBEL-6328-MIN                         
155300         MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                        
155400         MOVE 9999999         TO W-SUBEL-6328-MAX                         
155500         MOVE MSGI-IDUSER     TO W-IDUSER-GODK-6328                       
155600                                                                          
155700         PERFORM IMS-GU-WDGX6327                                          
155800         IF SEGMENT-FOUND                                                 
155900           PERFORM IMS-GNP-WDGX6328                                       
156000           IF SEGMENT-MISSING                                             
156100             MOVE NOO                    TO INDATA-SW                     
156200             MOVE NOO                    TO GODK-ADM-DC-SW                
156300             MOVE ERR-EJ-BEHORIG-GODK-LA TO MED-IDMFSFEL                  
156400             CALL WMEDKONV USING MED-WMEDAREA                             
156500             MOVE MED-MFSFEL             TO MOD-TEMFSFEL                  
156600             PERFORM MFS-NOT-TOUCH-FIELD                                  
156700             MOVE MFS-CLOSE-FIELD  TO MOD-FLSVAR-ANN-ATTR                 
156800                                      MOD-FLSVAR-GODKANN-ATTR             
156900             MOVE MFS-ERASE-FIELD  TO MOD-FLSVAR-ANN                      
157000                                      MOD-FLSVAR-GODKANN                  
157100           END-IF                                                         
157200         ELSE                                                             
157300           MOVE NOO                    TO INDATA-SW                       
157400           MOVE NOO                    TO GODK-ADM-DC-SW                  
157500           MOVE ERR-EJ-BEHORIG-GODK-LA TO MED-IDMFSFEL                    
157600           CALL WMEDKONV USING MED-WMEDAREA                               
157700           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
157800           PERFORM MFS-NOT-TOUCH-FIELD                                    
157900           MOVE MFS-CLOSE-FIELD  TO MOD-FLSVAR-ANN-ATTR                   
158000                                    MOD-FLSVAR-GODKANN-ATTR               
158100           MOVE MFS-ERASE-FIELD  TO MOD-FLSVAR-ANN                        
158200                                    MOD-FLSVAR-GODKANN                    
158300         END-IF                                                           
158400       END-IF                                                             
158500       IF EJ-GODK-ADM-DC                                                  
158600         CONTINUE                                                         
158700       ELSE                                                               
158800         PERFORM IMS-GNP-KREE11                                           
158900       END-IF                                                             
159000     END-PERFORM                                                          
159100     .                                                                    
159200     EJECT                                                                
159300 K-INSERT-4712-MID SECTION.                                               
159400                                                                          
159500     MOVE MFS-KDMFSFOR       TO ALT-SPRAK                                 
159600     MOVE WS-IDDISTR-JUMP    TO ALT-IDDISTR-IN                            
159700     INSPECT ALT-IDDISTR-IN  REPLACING LEADING SPACE BY ZERO              
159800     MOVE WS-IDKUNDNR-JUMP   TO ALT-IDKUNDNR-IN                           
159900     INSPECT ALT-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO              
160000     MOVE WS-IDRAPPNR-JUMP   TO ALT-IDRAPPNR-IN                           
160100     INSPECT ALT-IDRAPPNR-IN REPLACING LEADING SPACE BY ZERO              
160200     PERFORM IMS-INSERT-ALT-MSG                                           
160300     .                                                                    
160400     EJECT                                                                
160500 L-UPDATE-CODE-60-62 SECTION.                                             
160600                                                                          
160700     PERFORM IMS-GHU-KREE01                                               
160800     IF SEGMENT-FOUND                                                     
160900       IF ANM-KDLEVANM    = '1'                                           
160910         MOVE MSGI-IDFTG   TO WS-IDFTG                                    
161000         IF IDFTG-US OR IDFTG-CA                                          
161100           CONTINUE                                                       
161200         ELSE                                                             
161300           IF ANM-IDUSER-ADM = SPACE                                      
161400             MOVE MSGI-IDUSER     TO ANM-IDUSER-ADM                       
161500             MOVE MSGI-BEANST     TO ANM-BEANST                           
161600           END-IF                                                         
161700         END-IF                                                           
161800         MOVE '3'                 TO ANM-KDLEVANM                         
161900         PERFORM IMS-REPL-KREE                                            
162000         PERFORM IMS-GHNP-KREE11                                          
162100         PERFORM UNTIL SEGMENT-MISSING                                    
162200           MOVE 'Y  '             TO LEV-KDKREBEH                         
162300           PERFORM IMS-REPL-KREE                                          
162400           PERFORM IMS-GHNP-KREE11                                        
162500         END-PERFORM                                                      
162600         MOVE INF-LINE-UPDATED    TO MED-IDMFSFEL                         
162700         CALL WMEDKONV USING MED-WMEDAREA                                 
162800         MOVE MED-MFSFEL          TO MOD-TEMFSINF                         
162900         PERFORM MFS-ERASE-INPUT-FIELD                                    
163000                                                                          
163100         PERFORM IMS-GU-KREE01                                            
163200         MOVE +1                  TO INDX                                 
163300         MOVE ANM-IDDISTR         TO MOD-IDDISTR  (INDX)                  
163400                                     MOD-IDDISTR-ENTER                    
163500         MOVE ANM-IDKUNDNR        TO MOD-IDKUNDNR (INDX)                  
163600                                     MOD-IDKUNDNR-ENTER                   
163700         MOVE ANM-IDRAPPNR        TO MOD-IDRAPPNR (INDX)                  
163800                                     MOD-IDRAPPNR-ENTER                   
163900         MOVE ANM-IDUSER          TO MOD-IDUSER   (INDX)                  
164000         MOVE ANM-KDLEVANM        TO MOD-KDLEVANM (INDX)                  
164100                                     MOD-KDLEVANM-ENTER                   
164200         MOVE ANM-DALEVANM(3:6)   TO MOD-TILEVANM (INDX)                  
164300         MOVE ANM-DARETILL(3:6)   TO MOD-TIRETILL (INDX)                  
164400                                                                          
164500         MOVE +0                  TO WS-KVRADER-TOT                       
164600                                     WS-KVRADER-BEH                       
164700         PERFORM IMS-GNP-KREE11                                           
164800         PERFORM UNTIL SEGMENT-MISSING                                    
164900           ADD +1                 TO WS-KVRADER-TOT                       
165000                                     WS-KVRADER-BEH                       
165100           PERFORM IMS-GNP-KREE11                                         
165200         END-PERFORM                                                      
165300         MOVE WS-KVRADER-TOT      TO MOD-KVRADER-TOT (INDX)               
165400         MOVE WS-KVRADER-BEH      TO MOD-KVRADER-BEH (INDX)               
165500                                                                          
165600         ADD +1                   TO INDX                                 
165700         PERFORM UNTIL INDX > MAX-INDX                                    
165800           PERFORM MFS-ERASE-LINE-FIELD-OUT                               
165900           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)          
166000           ADD +1                 TO INDX                                 
166100         END-PERFORM                                                      
166200       END-IF                                                             
166300     END-IF                                                               
166400     .                                                                    
166500     EJECT                                                                
166600 S01-KOLLA-BEHORIGHET SECTION.                                            
166700                                                                          
166800     MOVE 'F'                     TO SEC-KDSVAR                           
166900     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
167000                   SEGMENT-MISSING         OR                             
167100                   SEGMENT-FINISH                                         
167200        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
167300        MOVE '4711'               TO SEC-IDTRANS                          
167400        MOVE SEQA-IDDISTR         TO WS-IDDISTR                           
167500        MOVE WS-IDDISTR           TO SEC-IDKEY                            
167600                                                                          
167700        CALL WSECURIT USING SEC-IDUSER                                    
167800                            SEC-IDTRANS                                   
167900                            SEC-IDKEY                                     
168000                            SEC-KDSVAR                                    
168100                                                                          
168200        IF SEC-KDSVAR = 'F'                                               
168300           PERFORM IMS-GN-WDA2A1                                          
168400        END-IF                                                            
168500     END-PERFORM                                                          
168600     .                                                                    
168700     EJECT                                                                
168800 MFS-NOT-TOUCH-FIELD SECTION.                                             
168900                                                                          
169000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLEVANM-ENTER                    
169100                                    MOD-IDDISTR-ENTER                     
169200                                    MOD-IDKUNDNR-ENTER                    
169300                                    MOD-IDRAPPNR-ENTER                    
169400                                    MOD-KDLEVANM-NEXT                     
169500                                    MOD-IDDISTR-NEXT                      
169600                                    MOD-IDKUNDNR-NEXT                     
169700                                    MOD-IDRAPPNR-NEXT                     
169800                                    MOD-IDDISTR-ANN                       
169900                                    MOD-IDKUNDNR-ANN                      
170000                                    MOD-IDRAPPNR-ANN                      
170100                                    MOD-FLSVAR-ANN                        
170200                                    MOD-FLSVAR-GODKANN                    
170300     MOVE +1                       TO INDX                                
170400     PERFORM UNTIL INDX > MAX-INDX                                        
170500       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR     (INDX)              
170600                                      MOD-IDKUNDNR    (INDX)              
170700                                      MOD-IDRAPPNR    (INDX)              
170800                                      MOD-IDUSER      (INDX)              
170900                                      MOD-KDLEVANM    (INDX)              
171000                                      MOD-TILEVANM    (INDX)              
171100                                      MOD-TIRETILL    (INDX)              
171200                                      MOD-KVRADER-BEH (INDX)              
171300                                      MOD-KVRADER-TOT (INDX)              
171400                                      MOD-KDCMDVAL    (INDX)              
171500       ADD +1                    TO INDX                                  
171600     END-PERFORM                                                          
171700     .                                                                    
171800     EJECT                                                                
171900 MFS-ERASE-FIELD-OUT SECTION.                                             
172000                                                                          
172100*    --- ALLA UTDATA-FÄLT                                                 
172200*    --- INCL. SCROLL KEYS                                                
172300     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                               
172400                             MOD-IDKUNDNR-UT                              
172500                             MOD-KDLEVANM-UT                              
172600                             MOD-KDLEVANM-ENTER                           
172700                             MOD-IDDISTR-ENTER                            
172800                             MOD-IDKUNDNR-ENTER                           
172900                             MOD-IDRAPPNR-ENTER                           
173000                             MOD-KDLEVANM-NEXT                            
173100                             MOD-IDDISTR-NEXT                             
173200                             MOD-IDKUNDNR-NEXT                            
173300                             MOD-IDRAPPNR-NEXT                            
173400                             MOD-IDDISTR-ANN                              
173500                             MOD-IDKUNDNR-ANN                             
173600                             MOD-IDRAPPNR-ANN                             
173700     .                                                                    
173800 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
173900                                                                          
174000*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
174100     MOVE MFS-ERASE-FIELD          TO MOD-IDDISTR     (INDX)              
174200                                      MOD-IDKUNDNR    (INDX)              
174300                                      MOD-IDRAPPNR    (INDX)              
174400                                      MOD-IDUSER      (INDX)              
174500                                      MOD-KDLEVANM    (INDX)              
174600                                      MOD-TILEVANM    (INDX)              
174700                                      MOD-TIRETILL    (INDX)              
174800                                      MOD-KVRADER-BEH (INDX)              
174900                                      MOD-KVRADER-TOT (INDX)              
175000     .                                                                    
175100 MFS-ERASE-FIELD-IN SECTION.                                              
175200                                                                          
175300*    --- ALLA INDATA-FÄLT                                                 
175400     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
175500                             MOD-IDKUNDNR-IN                              
175600                             MOD-KDLEVANM-IN                              
175700     .                                                                    
175800     EJECT                                                                
175900 MFS-ERASE-INPUT-FIELD  SECTION.                                          
176000                                                                          
176100*    --- ALLA INDATA-FÄLT                                                 
176200     MOVE MFS-ERASE-FIELD TO MOD-FLSVAR-ANN                               
176300                             MOD-FLSVAR-GODKANN                           
176400                                                                          
176500     MOVE +1 TO INDX                                                      
176600     PERFORM UNTIL INDX > MAX-INDX                                        
176700       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL (INDX)                        
176800       ADD +1 TO INDX                                                     
176900     END-PERFORM                                                          
177000     .                                                                    
177100     EJECT                                                                
177200* --- IMS SECTIONS ---                                                    
177300 IMS-GET-MSG SECTION.                                                     
177400                                                                          
177500     MOVE '  QC' TO GOOD-STATUSCODES                                      
177600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
177700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
177800     PERFORM IMS-STATUSCHECK                                              
177900     .                                                                    
178000 IMS-INSERT-MSG SECTION.                                                  
178100                                                                          
178200     IF MSGI-IDLAND-SPR = 'GB'                                            
178300       MOVE 'N' TO MFS-KDHUVOMR                                           
178400     END-IF                                                               
178500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
178600     MOVE SPACE TO GOOD-STATUSCODES                                       
178700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
178800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178900     PERFORM IMS-STATUSCHECK                                              
179000     .                                                                    
179100     EJECT                                                                
179200 IMS-INSERT-ALT-MSG SECTION.                                              
179300                                                                          
179400     IF MSGI-IDLAND-SPR = 'GB'                                            
179500       MOVE 'N' TO MFS-KDHUVOMR                                           
179600     END-IF                                                               
179700     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
179800     MOVE SPACE TO GOOD-STATUSCODES                                       
179900     CALL CBLTDLI USING ISRT ALT-MSG-PCB ALT-MSG-IO-AREA                  
180000     MOVE ALT-MSG-STATUS-CODE TO STATUS-WS                                
180100     PERFORM IMS-STATUSCHECK                                              
180200     EJECT                                                                
180300     .                                                                    
180400 IMS-GU-WDA2A1   SECTION.                                                 
180500                                                                          
180600     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
180700                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X ')'                    
180800          DELIMITED BY SIZE INTO SSA1                                     
180900     MOVE '  GE' TO GOOD-STATUSCODES                                      
181000     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-AREA SSA1                     
181100     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
181200     PERFORM IMS-STATUSCHECK                                              
181300     .                                                                    
181400                                                                          
181500 IMS-GN-WDA2A1   SECTION.                                                 
181600                                                                          
181700     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
181800                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X ')'                    
181900          DELIMITED BY SIZE INTO SSA1                                     
182000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
182100     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-AREA SSA1                     
182200     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
182300     PERFORM IMS-STATUSCHECK                                              
182400     .                                                                    
182500     EJECT                                                                
182600 IMS-GU-KREE01    SECTION.                                                
182700                                                                          
182800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
182900          DELIMITED BY SIZE INTO SSA1                                     
183000     MOVE '    ' TO GOOD-STATUSCODES                                      
183100     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
183200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
183300     PERFORM IMS-STATUSCHECK                                              
183400     .                                                                    
183500                                                                          
183600 IMS-GNP-KREE11   SECTION.                                                
183700                                                                          
183800     MOVE 'WLKREE11'          TO SSA1                                     
183900     MOVE '  GE' TO GOOD-STATUSCODES                                      
184000     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
184100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
184200     PERFORM IMS-STATUSCHECK                                              
184300     .                                                                    
184400                                                                          
184500 IMS-GHU-KREE01   SECTION.                                                
184600                                                                          
184700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
184800          DELIMITED BY SIZE INTO SSA1                                     
184900     MOVE '    ' TO GOOD-STATUSCODES                                      
185000     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
185100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
185200     PERFORM IMS-STATUSCHECK                                              
185300     .                                                                    
185400                                                                          
185500 IMS-GHNP-KREE11   SECTION.                                               
185600                                                                          
185700     MOVE 'WLKREE11'          TO SSA1                                     
185800     MOVE '  GE' TO GOOD-STATUSCODES                                      
185900     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
186000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSCHECK                                              
186200     .                                                                    
186300                                                                          
186400 IMS-REPL-KREE SECTION.                                                   
186500                                                                          
186600     MOVE '  ' TO GOOD-STATUSCODES                                        
186700     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
186800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
186900     PERFORM IMS-STATUSCHECK                                              
187000     .                                                                    
187100     EJECT                                                                
187200 IMS-GET-410711                 SECTION.                                  
187300                                                                          
187400     STRING 'WL410701(WDGXKEY  =' W-WDGXKEY-4107-X ')'                    
187500          DELIMITED BY SIZE INTO SSA1                                     
187600     STRING 'WL410711(KDSEGKEY =' W-KDSEGKEY-X ')'                        
187700          DELIMITED BY SIZE INTO SSA2                                     
187800     MOVE '  GE' TO GOOD-STATUSCODES                                      
187900     CALL CBLTDLI USING GU 4107-PCB DLI-IO-AREA-0 SSA1 SSA2               
188000     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
188100     PERFORM IMS-STATUSCHECK                                              
188200     .                                                                    
188300     EJECT                                                                
188400 IMS-GU-WLRETA01  SECTION.                                                
188500                                                                          
188600     STRING 'WLRETA01(WDA3FSEQ =' W-WDA3FSEQ-X ')'                        
188700          DELIMITED BY SIZE INTO SSA1                                     
188800     MOVE '  GE' TO GOOD-STATUSCODES                                      
188900     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA-WDA3 SSA1                 
189000     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
189100     PERFORM IMS-STATUSCHECK                                              
189200     .                                                                    
189300     EJECT                                                                
189400 IMS-GU-WDGX6327 SECTION.                                                 
189500                                                                          
189600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6327-X ')'                    
189700          DELIMITED BY SIZE INTO SSA1                                     
189800     MOVE '  GE' TO GOOD-STATUSCODES                                      
189900     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6327 SSA1                  
190000     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUSCHECK                                              
190200     .                                                                    
190300     EJECT                                                                
190400 IMS-GNP-WDGX6328 SECTION.                                                
190500                                                                          
190600     STRING 'WDGX6328(KY6328  >=' W-KY6328-MIN-X                          
190700                    '&KY6328  <=' W-KY6328-MAX-X                          
190800                    '&IDUSERGK =' W-IDUSER-6328-X ')'                     
190900          DELIMITED BY SIZE INTO SSA1                                     
191000     MOVE '  GE' TO GOOD-STATUSCODES                                      
191100     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
191200     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
191300     PERFORM IMS-STATUSCHECK                                              
191400     .                                                                    
191500     EJECT                                                                
191600 IMS-GHU-WDR501-4103 SECTION.                                             
191700                                                                          
191800     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
191900          DELIMITED BY SIZE INTO SSA1                                     
192000     MOVE '  GE' TO GOOD-STATUSCODES                                      
192100     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4103 SSA1                 
192200     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
192300     PERFORM IMS-STATUSCHECK                                              
192400     .                                                                    
192500     SKIP3                                                                
192600 IMS-DLET-WDR501-4103 SECTION.                                            
192700                                                                          
192800     MOVE '  ' TO GOOD-STATUSCODES                                        
192900     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4103                     
193000     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
193100     PERFORM IMS-STATUSCHECK                                              
193200     .                                                                    
193300     EJECT                                                                
193400 IMS-GU-WDB601    SECTION.                                                
193500                                                                          
193600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
193700          DELIMITED BY SIZE INTO SSA1                                     
193800     MOVE '  ' TO GOOD-STATUSCODES                                        
193900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
194000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
194100     PERFORM IMS-STATUSCHECK                                              
194200     .                                                                    
194300     EJECT                                                                
194400 IMS-STATUSCHECK SECTION.                                                 
194500                                                                          
194600     SET STATUS-IX TO 1                                                   
194700     SEARCH GOOD-STATUS                                                   
194800       AT END                                                             
194900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
195000         DELIMITED BY SIZE INTO ERROR-TEXT                                
195100         CALL FELLOG                                                      
195200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
195300         CONTINUE                                                         
195400     END-SEARCH                                                           
195500     .                                                                    
