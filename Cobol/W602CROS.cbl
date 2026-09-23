000100*********************************************                             
000201 ID DIVISION.                                                             
000301 PROGRAM-ID.     W602CROS.                                                
000401 AUTHOR.         SRINADH NADIMPALLI.                                      
000501 DATE-WRITTEN.   24/08/28.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*    FUNCTION:                                                            
000901*        RECEIVE AND SHOW DC CROSS CASES                                  
001001*                                                                         
001101*        THE PROGRAM UPDATES   WDE6                                       
001201*                                                                         
001301*    ABENDCODES:                                                          
001401*        U0016 -  . . . .                                                 
001501*        U1000 -  . . . .                                                 
001601*                                                                         
001701                                                                          
001801     SKIP3                                                                
001901 ENVIRONMENT DIVISION.                                                    
002001     SKIP2                                                                
002101 INPUT-OUTPUT SECTION.                                                    
002201                                                                          
002301 FILE-CONTROL.                                                            
002401     EJECT                                                                
002501 DATA DIVISION.                                                           
002601     SKIP2                                                                
002701 FILE SECTION.                                                            
002801     EJECT                                                                
002901 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003101 77  IDPGM                       PIC X(8)    VALUE 'W602CROS'.            
003201 77  YES                         PIC X       VALUE 'J'.                   
003301 77  NOO                         PIC X       VALUE 'N'.                   
003401 77  MAX-INDX                    PIC S9(4)  VALUE +500 COMP SYNC.         
003501 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003601 77  CASE-RECEIVED               PIC X      VALUE 'N'.                    
003701 77  PRODNR-FOUND                PIC X      VALUE 'N'.                    
003801 77  PREV-CASE-SHIPPED           PIC X      VALUE 'N'.                    
003901 77  PREV-CASE-STATUS            PIC S9              COMP-3.              
004000 77  WS-KVANT                    PIC S9(5)   VALUE ZERO COMP.             
004101 77  WS-CDC                      PIC 9(2)   VALUE 11.                     
004200 77  WS-REC-KOLLI                PIC 9(6)    VALUE 0.                     
004300 77  WS-UNREC-KOLLI              PIC 9(6)    VALUE 0.                     
004400 77  WS-REC-VKORDBTO             PIC 9(6)V9  VALUE 0.                     
004500 77  WS-UNREC-VKORDBTO           PIC 9(6)V9  VALUE 0.                     
004600 77  WS-UNREC-VLORDBTO           PIC 9(4)V9(3) VALUE 0.                   
004700 77  WS-REC-VLORDBTO             PIC 9(4)V9(3) VALUE 0.                   
004801 77  SHOW-XD-SW                  PIC X       VALUE 'J'.                   
004901     88  SHOW-XD                             VALUE 'N'.                   
005000     EJECT                                                                
005100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES TODAYS-DATE.                                        
005300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005500     03  TODAYS-DATE-DAY         PIC 9(2).                                
005600 77  TODAYS-TIME                 PIC 9(8)    VALUE ZERO.                  
005700*01  WS-TIHHMMSS                 PIC 9(8)   VALUE ZERO.                   
005800*01  FILLER REDEFINES WS-TIHHMMSS.                                        
005900*    03 WS-TIHHMM                PIC 9(4).                                
006000*    03 FILLER                   PIC 9(4).                                
006100     EJECT                                                                
006201 01  FILLER                      PIC X(16)   VALUE 'WS-SECTION'.          
006301 01  WS-SECTION                  PIC X(32)   VALUE SPACE.                 
006401 01  FILLER                      PIC X(16)   VALUE                        
006501                                             'WS-IMS-SECTION'.            
006601 01  WS-IMS-SECTION              PIC X(32)   VALUE SPACE.                 
006700 01  WS-IDKUNDRF-GRP.                                                     
006800     03  WS-IDKUNDRF             PIC X(10).                               
006900     03  FILLER REDEFINES WS-IDKUNDRF.                                    
007000         05  WS-IDORDNR5         PIC 9(5).                                
007100         05  WS-IDORDNR5-FILLER  PIC X(5).                                
007200     03  FILLER REDEFINES WS-IDKUNDRF.                                    
007300         05  WS-IDORDNR7         PIC 9(7).                                
007400         05  FILLER              PIC X(3).                                
007500                                                                          
007600 01  WS-IDKUNDRF-IDORDNR5        PIC 9(5).                                
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
008301     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
008400     SKIP2                                                                
008500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008600*01  -COPY WL01TIDZ                                                       
008700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  ERROR-TEXT.                                                          
009200     03  FILLER                  PIC X(10)  VALUE 'ERROR-TEXT'.           
009300     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
009400     EJECT                                                                
009500*01  -COPY W403PLAT                                                       
009600     EJECT                                                                
009701*01  -COPY WWDC99                                                         
009801     EJECT                                                                
009900*    --- AREAS FOR IMS-SECTIONS                                           
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  KEYS-FOR-DLI.                                                        
010500     03  W-IDDC-B6-X.                                                     
010600         05 W-IDDC-B6            PIC X(2).                                
010700     03  W-KDKOLSTA-X.                                                    
010801         05  W-KDKOLSTA          PIC S9(1)   COMP-3 VALUE +6.             
010901     03  W-KDSEGKEY-X.                                                    
011001         05  W-KDSEGKEY         PIC  9(1)          VALUE 1.               
011101     03  W-IDDCCROSS-X.                                                   
011200         05  W-IDDCCROSS        PIC  X(2).                                
011301     03  W-IDDCSEND-X.                                                    
011401         05  W-IDDCSEND         PIC  X(2).                                
011500     03  W-IDKOLLI-X.                                                     
011600         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
011700     03  W-IDPRODNR-X.                                                    
011800         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
011900     03  W-IDPRODNR-ESEQ-X.                                               
012000         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
012100     03  W-WDE6H1KY-MIN.                                                  
012200         05 W-IDDC-CROSS-MIN     PIC X(2).                                
012300         05 FILLER               PIC X(21) VALUE LOW-VALUE.               
012400     03  W-WDE6H1KY-MAX.                                                  
012500         05 W-IDDC-CROSS-MAX     PIC X(2).                                
012600         05 FILLER               PIC X(21) VALUE HIGH-VALUE.              
012700     03  W-TIRFSDAT-X            PIC 9(6).                                
012800     03  W-IDDISTR-MIN-X.                                                 
012900         05  W-IDDISTR-MIN     PIC S9(5)           COMP-3.                
013000     03  W-IDDISTR-MAX-X.                                                 
013100         05  W-IDDISTR-MAX     PIC S9(5)           COMP-3.                
013200     03  W-IDKUNDNR-MIN-X.                                                
013300         05  W-IDKUNDNR-MIN    PIC S9(7)           COMP-3.                
013400     03  W-IDKUNDNR-MAX-X.                                                
013500         05  W-IDKUNDNR-MAX    PIC S9(7)           COMP-3.                
013600     03  W-IDLEVNR-MIN     PIC X(5).                                      
013700     03  W-IDLEVNR-MAX     PIC X(5).                                      
013800     03  W-IDSUPREF-MIN    PIC X(10).                                     
013900     03  W-IDSUPREF-MAX    PIC X(10).                                     
014000     03  W-IDTRPTNR-CROSS-MIN-X.                                          
014100         05 W-IDTRPTNR-CROSS-MIN  PIC S9(3)           COMP-3.             
014200     03  W-IDTRPTNR-CROSS-MAX-X.                                          
014300         05 W-IDTRPTNR-CROSS-MAX  PIC S9(3)           COMP-3.             
014400     03  W-TIRECDAT-X     PIC 9(6) VALUE ZERO.                            
014500     03  W-IDSHIPM-X       PIC 9(7).                                      
014600     03  W-WDE4ASEQ-X.                                                    
014700       05  W-4A1-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.        
014800       05  W-4A1-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.        
014900       05  W-4A1-IDKUNDRF.                                                
015000         07  W-4A1-IDORDNR         PIC X(5).                              
015100         07  FILLER                PIC X(5).                              
015200     SKIP2                                                                
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FOUND                       VALUE '  '.                  
015600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015800     88  SEGMENT-END                         VALUE 'GB'.                  
015900     SKIP2                                                                
016000 01  GOOD-STATUSCODES.                                                    
016100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(256).                              
016400 01  SSA2                        PIC X(64).                               
016500 01  SSA3                        PIC X(64).                               
016600     EJECT                                                                
016700 01  MESSAGE-CODES.                                                       
016800     05  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
016900     03  ERR-UNAUTHORIZED        PIC X(3)   VALUE '00A'.                  
017000     03  UPDATE-DONE             PIC X(3)   VALUE '001'.                  
017100     03  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
017200     03  IS-INVALID              PIC X(3)   VALUE '023'.                  
017300     03  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
017400     03  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
017500     03  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
017600     03  PRINTING-REQUESTED      PIC X(3)   VALUE '015'.                  
017700     03  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
017800     03  MUST-ENTER-EMP-ID       PIC X(3)   VALUE '026'.                  
017900     03  TRACKING-ID-MISSING     PIC X(3)   VALUE '422'.                  
018000     03  CASE-NOT-FOUND          PIC X(3)   VALUE '287'.                  
018100     03  RECEIVED-AT-WRONGDC-SENDTO PIC X(3)   VALUE '427'.               
018201     03  UPDATE-DONE-NEXT-DCCROS PIC X(3)   VALUE '428'.                  
018301     03  UPDATE-DONE-NEXT-TRPTNR PIC X(3)   VALUE '429'.                  
018401     03  WRONG-IDDC              PIC X(08)   VALUE 'IDDC'.                
018500*    --- IMS FUNCTION CODES                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE621'.                      
019000 01  DLI-IO-WDE621.                                                       
019100*    03  -COPY WDE621                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
019400 01  DLI-IO-WDE611.                                                       
019500*    03  -COPY WDE611                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
019800 01  DLI-IO-WDE601.                                                       
019900*    03  -COPY WDE601                                                     
020000     EJECT                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
020200 01  DLI-IO-WDE401.                                                       
020300*    03  -COPY WDE401                                                     
020400     EJECT                                                                
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
020600 01  DLI-IO-WDE101.                                                       
020700*    03  -COPY WDE101                                                     
020800     EJECT                                                                
020900 01  FILLER          PIC X(16) VALUE 'DLI-IO-WDE6H1'.                     
021000 01  DLI-IO-WDE6H1.                                                       
021100*    03  -COPY WDE6H1                                                     
021200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021300 01   DLI-IO-AREA-B601.                                                   
021401*     03  -COPY WDB601                                                    
021501     EJECT                                                                
021601 LINKAGE SECTION.                                                         
021700*01  -COPY W602CROS                                                       
021800     EJECT                                                                
021900*01  -COPY W0009  -PRE MSG-                                               
022000     EJECT                                                                
022100*01  -COPY W0008  -PRE WDE6-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008  -PRE WDE4-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE WDE41-                                             
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE WDE1-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008  -PRE WDE6H-                                             
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008  -PRE WDB6-                                              
023701     05  FILLER                  PIC X.                                   
023801 01  PLATS-DM-PCB                PIC X.                                   
023900 01  PLATS-DN-PCB                PIC X.                                   
024000 01  PLATS-DP-PCB                PIC X.                                   
024100 01  PLATS-DO-PCB                PIC X.                                   
024200 01  PLATS-WDE6C-PCB             PIC X.                                   
024300 01  PLATS-GMTC-PCB              PIC X.                                   
024400 01  PLATS-WDB6-PCB              PIC X.                                   
024500     EJECT                                                                
024600 PROCEDURE DIVISION  USING 602CROS-W602CROS                               
024700                           WDE6-PCB WDE4-PCB                              
024800                           WDE41-PCB                                      
024900                           WDE1-PCB WDE6H-PCB WDB6-PCB                    
025000                           PLATS-DM-PCB  PLATS-DN-PCB                     
025100                           PLATS-DP-PCB  PLATS-DO-PCB                     
025200                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
025300                           PLATS-WDB6-PCB.                                
025400 MAIN SECTION.                                                            
025500     ENTRY 'DLITCBL' USING 602CROS-W602CROS                               
025600                           WDE6-PCB WDE4-PCB                              
025700                           WDE41-PCB                                      
025800                           WDE1-PCB WDE6H-PCB WDB6-PCB                    
025900                           PLATS-DM-PCB  PLATS-DN-PCB                     
026000                           PLATS-DP-PCB  PLATS-DO-PCB                     
026100                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
026200                           PLATS-WDB6-PCB.                                
026300                                                                          
026400     PERFORM A-INIT                                                       
026500     PERFORM B-CHECK-KEYS                                                 
026601                                                                          
026700     IF 602CROS-KDPGMACT = 'E'                                            
026800        PERFORM H-UPPDATERA                                               
026900     ELSE                                                                 
027000        PERFORM F-READ-SHOW-INFO                                          
027100     END-IF                                                               
027201                                                                          
027300     PERFORM Z-FINIT                                                      
027400                                                                          
027500     MOVE ZERO TO RETURN-CODE                                             
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027901                                                                          
028000 A-INIT SECTION.                                                          
028100     MOVE 'A-INIT'                 TO WS-SECTION                          
028201                                                                          
028300     ACCEPT TODAYS-DATE FROM DATE                                         
028400     ACCEPT TODAYS-TIME FROM TIME                                         
028500*    MOVE TODAYS-TIME TO WS-TIHHMMSS                                      
028600     MOVE SPACES                   TO 602CROS-IDMSG-INFO                  
028700     .                                                                    
028800     EJECT                                                                
028901                                                                          
029000 B-CHECK-KEYS SECTION.                                                    
029101     MOVE 'B-CHECK-KEYS'           TO WS-SECTION                          
029201                                                                          
029301     MOVE 602CROS-IDDC-KEY         TO W-IDDC-CROSS-MIN                    
029401                                      W-IDDC-CROSS-MAX                    
029501                                      W-IDDCCROSS                         
029601                                      W-IDDC-B6                           
029701                                                                          
029801     IF 602CROS-KDPGMACT = 'E' AND 602CROS-KDCALL = 001                   
029901        MOVE +1                    TO INDX                                
030001        MOVE 602CROS-KVRADER       TO MAX-INDX                            
030101        PERFORM UNTIL INDX > MAX-INDX                                     
030201           IF 602CROS-IDKOLLI (INDX) > ZERO                               
030301              MOVE 602CROS-IDKOLLI (INDX)  TO W-IDKOLLI                   
030401           END-IF                                                         
030501           IF 602CROS-IDORDNR5(INDX) > ZERO                               
030601              MOVE 602CROS-IDORDNR5(INDX)  TO WS-IDKUNDRF                 
030701                                             W-4A1-IDKUNDRF               
030801           END-IF                                                         
030901           IF 602CROS-IDDISTR (INDX) > ZERO                               
031001              MOVE 602CROS-IDDISTR (INDX)  TO W-4A1-IDDISTR               
031101           END-IF                                                         
031201           IF 602CROS-IDKUNDNR (INDX) > ZERO                              
031301              MOVE 602CROS-IDKUNDNR (INDX) TO W-4A1-IDKUNDNR              
031401           END-IF                                                         
031501           ADD +1                          TO INDX                        
031601        END-PERFORM                                                       
031701     ELSE                                                                 
031800        MOVE 602CROS-IDDC-KEY      TO W-IDDC-CROSS-MIN                    
031900                                      W-IDDC-CROSS-MAX                    
032000                                      W-IDDCCROSS                         
032100        IF 602CROS-IDKUNDNR-KEY NOT = ZERO                                
032200           MOVE 602CROS-IDKUNDNR-KEY TO W-IDKUNDNR-MIN                    
032300                                        W-IDKUNDNR-MAX                    
032400        ELSE                                                              
032500           MOVE ZERO               TO W-IDKUNDNR-MIN                      
032600           MOVE +9999999           TO W-IDKUNDNR-MAX                      
032700        END-IF                                                            
032800                                                                          
032900        IF 602CROS-IDSUPREF-KEY NOT = SPACES                              
033000          MOVE 602CROS-IDSUPREF-KEY TO W-IDSUPREF-MIN                     
033100                                      W-IDSUPREF-MAX                      
033200        ELSE                                                              
033300          MOVE LOW-VALUE           TO W-IDSUPREF-MIN                      
033400          MOVE HIGH-VALUE          TO W-IDSUPREF-MAX                      
033500        END-IF                                                            
033600                                                                          
033700        IF 602CROS-IDLEVNR-KEY NOT = SPACES                               
033800          MOVE 602CROS-IDLEVNR-KEY TO W-IDLEVNR-MIN                       
033900                                      W-IDLEVNR-MAX                       
034000        ELSE                                                              
034100          MOVE LOW-VALUE           TO W-IDLEVNR-MIN                       
034200          MOVE HIGH-VALUE          TO W-IDLEVNR-MAX                       
034300        END-IF                                                            
034400        IF 602CROS-IDTRPTNR-CROSS-KEY NOT = ZERO                          
034500          MOVE 602CROS-IDTRPTNR-CROSS-KEY TO W-IDTRPTNR-CROSS-MIN         
034600                                             W-IDTRPTNR-CROSS-MAX         
034700        ELSE                                                              
034800          MOVE ZERO                TO W-IDTRPTNR-CROSS-MIN                
034900          MOVE +999                TO W-IDTRPTNR-CROSS-MAX                
035000        END-IF                                                            
035100                                                                          
035200        IF 602CROS-IDDISTR-KEY NOT = ZERO                                 
035300          MOVE 602CROS-IDDISTR-KEY TO W-IDDISTR-MIN                       
035400                                      W-IDDISTR-MAX                       
035500        ELSE                                                              
035600          MOVE ZERO                TO W-IDDISTR-MIN                       
035700          MOVE +99999              TO W-IDDISTR-MAX                       
035800        END-IF                                                            
035900                                                                          
036000        IF 602CROS-TIRFSDAT-KEY NOT = ZERO                                
036100          MOVE 602CROS-TIRFSDAT-KEY TO W-TIRFSDAT-X                       
036200        ELSE                                                              
036300          MOVE ZERO                TO W-TIRFSDAT-X                        
036400        END-IF                                                            
036501     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 H-UPPDATERA SECTION.                                                     
036901     MOVE 'H-UPPDATERA '           TO WS-SECTION                          
037001                                                                          
037100     MOVE +1                       TO INDX                                
037200     MOVE 602CROS-KVRADER          TO MAX-INDX                            
037301                                                                          
037400     PERFORM IMS-GU-WDB601                                                
037500     IF SEGMENT-MISSING                                                   
037600       MOVE WRONG-IDDC             TO 602CROS-IDMSG-INFO                  
037700     END-IF                                                               
037800                                                                          
037900     MOVE '011'                    TO MSGI-KDCALL                         
038000     MOVE DCS-IDTIDZON             TO MSGI-IDTIDZON                       
038100     MOVE TODAYS-DATE              TO MSGI-TILOKDAT                       
038200     MOVE TODAYS-TIME              TO MSGI-TILOKTID                       
038300                                                                          
038400     CALL WL01TIDZ                 USING MSGI-WL01TIDZ                    
038500     MOVE MSGI-TILOKDAT(1:6)       TO TODAYS-DATE                         
038601     MOVE MSGI-TILOKTID(1:4)       TO TODAYS-TIME(1:4)                    
038701                                                                          
038801     PERFORM UNTIL INDX > MAX-INDX                                        
038901         IF 602CROS-KDCMDVAL(INDX)= 'REC'                                 
039001            PERFORM HA-UPDATE-WDE621                                      
039101         END-IF                                                           
039201         ADD +1                    TO INDX                                
039301     END-PERFORM                                                          
039401     .                                                                    
039501     EJECT                                                                
039601 HA-UPDATE-WDE621 SECTION.                                                
039701     MOVE 'HA-UPDATE-WDE621'       TO WS-SECTION                          
039801                                                                          
039901     IF 602CROS-IDKOLLI (INDX) > ZERO                                     
040001        MOVE 602CROS-IDKOLLI (INDX) TO W-IDKOLLI                          
040101     END-IF                                                               
040200     IF 602CROS-IDORDNR5(INDX) > ZERO                                     
040300        MOVE 602CROS-IDORDNR5(INDX) TO WS-IDKUNDRF                        
040400                                       W-4A1-IDKUNDRF                     
040500     END-IF                                                               
040600     IF 602CROS-IDDISTR (INDX) > ZERO                                     
040700        MOVE 602CROS-IDDISTR (INDX) TO W-4A1-IDDISTR                      
040800     END-IF                                                               
040900     IF 602CROS-IDKUNDNR (INDX) > ZERO                                    
041000        MOVE 602CROS-IDKUNDNR (INDX) TO W-4A1-IDKUNDNR                    
041100     END-IF                                                               
041200     IF 602CROS-IDPRODNR (INDX) > ZERO                                    
041300        MOVE 602CROS-IDPRODNR (INDX) TO W-IDPRODNR                        
041400     ELSE                                                                 
041500        PERFORM IMS-GU-WDE401-ASEK                                        
041600        PERFORM UNTIL SEGMENT-MISSING                                     
041701           MOVE KORD-IDPRODNR        TO W-IDPRODNR                        
041800           IF KORD-IDDC = WS-CDC AND                                      
041900             KORD-KVORDRAD-LEVPL = ZERO                                   
042000             MOVE KORD-IDPRODNR      TO W-IDPRODNR                        
042100           END-IF                                                         
042200           PERFORM IMS-GN-WDE401-ASEK                                     
042300        END-PERFORM                                                       
042400     END-IF                                                               
042500                                                                          
042601     IF W-IDPRODNR = 0                                                    
042701        MOVE 'F' TO 602CROS-KDSVAR                                        
042801        MOVE CASE-NOT-FOUND TO 602CROS-IDMSG-INFO                         
042901     ELSE                                                                 
043001        PERFORM IMS-GU-WDE601                                             
043100        PERFORM IMS-GNP-WDE611                                            
043200        PERFORM IMS-GHNP-WDE621                                           
043300        IF SEGMENT-MISSING OR SEGMENT-END                                 
043400           MOVE 'F' TO 602CROS-KDSVAR                                     
043500        ELSE                                                              
043600         IF CROSS-TIRECXDAT > 0                                           
043700          MOVE YES TO CASE-RECEIVED                                       
043800         ELSE                                                             
043900          MOVE NOO TO CASE-RECEIVED                                       
044000         END-IF                                                           
044100         IF CASE-RECEIVED = 'N'                                           
044200          MOVE TODAYS-DATE      TO CROSS-TIRECXDAT                        
044300                                 602CROS-TIRECXDAT (INDX)                 
044401                                   CROSS-TIRFSDAT                         
044500          MOVE TODAYS-TIME(1:4) TO CROSS-TIRECXTID                        
044600                                602CROS-TIRECXTID (INDX)                  
044700          PERFORM HB-DEFINERA-PLATS                                       
044800          IF PLATS-KDSVAR = SPACE                                         
044900           MOVE PLATS-IDTRPTNR TO CROSS-IDTRPTNR-CROSS                    
045001                                 602CROS-IDTRPTNR-CROSS(INDX)             
045100           PERFORM IMS-REPL-WDE621                                        
045200           MOVE UPDATE-DONE-NEXT-TRPTNR TO 602CROS-IDMSG-INFO             
045300          IF 602CROS-KDCALL = 002                                         
045400             ADD KOLLI-VKORDBTO-KOLLI      TO                             
045501                                           602CROS-REC-VKORDBTO           
045600             ADD KOLLI-VLORDBTO-KOLLI      TO                             
045701                                           602CROS-REC-VLORDBTO           
045800             ADD +1                        TO 602CROS-REC-KVKOLLI         
045900             SUBTRACT 1                    FROM                           
046001                                           602CROS-UNREC-KVKOLLI          
046100             SUBTRACT KOLLI-VKORDBTO-KOLLI FROM                           
046201                                           602CROS-UNREC-VKORDBTO         
046300             SUBTRACT KOLLI-VLORDBTO-KOLLI FROM                           
046401                                           602CROS-UNREC-VLORDBTO         
046500          END-IF                                                          
046601                                                                          
046700          IF PLATS-IDDC-CROSS > SPACES                                    
046801             PERFORM IMS-GU-WDE601                                        
046901             PERFORM IMS-GNP-WDE611                                       
047000             PERFORM IMS-GHNP-WDE621-LAST                                 
047100             MOVE CROSS-KDSEGKEY TO W-KDSEGKEY                            
047200             ADD +1 TO W-KDSEGKEY                                         
047300             MOVE W-KDSEGKEY-X TO CROSS-KDSEGKEY                          
047400             MOVE PLATS-IDDC-CROSS  TO  CROSS-IDDC-CROSS                  
047501                                        602CROS-NEXT-TRPTNR-CROSS         
047600*            MOVE KOLLI-DARFS(3:6)  TO CROSS-TIRFSDAT                     
047701             MOVE ZERO              TO CROSS-TIRFSDAT                     
047800             MOVE KOLLI-IDDISTR     TO  CROSS-IDDISTR                     
047900             MOVE KOLLI-IDKUNDNR    TO  CROSS-IDKUNDNR                    
048000             MOVE VORD-IDPRODNR     TO  CROSS-IDPRODNR                    
048100             MOVE KOLLI-IDKOLLI     TO  CROSS-IDKOLLI                     
048200             MOVE KOLLI-IDLEVNR     TO  CROSS-IDLEVNR                     
048300             MOVE KOLLI-IDSUPREF    TO  CROSS-IDSUPREF                    
048400             MOVE 602CROS-IDDC-KEY  TO  CROSS-IDDC-SEND                   
048501             MOVE ZERO              TO  CROSS-IDTRPTNR-CROSS              
048601             MOVE ZERO              TO  CROSS-TIRECXDAT                   
048701             MOVE ZERO              TO  CROSS-TIRECXTID                   
048801             MOVE ZERO              TO  CROSS-TISKEPPN                    
048901             MOVE ZERO              TO  CROSS-IDSHIPM-CROSS               
049001             MOVE SPACE             TO  CROSS-IDLBBET-CROSS               
049101             MOVE 1                 TO  CROSS-KDKOLSTA-CROSS              
049200             PERFORM IMS-ISRT-WDE621                                      
049301*        MOVE UPDATE-DONE-NEXT-DCCROS TO 602CROS-IDMSG-INFO               
049400          END-IF                                                          
049500         END-IF                                                           
049600        END-IF                                                            
049700        MOVE SPACES TO 602CROS-KDSVAR                                     
049800       END-IF                                                             
049901     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050201                                                                          
050300 HB-DEFINERA-PLATS SECTION.                                               
050401     MOVE 'HB-DEFINERA-PLATS'      TO WS-SECTION                          
050500                                                                          
050600     IF KOLLI-KDORDKL = 4                                                 
050700        MOVE +1               TO PLATS-KDCALL                             
050800     ELSE                                                                 
050900        MOVE +2               TO PLATS-KDCALL                             
051000     END-IF                                                               
051100     MOVE CROSS-IDDC-CROSS    TO PLATS-IDDC                               
051200     MOVE KOLLI-IDDISTR       TO PLATS-IDDISTR                            
051300     MOVE KOLLI-IDKUNDNR      TO PLATS-IDKUNDNR                           
051400     MOVE VORD-KDFRAKT        TO PLATS-KDFRAKT                            
051500     MOVE WS-IDORDNR5         TO PLATS-IDORDNR                            
051600     MOVE KOLLI-KDORDKL       TO PLATS-KDORDKLX                           
051700     MOVE ZERO                TO PLATS-DIKOLLIH                           
051800                                 PLATS-DIKOLLIL                           
051900                                 PLATS-DIKOLLIB                           
052000                                 PLATS-VKORDNTO-KOLLI                     
052100     MOVE SPACE               TO PLATS-KDKOLLID                           
052200     MOVE SPACE               TO PLATS-ADFLGEO                            
052300     MOVE KOLLI-ADFLOMR       TO PLATS-ADFLOMR                            
052400     MOVE KOLLI-ADRUTNIV      TO PLATS-ADRUTNIV                           
052500     MOVE ZERO                TO PLATS-IDTRPTNR                           
052600                                 PLATS-DIHMODUL                           
052700                                 PLATS-DIDMODUL                           
052800                                 PLATS-ADVMODUL                           
052900                                 PLATS-ADHMODUL                           
053000     MOVE SPACE               TO PLATS-FLUTLAST                           
053100                                 PLATS-IDDC-CROSS                         
053200                                                                          
053300     CALL W403PLAT USING PLATS-W403PLAT                                   
053400                         PLATS-DM-PCB                                     
053500                         PLATS-DN-PCB                                     
053600                         PLATS-DP-PCB                                     
053700                         PLATS-DO-PCB                                     
053800                         PLATS-WDE6C-PCB                                  
053900                         PLATS-GMTC-PCB                                   
054000                         PLATS-WDB6-PCB                                   
054100     .                                                                    
054200     EJECT                                                                
054301                                                                          
054400 F-READ-SHOW-INFO SECTION.                                                
054501     MOVE 'F-READ-SHOW-INFO '         TO WS-SECTION                       
054601                                                                          
054701     IF 602CROS-KDCALL = 001                                              
054801                                                                          
054901       MOVE +1                         TO INDX                            
055001       MOVE 602CROS-KVRADER            TO MAX-INDX                        
055101       MOVE ' '                        TO 602CROS-KDSVAR                  
055201       PERFORM UNTIL INDX > MAX-INDX                                      
055301          IF 602CROS-IDKOLLI (INDX) > ZERO                                
055401             MOVE 602CROS-IDKOLLI (INDX) TO W-IDKOLLI                     
055501          END-IF                                                          
055601          IF 602CROS-IDORDNR5(INDX) > ZERO                                
055701             MOVE 602CROS-IDORDNR5(INDX) TO WS-IDKUNDRF                   
055801                                         W-4A1-IDKUNDRF                   
055901          END-IF                                                          
056001          IF 602CROS-IDDISTR (INDX) > ZERO                                
056101             MOVE 602CROS-IDDISTR (INDX) TO W-4A1-IDDISTR                 
056201          END-IF                                                          
056301          IF 602CROS-IDKUNDNR (INDX) > ZERO                               
056401             MOVE 602CROS-IDKUNDNR (INDX) TO W-4A1-IDKUNDNR               
056501          END-IF                                                          
056601          IF 602CROS-IDPRODNR (INDX) > ZERO                               
056701             MOVE 602CROS-IDPRODNR (INDX) TO W-IDPRODNR                   
056801          ELSE                                                            
056901             PERFORM IMS-GU-WDE401-ASEK                                   
057001             PERFORM UNTIL SEGMENT-MISSING OR                             
057101                           SEGMENT-END OR                                 
057201                           PRODNR-FOUND = 'Y'                             
057301               PERFORM FB-GET-WDE6-DATA                                   
057401               PERFORM IMS-GN-WDE401-ASEK                                 
057501             END-PERFORM                                                  
057601          END-IF                                                          
057701          ADD +1 TO INDX                                                  
057801       END-PERFORM                                                        
057901     ELSE                                                                 
058001       MOVE +1                 TO INDX                                    
058100       MOVE ZERO               TO WS-KVANT                                
058200       PERFORM IMS-GU-WDE6H1                                              
058300       IF SEGMENT-MISSING                                                 
058400          MOVE LINES-NOT-FOUND TO 602CROS-IDMSG-INFO                      
058501          MOVE ZERO            TO 602CROS-UNREC-VKORDBTO                  
058601          MOVE ZERO            TO 602CROS-UNREC-VLORDBTO                  
058701          MOVE ZERO            TO 602CROS-REC-VKORDBTO                    
058801          MOVE ZERO            TO 602CROS-REC-VLORDBTO                    
058901          MOVE ZERO            TO 602CROS-REC-KVKOLLI                     
059001          MOVE ZERO            TO 602CROS-UNREC-KVKOLLI                   
059101          MOVE ZERO            TO 602CROS-KVRADER                         
059200       ELSE                                                               
059300          PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-END OR                 
059400                        SEGMENT-MISSING                                   
059500             PERFORM FA-SHOW-ROWDATA                                      
059600             PERFORM IMS-GN-WDE6H1                                        
059700          END-PERFORM                                                     
059800          MOVE WS-UNREC-VKORDBTO TO 602CROS-UNREC-VKORDBTO                
059900          MOVE WS-UNREC-VLORDBTO TO 602CROS-UNREC-VLORDBTO                
060000          MOVE WS-REC-VKORDBTO   TO 602CROS-REC-VKORDBTO                  
060100          MOVE WS-REC-VLORDBTO   TO 602CROS-REC-VLORDBTO                  
060200          MOVE WS-REC-KOLLI      TO 602CROS-REC-KVKOLLI                   
060300          MOVE WS-UNREC-KOLLI    TO 602CROS-UNREC-KVKOLLI                 
060400          MOVE WS-KVANT          TO 602CROS-KVRADER                       
060500          IF WS-KVANT > 500                                               
060600             MOVE 500 TO 602CROS-KVRADER                                  
060700             MOVE TOO-MANY-LINES TO 602CROS-IDMSG-INFO                    
060800          END-IF                                                          
060900       END-IF                                                             
061001     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061301                                                                          
061400 FA-SHOW-ROWDATA SECTION.                                                 
061501     MOVE 'FA-SHOW-ROWDATA  '      TO WS-SECTION                          
061600                                                                          
061700     MOVE SEQH-IDPRODNR          TO W-IDPRODNR                            
061800     MOVE SEQH-IDKOLLI           TO W-IDKOLLI                             
061901     MOVE SEQH-IDDC-CROSS        TO W-IDDCCROSS                           
062001     MOVE NOO                    TO PREV-CASE-SHIPPED                     
062101                                                                          
062200     PERFORM IMS-GU-WDE611                                                
062301     PERFORM IMS-GNP-WDE621                                               
062401     PERFORM UNTIL SEGMENT-MISSING OR PREV-CASE-SHIPPED = 'J'             
062501        IF CROSS-IDDC-CROSS = W-IDDCCROSS                                 
062601          IF CROSS-KDSEGKEY = 1                                           
062701            MOVE YES                 TO PREV-CASE-SHIPPED                 
062801          ELSE                                                            
062901            IF PREV-CASE-STATUS > 6                                       
063001              MOVE YES               TO PREV-CASE-SHIPPED                 
063101            END-IF                                                        
063201          END-IF                                                          
063301        ELSE                                                              
063401          MOVE CROSS-KDKOLSTA-CROSS TO PREV-CASE-STATUS                   
063501        END-IF                                                            
063601        PERFORM IMS-GNP-WDE621                                            
063701     END-PERFORM                                                          
063801                                                                          
063901     IF KOLLI-KDKOLSTA > 6 AND PREV-CASE-SHIPPED = 'J'                    
064000       MOVE '602CROS'             TO IDPGM                                
064100       MOVE SEQH-IDPRODNR         TO W-IDPRODNR-ESEQ                      
064200       PERFORM IMS-GU-WDE401-ESEQ                                         
064300       IF SEGMENT-MISSING                                                 
064400         MOVE ZERO                TO KORD-IDORDNR5                        
064500       END-IF                                                             
064601                                                                          
064700       MOVE KOLLI-IDSHIPM         TO W-IDSHIPM-X                          
064801       MOVE YES                   TO SHOW-XD-SW                           
064901       MOVE W-IDDCCROSS           TO WS-IDDC                              
065001                                                                          
065101*DC26 DONT SHOW CASES PRIOR TO 15TH APR OR NO SHIPMENT IN WDE1 DB         
065102*DC1C DONT SHOW CASES IF NO SHIPMENT IN WDE1 DB                           
065200       PERFORM IMS-GU-WDE101                                              
065301                                                                          
065401       IF SEGMENT-MISSING                                                 
065501         MOVE ZERO                 TO SHIP-TISKEPPN                       
065601         IF SDC-AT OR LDC-SE-1C                                           
065701           MOVE NOO                TO SHOW-XD-SW                          
065801         END-IF                                                           
066001       ELSE                                                               
067001         IF SEGMENT-FOUND                                                 
067101           IF SDC-AT                   AND                                
068001              (SHIP-TISKEPPN = 260415  OR                                 
068101               SHIP-TISKEPPN < 260415)                                    
070101             MOVE NOO              TO SHOW-XD-SW                          
070201           END-IF                                                         
070301         END-IF                                                           
070401       END-IF                                                             
070501                                                                          
070601       IF SHOW-XD-SW = YES                                                
070701         PERFORM FAB-MOVE-XD-DATA                                         
070801       END-IF                                                             
070901     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071201                                                                          
071300 FAA-GET-IDLBBET-RFS SECTION.                                             
071401     MOVE 'FAA-GET-IDLBBET- '      TO WS-SECTION                          
071501                                                                          
071601     MOVE KOLLI-IDDC               TO WS-IDDC                             
071701                                                                          
071801     IF (SEQH-IDDC-SEND = KOLLI-IDDC) OR                                  
071901        (SEQH-IDDC-SEND = '11' AND GOOD-DDC)                              
072001        MOVE KOLLI-IDLBBET         TO 602CROS-IDLBBET (INDX)              
072101        MOVE KOLLI-DARFS(3:6)      TO 602CROS-TIRFSDAT(INDX)              
072201        MOVE KOLLI-IDSHIPM         TO W-IDSHIPM-X                         
072301        PERFORM IMS-GU-WDE101                                             
072401        IF SEGMENT-FOUND                                                  
072501           MOVE SHIP-TISKEPPN     TO 602CROS-TISKEPPN (INDX)              
072601        ELSE                                                              
072701           MOVE 0                 TO 602CROS-TISKEPPN (INDX)              
072801        END-IF                                                            
072901     ELSE                                                                 
073001        MOVE SEQH-IDDC-SEND        TO W-IDDCCROSS                         
073101        PERFORM IMS-GU-WDE621-IDDCSEND                                    
073201        IF SEGMENT-FOUND                                                  
073301*          MOVE CROSS-IDSHIPM-CROSS  TO W-IDSHIPM-X                       
073401           MOVE CROSS-TISKEPPN       TO 602CROS-TISKEPPN (INDX)           
073501           MOVE CROSS-IDLBBET-CROSS  TO 602CROS-IDLBBET (INDX)            
073601        END-IF                                                            
073701     END-IF                                                               
073801     .                                                                    
073901     EJECT                                                                
074001                                                                          
074101 FAB-MOVE-XD-DATA    SECTION.                                             
074201     MOVE 'FAB-MOVE-XD-DATA '      TO WS-SECTION                          
074301                                                                          
074401     IF INDX <= MAX-INDX                                                  
074501       IF 602CROS-NOTREC-KEY= 'Y' AND                                     
074601          SEQH-TIRECXDAT > ZERO                                           
074701          CONTINUE                                                        
074801       ELSE                                                               
074901          ADD 1                   TO WS-KVANT                             
075001          MOVE SEQH-IDPRODNR      TO 602CROS-IDPRODNR (INDX)              
075101          MOVE KORD-IDORDNR5      TO 602CROS-IDORDNR5 (INDX)              
075201          MOVE SEQH-IDDISTR       TO 602CROS-IDDISTR (INDX)               
075301          MOVE SEQH-IDKUNDNR      TO 602CROS-IDKUNDNR (INDX)              
075401          MOVE SEQH-IDPRODNR      TO 602CROS-IDPRODNR (INDX)              
075501          MOVE SEQH-IDKOLLI       TO 602CROS-IDKOLLI (INDX)               
075601          MOVE SEQH-IDLEVNR       TO 602CROS-IDLEVNR (INDX)               
075701          MOVE SEQH-IDSUPREF      TO 602CROS-IDSUPREF (INDX)              
075801          MOVE SEQH-IDTRPTNR-CROSS TO                                     
075901                               602CROS-IDTRPTNR-CROSS (INDX)              
076001                                                                          
076101          PERFORM FAA-GET-IDLBBET-RFS                                     
076201                                                                          
076301          MOVE SEQH-IDDC-SEND     TO 602CROS-IDDC-SEND (INDX)             
076401          MOVE SEQH-TIRECXDAT     TO 602CROS-TIRECXDAT (INDX)             
076501          MOVE KOLLI-IDPSN(1)     TO 602CROS-IDPSN (INDX)                 
076601          MOVE KOLLI-VKORDBTO-KOLLI TO 602CROS-VKORDBTO (INDX)            
076701          MOVE KOLLI-VLORDBTO-KOLLI TO 602CROS-VLORDBTO (INDX)            
076801          MOVE KOLLI-KDKOLLI      TO 602CROS-KDKOLLI (INDX)               
076901**        MOVE KOLLI-IDLBBET      TO 602CROS-IDLBBET (INDX)               
077001          MOVE KOLLI-DARFS(3:6)   TO 602CROS-TIRFSDAT(INDX)               
077101**        MOVE SHIP-TISKEPPN      TO 602CROS-TISKEPPN (INDX)              
077201          MOVE SEQH-TIRECXTID     TO 602CROS-TIRECXTID (INDX)             
077301                                                                          
077401          IF SEQH-TIRECXTID > 0                                           
077501            ADD +1                 TO WS-REC-KOLLI                        
077601            COMPUTE WS-REC-VKORDBTO = WS-REC-VKORDBTO +                   
077701                                      KOLLI-VKORDBTO-KOLLI                
077801            COMPUTE WS-REC-VLORDBTO = WS-REC-VLORDBTO +                   
077901                                      KOLLI-VLORDBTO-KOLLI                
078001          ELSE                                                            
078101            ADD +1                 TO WS-UNREC-KOLLI                      
078201            COMPUTE WS-UNREC-VKORDBTO = WS-UNREC-VKORDBTO +               
078301                                       KOLLI-VKORDBTO-KOLLI               
078401            COMPUTE WS-UNREC-VLORDBTO = WS-UNREC-VLORDBTO +               
078501                                      KOLLI-VLORDBTO-KOLLI                
078601          END-IF                                                          
078701          IF 602CROS-KDCMDVAL(INDX) = ALL '+' OR SPACE OR                 
078801                                     LOW-VALUE                            
078901            MOVE SPACE             TO 602CROS-KDCMDVAL(INDX)              
079001                              602CROS-IDMSG-ERROR-LINE(INDX)              
079101          END-IF                                                          
079201          ADD +1                   TO INDX                                
079301       END-IF                                                             
079401     END-IF                                                               
079501     .                                                                    
079601     EJECT                                                                
079701                                                                          
079801 FB-GET-WDE6-DATA SECTION.                                                
079901     MOVE 'FB-GET-WDE6-DATA '      TO WS-SECTION                          
080001                                                                          
080101     MOVE KORD-IDPRODNR               TO W-IDPRODNR                       
080201     MOVE KORD-IDDC                   TO W-IDDCSEND                       
080301     IF KORD-IDDC = WS-CDC AND                                            
080401        KORD-KVORDRAD-LEVPL = ZERO                                        
080501        MOVE KORD-IDPRODNR            TO W-IDPRODNR                       
080601     END-IF                                                               
080701                                                                          
080801     PERFORM IMS-GU-WDE601                                                
080901     PERFORM IMS-GNP-WDE611                                               
081001     PERFORM IMS-GNP-WDE621-RECDT-EQZERO                                  
081101                                                                          
081201     IF SEGMENT-MISSING                                                   
081301      MOVE 'F'                        TO 602CROS-KDSVAR                   
081401     END-IF                                                               
081501     IF SEGMENT-FOUND AND (CROSS-IDKOLLI = W-IDKOLLI)                     
081601       IF 602CROS-IDDC-KEY = CROSS-IDDC-CROSS                             
081701         CONTINUE                                                         
081801       ELSE                                                               
081901         MOVE RECEIVED-AT-WRONGDC-SENDTO                                  
082001                                      TO 602CROS-IDMSG-INFO               
082101         MOVE CROSS-IDDC-CROSS        TO                                  
082201                                    602CROS-IDTRPTNR-CROSS(INDX)          
082301       END-IF                                                             
082401       MOVE 'Y'                       TO PRODNR-FOUND                     
082501       MOVE ' '                       TO 602CROS-KDSVAR                   
082601       MOVE KORD-IDPRODNR             TO 602CROS-IDPRODNR (INDX)          
082701     END-IF                                                               
082801     .                                                                    
082901     EJECT                                                                
083001 Z-FINIT SECTION.                                                         
083101     .                                                                    
083200     EJECT                                                                
083300 S99-ABEND SECTION.                                                       
083400                                                                          
083500     CALL ABEND USING RKOD-ABEND                                          
083600     .                                                                    
083700     EJECT                                                                
083800* --- IMS SECTIONS  ---                                                   
083900                                                                          
084000     EJECT                                                                
084100 IMS-GU-WDE6H1 SECTION.                                                   
084201     MOVE 'IMS-GU-WDE6H1'             TO WS-IMS-SECTION                   
084300                                                                          
084400     STRING 'WDE6H1  (WDE6H1KY>=' W-WDE6H1KY-MIN                          
084500                    '&WDE6H1KY<=' W-WDE6H1KY-MAX                          
084600                    '&TIRFSDAT>=' W-TIRFSDAT-X                            
084700                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
084800                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
084900                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
085000                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X                        
085100                    '&IDTRPTNC>=' W-IDTRPTNR-CROSS-MIN-X                  
085200                    '&IDTRPTNC<=' W-IDTRPTNR-CROSS-MAX-X                  
085300                    '&IDLEVNR >=' W-IDLEVNR-MIN                           
085400                    '&IDLEVNR <=' W-IDLEVNR-MAX                           
085500                    '&IDSUPREF>=' W-IDSUPREF-MIN                          
085600                    '&IDSUPREF<=' W-IDSUPREF-MAX')'                       
085700            DELIMITED BY SIZE INTO SSA1                                   
085800     MOVE '  GE'            TO GOOD-STATUSCODES                           
085900     CALL CBLTDLI USING GU  WDE6H-PCB DLI-IO-WDE6H1 SSA1                  
086000     MOVE WDE6H-STATUS-CODE TO STATUS-WS                                  
086100     PERFORM IMS-STATUSCHECK                                              
086200     .                                                                    
086300     EJECT                                                                
086400 IMS-GN-WDE6H1 SECTION.                                                   
086501     MOVE 'IMS-GN-WDE6H1'             TO WS-IMS-SECTION                   
086600                                                                          
086700     STRING 'WDE6H1  (WDE6H1KY>=' W-WDE6H1KY-MIN                          
086800                    '&WDE6H1KY<=' W-WDE6H1KY-MAX                          
086900                    '&TIRFSDAT>=' W-TIRFSDAT-X                            
087000                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
087100                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
087200                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
087300                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X                        
087400                    '&IDTRPTNC>=' W-IDTRPTNR-CROSS-MIN-X                  
087500                    '&IDTRPTNC<=' W-IDTRPTNR-CROSS-MAX-X                  
087600                    '&IDLEVNR >=' W-IDLEVNR-MIN                           
087700                    '&IDLEVNR <=' W-IDLEVNR-MAX                           
087800                    '&IDSUPREF>=' W-IDSUPREF-MIN                          
087900                    '&IDSUPREF<=' W-IDSUPREF-MAX')'                       
088000            DELIMITED BY SIZE INTO SSA1                                   
088100     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
088200     CALL CBLTDLI USING GN  WDE6H-PCB DLI-IO-WDE6H1 SSA1                  
088300     MOVE WDE6H-STATUS-CODE TO STATUS-WS                                  
088400     PERFORM IMS-STATUSCHECK                                              
088500     .                                                                    
088600     EJECT                                                                
088700 IMS-GU-WDE601 SECTION.                                                   
088801     MOVE 'IMS-GU-WDE601'             TO WS-IMS-SECTION                   
088900                                                                          
089000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
089100          DELIMITED BY SIZE INTO SSA1                                     
089200     MOVE '  GE' TO GOOD-STATUSCODES                                      
089300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
089400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
089500     PERFORM IMS-STATUSCHECK                                              
089600     .                                                                    
089700     EJECT                                                                
089800 IMS-GNP-WDE611 SECTION.                                                  
089901     MOVE 'IMS-GNP-WDE611'            TO WS-IMS-SECTION                   
090000                                                                          
090100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '  GE' TO GOOD-STATUSCODES                                      
090400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
090500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSCHECK                                              
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-GU-WDE611 SECTION.                                                   
091001     MOVE 'IMS-GU-WDE611'             TO WS-IMS-SECTION                   
091100                                                                          
091200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
091300          DELIMITED BY SIZE INTO SSA1                                     
091400     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
091500          DELIMITED BY SIZE INTO SSA2                                     
091600     MOVE '  GE' TO GOOD-STATUSCODES                                      
091700     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
091800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
091900     PERFORM IMS-STATUSCHECK                                              
092000     .                                                                    
092100     EJECT                                                                
092201 IMS-GU-WDE621 SECTION.                                                   
092301     MOVE 'IMS-GU-WDE621'             TO WS-IMS-SECTION                   
092401                                                                          
092501     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
092601          DELIMITED BY SIZE INTO SSA1                                     
092701     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
092801          DELIMITED BY SIZE INTO SSA2                                     
092901     STRING 'WDE621  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
093001          DELIMITED BY SIZE INTO SSA3                                     
093101     MOVE '  GE' TO GOOD-STATUSCODES                                      
093201     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2 SSA3          
093301     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
093401     PERFORM IMS-STATUSCHECK                                              
093501     .                                                                    
093601     EJECT                                                                
093700 IMS-GU-WDE621-IDDCSEND SECTION.                                          
093801     MOVE 'IMS-GU-WDE621-IDDC'           TO WS-IMS-SECTION                
093900                                                                          
094000     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
094100          DELIMITED BY SIZE INTO SSA1                                     
094200     STRING 'WDE621  (IDDCCROS =' W-IDDCCROSS-X')'                        
094300          DELIMITED BY SIZE INTO SSA2                                     
094400     MOVE '  GE' TO GOOD-STATUSCODES                                      
094500     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE621 SSA1 SSA2               
094600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSCHECK                                              
094800     .                                                                    
094901 IMS-GHNP-WDE621 SECTION.                                                 
095001     MOVE 'IMS-GHNP-WDE621'           TO WS-IMS-SECTION                   
095101                                                                          
095201     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
095301          DELIMITED BY SIZE INTO SSA1                                     
095401     STRING 'WDE621  (IDDCCROS =' W-IDDCCROSS-X')'                        
095501          DELIMITED BY SIZE INTO SSA2                                     
095601     MOVE '  GE' TO GOOD-STATUSCODES                                      
095701     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE621 SSA1 SSA2             
095801     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
095901     PERFORM IMS-STATUSCHECK                                              
096001     .                                                                    
096100     EJECT                                                                
096201 IMS-GNP-WDE621 SECTION.                                                  
096301     MOVE 'IMS-GNP-WDE621'            TO WS-IMS-SECTION                   
096401                                                                          
096501     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
096601          DELIMITED BY SIZE INTO SSA1                                     
096701     MOVE   'WDE621  'TO SSA2                                             
096801     MOVE '  GE' TO GOOD-STATUSCODES                                      
096901     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE621 SSA1 SSA2              
097001     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
097101     PERFORM IMS-STATUSCHECK                                              
097201     .                                                                    
097301     EJECT                                                                
097401 IMS-GNP-WDE621-RECDT-EQZERO SECTION.                                     
097501     MOVE 'IMS-GNP-WDE621-RECDT-'     TO WS-IMS-SECTION                   
097601                                                                          
097701     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
097801          DELIMITED BY SIZE INTO SSA1                                     
097901     STRING 'WDE621  (TIRECDAT =' W-TIRECDAT-X ')'                        
098000          DELIMITED BY SIZE INTO SSA2                                     
098101     MOVE '  GE' TO GOOD-STATUSCODES                                      
098201     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE621 SSA1 SSA2              
098301     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
098401     PERFORM IMS-STATUSCHECK                                              
098501     .                                                                    
098601     EJECT                                                                
098701 IMS-GHNP-WDE621-LAST SECTION.                                            
098801     MOVE 'IMS-GHNP-WDE621-LAST'      TO WS-IMS-SECTION                   
098900                                                                          
099000     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
099100          DELIMITED BY SIZE INTO SSA1                                     
099200     MOVE   'WDE621  *L'TO SSA2                                           
099300     MOVE '  GE' TO GOOD-STATUSCODES                                      
099400     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE621 SSA1 SSA2             
099500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
099600     PERFORM IMS-STATUSCHECK                                              
099700     .                                                                    
099800     EJECT                                                                
099901 IMS-GNP-WDE621-FIRST SECTION.                                            
100001     MOVE 'IMS-GNP-WDE621-FIRST'      TO WS-IMS-SECTION                   
100101                                                                          
100201     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
100301          DELIMITED BY SIZE INTO SSA1                                     
100401     MOVE   'WDE621  *F'TO SSA2                                           
100501     MOVE '  GE' TO GOOD-STATUSCODES                                      
100601     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE621 SSA1 SSA2              
100701     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
100801     PERFORM IMS-STATUSCHECK                                              
100901     .                                                                    
101001     EJECT                                                                
101101 IMS-ISRT-WDE621 SECTION.                                                 
101201     MOVE 'IMS-ISRT-WDE621'           TO WS-IMS-SECTION                   
101300                                                                          
101400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
101500          DELIMITED BY SIZE INTO SSA1                                     
101600     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
101700          DELIMITED BY SIZE INTO SSA2                                     
101800     MOVE 'WDE621 ' TO SSA3                                               
101900     MOVE '  II' TO GOOD-STATUSCODES                                      
102000     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
102100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
102200     PERFORM IMS-STATUSCHECK                                              
102300     .                                                                    
102400     EJECT                                                                
102500 IMS-REPL-WDE621 SECTION.                                                 
102601     MOVE 'IMS-REPL-WDE621'           TO WS-IMS-SECTION                   
102700                                                                          
102800     MOVE 'WDE621 ' TO SSA1                                               
102900     MOVE '  ' TO GOOD-STATUSCODES                                        
103000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE621 SSA1                  
103100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
103200     PERFORM IMS-STATUSCHECK                                              
103300     .                                                                    
103400     EJECT                                                                
103500 IMS-GU-WDE101 SECTION.                                                   
103601     MOVE 'IMS-GU-WDE101'             TO WS-IMS-SECTION                   
103700                                                                          
103800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
103900          DELIMITED BY SIZE INTO SSA1                                     
104000     MOVE '  GE' TO GOOD-STATUSCODES                                      
104100     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
104200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
104300     PERFORM IMS-STATUSCHECK                                              
104400     .                                                                    
104500     SKIP3                                                                
104600 IMS-GU-WDE401-ESEQ SECTION.                                              
104701     MOVE 'IMS-GU-WDE401-ESEQ'        TO WS-IMS-SECTION                   
104801                                                                          
104900     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-ESEQ-X ')'                   
105000            DELIMITED BY SIZE INTO SSA1                                   
105100     MOVE '  GE'            TO GOOD-STATUSCODES                           
105200     CALL CBLTDLI USING GU  WDE4-PCB DLI-IO-WDE401 SSA1                   
105300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
105400     PERFORM IMS-STATUSCHECK                                              
105500     .                                                                    
105600     EJECT                                                                
105700 IMS-GU-WDE401-ASEK       SECTION.                                        
105801     MOVE 'IMS-GU-WDE401-ASEK'        TO WS-IMS-SECTION                   
105900                                                                          
106000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
106100     DELIMITED BY SIZE INTO SSA1                                          
106200     MOVE '  GE' TO GOOD-STATUSCODES                                      
106300     CALL CBLTDLI USING GU WDE41-PCB DLI-IO-WDE401 SSA1                   
106400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
106500     PERFORM IMS-STATUSCHECK                                              
106600     .                                                                    
106700     SKIP2                                                                
106800 IMS-GN-WDE401-ASEK       SECTION.                                        
106901     MOVE 'IMS-GN-WDE401-ASEK'        TO WS-IMS-SECTION                   
107000                                                                          
107100     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
107200     DELIMITED BY SIZE INTO SSA1                                          
107300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
107400     CALL CBLTDLI USING GN WDE41-PCB DLI-IO-WDE401 SSA1                   
107500     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
107600     PERFORM IMS-STATUSCHECK                                              
107700     .                                                                    
107800     EJECT                                                                
107901                                                                          
108000 IMS-GU-WDB601    SECTION.                                                
108101     MOVE 'IMS-GU-WDB601'             TO WS-IMS-SECTION                   
108201                                                                          
108300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
108400             DELIMITED BY SIZE INTO SSA1                                  
108500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
108600     CALL CBLTDLI             USING GU                                    
108700                                    WDB6-PCB                              
108800                                    DLI-IO-AREA-B601                      
108900                                    SSA1                                  
109000     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
109100     PERFORM IMS-STATUSCHECK                                              
109201     .                                                                    
109301     EJECT                                                                
109401                                                                          
109501 IMS-STATUSCHECK SECTION.                                                 
109601     MOVE 'IMS-STATUSCHECK'           TO WS-IMS-SECTION                   
109701                                                                          
109801     SET STATUS-IX TO 1                                                   
109901     SEARCH GOOD-STATUS                                                   
110001       AT END                                                             
110101         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
110201           DELIMITED BY SIZE INTO ERROR-TEXT                              
110301         DISPLAY ERROR-TEXT                                               
110401         CALL FELLOG                                                      
110501       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
110600         CONTINUE                                                         
111000     END-SEARCH                                                           
120000     .                                                                    
