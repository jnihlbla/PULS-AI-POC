000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5029500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/11/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.ACSUPLOAD                                  
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        INVENTORY ORDER MENU                                             
001100*                                                                         
001200*        THE PROGRAM READS     WDJ7                                       
001300*        THE PROGRAM READS     WDB6                                       
001400*        THE PROGRAM READS     WDR2                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W50295T                                             
001800*        REQUEST:     W50295I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W50295O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W5029500'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  WS-ADDRESS                  PIC X(23)   VALUE                        
004400               'CARPARTS.PULS.ACSUPLOAD'.                                 
004500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004600                                                                          
004700                                                                          
004800 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-NOT-OK                       VALUE 'N'.                   
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005500 77  WS-EXIT-SW                  PIC X       VALUE 'N'.                   
005600     88  WS-EXIT                             VALUE 'Y'.                   
005700     EJECT                                                                
005800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005900 01  GENERAL-SUBPROGRAMS.                                                 
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006500*                                                                         
006600 01  MESSAGE-CODES.                                                       
006700     03  ERROR-CODES.                                                     
006800         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
006900         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
007000         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
007100         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
007200         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
007300         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
007400         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
007500         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
007600         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
007700         05  ERR-MUST-NOT-BE-ENTERED PIC X(3)    VALUE '033'.             
007800         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
007900         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
008000         05  ERR-SHLD-NOT-BE-ZERO    PIC X(3)    VALUE '126'.             
008100         05  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '168'.             
008200         05  ERR-INV-INFO-NOT-FOUND  PIC X(3)    VALUE '221'.             
008300         05  ERR-INV-NOT-COMPLETED   PIC X(3)    VALUE '222'.             
008400         05  ERR-ACS-NOT-ALLOWED     PIC X(3)    VALUE '331'.             
008500         05  ERR-INV-NOT-STARTED     PIC X(3)    VALUE '332'.             
008600     03  INFO-CODES.                                                      
008700         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
008800         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
008900         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
009000         05  INF-PROCESS-STARTED     PIC X(3)    VALUE '015'.             
009100         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
009200         05  INF-INV-INFO-MISSING    PIC X(3)    VALUE '220'.             
009300*                                                                         
009400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009500     SKIP3                                                                
009600*01  -COPY WZ01SUB                                                        
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009900     SKIP3                                                                
010000 01  REQU-AREA.                                                           
010100*    03  -COPY WZ01REQU                                                   
010200*    03  -COPY W50295I1                                                   
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010500     SKIP3                                                                
010600 01  RESP-AREA.                                                           
010700*    03  -COPY WZ01RESP                                                   
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011000     SKIP3                                                                
011100*01  -COPY WZ01SEND                                                       
011200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
011300     SKIP3                                                                
011400 01  SEND-AREA.                                                           
011500*    03  -COPY WZ01SOP  -PRE SOP-                                         
011600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  KEYS-FOR-DLI.                                                        
012100     03  W-IDDC-B6-X.                                                     
012200         05  W-IDDC-B6            PIC X(2)    VALUE SPACE.                
012300     03  W-WDGXKEY-5104-X.                                                
012400          05 W-IDHTYP-5103        PIC X(4)    VALUE '5103'.               
012500          05 W-LOWVALUE           PIC X(26)   VALUE LOW-VALUE.            
012600     03  W-IDDC-5104-X.                                                   
012700         05  W-IDDC-5104          PIC X(2)    VALUE SPACE.                
012800     03  W-WDJ701KY-MIN-X.                                                
012900         05  W-WDJ701-IDDC-MIN    PIC X(2)    VALUE SPACE.                
013000         05  W-WDJ701-IDARTNR-MIN PIC S9(9)   VALUE 0      COMP-3.        
013100     03  W-WDJ701KY-MAX-X.                                                
013200         05  W-WDJ701-IDDC-MAX    PIC X(2)    VALUE SPACE.                
013300         05  W-WDJ701-IDARTNR-MAX PIC S9(9) COMP-3                        
013400                                              VALUE 999999999.            
013500     03  W-WDJ701-FLKLAR-X.                                               
013600         05  W-FLKLAR             PIC X(1)    VALUE SPACE.                
013700     SKIP2                                                                
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FOUND                       VALUE '  '.                  
014100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014300     SKIP2                                                                
014400 01  GOOD-STATUSCODES.                                                    
014500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014600     SKIP3                                                                
014700 01  SSA1                        PIC X(64).                               
014800 01  SSA2                        PIC X(64).                               
014900     EJECT                                                                
015000*    --- IMS FUNCTION CODES                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300                                                                          
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
015500 01  DLI-IO-WDJ701.                                                       
015600*    03  -COPY WDJ701                                                     
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
015800 01  DLI-IO-WDB601.                                                       
015900*    03  -COPY WDB601                                                     
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
016100 01  DLI-IO-WDGX5104.                                                     
016200*    03  -COPY WDGX5104                                                   
016300     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500*01  -COPY W0009  -PRE MSG-                                               
016600                                                                          
016700*01  -COPY W0009  -PRE ALT0606-                                           
016800                                                                          
016900*01  -COPY W0008  -PRE WDJ7-                                              
017000     05  FILLER                  PIC X.                                   
017100                                                                          
017200*01  -COPY W0008  -PRE WDB6-                                              
017300     05  FILLER                  PIC X.                                   
017400                                                                          
017500*01  -COPY W0008  -PRE 5104-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800 PROCEDURE DIVISION  USING MSG-PCB ALT0606-PCB WDJ7-PCB 5104-PCB          
017900                           WDB6-PCB.                                      
018000 MAIN SECTION.                                                            
018100     ENTRY 'DLITCBL' USING MSG-PCB ALT0606-PCB WDJ7-PCB 5104-PCB          
018200                           WDB6-PCB.                                      
018300                                                                          
018400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018500     IF SUB-KDRC = 0                                                      
018600       PERFORM A-INIT                                                     
018700       PERFORM B-CHECK-KEYS                                               
018800       IF KEYS-OK                                                         
018900         PERFORM F-READ-SHOW-INFO                                         
019000       END-IF                                                             
019100       PERFORM S02-RETURN-RESPONSE                                        
019200     END-IF                                                               
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800                                                                          
019900     MOVE ALL '+'                     TO RESP-AREA                        
020000     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
020100                                         RESP-IDMSG-INFO                  
020200                                         RESP-IDELMT-ERROR                
020300     MOVE '001'                       TO RESP-IDMSGVER                    
020400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
020500     .                                                                    
020600     EJECT                                                                
020700 B-CHECK-KEYS SECTION.                                                    
020800                                                                          
020900     MOVE YES TO KEYS-SW                                                  
021000                                                                          
021100                                                                          
021200     IF REQU-IDDC = ALL '+'                                               
021300        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
021400        MOVE 'IDDC'              TO RESP-IDELMT-ERROR                     
021500        MOVE NOO                 TO KEYS-SW                               
021600     END-IF                                                               
021700                                                                          
021800     IF KEYS-OK                                                           
021900        MOVE REQU-IDDC          TO W-IDDC-B6                              
022000        PERFORM IMS-GU-WDB601                                             
022100        IF SEGMENT-FOUND                                                  
022200           IF DCS-FLINVACS = YES                                          
022300              CONTINUE                                                    
022400           ELSE                                                           
022500              MOVE ERR-INV-NOT-STARTED                                    
022600                                TO RESP-IDMSG-ERROR                       
022700              MOVE NOO          TO KEYS-SW                                
022800           END-IF                                                         
022900        ELSE                                                              
023000           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
023100           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                      
023200           MOVE NOO             TO KEYS-SW                                
023300        END-IF                                                            
023400     END-IF                                                               
023500                                                                          
023600     IF KEYS-OK                                                           
023700      IF REQU-KDPGMACT = 'E'                                              
023800         CONTINUE                                                         
023900      ELSE                                                                
024000         MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                      
024100         MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                     
024200         MOVE NOO                TO KEYS-SW                               
024300      END-IF                                                              
024400     END-IF                                                               
024500                                                                          
024600     IF KEYS-OK                                                           
024700      IF REQU-KDACS = 'M' OR 'U'                                          
024800         CONTINUE                                                         
024900      ELSE                                                                
025000         MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                      
025100         MOVE 'KDACS'            TO RESP-IDELMT-ERROR                     
025200         MOVE NOO                TO KEYS-SW                               
025300      END-IF                                                              
025400     END-IF                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 F-READ-SHOW-INFO SECTION.                                                
025800                                                                          
025900     MOVE REQU-IDDC             TO W-IDDC-5104                            
026000     PERFORM IMS-GU-WDGX5104                                              
026100     IF SEGMENT-FOUND                                                     
026200       IF 5104-DASTADAT > 5104-DASTODAT                                   
026300          PERFORM FA-READ-BASICDATA                                       
026400       ELSE                                                               
026500          MOVE ERR-INV-NOT-STARTED    TO RESP-IDMSG-ERROR                 
026600       END-IF                                                             
026700     ELSE                                                                 
026800       MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                           
026900       MOVE 'IDDC'          TO RESP-IDELMT-ERROR                          
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 FA-READ-BASICDATA SECTION.                                               
027400                                                                          
027500     MOVE REQU-IDDC                TO W-WDJ701-IDDC-MIN                   
027600                                      W-WDJ701-IDDC-MAX                   
027700     MOVE NOO                      TO W-FLKLAR                            
027800     PERFORM IMS-GU-WDJ701                                                
027900     IF SEGMENT-FOUND                                                     
028000       SET WS-EXIT                 TO TRUE                                
028100     END-IF                                                               
028200     IF WS-EXIT                                                           
028300       MOVE ERR-INV-NOT-COMPLETED  TO RESP-IDMSG-ERROR                    
028400     ELSE                                                                 
028500       PERFORM FAB-UPDATE-WDGX5104                                        
028600     END-IF                                                               
028700     .                                                                    
028800                                                                          
028900 FAB-UPDATE-WDGX5104 SECTION.                                             
029000     MOVE REQU-IDDC            TO W-IDDC-5104                             
029100     PERFORM IMS-GHU-WDGX5104                                             
029200     IF SEGMENT-FOUND                                                     
029300       IF REQU-KDACS = 'M'                                                
029400         MOVE 'M'             TO 5104-KDACS                               
029500       ELSE                                                               
029600         MOVE 'U'             TO 5104-KDACS                               
029700         MOVE WS-CURRENT-DATE TO 5104-DASTODAT                            
029800         MOVE REQU-IDUSER     TO 5104-IDUSER                              
029900       END-IF                                                             
030000       PERFORM IMS-REPL-WDGX5104                                          
030100       PERFORM FABA-ORDER-ROUTINE                                         
030200     ELSE                                                                 
030300       MOVE NOT-FOUND          TO RESP-IDMSG-ERROR                        
030400       MOVE 'IDDC'             TO RESP-IDELMT-ERROR                       
030500     END-IF                                                               
030600     .                                                                    
030700 FABA-ORDER-ROUTINE SECTION.                                              
030800     IF REQU-KDACS = 'M'                                                  
030900        MOVE 'W571B2'         TO SOP-REQU-IDPROCESS                       
031000     ELSE                                                                 
031100        MOVE 'W571B3'         TO SOP-REQU-IDPROCESS                       
031200     END-IF                                                               
031300     MOVE 'A'                 TO SOP-REQU-KDSOPFUNK                       
031310     MOVE ZERO                TO SOP-REQU-TIORDDAT                        
031400     MOVE SPACES              TO SOP-REQU-TESYMBV                         
031500     STRING 'IDDC(' REQU-IDDC ')' DELIMITED BY SIZE                       
031600                            INTO SOP-REQU-TESYMBV                         
031700     PERFORM S04-SEND-OPEN                                                
031800     PERFORM S04-SEND-MESSAGE                                             
031900     PERFORM S04-SEND-CLOSE                                               
032000     MOVE INF-PROCESS-STARTED TO RESP-IDMSG-INFO                          
032100     .                                                                    
032200                                                                          
032300     EJECT                                                                
032400*    --- DISPATCHER SECTIONS                                              
032500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
032600                                                                          
032700     MOVE 'GETARG'               TO SUB-KDFUNC                            
032800     MOVE WS-ADDRESS             TO SUB-ADDISPABS                         
032900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
033000                                                                          
033100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
033200                                                                          
033300     IF SUB-KDRC > 0                                                      
033400       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
033500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
033600       DELIMITED BY SIZE       INTO ERROR-TEXT                            
033700       DISPLAY ERROR-TEXT                                                 
033800       CALL FELLOG                                                        
033900     END-IF                                                               
034000     .                                                                    
034100     SKIP3                                                                
034200 S02-RETURN-RESPONSE SECTION.                                             
034300                                                                          
034400     MOVE 'RETURN'                TO SUB-KDFUNC                           
034500     MOVE LENGTH OF RESP-AREA     TO SUB-KVDLEN                           
034600                                                                          
034700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
034800                                                                          
034900     IF SUB-KDRC > 0                                                      
035000       MOVE SUB-KDRC              TO KDRC-DISPLAY                         
035100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
035200       DELIMITED BY SIZE        INTO ERROR-TEXT                           
035300       DISPLAY ERROR-TEXT                                                 
035400       CALL FELLOG                                                        
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 S04-SEND-OPEN SECTION.                                                   
035900                                                                          
036000     MOVE 'OPEN'                 TO SEND-KDFUNC                           
036100     MOVE 'CARPARTS.PULS.SOP'    TO SEND-ADDISPABS                        
036200     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
036300                                                                          
036400     IF SEND-KDRC > 0                                                     
036500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
036600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
036700       DELIMITED BY SIZE       INTO ERROR-TEXT                            
036800       DISPLAY ERROR-TEXT                                                 
036900       CALL FELLOG                                                        
037000     END-IF                                                               
037100     .                                                                    
037200     SKIP3                                                                
037300 S04-SEND-MESSAGE SECTION.                                                
037400                                                                          
037500     MOVE 'PUT'                      TO SEND-KDFUNC                       
037600     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
037700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
037800                                                                          
037900     IF SEND-KDRC > 0                                                     
038000       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
038100       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
038200       DELIMITED BY SIZE           INTO ERROR-TEXT                        
038300       DISPLAY ERROR-TEXT                                                 
038400       CALL FELLOG                                                        
038500     END-IF                                                               
038600     .                                                                    
038700     SKIP3                                                                
038800 S04-SEND-CLOSE SECTION.                                                  
038900                                                                          
039000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
039100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039200                                                                          
039300     IF SEND-KDRC > 0                                                     
039400       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
039500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
039600       DELIMITED BY SIZE           INTO ERROR-TEXT                        
039700       DISPLAY ERROR-TEXT                                                 
039800       CALL FELLOG                                                        
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 IMS-GU-WDJ701 SECTION.                                                   
040300                                                                          
040400     STRING 'WDJ701  (WDJ701KY>=' W-WDJ701KY-MIN-X                        
040500                     '&WDJ701KY<=' W-WDJ701KY-MAX-X                       
040600                     '&FLKLAR   =' W-WDJ701-FLKLAR-X ')'                  
040700          DELIMITED BY SIZE INTO SSA1                                     
040800     MOVE '  GE' TO GOOD-STATUSCODES                                      
040900     CALL CBLTDLI USING GU WDJ7-PCB DLI-IO-WDJ701 SSA1                    
041000     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
041100     PERFORM IMS-STATUSCHECK                                              
041200     .                                                                    
041300     EJECT                                                                
041400 IMS-GU-WDGX5104 SECTION.                                                 
041500                                                                          
041600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
041700          DELIMITED BY SIZE INTO SSA1                                     
041800     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
041900          DELIMITED BY SIZE INTO SSA2                                     
042000     MOVE '  GE' TO GOOD-STATUSCODES                                      
042100     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
042200     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
042300     PERFORM IMS-STATUSCHECK                                              
042400     .                                                                    
042500     SKIP3                                                                
042600 IMS-GHU-WDGX5104 SECTION.                                                
042700                                                                          
042800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
042900          DELIMITED BY SIZE INTO SSA1                                     
043000     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
043100          DELIMITED BY SIZE INTO SSA2                                     
043200     MOVE '  GE' TO GOOD-STATUSCODES                                      
043300     CALL CBLTDLI USING GHU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2            
043400     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
043500     PERFORM IMS-STATUSCHECK                                              
043600     .                                                                    
043700 IMS-GU-WDB601 SECTION.                                                   
043800                                                                          
043900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
044000          DELIMITED BY SIZE INTO SSA1                                     
044100     MOVE '  GE'              TO GOOD-STATUSCODES                         
044200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
044300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
044400     PERFORM IMS-STATUSCHECK                                              
044500     .                                                                    
044600     EJECT                                                                
044700 IMS-REPL-WDGX5104 SECTION.                                               
044800                                                                          
044900     MOVE '  '                TO GOOD-STATUSCODES                         
045000     CALL CBLTDLI USING REPL 5104-PCB DLI-IO-WDGX5104                     
045100     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
045200     PERFORM IMS-STATUSCHECK                                              
045300     .                                                                    
045400 IMS-STATUSCHECK SECTION.                                                 
045500                                                                          
045600     SET STATUS-IX TO 1                                                   
045700     SEARCH GOOD-STATUS                                                   
045800       AT END                                                             
045900         STRING ' INVALID STATUS FROM IMS: ' STATUS-WS                    
046000           DELIMITED BY SIZE INTO ERROR-TEXT                              
046100         DISPLAY ERROR-TEXT                                               
046200         CALL FELLOG                                                      
046300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
046400         CONTINUE                                                         
046500     END-SEARCH                                                           
046600     .                                                                    
