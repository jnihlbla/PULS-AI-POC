000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6016A00.                                                
000300 AUTHOR.         SRINADH NADIMPALLI.                                      
000400 DATE-WRITTEN.   23/11/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       RECONCILEBUFFERSALDO                                     
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        RECONCILIATION SYNQ                                              
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W6A16A                                              
001500*        REQUEST:     W6016AI1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        RESPONSE:    W6016AO1                                            
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
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6016A00'.            
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700                                                                          
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
004200     88  YES-UPDATE                          VALUE 'J'.                   
004300     88  NO-UPDATE                           VALUE 'N'.                   
004400                                                                          
004500                                                                          
004600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004700     88  KEYS-OK                             VALUE 'J'.                   
004800     88  KEYS-WRONG                          VALUE 'N'.                   
004900     EJECT                                                                
005000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005600     SKIP3                                                                
005700*    --- PARAMETERS TO ABEND                                              
005800                                                                          
005900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006200     SKIP3                                                                
006300 01  MESSAGE-CODES.                                                       
006400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006500     EJECT                                                                
006600*                                                                         
006700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006800     SKIP3                                                                
006900*01  -COPY WZ01SUB                                                        
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007200     SKIP3                                                                
007300 01  REQU-AREA.                                                           
007400*    03  -COPY WZ01REQ2                                                   
007500*    03  -COPY W6016AI1                                                   
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007800     SKIP3                                                                
007900 01  RESP-AREA.                                                           
008000*    03  -COPY WZ01RES2                                                   
008100*    03  -COPY W6016AO1                                                   
008200     EJECT                                                                
008300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700 01  KEYS-FOR-DLI.                                                        
008800** KEYS DECLARATION FOR WDK811                                            
008900     03  W-WDD811KY-MIN-X.                                                
009000         05  W-IDDC-WDD8-MIN      PIC X(2)         VALUE '11'.            
009100         05  W-ADBUFFOM-WDD8-MIN  PIC S9(3) COMP-3 VALUE +001.            
009200         05  W-FILLER-MIN         PIC X(13) VALUE LOW-VALUE.              
009300                                                                          
009400     03  W-WDD811KY-MAX-X.                                                
009500         05  W-IDDC-WDD8-MAX      PIC X(2)         VALUE '11'.            
009600         05  W-ADBUFFOM-WDD8-MAX  PIC S9(3) COMP-3 VALUE +001.            
009700         05  W-FILLER-MAX         PIC X(13) VALUE HIGH-VALUE.             
009800** GENERAL KEYS                                                           
009900     03  W-ADBUFFOM-X.                                                    
010000         05  W-ADBUFFOM           PIC S9(3) COMP-3 VALUE +1.              
010100                                                                          
010200     03  W-ADBUFGAN-X.                                                    
010300         05  W-ADBUFGAN           PIC S9(3) COMP-3 VALUE ZERO.            
010400                                                                          
010500     03  W-ADBUFPL-X.                                                     
010600         05  W-ADBUFPL            PIC S9(5) COMP-3 VALUE ZERO.            
010700                                                                          
010800     03  W-IDDC-X.                                                        
010900         05  W-IDDC               PIC X(2)   VALUE '11'.                  
011000     03  W-IDARTNR-X.                                                     
011100         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
011200     SKIP2                                                                
011300*    --- STATUS CODES FROM IMS                                            
011400 01  STATUS-WS                    PIC XX.                                 
011500     88  SEGMENT-FOUND                       VALUE '  '.                  
011600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GOOD-STATUSCODES.                                                    
012000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(128).                              
012400     EJECT                                                                
012500*    --- IMS FUNCTION CODES                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD801'.                      
012900 01  DLI-IO-WDD801.                                                       
013000*    03  -COPY WDD801                                                     
013100     EJECT                                                                
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
013300 01  DLI-IO-WDD811.                                                       
013400*    03  -COPY WDD811                                                     
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009   -PRE MSG-                                              
013800 01  ATAB-PCB                    PIC X.                                   
013900*01  -COPY W0008  -PRE WDD8-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB WDD8-PCB.                     
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB WDD8-PCB.                     
014500                                                                          
014600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014700     IF SUB-KDRC = 0                                                      
014800       PERFORM A-INIT                                                     
014900       PERFORM B-CHECK-KEYS                                               
015000       IF KEYS-OK                                                         
015100         PERFORM H-UPDATE                                                 
015200       END-IF                                                             
015300       PERFORM S02-RETURN-RESPONSE                                        
015400     END-IF                                                               
015500                                                                          
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200     MOVE 200 TO RESP-KDSTATUS-API                                        
016300     MOVE REQU-KVRADER TO RESPONSE-KVRADER                                
016400     MOVE +1 TO INDX                                                      
016500     .                                                                    
016600     EJECT                                                                
016700 B-CHECK-KEYS SECTION.                                                    
016800                                                                          
016900     MOVE YES TO KEYS-SW                                                  
017000                                                                          
017100                                                                          
017200     IF KEYS-WRONG                                                        
017300       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
017400     END-IF                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 H-UPDATE SECTION.                                                        
017800                                                                          
017900     PERFORM UNTIL INDX > REQU-KVRADER                                    
018000     MOVE NOO TO UPDATE-SW                                                
018100     MOVE REQU-SALDO-IDARTNR (INDX) TO W-IDARTNR                          
018200                                       RESPONSE-IDARTNR(INDX)             
018300     PERFORM IMS-GU-WDD801                                                
018400                                                                          
018500     IF SEGMENT-MISSING                                                   
018600      MOVE 'PART NUMBER MISSING IN PULS'                                  
018700           TO RESPONSE-MESSAGE1(INDX)                                     
018800      MOVE 206 TO RESP-KDSTATUS-API                                       
018900     ELSE                                                                 
019000      PERFORM IMS-GHU-WDD811                                              
019100      IF SEGMENT-FOUND                                                    
019200        IF REQU-SALDO-KVBUFF-F (INDX) >= 0 AND                            
019300           REQU-SALDO-KVBUFF-OF (INDX) >= 0 AND                           
019400           REQU-SALDO-KVKOLLI-F (INDX) >= 0 AND                           
019500           REQU-SALDO-KVKOLLI-OF(INDX) >= 0                               
019600          IF SALDO-KVBUFF-F NOT = REQU-SALDO-KVBUFF-F (INDX)              
019700           MOVE YES TO UPDATE-SW                                          
019800          END-IF                                                          
019900          IF SALDO-KVBUFF-OF NOT = REQU-SALDO-KVBUFF-OF (INDX)            
020000           MOVE YES TO UPDATE-SW                                          
020100          END-IF                                                          
020200          IF SALDO-KVKOLLI-F NOT = REQU-SALDO-KVKOLLI-F (INDX)            
020300           MOVE YES TO UPDATE-SW                                          
020400          END-IF                                                          
020500          IF SALDO-KVKOLLI-OF NOT = REQU-SALDO-KVKOLLI-OF (INDX)          
020600           MOVE YES TO UPDATE-SW                                          
020700          END-IF                                                          
020800        END-IF                                                            
020900        IF YES-UPDATE                                                     
021000         MOVE REQU-SALDO-KVBUFF-F (INDX) TO SALDO-KVBUFF-F                
021100         MOVE REQU-SALDO-KVBUFF-OF (INDX) TO SALDO-KVBUFF-OF              
021200         MOVE REQU-SALDO-KVKOLLI-F (INDX) TO SALDO-KVKOLLI-F              
021300         MOVE REQU-SALDO-KVKOLLI-OF (INDX) TO SALDO-KVKOLLI-OF            
021400         MOVE 'SALDO ADJUSTED' TO RESPONSE-MESSAGE1(INDX)                 
021500         PERFORM IMS-REPL-WDD811                                          
021600        ELSE                                                              
021700         MOVE 'SALDO CORRECT' TO RESPONSE-MESSAGE1(INDX)                  
021800        END-IF                                                            
021900      ELSE                                                                
022000      IF SEGMENT-MISSING AND (REQU-SALDO-KVBUFF-F (INDX) > 0 OR           
022110                  REQU-SALDO-KVBUFF-OF (INDX) > 0)                        
022200        MOVE '11' TO SALDO-IDDC                                           
022300        MOVE '01' TO SALDO-ADBUFFOMR                                      
022400        MOVE ZERO TO SALDO-DABUFPAF                                       
022500        MOVE ZERO TO SALDO-ADBUFFGANG                                     
022600        MOVE ZERO TO SALDO-ADBUFFPL                                       
022700        MOVE ZERO TO SALDO-KDBRIST                                        
022800        MOVE ZERO TO SALDO-KDPAF                                          
022900        MOVE REQU-SALDO-IDARTNR (INDX) TO W-IDARTNR                       
023000        MOVE REQU-SALDO-KVBUFF-F (INDX) TO  SALDO-KVBUFF-F                
023100        MOVE REQU-SALDO-KVBUFF-OF (INDX) TO SALDO-KVBUFF-OF               
023200        MOVE REQU-SALDO-KVKOLLI-F (INDX) TO  SALDO-KVKOLLI-F              
023300        MOVE REQU-SALDO-KVKOLLI-OF (INDX) TO SALDO-KVKOLLI-OF             
023400        PERFORM IMS-ISRT-WDD811                                           
023500        MOVE 'SALDO CREATED' TO RESPONSE-MESSAGE1(INDX)                   
023600      END-IF                                                              
023700     END-IF                                                               
023800     END-IF                                                               
023900     ADD 1 TO INDX                                                        
024000     END-PERFORM                                                          
024100     .                                                                    
024200     EJECT                                                                
024300*    --- DISPATCHER SECTIONS                                              
024400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
024500                                                                          
024600     MOVE 'GETARG'               TO SUB-KDFUNC                            
024700     MOVE 'CARPARTS.PULS.RECONCILEBUFFER'    TO SUB-ADDISPABS             
024800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
024900                                                                          
025000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
025100                                                                          
025200     IF SUB-KDRC > 0                                                      
025300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
025400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
025500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025700     END-IF                                                               
025800     .                                                                    
025900     SKIP3                                                                
026000 S02-RETURN-RESPONSE SECTION.                                             
026100                                                                          
026200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
026300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
026400                                                                          
026500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
026600                                                                          
026700     IF SUB-KDRC > 0                                                      
026800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
027000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
027100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
027200     END-IF                                                               
027300     .                                                                    
027400 IMS-GHU-WDD811 SECTION.                                                  
027500     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
027600          DELIMITED BY SIZE INTO SSA1                                     
027700     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
027800                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
027900                    '&ADBUFFOM =' W-ADBUFFOM-X                            
028000                    '&ADBUFGAN =' W-ADBUFGAN-X                            
028100                    '&ADBUFPL  =' W-ADBUFPL-X  ')'                        
028200          DELIMITED BY SIZE INTO SSA2                                     
028300     MOVE '  GE' TO GOOD-STATUSCODES                                      
028400     CALL CBLTDLI USING GHU WDD8-PCB DLI-IO-WDD811 SSA1 SSA2              
028500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
028600     PERFORM IMS-STATUSCHECK                                              
028700     .                                                                    
028800 IMS-ISRT-WDD811 SECTION.                                                 
028900     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
029000          DELIMITED BY SIZE INTO SSA1                                     
029100     MOVE 'WDD811 ' TO SSA2                                               
029200     MOVE '  II' TO GOOD-STATUSCODES                                      
029300     CALL CBLTDLI USING ISRT WDD8-PCB DLI-IO-WDD811 SSA1 SSA2             
029400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSCHECK                                              
029600     .                                                                    
029700 IMS-GU-WDD801 SECTION.                                                   
029800                                                                          
029900     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
030000          DELIMITED BY SIZE INTO SSA1                                     
030100     MOVE '  GE' TO GOOD-STATUSCODES                                      
030200     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
030300     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
030400     PERFORM IMS-STATUSCHECK                                              
030500     .                                                                    
030600 IMS-REPL-WDD811 SECTION.                                                 
030700                                                                          
030800     MOVE '  ' TO GOOD-STATUSCODES                                        
030900     CALL CBLTDLI USING REPL WDD8-PCB DLI-IO-WDD811                       
031000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSCHECK                                              
031200     .                                                                    
031300 IMS-STATUSCHECK SECTION.                                                 
031400     SET STATUS-IX TO 1                                                   
031500     SEARCH GOOD-STATUS AT END CALL FELLOG                                
031600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
031700     END-SEARCH                                                           
031800     CONTINUE                                                             
031900     .                                                                    
032000     EJECT                                                                
