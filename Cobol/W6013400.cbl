000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6013400.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   23/08/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.INBOUNDINFO                                
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAM TO SUMMARIZE INBOUND PRIORITY QUEUE INFO                 
001100*                                                                         
001200*        THE PROGRAM READS     W6D1                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W6A134                                              
001600*        REQUEST:     W60134I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    W60134O1                                            
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600 WORKING-STORAGE SECTION.                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W6013400'.            
002800                                                                          
002900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003100 77  KDRC-DISPLAY                PIC Z(5).                                
003200 77  KDRC-NUM                    PIC S9(9).                               
003300                                                                          
003400 77  YES                         PIC X       VALUE 'J'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  IX                          PIC S9(9) COMP SYNC VALUE ZERO.          
003800 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +500.          
003900 77  MSG-IX                      PIC S9(9) COMP SYNC.                     
004000                                                                          
004100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004200     88  KEYS-OK                             VALUE 'J'.                   
004300     88  KEYS-WRONG                          VALUE 'N'.                   
004400                                                                          
004500 77  CONTINUE-SW                 PIC X       VALUE 'J'.                   
004600     88  CONTINUE-YES                        VALUE 'J'.                   
004700     88  CONTINUE-NO                         VALUE 'N'.                   
004800                                                                          
004900 77  REKY-SW                     PIC X       VALUE 'N'.                   
005000     88  REKY-FOUND                          VALUE 'J'.                   
005100     88  REKY-MISSING                        VALUE 'N'.                   
005200                                                                          
005300 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
005400     88  UPDATE-OK                           VALUE 'J'.                   
005500     88  UPDATE-NOT-OK                       VALUE 'N'.                   
005600                                                                          
005700 01  SMALL-LETTERS               PIC X(31)   VALUE                        
005800     'ABCDEFGHIJKLMNOPQRSTUVWXYZ≈ƒ÷¸…'.                                   
005900 01  CAPS-LETTERS                PIC X(31)   VALUE                        
006000     'ABCDEFGHIJKLMNOPQRSTUVWXYZ≈ƒ÷‹…'.                                   
006100                                                                          
006200 01  WS-SAVE-FIELDS.                                                      
006300     03  WS-SAVE-ADINLOMR        PIC X(4)    VALUE SPACES.                
006400                                                                          
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
007200                                                                          
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800                                                                          
007900 01  MESSAGE-CODES.                                                       
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008100     03  INF-NO-LINES            PIC X(3)    VALUE '027'.                 
008200     03  INF-MORE-LINES-EXIST    PIC X(3)    VALUE '011'.                 
008300                                                                          
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008600                                                                          
008700*01  -COPY WZ01SUB                                                        
008800                                                                          
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
009000                                                                          
009100*01  -COPY WMSGCONV                                                       
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009400                                                                          
009500 01  REQU-AREA.                                                           
009600*    03  -COPY WZ01REQ2.                                                  
009700*    03  -COPY W60134I1.                                                  
009800                                                                          
009900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010000                                                                          
010100 01  RESP-AREA.                                                           
010200*    03  -COPY WZ01RES2                                                   
010300*    03  -COPY W60134O1                                                   
010400                                                                          
010500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800                                                                          
010900 01  KEYS-FOR-DLI.                                                        
011000     03  W-IDDC-X.                                                        
011100         05  W-IDDC              PIC X(2).                                
011200                                                                          
011300*    --- STATUS-KOD FR≈N IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FOUND                       VALUE '  '.                  
011600     88  SEGMENT-MISSING                     VALUE 'GE' 'GB'.             
011700                                                                          
011800 01  GOOD-STATUSCODES.                                                    
011900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000                                                                          
012100 01  SSA1                        PIC X(128).                              
012200                                                                          
012300*    --- IMS FUNCTION CODES                                               
012400*01  -COPY W0003                                                          
012500                                                                          
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D1E1'.                      
012700 01  DLI-IO-W6D1E1.                                                       
012800*    03  -COPY W6D1E1                                                     
012900                                                                          
013000 LINKAGE SECTION.                                                         
013100*01  -COPY W0009  -PRE MSG-                                               
013200                                                                          
013300*01  -COPY W0008  -PRE W6D1E-                                             
013400     05  FILLER                  PIC X.                                   
013500                                                                          
013600 PROCEDURE DIVISION  USING MSG-PCB   W6D1E-PCB.                           
013700 MAIN SECTION.                                                            
013800                                                                          
013900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014000                                                                          
014100     IF SUB-KDRC = 0                                                      
014200       PERFORM A-INIT                                                     
014300       PERFORM B-CHECK-KEYS                                               
014400       IF KEYS-OK                                                         
014500         PERFORM F-READ-SHOW-INFO                                         
014600       END-IF                                                             
014700       PERFORM S11-MSG-CONV                                               
014800       PERFORM S02-RETURN-RESPONSE                                        
014900     END-IF                                                               
015000                                                                          
015100     PERFORM Z-FINIT                                                      
015200     MOVE ZERO                   TO RETURN-CODE                           
015300     GOBACK                                                               
015400     .                                                                    
015500                                                                          
015600 A-INIT SECTION.                                                          
015700                                                                          
015800     MOVE SPACES                 TO RESP-IDMSG-INFO                       
015900                                    RESP-IDMSG-ERROR                      
016000                                    RESP-IDELMT-ERROR                     
016100                                                                          
016200     MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)                             
016300                                 TO REQU-IDDC-KEY                         
016400     .                                                                    
016500                                                                          
016600 B-CHECK-KEYS SECTION.                                                    
016700                                                                          
016800     SET KEYS-OK                 TO TRUE                                  
016900                                                                          
017000     IF REQU-IDDC-KEY = SPACES OR LOW-VALUES                              
017100       SET KEYS-WRONG            TO TRUE                                  
017200       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
017300       MOVE 'IDDC'               TO RESP-IDELMT-ERROR                     
017400     END-IF                                                               
017500                                                                          
017600     .                                                                    
017700                                                                          
017800 F-READ-SHOW-INFO SECTION.                                                
017900                                                                          
018000     MOVE REQU-IDDC-KEY          TO W-IDDC                                
018100     MOVE 0                      TO IX                                    
018200     PERFORM IMS-GN-W6D1E1                                                
018300     PERFORM                                                              
018400       UNTIL SEGMENT-MISSING                                              
018500       IF SEQE-KDINLSTA = 'FPK' OR SPACES                                 
018600         IF SEQE-ADINLOMR = WS-SAVE-ADINLOMR                              
018700           ADD 1                 TO RESP-KVRADER-TOT  (IX)                
018800           IF SEQE-KDINLPRIO < 31                                         
018900             ADD 1               TO RESP-KVRADER-PRIO (IX)                
019000           END-IF                                                         
019100         ELSE                                                             
019200           COMPUTE IX = IX + 1                                            
019300           IF IX > MAX-IX                                                 
019400             SET SEGMENT-MISSING TO TRUE                                  
019500             MOVE INF-MORE-LINES-EXIST                                    
019600                                 TO RESP-IDMSG-INFO                       
019700           ELSE                                                           
019800             MOVE SEQE-ADINLOMR  TO RESP-ADINLOMR     (IX)                
019900                                    WS-SAVE-ADINLOMR                      
020000             MOVE 1              TO RESP-KVRADER-TOT  (IX)                
020100             IF SEQE-KDINLPRIO < 31                                       
020200               MOVE 1            TO RESP-KVRADER-PRIO (IX)                
020300             ELSE                                                         
020400               MOVE 0            TO RESP-KVRADER-PRIO (IX)                
020500             END-IF                                                       
020600           END-IF                                                         
020700         END-IF                                                           
020800       END-IF                                                             
020900       PERFORM IMS-GN-W6D1E1                                              
021000     END-PERFORM                                                          
021100                                                                          
021200     MOVE IX                     TO RESP-KVRADER                          
021300     IF IX = 0                                                            
021400       MOVE INF-NO-LINES         TO RESP-IDMSG-INFO                       
021500     END-IF                                                               
021600     .                                                                    
021700                                                                          
021800 Z-FINIT SECTION.                                                         
021900                                                                          
022000     CONTINUE                                                             
022100     .                                                                    
022200                                                                          
022300*    --- DISPATCHER SECTIONS                                              
022400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
022500                                                                          
022600     MOVE 'GETARG'               TO SUB-KDFUNC                            
022700     MOVE 'CARPARTS.PULS.INBOUNDINFO'                                     
022800                                 TO SUB-ADDISPABS                         
022900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
023000                                                                          
023100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
023200                                                                          
023300     COMPUTE KDRC-NUM             = SUB-KDRC                              
023400                                                                          
023500     IF SUB-KDRC > 0                                                      
023600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
023700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
023800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
023900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024000     END-IF                                                               
024100     .                                                                    
024200                                                                          
024300 S02-RETURN-RESPONSE SECTION.                                             
024400                                                                          
024500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
024600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
024700                                                                          
024800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
024900                                                                          
025000     COMPUTE KDRC-NUM             = SUB-KDRC                              
025100                                                                          
025200     IF SUB-KDRC > 0                                                      
025300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
025400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
025500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025700     END-IF                                                               
025800     .                                                                    
025900                                                                          
026000 S11-MSG-CONV SECTION.                                                    
026100     MOVE SPACES                  TO RESP-MESSAGES (1)                    
026200                                     RESP-MESSAGES (2)                    
026300     MOVE 1                       TO MSG-IX                               
026400*    REQUEST OK                                                           
026500     MOVE 200                     TO RESP-KDSTATUS-API                    
026600     IF RESP-IDMSG-INFO > SPACE                                           
026700       MOVE SPACES                TO MSG-CONV-AREA                        
026800       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
026900       CALL WMSGCONV           USING MSG-CONV-AREA                        
027000       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
027100       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
027200       ADD 1                      TO MSG-IX                               
027300     END-IF                                                               
027400     IF RESP-IDMSG-ERROR > SPACE                                          
027500*      BAD REQUEST                                                        
027600       MOVE 400                   TO RESP-KDSTATUS-API                    
027700       MOVE SPACES                TO MSG-CONV-AREA                        
027800       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
027900       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
028000       CALL WMSGCONV           USING MSG-CONV-AREA                        
028100       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
028200       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
028300     END-IF                                                               
028400     .                                                                    
028500                                                                          
028600 IMS-GN-W6D1E1 SECTION.                                                   
028700                                                                          
028800     STRING 'W6D1E1  (IDDC     =' W-IDDC-X ')'                            
028900             DELIMITED BY SIZE INTO SSA1                                  
029000     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
029100     CALL CBLTDLI             USING GN                                    
029200                                    W6D1E-PCB                             
029300                                    DLI-IO-W6D1E1                         
029400                                    SSA1                                  
029500     MOVE W6D1E-STATUS-CODE      TO STATUS-WS                             
029600     PERFORM IMS-STATUSCHECK                                              
029700     .                                                                    
029800                                                                          
029900 IMS-STATUSCHECK SECTION.                                                 
030000                                                                          
030100     SET STATUS-IX               TO 1                                     
030200     SEARCH GOOD-STATUS                                                   
030300       AT END                                                             
030400         CALL FELLOG                                                      
030500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
030600         CONTINUE                                                         
030700     END-SEARCH                                                           
030800     .                                                                    
030900                                                                          
