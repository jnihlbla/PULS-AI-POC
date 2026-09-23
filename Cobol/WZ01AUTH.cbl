000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ01AUTH.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   21/02/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM IS USED TO VERIFY THE API AUTHENTICATION            
001000*        PARAMETERS THAT ARE PASSED IN THE REQUEST HEADER.                
001100*                                                                         
001200*        RETURNS THE IDORIGSYS FROM THE MAPPING TABLE.                    
001300*                                                                         
001400*        ANSWER - 0  (CALL OK)                                            
001500*               - 4  (USER MAPPING NOT FOUND)                             
001600*               - 10 (INVALID KDCALL)                                     
001700*               - 11 (INVALID IDREQVER)                                   
001800*               - 12 (INVALID JWT)                                        
001900*                                                                         
002000*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(8)    VALUE 'WZ01AUTH'.            
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200                                                                          
003300 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
003400     88  ALL-OK                              VALUE 'J'.                   
003500     88  SOME-ERROR                          VALUE 'N'.                   
003600                                                                          
003700 77  WS-JWT-HEADER               PIC X(1000).                             
003800 77  WS-JWT-PAYLOAD              PIC X(3000).                             
003900 77  WS-JWT-SIGNATURE            PIC X(1000).                             
004000 77  WS-JWT-DECODED              PIC X(3000).                             
004100                                                                          
004200 01  WS-PAYLOAD-PARSED.                                                   
004300     03  unique_name             PIC X(60).                               
004400     03  upn                     PIC X(60).                               
004500                                                                          
004600 77  WS-UNIQUE-NAME-USER         PIC X(8).                                
004700 77  WS-UNIQUE-NAME-DOMAIN       PIC X(60).                               
004800 77  WS-UPN-USER                 PIC X(8).                                
004900 77  WS-UPN-DOMAIN               PIC X(60).                               
005000 77  WS-IDUSER                   PIC X(8).                                
005100                                                                          
005200 01  WS-AUTH-REQUEST             PIC X(5000).                             
005300*01  -COPY WZ01REQ2 -RED WS-AUTH-REQUEST.                                 
005400                                                                          
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600     03  AIBTDLI                 PIC X(8)    VALUE 'AIBTDLI'.             
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
005900     03  WB64CONV                PIC X(8)    VALUE 'WB64CONV'.            
006000     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
006100                                                                          
006200*    -COPY WB64CONV                                                       
006300                                                                          
006400 01  ERROR-TEXT.                                                          
006500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006700                                                                          
006800*    --- PARAMETERS TO VIMSID                                             
006900 01  WS-VIMSID                   PIC X(8)    VALUE SPACE.                 
007000 01  FILLER REDEFINES WS-VIMSID.                                          
007100     03  IMS-REGION              PIC X(3).                                
007200         88  TEST-REGION                     VALUE 'IMD' 'IMP'            
007300                                                   'IMY' 'IMB'.           
007400         88  ENV-DEVE                        VALUE 'IMP'.                 
007500         88  ENV-IGRT                        VALUE 'IMY'.                 
007600         88  ENV-XDEV                        VALUE 'IMD'.                 
007700         88  ENV-ACPT                        VALUE 'IMB'.                 
007800         88  ENV-PROD                        VALUE 'IMG'.                 
007900     03  FILLER                  PIC X(5).                                
008000                                                                          
008100*    -- IMS FUNKTIONSKODER                                                
008200*    -COPY W0003                                                          
008300                                                                          
008400 01  KEYS-TILL-DLI.                                                       
008500     03  W-WDGXKEY-X.                                                     
008600         05  W-IDHTYP            PIC X(04)   VALUE '0101'.                
008700         05  FILLER              PIC X(26)   VALUE LOW-VALUES.            
008800     03  W-IDUSERKEY-X.                                                   
008900         05  W-IDUSERKEY         PIC X(50)   VALUE SPACES.                
009000                                                                          
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FOUND                       VALUE '  '.                  
009300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009400                                                                          
009500 01  VALID-STATUS-CODES.                                                  
009600     03  VALID-STATUS OCCURS 5 INDEXED BY STATUS-IX                       
009700                                 PIC XX.                                  
009800                                                                          
009900 01  SSA1                        PIC X(128).                              
010000 01  SSA2                        PIC X(128).                              
010100                                                                          
010200 01  DLI-IO-AREA                 PIC X(1000).                             
010300*01  -COPY WDGX0102 -RED DLI-IO-AREA.                                     
010400                                                                          
010500 01  AIB-AREA-START              PIC X(16)   VALUE                        
010600                                 'AIB-AREA-START  '.                      
010700                                                                          
010800*    -COPY W0031                                                          
010900                                                                          
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY WZ01AUTH.                                                      
011300 01  AUTH-REQUEST                PIC X(5000).                             
011400*01  -COPY W0008  -PRE WDR5-                                              
011500     05 FILLER                   PIC X.                                   
011600                                                                          
011700                                                                          
011800 PROCEDURE DIVISION USING AUTH-WZ01AUTH AUTH-REQUEST.                     
011900 MAIN SECTION.                                                            
012000                                                                          
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     IF ALL-OK                                                            
012400       EVALUATE REQU-IDREQVER OF REQU-WZ01REQ2                            
012500         WHEN 1                                                           
012600*          ADDED FOR CONSISTENCY PURPOSE.                                 
012700*          WZ01REQU DOES NOT HAVE ANY AUTH DATA!                          
012800           CONTINUE                                                       
012900         WHEN 2                                                           
013000           PERFORM B-HANDLE-WZ01REQ2                                      
013100         WHEN OTHER                                                       
013200*          UNKNOWN REQUEST HEADER VERSION!                                
013300           MOVE 11               TO AUTH-KDRC                             
013400           SET SOME-ERROR        TO TRUE                                  
013500       END-EVALUATE                                                       
013600     END-IF                                                               
013700                                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     IF AUTH-KDRC = 0                                                     
014100       MOVE ZERO                 TO RETURN-CODE                           
014200     ELSE                                                                 
014300       MOVE 12                   TO RETURN-CODE                           
014400     END-IF                                                               
014500                                                                          
014600     GOBACK                                                               
014700     .                                                                    
014800                                                                          
014900 A-INIT SECTION.                                                          
015000                                                                          
015100     SET ALL-OK                  TO TRUE                                  
015200     MOVE 0                      TO AUTH-KDRC                             
015300                                                                          
015400     MOVE AUTH-REQUEST           TO WS-AUTH-REQUEST                       
015500                                                                          
015600     IF AUTH-KDCALL = 1                                                   
015700       CONTINUE                                                           
015800     ELSE                                                                 
015900*      INVALID KDCALL                                                     
016000       MOVE 10                   TO AUTH-KDRC                             
016100       SET SOME-ERROR            TO TRUE                                  
016200     END-IF                                                               
016300                                                                          
016400     CALL VIMSID              USING WS-VIMSID                             
016500     .                                                                    
016600                                                                          
016700 B-HANDLE-WZ01REQ2 SECTION.                                               
016800                                                                          
016900     IF REQU-IDUSER OF REQU-WZ01REQ2 = SPACES OR LOW-VALUES               
017000                                              OR ALL '+'                  
017100       MOVE SPACES               TO WS-IDUSER                             
017200     ELSE                                                                 
017300       MOVE REQU-IDUSER OF REQU-WZ01REQ2                                  
017400                                 TO WS-IDUSER                             
017500     END-IF                                                               
017600                                                                          
017700     IF REQU-IDUSERKEY OF REQU-WZ01REQ2 = SPACES OR LOW-VALUES            
017800                                                 OR ALL '+'               
017900       CONTINUE                                                           
018000     ELSE                                                                 
018100       PERFORM BA-HANDLE-USER-KEY                                         
018200     END-IF                                                               
018300                                                                          
018400     IF REQU-IDJWT-ACCESS OF REQU-WZ01REQ2 = SPACES OR LOW-VALUES         
018500                                                    OR ALL '+'            
018600       CONTINUE                                                           
018700     ELSE                                                                 
018800       PERFORM BB-HANDLE-JWT                                              
018900     END-IF                                                               
019000                                                                          
019100     IF WS-IDUSER = SPACES                                                
019200       IF AUTH-KDRC = 0                                                   
019300         MOVE 04                 TO AUTH-KDRC                             
019400       END-IF                                                             
019500*      Allow READ access without explicit user info.                      
019600       IF AUTH-KDRC = 4 AND REQU-QUERY                                    
019700         MOVE 0                  TO AUTH-KDRC                             
019800       END-IF                                                             
019900     END-IF                                                               
020000     MOVE WS-IDUSER              TO REQU-IDUSER                           
020100                                 OF REQU-WZ01REQ2                         
020200     .                                                                    
020300                                                                          
020400 BA-HANDLE-USER-KEY SECTION.                                              
020500                                                                          
020600     MOVE FUNCTION LOWER-CASE (REQU-IDUSERKEY OF REQU-WZ01REQ2)           
020700                                 TO W-IDUSERKEY                           
020800     PERFORM IMS-GU-USER-KEY                                              
020900     IF SEGMENT-FOUND                                                     
021000       MOVE 0102-IDORIGSYS       TO AUTH-IDSYSTEM                         
021100       IF REQU-IDORIGSYS OF REQU-WZ01REQ2 = SPACES OR LOW-VALUES          
021200                                                   OR ALL '+'             
021300         MOVE 0102-IDORIGSYS     TO REQU-IDORIGSYS                        
021400                                 OF REQU-WZ01REQ2                         
021500       END-IF                                                             
021600       IF REQU-IDUSER OF REQU-WZ01REQ2 = SPACES OR LOW-VALUES             
021700                                                OR ALL '+'                
021800         MOVE 0102-IDORIGSYS     TO WS-IDUSER                             
021900       END-IF                                                             
022000     ELSE                                                                 
022100*      NO MATCHING USER-KEY FOUND. CALLING PROGRAM TO DECIDE              
022200*      WHAT TO BE DONE.                                                   
022300       MOVE 04                   TO AUTH-KDRC                             
022400     END-IF                                                               
022500     .                                                                    
022600                                                                          
022700 BB-HANDLE-JWT SECTION.                                                   
022800                                                                          
022900     MOVE 001                    TO B64-KDCALL                            
023000     UNSTRING REQU-IDJWT-ACCESS OF REQU-WZ01REQ2                          
023100                       DELIMITED BY '.'                                   
023200                               INTO WS-JWT-HEADER                         
023300                                    WS-JWT-PAYLOAD                        
023400                                    WS-JWT-SIGNATURE                      
023500                                                                          
023600     COMPUTE B64-KVDLEN-IN  = FUNCTION LENGTH (                           
023700                              FUNCTION TRIM (WS-JWT-PAYLOAD))             
023800                                                                          
023900     COMPUTE B64-KVDLEN-OUT = FUNCTION LENGTH (WS-JWT-DECODED)            
024000                                                                          
024100     CALL WB64CONV            USING B64-CONTROL-AREA                      
024200                                    B64-KVDLEN-IN                         
024300                                    WS-JWT-PAYLOAD                        
024400                                    B64-KVDLEN-OUT                        
024500                                    WS-JWT-DECODED                        
024600                                                                          
024700     IF B64-KDRC = ZERO                                                   
024800       INITIALIZE WS-PAYLOAD-PARSED                                       
024900       JSON PARSE WS-JWT-DECODED (1:B64-KVDLEN-OUT)                       
025000                               INTO WS-PAYLOAD-PARSED                     
025100         NAME OF WS-PAYLOAD-PARSED IS OMITTED                             
025200       END-JSON                                                           
025300       UNSTRING unique_name DELIMITED BY '@'                              
025400                               INTO WS-UNIQUE-NAME-USER                   
025500                                    WS-UNIQUE-NAME-DOMAIN                 
025600       UNSTRING upn         DELIMITED BY '@'                              
025700                               INTO WS-UPN-USER                           
025800                                    WS-UPN-DOMAIN                         
025900       MOVE FUNCTION UPPER-CASE (WS-UNIQUE-NAME-DOMAIN)                   
026000                                 TO WS-UNIQUE-NAME-DOMAIN                 
026100       MOVE FUNCTION UPPER-CASE (WS-UPN-DOMAIN)                           
026200                                 TO WS-UPN-DOMAIN                         
026300       MOVE FUNCTION UPPER-CASE (WS-UNIQUE-NAME-USER)                     
026400                                 TO WS-UNIQUE-NAME-USER                   
026500       MOVE FUNCTION UPPER-CASE (WS-UPN-USER)                             
026600                                 TO WS-UPN-USER                           
026700       IF ENV-PROD AND                                                    
026800          ( WS-UNIQUE-NAME-USER   = 'H-HARMS1' OR                         
026900            WS-UNIQUE-NAME-USER   = 'M-BRUIJ1' OR                         
027000            WS-UPN-USER           = 'H-HARMS1' OR                         
027100            WS-UPN-USER           = 'M-BRUIJ1'    )                       
027200         MOVE 04                 TO AUTH-KDRC                             
027300       ELSE                                                               
027400         IF REQU-IDUSER OF REQU-WZ01REQ2 = SPACES OR LOW-VALUES           
027500                                                  OR ALL '+'              
027600           IF WS-UNIQUE-NAME-USER = SPACES AND                            
027700              WS-UPN-USER = SPACES                                        
027800             MOVE 04             TO AUTH-KDRC                             
027900           ELSE                                                           
028000             MOVE 0              TO AUTH-KDRC                             
028100             IF WS-UNIQUE-NAME-USER = SPACES                              
028200               MOVE WS-UPN-USER  TO WS-IDUSER                             
028300             ELSE                                                         
028400               MOVE WS-UNIQUE-NAME-USER                                   
028500                                 TO WS-IDUSER                             
028600             END-IF                                                       
028700           END-IF                                                         
028800         END-IF                                                           
028900       END-IF                                                             
029000     ELSE                                                                 
029100       MOVE 12                   TO AUTH-KDRC                             
029200       SET SOME-ERROR            TO TRUE                                  
029300     END-IF                                                               
029400     .                                                                    
029500                                                                          
029600 Z-FINIT SECTION.                                                         
029700                                                                          
029800     MOVE WS-AUTH-REQUEST        TO AUTH-REQUEST                          
029900     .                                                                    
030000                                                                          
030100 IMS-GU-USER-KEY SECTION.                                                 
030200                                                                          
030300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
030400              DELIMITED BY SIZE INTO SSA1                                 
030500     STRING 'WDGX0102(IDUSERKY =' W-IDUSERKEY-X ')'                       
030600              DELIMITED BY SIZE INTO SSA2                                 
030700                                                                          
030800     MOVE 128                    TO AIB-LEN                               
030900     MOVE SPACES                 TO AIB-SUB-FUNCTION                      
031000     MOVE 'ATAB'                 TO AIB-PCB-NAME                          
031100     MOVE LENGTH OF DLI-IO-AREA  TO AIB-IOAREA-LENGTH                     
031200                                                                          
031300     MOVE '  GE'                 TO VALID-STATUS-CODES                    
031400                                                                          
031500     CALL AIBTDLI             USING GU                                    
031600                                    AIB-AREA                              
031700                                    DLI-IO-AREA                           
031800                                    SSA1                                  
031900                                    SSA2                                  
032000                                                                          
032100     SET ADDRESS OF WDR5-PCB     TO AIB-PCB-PTR                           
032200*    SET AIB-PCB-PTR             TO ADDRESS OF WDR5-PCB                   
032300     MOVE WDR5-STATUS-CODE       TO STATUS-WS                             
032400     PERFORM IMS-STATUS-CHECK                                             
032500     .                                                                    
032600                                                                          
032700 IMS-STATUS-CHECK   SECTION.                                              
032800                                                                          
032900     IF STATUS-WS = LOW-VALUE                                             
033000       STRING ' CAN NOT FIND PCB WITH NAME: '                             
033100              AIB-PCB-NAME ' IN THE PSB.'                                 
033200         DELIMITED BY SIZE INTO ERROR-TEXT                                
033300     ELSE                                                                 
033400       SET STATUS-IX TO 1                                                 
033500       SEARCH VALID-STATUS                                                
033600         AT END                                                           
033700           STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS             
033800           DELIMITED BY SIZE INTO ERROR-TEXT                              
033900           DISPLAY IDPGM ' ' ERROR-TEXT                                   
034000           CALL FELLOG                                                    
034100         WHEN VALID-STATUS (STATUS-IX) = STATUS-WS                        
034200           CONTINUE                                                       
034300       END-SEARCH                                                         
034400     END-IF                                                               
034500     .                                                                    
