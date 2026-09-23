000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ041000.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/07/09.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    NAME:                                                                
001000*        CARPARTS.DAP.DISTRRESTARTLOCATE                                  
001100*    FUNCTION:                                                            
001200*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001300*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001400*                                                                         
001500*        THE PROGRAM READS TABLE TZ4REKY                                  
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WZ0410T                                             
001900*        REQUEST:     WZ01REQU                                            
002000*                     WZ0410I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WZ01RESP                                            
002400*                     WZ0410O1                                            
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)  VALUE 'WZ041000'.             
003200 77  IDSYSTEM                    PIC X(04)  VALUE 'WZ04'.                 
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700                                                                          
003800*    --- CONSTANTS                                                        
003900 77  YES                         PIC X      VALUE 'Y'.                    
004000 77  JA                          PIC X      VALUE 'J'.                    
004100 77  NOO                         PIC X      VALUE 'N'.                    
004200                                                                          
004300 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004400 77  WS-GET                      PIC X      VALUE 'G'.                    
004500 77  WS-EXECUTE                  PIC X      VALUE 'E'.                    
004600 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004700 77  WS-ADRESS                   PIC X(50)                                
004800                          VALUE 'CARPARTS.DAP.DISTRRESTARTLOCATE'.        
004900                                                                          
005000 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005100     88  KEYS-OK                            VALUE 'Y'.                    
005200     88  KEYS-WRONG                         VALUE 'N'.                    
005300                                                                          
005400 77  FIRST-SW                    PIC X      VALUE 'Y'.                    
005500     88  SW-FIRST-YES                       VALUE 'Y'.                    
005600     88  SW-FIRST-NO                        VALUE 'N'.                    
005700                                                                          
005800 77  SW-RELATIVE-TIME            PIC X      VALUE 'N'.                    
005900     88  RELATIVE-TIME-YES                  VALUE 'Y'.                    
006000     88  RELATIVE-TIME-NO                   VALUE 'N'.                    
006100                                                                          
006200*    --- WORK FIELDS FOR  CONSTUCTING DATE AND TIME                       
006300 01  WS-TIREGDAT                 PIC S9(7) COMP-3.                        
006400 01  WS-LENGTH-TIREGDAT          PIC S9(4)  BINARY.                       
006500 01  WS-TIKLOCK                  PIC S9(9) COMP-3.                        
006600 01  WS-TIKLOCK-N                PIC S9(4)  BINARY.                       
006700 01  WS-LENGTH-TIKLOCK           PIC S9(4)  BINARY.                       
006800                                                                          
006900*    --- WORK FIELDS COUNTING SPACES                                      
007000 01  NSPACE                      PIC S9(4)  BINARY.                       
007100                                                                          
007200*    --- SEARCHING KEY FIELDS                                             
007300 01  IDOUTDEST-KY-X.                                                      
007400     03 IDOUTDEST-LOW-KY         PIC X(60)  VALUE LOW-VALUE.              
007500     03 IDOUTDEST-HIGH-KY        PIC X(60)  VALUE HIGH-VALUE.             
007600 01  TIREGDAT-KY-X.                                                       
007700     03 TIREGDAT-LOW-KY          PIC S9(7)  VALUE ZERO    COMP-3.         
007800     03 TIREGDAT-HIGH-KY         PIC S9(7)  VALUE 999999  COMP-3.         
007900 01  TIKLOCK-KY-X.                                                        
008000     03 TIKLOCK-LOW-KY           PIC S9(9)  VALUE ZERO    COMP-3.         
008100     03 TIKLOCK-HIGH-KY          PIC S9(9)  VALUE 99999999 COMP-3.        
008200 01  IDLIST-KY-X.                                                         
008300     03 IDLIST-LOW-KY            PIC X(10)  VALUE LOW-VALUE.              
008400     03 IDLIST-HIGH-KY           PIC X(10)  VALUE HIGH-VALUE.             
008500 01  IDOUTREC-KY-X.                                                       
008600     03 IDOUTREC-LOW-KY          PIC X(30)  VALUE LOW-VALUE.              
008700     03 IDOUTREC-HIGH-KY         PIC X(30)  VALUE HIGH-VALUE.             
008800 01  FLRULEMISS-KY-X.                                                     
008900     03 FLRULEMISS-LOW-KY        PIC X      VALUE ' '.                    
009000     03 FLRULEMISS-HIGH-KY       PIC X      VALUE '9'.                    
009100 01  IDOUTTYPE-KY-X.                                                      
009200     03 IDOUTTYPE-LOW-KY         PIC X(15)  VALUE LOW-VALUE.              
009300     03 IDOUTTYPE-HIGH-KY        PIC X(15)  VALUE HIGH-VALUE.             
009400 01  KDOUTMETH-KY-X.                                                      
009500     03 KDOUTMETH-LOW-KY         PIC X(4)  VALUE LOW-VALUE.               
009600     03 KDOUTMETH-HIGH-KY        PIC X(4)  VALUE HIGH-VALUE.              
009700                                                                          
009800*    --- "GET IDLIST" KEY FIELDS                                          
009900 01  IDLIST-KY-X-GET.                                                     
010000     03 IDLIST-GET               PIC X(10)  VALUE LOW-VALUE.              
010100                                                                          
010200                                                                          
010300*    --- WORK FIELDS                                                      
010400 01  WS-COUNTER-TZ4REKY          PIC S9(7)  VALUE ZERO    COMP-3.         
010500 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
010600                                                                          
010700*    --- WORK FIELDS FOR JOB                                              
010800 01  WORK-JOBNAME                PIC X(8)   VALUE                         
010900                                  'WZ14NNNN'.                             
011000 01  WORK-ACCOUNT                PIC X(30)  VALUE                         
011100                                  '510WOS39000WZ14J003'.                  
011200 01  WORK-MSGCLASS-CLASS         PIC X(30)  VALUE                         
011300                                  'MSGCLASS=H,CLASS=E'.                   
011400 01  WORK-JOBFORMS               PIC X(4)   VALUE                         
011500                                  'STD '.                                 
011600 01  WORK-PROCLIBS               PIC X(50)  VALUE                         
011700                                  'W.XXXX.PROCLIB,W.PROD.PROCLIB'.        
011800 01  WORK-ENV                    PIC X(8)   VALUE                         
011900                                  'ENVXXXX '.                             
012000 01  WORK-SYSTZ                  PIC X(8)   VALUE                         
012100                                  'SYSTZXX '.                             
012200 01  WORK-ROUTE-XEQ              PIC X(8)   VALUE                         
012300                                  'LOCAL'.                                
012400 01  WORK-JOB-DEST               PIC X(12)  VALUE                         
012500                                  'LOCAL QUEUE.'.                         
012600 01  WORK-IDMAIL                 PIC X(50)  VALUE                         
012700                                  'wsyst@volvocars.com'.                  
012800 01  WORK-TIMESTAMP              PIC X(14).                               
012900                                                                          
013000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013100 01  GENERAL-SUBPROGRAMS.                                                 
013200     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
013300     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
013400     03  W009WAIT                PIC X(8)   VALUE 'W009WAIT'.             
013500     03  VIMSID                  PIC X(8)   VALUE 'VIMSID  '.             
013600     03  WZ20SUBM                PIC X(8)   VALUE 'WZ20SUBM'.             
013700     03  W980USPA                PIC X(8)   VALUE 'W980USPA'.             
013800                                                                          
013900*    --- PARAMETERS TO ABEND                                              
014000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
014400                                                                          
014500*    --- PARAMETERS TO VIMSID                                             
014600 01  VIMSID-PARM.                                                         
014700   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
014800   03  FILLER                    PIC X(4)    VALUE SPACE.                 
014900                                                                          
015000*    --- PARAMETERS TO WZ20SUBM                                           
015100 01  -COPY WZ20SUBM                                                       
015200                                                                          
015300*    --- PARAMETERS TO W980USPA                                           
015400 01  -COPY W980USPA                                                       
015500                                                                          
015600 01  MESSAGE-CODES.                                                       
015700     03  ERROR-CODES.                                                     
015800         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
015900         05  ERR-INVALID-FIELD       PIC X(3)   VALUE '023'.              
016000         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
016100         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
016200         05  ERR-SYSTEM-ERROR        PIC X(3)   VALUE '099'.              
016300     03  INFO-CODES.                                                      
016400         05  INF-RESTARTED           PIC X(3)   VALUE '105'.              
016500                                                                          
016600*    --- PARAMETERS TO W009WAIT                                           
016700 77  WAIT-TIME                   PIC S9(9)   COMP VALUE +0.               
016800                                                                          
016900*01  -COPY WZ01SUB                                                        
017000*                                                                         
017100 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
017200 01  REQU-AREA.                                                           
017300*    03 -COPY WZ01REQU                                                    
017400*    03 -COPY WZ0410I1                                                    
017500                                                                          
017600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
017700 01  RESP-AREA.                                                           
017800*    03 -COPY WZ01RESP                                                    
017900*    03 -COPY WZ0410O1                                                    
018000                                                                          
018100 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
018200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018300                                                                          
018400 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
018500 01  DB2-WS.                                                              
018600     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
018700         88  LINES-FOUND                    VALUE 000.                    
018800         88  LINES-MISSING                  VALUE 100.                    
018900         88  RESOURCE-WRONG                 VALUE 904.                    
019000     03  GOOD-SQLCODECODES.                                               
019100         05  GOOD-SQLCODE OCCURS 5                                        
019200             INDEXED BY SQLCODE-IX PIC 9(3).                              
019300                                                                          
019400 01  FILLER                      PIC X(16)   VALUE 'TZ4REKY-AREA'.        
019500                                                                          
019600*01  -COPY TZ4REKY -PRE REKY-                                             
019700                                                                          
019800       EXEC SQL INCLUDE TZ4REKY END-EXEC.                                 
019900                                                                          
020000 LINKAGE SECTION.                                                         
020100                                                                          
020200 PROCEDURE DIVISION.                                                      
020300 MAIN SECTION.                                                            
020400                                                                          
020500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
020600     IF SUB-KDRC = ZERO                                                   
020700       PERFORM A-INIT                                                     
020800       PERFORM B-CHECK-KEYS                                               
020900       IF KEYS-OK                                                         
021000         IF REQU-KDPGMACT = WS-SEARCH                                     
021100*          -- SEARCH WITH GENERAL PARTIAL KEYS                            
021200           PERFORM F-SEARCH-SHOW-INFO                                     
021300         ELSE                                                             
021400           IF REQU-KDPGMACT = WS-GET                                      
021500*            -- READ WITH IDLIST AS EXACT KEY                             
021600             PERFORM G-GET-IDLIST-SHOW-INFO                               
021700           ELSE                                                           
021800             IF REQU-KDPGMACT = WS-EXECUTE                                
021900*              -- SUBMIT JOB TO RESTART SELECTED DISTRIBUTIONS            
022000               PERFORM H-RESTART-SELECTED                                 
022100*              -- SHOW THE DATA FOR THE KEYS ENTERED                      
022200               PERFORM F-SEARCH-SHOW-INFO                                 
022300             END-IF                                                       
022400           END-IF                                                         
022500         END-IF                                                           
022600       END-IF                                                             
022700       PERFORM S02-RETURN-RESPONSE                                        
022800     END-IF                                                               
022900                                                                          
023000     MOVE ZERO TO RETURN-CODE                                             
023100     GOBACK                                                               
023200     .                                                                    
023300                                                                          
023400 A-INIT SECTION.                                                          
023500                                                                          
023600     INITIALIZE GOOD-SQLCODECODES                                         
023700                                                                          
023800     MOVE ALL '+' TO RESP-AREA                                            
023900     MOVE ZERO    TO RESP-KVRADER                                         
024000     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
024100     MOVE SPACE   TO RESP-IDMSG-INFO                                      
024200     MOVE SPACE   TO RESP-IDELMT-ERROR                                    
024300     .                                                                    
024400                                                                          
024500 B-CHECK-KEYS SECTION.                                                    
024600                                                                          
024700     MOVE YES                    TO KEYS-SW                               
024800                                                                          
024900     IF REQU-TIREGDAT-KEY = SPACE                                         
025000       MOVE ALL '+' TO REQU-TIREGDAT-KEY                                  
025100     END-IF                                                               
025200     IF REQU-TIREGDAT-KEY = '*'                                           
025300       MOVE 0                    TO REQU-TIREGDAT-KEY                     
025400     END-IF                                                               
025500                                                                          
025600     IF REQU-TIKLOCK-KEY = SPACE                                          
025700       MOVE ALL '+' TO REQU-TIKLOCK-KEY                                   
025800     END-IF                                                               
025900                                                                          
026000     IF REQU-TIREGDAT-KEY = ALL '+'                                       
026100       CONTINUE                                                           
026200     ELSE                                                                 
026300       IF FUNCTION TEST-NUMVAL(REQU-TIREGDAT-KEY) = 0                     
026400         COMPUTE WS-TIREGDAT = FUNCTION NUMVAL(REQU-TIREGDAT-KEY)         
026500         IF WS-TIREGDAT >= 0                                              
026600           COMPUTE WS-LENGTH-TIREGDAT = FUNCTION LENGTH                   
026700                                        (FUNCTION TRIM                    
026800                                         (REQU-TIREGDAT-KEY))             
026900           IF WS-LENGTH-TIREGDAT = 0                                      
027000             MOVE 1              TO WS-LENGTH-TIREGDAT                    
027100           END-IF                                                         
027200         ELSE                                                             
027300           MOVE NOO              TO KEYS-SW                               
027400           MOVE ERR-INVALID-KEY  TO RESP-IDMSG-ERROR                      
027500           MOVE 'TIREGDAT'       TO RESP-IDELMT-ERROR                     
027600         END-IF                                                           
027700       ELSE                                                               
027800         MOVE NOO                TO KEYS-SW                               
027900         MOVE ERR-INVALID-KEY    TO RESP-IDMSG-ERROR                      
028000         MOVE 'TIREGDAT'         TO RESP-IDELMT-ERROR                     
028100       END-IF                                                             
028200     END-IF                                                               
028300                                                                          
028400     MOVE NOO                    TO SW-RELATIVE-TIME                      
028500     IF REQU-TIKLOCK-KEY = ALL '+'                                        
028600       CONTINUE                                                           
028700     ELSE                                                                 
028800       IF FUNCTION TEST-NUMVAL(REQU-TIKLOCK-KEY) = 0                      
028900         COMPUTE WS-TIKLOCK = FUNCTION NUMVAL(REQU-TIKLOCK-KEY)           
029000         IF WS-TIKLOCK >= 0                                               
029100           COMPUTE WS-LENGTH-TIKLOCK = FUNCTION LENGTH                    
029200                                       (FUNCTION TRIM                     
029300                                        (REQU-TIKLOCK-KEY))               
029400           IF WS-LENGTH-TIKLOCK = 0                                       
029500             MOVE 1              TO WS-LENGTH-TIKLOCK                     
029600           END-IF                                                         
029700         ELSE                                                             
029800           MOVE YES              TO SW-RELATIVE-TIME                      
029900           COMPUTE WS-TIKLOCK-N = WS-TIKLOCK * -1                         
030000         END-IF                                                           
030100       ELSE                                                               
030200         MOVE NOO                TO KEYS-SW                               
030300         MOVE ERR-INVALID-KEY    TO RESP-IDMSG-ERROR                      
030400         MOVE 'TIKLOCK'          TO RESP-IDELMT-ERROR                     
030500       END-IF                                                             
030600     END-IF                                                               
030700                                                                          
030800     IF REQU-IDMSGVER NUMERIC                                             
030900       CONTINUE                                                           
031000     ELSE                                                                 
031100       MOVE NOO                  TO KEYS-SW                               
031200       MOVE ERR-SYSTEM-ERROR     TO RESP-IDMSG-ERROR                      
031300       MOVE 'IDMSGVER'           TO RESP-IDELMT-ERROR                     
031400     END-IF                                                               
031500                                                                          
031600     IF REQU-KDPGMACT = WS-SEARCH OR WS-GET OR WS-EXECUTE                 
031700       CONTINUE                                                           
031800     ELSE                                                                 
031900       MOVE NOO                  TO KEYS-SW                               
032000       MOVE ERR-SYSTEM-ERROR     TO RESP-IDMSG-ERROR                      
032100       MOVE 'KDPGMACT'           TO RESP-IDELMT-ERROR                     
032200     END-IF                                                               
032300                                                                          
032400     IF (REQU-IDUSER > SPACE AND NOT = ALL '+')                           
032500       CONTINUE                                                           
032600     ELSE                                                                 
032700       MOVE NOO                  TO KEYS-SW                               
032800       MOVE ERR-SYSTEM-ERROR     TO RESP-IDMSG-ERROR                      
032900       MOVE 'IDUSER'             TO RESP-IDELMT-ERROR                     
033000     END-IF                                                               
033100                                                                          
033200     .                                                                    
033300                                                                          
033400 F-SEARCH-SHOW-INFO SECTION.                                              
033500                                                                          
033600*    -- PROGRAM HAS BEEN CALLED WITH KDPGMACT = S FROM D&P PANELS         
033700*    -- NORMALLY WITH PARTIAL KEYS                                        
033800                                                                          
033900     MOVE REQU-IDOUTDEST-KEY     TO RESP-IDOUTDEST-KEY                    
034000     IF (REQU-IDOUTDEST-KEY NOT = ALL '+')                                
034100       MOVE ZERO TO NSPACE                                                
034200       INSPECT FUNCTION REVERSE (REQU-IDOUTDEST-KEY)                      
034300        TALLYING NSPACE FOR LEADING SPACE                                 
034400       COMPUTE NSPACE = LENGTH OF REQU-IDOUTDEST-KEY - NSPACE + 1         
034500                                                                          
034600                                                                          
034700       MOVE REQU-IDOUTDEST-KEY TO IDOUTDEST-LOW-KY                        
034800                                  IDOUTDEST-HIGH-KY                       
034900       INSPECT IDOUTDEST-LOW-KY (NSPACE:)                                 
035000          CONVERTING SPACE TO LOW-VALUE                                   
035100       INSPECT IDOUTDEST-HIGH-KY                                          
035200          CONVERTING SPACE TO HIGH-VALUE                                  
035300     END-IF                                                               
035400                                                                          
035500     IF REQU-TIREGDAT-KEY = ALL '+'                                       
035600       MOVE FUNCTION CURRENT-DATE(3:6)                                    
035700                                 TO REQU-TIREGDAT-KEY                     
035800                                    WS-TIREGDAT                           
035900       MOVE 6                    TO WS-LENGTH-TIREGDAT                    
036000     END-IF                                                               
036100     IF REQU-TIREGDAT-KEY = 0                                             
036200       MOVE '000101' TO TIREGDAT-LOW-KY                                   
036300       MOVE '991231' TO TIREGDAT-HIGH-KY                                  
036400     ELSE                                                                 
036500       MOVE REQU-TIREGDAT-KEY    TO RESP-TIREGDAT-KEY                     
036600       MOVE WS-TIREGDAT          TO TIREGDAT-LOW-KY                       
036700                                    TIREGDAT-HIGH-KY                      
036800       COMPUTE WS-LENGTH-TIREGDAT = 6 - WS-LENGTH-TIREGDAT                
036900       PERFORM WS-LENGTH-TIREGDAT TIMES                                   
037000         MULTIPLY 10             BY TIREGDAT-LOW-KY                       
037100         MULTIPLY 10             BY TIREGDAT-HIGH-KY                      
037200         ADD 9                   TO TIREGDAT-HIGH-KY                      
037300       END-PERFORM                                                        
037400     END-IF                                                               
037500                                                                          
037600     IF REQU-TIKLOCK-KEY = ALL '+'                                        
037700       MOVE ZERO                 TO TIKLOCK-LOW-KY                        
037800       MOVE 99999999             TO TIKLOCK-HIGH-KY                       
037900     ELSE                                                                 
038000       MOVE REQU-TIKLOCK-KEY     TO RESP-TIKLOCK-KEY                      
038100       IF WS-TIKLOCK >= 0                                                 
038200         MOVE WS-TIKLOCK         TO TIKLOCK-LOW-KY                        
038300                                      TIKLOCK-HIGH-KY                     
038400         COMPUTE WS-LENGTH-TIKLOCK = 8 - WS-LENGTH-TIKLOCK                
038500         PERFORM WS-LENGTH-TIKLOCK TIMES                                  
038600           MULTIPLY 10           BY TIKLOCK-LOW-KY                        
038700           MULTIPLY 10           BY TIKLOCK-HIGH-KY                       
038800           ADD 9                 TO TIKLOCK-HIGH-KY                       
038900         END-PERFORM                                                      
039000       END-IF                                                             
039100     END-IF                                                               
039200                                                                          
039300     MOVE REQU-IDLIST-KEY        TO RESP-IDLIST-KEY                       
039400     IF (REQU-IDLIST-KEY NOT = ALL '+')                                   
039500       MOVE REQU-IDLIST-KEY      TO IDLIST-LOW-KY                         
039600                                    IDLIST-HIGH-KY                        
039700       INSPECT IDLIST-LOW-KY  CONVERTING SPACE TO LOW-VALUE               
039800       INSPECT IDLIST-HIGH-KY CONVERTING SPACE TO HIGH-VALUE              
039900     END-IF                                                               
040000                                                                          
040100     MOVE REQU-IDOUTREC-KEY      TO RESP-IDOUTREC-KEY                     
040200     IF (REQU-IDOUTREC-KEY NOT = ALL '+')                                 
040300       MOVE REQU-IDOUTREC-KEY    TO IDOUTREC-LOW-KY                       
040400                                    IDOUTREC-HIGH-KY                      
040500       INSPECT IDOUTREC-LOW-KY CONVERTING SPACE TO LOW-VALUE              
040600       INSPECT IDOUTREC-HIGH-KY CONVERTING SPACE TO HIGH-VALUE            
040700     END-IF                                                               
040800                                                                          
040900     MOVE REQU-IDOUTTYPE-KEY     TO RESP-IDOUTTYPE-KEY                    
041000     IF (REQU-IDOUTTYPE-KEY NOT = ALL '+')                                
041100       MOVE REQU-IDOUTTYPE-KEY   TO IDOUTTYPE-LOW-KY                      
041200                                    IDOUTTYPE-HIGH-KY                     
041300       INSPECT IDOUTTYPE-LOW-KY CONVERTING SPACE TO LOW-VALUE             
041400       INSPECT IDOUTTYPE-HIGH-KY CONVERTING SPACE TO HIGH-VALUE           
041500     END-IF                                                               
041600                                                                          
041700     MOVE REQU-KDOUTMETH-KEY     TO RESP-KDOUTMETH-KEY                    
041800     IF (REQU-KDOUTMETH-KEY NOT = ALL '+')                                
041900       MOVE REQU-KDOUTMETH-KEY TO KDOUTMETH-LOW-KY                        
042000                                  KDOUTMETH-HIGH-KY                       
042100     END-IF                                                               
042200                                                                          
042300     MOVE REQU-FLRULEMISS-KEY    TO RESP-FLRULEMISS-KEY                   
042400     IF REQU-FLRULEMISS-KEY = JA                                          
042500       MOVE JA TO FLRULEMISS-LOW-KY                                       
042600                  FLRULEMISS-HIGH-KY                                      
042700     END-IF                                                               
042800                                                                          
042900                                                                          
043000     IF TIREGDAT-LOW-KY NOT > TIREGDAT-HIGH-KY                            
043100*      IF RELATIVE-TIME-NO                                                
043200         PERFORM FA-READ-BASICDATA                                        
043300*      ELSE                                                               
043400*        PERFORM FB-READ-DATA-RELATIVE-TIME                               
043500*      END-IF                                                             
043600     ELSE                                                                 
043700       MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                        
043800       MOVE 'TIREGDAT FROM/TO' TO RESP-IDELMT-ERROR                       
043900     END-IF                                                               
044000     .                                                                    
044100                                                                          
044200 FA-READ-BASICDATA SECTION.                                               
044300                                                                          
044400*    -- MAKE A SHORT DELAY (1.0 S) IF A SPECIFIC IDLIST IS                
044500*    -- SPECIFIED, IN ORDER TO DECREASE THE CHANCE THAT THE               
044600*    -- REPORTS HAVE NOT YET BEEN CREATED WHEN THIS PROGRAM IS            
044700*    -- CALLED FROM LDC PROGRAM WL0134.                                   
044800*    -- NOTE: TEMPOROARY FIX UNTIL WEB USE KDPGMACT = 'G'                 
044900     IF REQU-IDLIST-KEY NOT = ALL '+'                                     
045000       MOVE 100 TO WAIT-TIME                                              
045100       CALL W009WAIT USING WAIT-TIME                                      
045200     END-IF                                                               
045300                                                                          
045400     MOVE ZERO TO WS-COUNTER-TZ4REKY                                      
045500                                                                          
045600     PERFORM DB2-COUNT-CRS                                                
045700                                                                          
045800     IF WS-COUNTER-TZ4REKY = ZERO                                         
045900       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
046000*                                                                         
046100     ELSE                                                                 
046200       IF WS-COUNTER-TZ4REKY > WS-MAX-LINES                               
046300         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
046400       END-IF                                                             
046500     END-IF                                                               
046600                                                                          
046700     IF RESP-IDMSG-ERROR = SPACE                                          
046800       PERFORM DB2-DCL-OPN-TZ4REKY-CRS                                    
046900       PERFORM DB2-FETCH-TZ4REKY-CRS                                      
047000       MOVE ZERO TO WS-IX                                                 
047100                                                                          
047200       PERFORM UNTIL LINES-MISSING                                        
047300         PERFORM S03-MOVE-TO-RESPOND                                      
047400         PERFORM DB2-FETCH-TZ4REKY-CRS                                    
047500       END-PERFORM                                                        
047600                                                                          
047700       PERFORM DB2-CLOSE-TZ4REKY-CRS                                      
047800     END-IF                                                               
047900                                                                          
048000     MOVE WS-IX TO RESP-KVRADER                                           
048100     .                                                                    
048200                                                                          
048300 FB-READ-DATA-RELATIVE-TIME SECTION.                                      
048400                                                                          
048500*    -- MAKE A SHORT DELAY (1.0 S) IF A SPECIFIC IDLIST IS                
048600*    -- SPECIFIED, IN ORDER TO DECREASE THE CHANCE THAT THE               
048700*    -- REPORTS HAVE NOT YET BEEN CREATED WHEN THIS PROGRAM IS            
048800*    -- CALLED FROM LDC PROGRAM WL0134.                                   
048900*    -- NOTE: TEMPOROARY FIX UNTIL WEB USE KDPGMACT = 'G'                 
049000     IF REQU-IDLIST-KEY NOT = ALL '+'                                     
049100       MOVE 100 TO WAIT-TIME                                              
049200       CALL W009WAIT USING WAIT-TIME                                      
049300     END-IF                                                               
049400                                                                          
049500     MOVE ZERO                   TO WS-COUNTER-TZ4REKY                    
049600                                                                          
049700     PERFORM DB2-COUNT-CRS2                                               
049800                                                                          
049900     IF WS-COUNTER-TZ4REKY = ZERO                                         
050000       MOVE ERR-LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                      
050100     ELSE                                                                 
050200       IF WS-COUNTER-TZ4REKY > WS-MAX-LINES                               
050300         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
050400       END-IF                                                             
050500     END-IF                                                               
050600                                                                          
050700     IF RESP-IDMSG-ERROR = SPACE                                          
050800       PERFORM DB2-DCL-OPN-TZ4REKY-CRS2                                   
050900       PERFORM DB2-FETCH-TZ4REKY-CRS2                                     
051000       MOVE ZERO                 TO WS-IX                                 
051100                                                                          
051200       PERFORM UNTIL LINES-MISSING                                        
051300         PERFORM S03-MOVE-TO-RESPOND                                      
051400         PERFORM DB2-FETCH-TZ4REKY-CRS2                                   
051500       END-PERFORM                                                        
051600                                                                          
051700       PERFORM DB2-CLOSE-TZ4REKY-CRS2                                     
051800     END-IF                                                               
051900                                                                          
052000     MOVE WS-IX                  TO RESP-KVRADER                          
052100     .                                                                    
052200                                                                          
052300 G-GET-IDLIST-SHOW-INFO SECTION.                                          
052400                                                                          
052500*    -- PROGRAM HAS BEEN CALLED WITH KDPGMACT = G FROM LDC WL0134         
052600*    -- WITH ONLY IDLIST SET. MAX TWO LINES WITH THE SAME KEYS            
052700*    -- ARE EXPECTED TO BE RETURNED.                                      
052800                                                                          
052900*    -- MAKE A SHORT DELAY (1.0 S) IN ORDER TO DECREASE THE CHANCE        
053000*    -- THAT THE REPORTS HAVE NOT YET BEEN CREATED.                       
053100*    -- (THEY ARE CREATED ASYNCHRONOUSLY IN THE BACKGROUND)               
053200     MOVE 100 TO WAIT-TIME                                                
053300     CALL W009WAIT USING WAIT-TIME                                        
053400                                                                          
053500     MOVE REQU-IDOUTDEST-KEY     TO RESP-IDOUTDEST-KEY                    
053600     MOVE REQU-TIREGDAT-KEY      TO RESP-TIREGDAT-KEY                     
053700     MOVE REQU-TIKLOCK-KEY       TO RESP-TIKLOCK-KEY                      
053800     MOVE REQU-IDOUTREC-KEY      TO RESP-IDOUTREC-KEY                     
053900     MOVE REQU-IDOUTTYPE-KEY     TO RESP-IDOUTTYPE-KEY                    
054000     MOVE REQU-KDOUTMETH-KEY     TO RESP-KDOUTMETH-KEY                    
054100     MOVE REQU-FLRULEMISS-KEY    TO RESP-FLRULEMISS-KEY                   
054200                                                                          
054300     MOVE REQU-IDLIST-KEY        TO RESP-IDLIST-KEY                       
054400                                    IDLIST-GET                            
054500                                                                          
054600     PERFORM DB2-DCL-OPN-TZ4REKY-CRS-GET                                  
054700     PERFORM DB2-FETCH-TZ4REKY-CRS-GET                                    
054800                                                                          
054900     MOVE ZERO TO WS-IX                                                   
055000     PERFORM UNTIL LINES-MISSING OR WS-IX > WS-MAX-LINES                  
055100       PERFORM S03-MOVE-TO-RESPOND                                        
055200       PERFORM DB2-FETCH-TZ4REKY-CRS-GET                                  
055300     END-PERFORM                                                          
055400                                                                          
055500     PERFORM DB2-CLOSE-TZ4REKY-CRS-GET                                    
055600                                                                          
055700     MOVE WS-IX TO RESP-KVRADER                                           
055800     .                                                                    
055900                                                                          
056000 H-RESTART-SELECTED SECTION.                                              
056100                                                                          
056200     SET SW-FIRST-YES            TO TRUE                                  
056300     PERFORM                                                              
056400     VARYING WS-IX FROM 1 BY 1                                            
056500       UNTIL WS-IX > REQU-KVRADER                                         
056600       IF REQU-KDCMD-LINE (WS-IX) = 'S'                                   
056700         IF SW-FIRST-YES                                                  
056800           PERFORM HA-WRITE-JOB-DATA                                      
056900           SET SW-FIRST-NO       TO TRUE                                  
057000         END-IF                                                           
057100         PERFORM HB-WRITE-RESTART-KEY                                     
057200       END-IF                                                             
057300     END-PERFORM                                                          
057400     IF SW-FIRST-NO                                                       
057500       PERFORM HC-CLOSE-JOB                                               
057600     END-IF                                                               
057700     MOVE INF-RESTARTED          TO RESP-IDMSG-INFO                       
057800     .                                                                    
057900                                                                          
058000 HA-WRITE-JOB-DATA SECTION.                                               
058100                                                                          
058200*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
058300     CALL VIMSID              USING VIMSID-PARM                           
058400                                                                          
058500*    -- GET CURRENT TIMESTAMP FOR USE IN JOB NAME AND                     
058600*       TO SEND IN THE MAIL IN CASE OF FAILURE.                           
058700     MOVE FUNCTION CURRENT-DATE(1:14)                                     
058800                                 TO WORK-TIMESTAMP                        
058900                                                                          
059000     STRING 'WZ11'                             DELIMITED BY SIZE          
059100            WORK-TIMESTAMP(11:4)               DELIMITED BY SIZE          
059200                               INTO WORK-JOBNAME                          
059300     END-STRING                                                           
059400                                                                          
059500     MOVE 'LOCAL'                TO WORK-ROUTE-XEQ                        
059600     IF IMSID4 = 'IMG0'                                                   
059700       MOVE '540WZ010100WZ14J003'                                         
059800                                 TO WORK-ACCOUNT                          
059900       MOVE 'CLASS=V'            TO WORK-MSGCLASS-CLASS                   
060000       MOVE '1800'               TO WORK-JOBFORMS                         
060100       MOVE 'W.QASE.PROCLIB'     TO WORK-PROCLIBS                         
060200       MOVE 'ENVQASE'            TO WORK-ENV                              
060300       MOVE 'SYSTZ'              TO WORK-SYSTZ                            
060400       MOVE 'SAR.'               TO WORK-JOB-DEST                         
060500     ELSE                                                                 
060600       MOVE '510WOS39000WZ14J003'                                         
060700                                 TO WORK-ACCOUNT                          
060800       MOVE 'STD'                TO WORK-JOBFORMS                         
060900       MOVE 'LOCAL QUEUE.'       TO WORK-JOB-DEST                         
061000       IF IMSID4 = 'IMB0'                                                 
061100         MOVE 'MSGCLASS=H,CLASS=V'                                        
061200                                 TO WORK-MSGCLASS-CLASS                   
061300         MOVE 'W.ACPT.PROCLIB,W.QASE.PROCLIB'                             
061400                                 TO WORK-PROCLIBS                         
061500         MOVE 'ENVACPT'          TO WORK-ENV                              
061600         MOVE 'SYSTZAA'          TO WORK-SYSTZ                            
061700       ELSE                                                               
061800         MOVE 'MSGCLASS=H,CLASS=E'                                        
061900                                 TO WORK-MSGCLASS-CLASS                   
062000         IF IMSID4 = 'IMD0'                                               
062100           MOVE 'W.XDEV.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'            
062200                                 TO WORK-PROCLIBS                         
062300           MOVE 'ENVXDEV'        TO WORK-ENV                              
062400           MOVE 'SYSTZXX'        TO WORK-SYSTZ                            
062500         ELSE                                                             
062600           IF IMSID4 = 'IMP0'                                             
062700             MOVE 'W.DEVE.PROCLIB,W.IGRT.PROCLIB,W.PROD.PROCLIB'          
062800                                 TO WORK-PROCLIBS                         
062900             MOVE 'ENVDEVE'      TO WORK-ENV                              
063000             MOVE 'SYSTZDD'      TO WORK-SYSTZ                            
063100           ELSE                                                           
063200             MOVE 'W.IGRT.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'          
063300                                 TO WORK-PROCLIBS                         
063400             MOVE 'ENVIGRT'      TO WORK-ENV                              
063500             MOVE 'SYSTZTT'      TO WORK-SYSTZ                            
063600           END-IF                                                         
063700         END-IF                                                           
063800       END-IF                                                             
063900     END-IF                                                               
064000                                                                          
064100     MOVE 1                      TO SUBM-IDCALL                           
064200     MOVE 'OPEN'                 TO SUBM-KDFUNC                           
064300     PERFORM S04-CALL-WZ20SUBM                                            
064400                                                                          
064500     MOVE 'PUT'                  TO SUBM-KDFUNC                           
064600                                                                          
064700     MOVE SPACE                  TO SUBM-LINE                             
064800     STRING '//'                               DELIMITED BY SIZE          
064900            WORK-JOBNAME                       DELIMITED BY SPACE         
065000            ' JOB ('                           DELIMITED BY SIZE          
065100            WORK-ACCOUNT                       DELIMITED BY SPACE         
065200            ',W100),'                          DELIMITED BY SIZE          
065300            QUOTE 'RTN WZ14S9' QUOTE ','       DELIMITED BY SIZE          
065400       INTO SUBM-LINE                                                     
065500     END-STRING                                                           
065600     PERFORM S04-CALL-WZ20SUBM                                            
065700                                                                          
065800     CALL W980USPA            USING USPA-AREA                             
065900                                                                          
066000     MOVE SPACE                  TO SUBM-LINE                             
066100     STRING                                                               
066200       '//  USER='                             DELIMITED BY SIZE          
066300       USPA-IDUSER                             DELIMITED BY SPACE         
066400       ',PASSWORD='                            DELIMITED BY SIZE          
066500       USPA-IDPW                               DELIMITED BY SPACE         
066600       ','                                     DELIMITED BY SIZE          
066700       INTO SUBM-LINE                                                     
066800     END-STRING                                                           
066900     PERFORM S04-CALL-WZ20SUBM                                            
067000                                                                          
067100     MOVE SPACE                  TO SUBM-LINE                             
067200     STRING                                                               
067300       '// MSGLEVEL=(1,1),'                    DELIMITED BY SIZE          
067400       WORK-MSGCLASS-CLASS                     DELIMITED BY SPACE         
067500       INTO SUBM-LINE                                                     
067600     END-STRING                                                           
067700     PERFORM S04-CALL-WZ20SUBM                                            
067800                                                                          
067900     MOVE SPACE                  TO SUBM-LINE                             
068000     STRING                                                               
068100       '/*JOBPARM FORMS='                      DELIMITED BY SIZE          
068200       WORK-JOBFORMS                           DELIMITED BY SPACE         
068300       ',LINECT=0,LINES=9999'                  DELIMITED BY SIZE          
068400       INTO SUBM-LINE                                                     
068500     END-STRING                                                           
068600     PERFORM S04-CALL-WZ20SUBM                                            
068700                                                                          
068800     MOVE SPACE                  TO SUBM-LINE                             
068900     STRING                                                               
069000       '/*ROUTE XEQ '                          DELIMITED BY SIZE          
069100       WORK-ROUTE-XEQ                          DELIMITED BY SPACE         
069200       INTO SUBM-LINE                                                     
069300     END-STRING                                                           
069400     PERFORM S04-CALL-WZ20SUBM                                            
069500                                                                          
069600     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
069700       MOVE '//*+JBS BIND VCC1'  TO SUBM-LINE                             
069800     END-IF                                                               
069900     PERFORM S04-CALL-WZ20SUBM                                            
070000                                                                          
070100     MOVE SPACE                  TO SUBM-LINE                             
070200     STRING                                                               
070300       '//PROC JCLLIB ORDER=('                 DELIMITED BY SIZE          
070400       WORK-PROCLIBS                           DELIMITED BY SPACE         
070500       ')'                                     DELIMITED BY SIZE          
070600       INTO SUBM-LINE                                                     
070700     END-STRING                                                           
070800     PERFORM S04-CALL-WZ20SUBM                                            
070900                                                                          
071000     MOVE SPACE                  TO SUBM-LINE                             
071100     STRING                                                               
071200       '//ENV   INCL'                          DELIMITED BY SIZE          
071300       'UDE MEMBER='                           DELIMITED BY SIZE          
071400       WORK-ENV                                DELIMITED BY SPACE         
071500       INTO SUBM-LINE                                                     
071600     END-STRING                                                           
071700     PERFORM S04-CALL-WZ20SUBM                                            
071800                                                                          
071900     MOVE SPACE                  TO SUBM-LINE                             
072000     STRING                                                               
072100       '//ENV   INCL'                          DELIMITED BY SIZE          
072200       'UDE MEMBER='                           DELIMITED BY SIZE          
072300       WORK-SYSTZ                              DELIMITED BY SPACE         
072400       INTO SUBM-LINE                                                     
072500     END-STRING                                                           
072600     PERFORM S04-CALL-WZ20SUBM                                            
072700                                                                          
072800     MOVE '//DATA    EXEC WZ14P003'                                       
072900                                 TO SUBM-LINE                             
073000     PERFORM S04-CALL-WZ20SUBM                                            
073100                                                                          
073200     .                                                                    
073300                                                                          
073400 HB-WRITE-RESTART-KEY SECTION.                                            
073500                                                                          
073600     MOVE SPACE                  TO SUBM-LINE                             
073700     STRING REQU-IDOUTTYPE-LINE (WS-IX) DELIMITED BY SIZE                 
073800            REQU-IDOUTREC-LINE  (WS-IX) DELIMITED BY SIZE                 
073900            REQU-IDLIST-LINE    (WS-IX) DELIMITED BY SIZE                 
074000            REQU-TIREGDAT-LINE  (WS-IX) DELIMITED BY SIZE                 
074100            REQU-TIKLOCK-LINE   (WS-IX) DELIMITED BY SIZE                 
074200            REQU-IDLOPNR-LINE   (WS-IX) DELIMITED BY SIZE                 
074300       INTO SUBM-LINE                                                     
074400     END-STRING                                                           
074500     PERFORM S04-CALL-WZ20SUBM                                            
074600     .                                                                    
074700                                                                          
074800 HC-CLOSE-JOB SECTION.                                                    
074900                                                                          
075000     MOVE '/*'                   TO SUBM-LINE                             
075100     PERFORM S04-CALL-WZ20SUBM                                            
075200                                                                          
075300     MOVE '//IFDATA IF (DATA.WZ1403.RC > 0) THEN'                         
075400                                 TO SUBM-LINE                             
075500     PERFORM S04-CALL-WZ20SUBM                                            
075600                                                                          
075700     MOVE '//MAIL    EXEC WMAILSND'                                       
075800                                 TO SUBM-LINE                             
075900     PERFORM S04-CALL-WZ20SUBM                                            
076000                                                                          
076100     MOVE ')SEND'                TO SUBM-LINE                             
076200     PERFORM S04-CALL-WZ20SUBM                                            
076300                                                                          
076400     MOVE ' SUBJECT D&P MAIL (RESTART ERROR  )'                           
076500                                 TO SUBM-LINE                             
076600     PERFORM S04-CALL-WZ20SUBM                                            
076700                                                                          
076800     MOVE SPACE                  TO SUBM-LINE                             
076900     STRING ' TO '                             DELIMITED BY SIZE          
077000            WORK-IDMAIL                        DELIMITED BY SPACE         
077100       INTO SUBM-LINE                                                     
077200     END-STRING                                                           
077300     PERFORM S04-CALL-WZ20SUBM                                            
077400                                                                          
077500     MOVE ' MAIL'                TO SUBM-LINE                             
077600     PERFORM S04-CALL-WZ20SUBM                                            
077700                                                                          
077800     MOVE ' Request to restart selected distributions failed parti        
077900-         'ally or completely.'                                           
078000                                 TO SUBM-LINE                             
078100     PERFORM S04-CALL-WZ20SUBM                                            
078200                                                                          
078300     MOVE SPACE                  TO SUBM-LINE                             
078400     STRING 'Date and approximate time for this job was:'                 
078500                                               DELIMITED BY SIZE          
078600            WORK-TIMESTAMP                     DELIMITED BY SPACE         
078700       INTO SUBM-LINE                                                     
078800     END-STRING                                                           
078900     PERFORM S04-CALL-WZ20SUBM                                            
079000                                                                          
079100     MOVE SPACE                  TO SUBM-LINE                             
079200     STRING 'For more details, see job '       DELIMITED BY SIZE          
079300            WORK-JOBNAME                       DELIMITED BY SPACE         
079400            ' in '                             DELIMITED BY SIZE          
079500            WORK-JOB-DEST                      DELIMITED BY SIZE          
079600       INTO SUBM-LINE                                                     
079700     END-STRING                                                           
079800     PERFORM S04-CALL-WZ20SUBM                                            
079900                                                                          
080000     MOVE '/*'                   TO SUBM-LINE                             
080100     PERFORM S04-CALL-WZ20SUBM                                            
080200                                                                          
080300     MOVE '//IFDATA ENDIF'       TO SUBM-LINE                             
080400     PERFORM S04-CALL-WZ20SUBM                                            
080500                                                                          
080600     MOVE 'CLOSE'                TO SUBM-KDFUNC                           
080700     PERFORM S04-CALL-WZ20SUBM                                            
080800     MOVE SPACE                  TO SUBM-LINE                             
080900     .                                                                    
081000                                                                          
081100*   --- DISPATCHER SECTION START                                          
081200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
081300                                                                          
081400     MOVE 'GETARG'             TO SUB-KDFUNC                              
081500     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
081600     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
081700                                                                          
081800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
081900                                                                          
082000     IF SUB-KDRC > 0                                                      
082100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
082200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
082300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
082400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
082500     END-IF                                                               
082600     .                                                                    
082700 S02-RETURN-RESPONSE SECTION.                                             
082800                                                                          
082900     MOVE 'RETURN'             TO SUB-KDFUNC                              
083000                                                                          
083100     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
083200                              - ((WS-MAX-LINES - WS-IX)                   
083300                              * LENGTH OF RESP-TABELLRAD)                 
083400                                                                          
083500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
083600                                                                          
083700     IF SUB-KDRC > 0                                                      
083800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
083900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
084000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
084100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
084200     END-IF                                                               
084300     .                                                                    
084400*** - MOVE DATA TO RESPOND.                                               
084500 S03-MOVE-TO-RESPOND SECTION.                                             
084600                                                                          
084700     ADD 1 TO WS-IX                                                       
084800                                                                          
084900     MOVE REKY-IDOUTDEST       TO RESP-IDOUTDEST-LINE(WS-IX)              
085000     MOVE REKY-TIREGDAT        TO RESP-TIREGDAT-LINE(WS-IX)               
085100     MOVE REKY-TIKLOCK         TO RESP-TIKLOCK-LINE(WS-IX)                
085200     MOVE REKY-IDLOPNR         TO RESP-IDLOPNR-LINE(WS-IX)                
085300     MOVE REKY-IDOUTREC        TO RESP-IDOUTREC-LINE(WS-IX)               
085400     MOVE REKY-KDOUTMETH       TO RESP-KDOUTMETH-LINE(WS-IX)              
085500     MOVE REKY-IDOUTTYPE       TO RESP-IDOUTTYPE-LINE(WS-IX)              
085600     MOVE REKY-IDLIST          TO RESP-IDLIST-LINE(WS-IX)                 
085700     MOVE REKY-KVANTEX-PRINTAD TO RESP-KVANTEX-PRINTAD-LINE(WS-IX)        
085800     MOVE REKY-TIAAMMDD-RENS   TO RESP-TIAAMMDD-RENS-LINE(WS-IX)          
085900     .                                                                    
086000 S04-CALL-WZ20SUBM SECTION.                                               
086100                                                                          
086200     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
086300     .                                                                    
086400*   --- DB2 SECTIONS                                                      
086500                                                                          
086600 DB2-COUNT-CRS SECTION.                                                   
086700                                                                          
086800     EXEC SQL                                                             
086900       SELECT COUNT(*)                                                    
087000                                                                          
087100       INTO  :WS-COUNTER-TZ4REKY                                          
087200                                                                          
087300       FROM   TZ4REKY                                                     
087400                                                                          
087500       WHERE  IDOUTDEST  BETWEEN :IDOUTDEST-LOW-KY                        
087600                             AND :IDOUTDEST-HIGH-KY                       
087700        AND   TIREGDAT   BETWEEN :TIREGDAT-LOW-KY                         
087800                             AND :TIREGDAT-HIGH-KY                        
087900        AND   TIKLOCK    BETWEEN :TIKLOCK-LOW-KY                          
088000                             AND :TIKLOCK-HIGH-KY                         
088100        AND   IDLIST     BETWEEN :IDLIST-LOW-KY                           
088200                             AND :IDLIST-HIGH-KY                          
088300        AND   IDOUTREC   BETWEEN :IDOUTREC-LOW-KY                         
088400                             AND :IDOUTREC-HIGH-KY                        
088500        AND   FLRULEMISS BETWEEN :FLRULEMISS-LOW-KY                       
088600                             AND :FLRULEMISS-HIGH-KY                      
088700        AND   IDOUTTYPE  BETWEEN :IDOUTTYPE-LOW-KY                        
088800                             AND :IDOUTTYPE-HIGH-KY                       
088900        AND   KDOUTMETH  BETWEEN :KDOUTMETH-LOW-KY                        
089000                             AND :KDOUTMETH-HIGH-KY                       
089100     END-EXEC                                                             
089200                                                                          
089300     MOVE 000100  TO GOOD-SQLCODECODES                                    
089400                                                                          
089500     MOVE SQLCODE TO SQLCODE-WS                                           
089600     PERFORM DB2-STATUS-CHECK                                             
089700     .                                                                    
089800                                                                          
089900 DB2-DCL-OPN-TZ4REKY-CRS SECTION.                                         
090000                                                                          
090100     MOVE 000100 TO GOOD-SQLCODECODES                                     
090200                                                                          
090300     EXEC SQL                                                             
090400       DECLARE TZ4REKY-CRS CURSOR WITH HOLD FOR                           
090500                                                                          
090600       SELECT  IDOUTDEST                                                  
090700             , TIREGDAT                                                   
090800             , TIKLOCK                                                    
090900             , IDLOPNR                                                    
091000             , IDOUTREC                                                   
091100             , KDOUTMETH                                                  
091200             , IDOUTTYPE                                                  
091300             , IDLIST                                                     
091400             , KVANTEX_PRINTAD                                            
091500             , TIAAMMDD_RENS                                              
091600                                                                          
091700       FROM    TZ4REKY                                                    
091800                                                                          
091900       WHERE  IDOUTDEST  BETWEEN :IDOUTDEST-LOW-KY                        
092000                             AND :IDOUTDEST-HIGH-KY                       
092100        AND   TIREGDAT   BETWEEN :TIREGDAT-LOW-KY                         
092200                             AND :TIREGDAT-HIGH-KY                        
092300        AND   TIKLOCK    BETWEEN :TIKLOCK-LOW-KY                          
092400                             AND :TIKLOCK-HIGH-KY                         
092500        AND   IDLIST     BETWEEN :IDLIST-LOW-KY                           
092600                             AND :IDLIST-HIGH-KY                          
092700        AND   IDOUTREC   BETWEEN :IDOUTREC-LOW-KY                         
092800                             AND :IDOUTREC-HIGH-KY                        
092900        AND   FLRULEMISS BETWEEN :FLRULEMISS-LOW-KY                       
093000                             AND :FLRULEMISS-HIGH-KY                      
093100        AND   IDOUTTYPE  BETWEEN :IDOUTTYPE-LOW-KY                        
093200                             AND :IDOUTTYPE-HIGH-KY                       
093300        AND   KDOUTMETH  BETWEEN :KDOUTMETH-LOW-KY                        
093400                             AND :KDOUTMETH-HIGH-KY                       
093500                                                                          
093600       ORDER BY IDOUTDEST                                                 
093700              , TIREGDAT DESC                                             
093800              , TIKLOCK DESC                                              
093900              , IDOUTREC                                                  
094000     END-EXEC                                                             
094100                                                                          
094200     MOVE 000100  TO GOOD-SQLCODECODES                                    
094300                                                                          
094400     EXEC SQL                                                             
094500       OPEN TZ4REKY-CRS                                                   
094600     END-EXEC                                                             
094700                                                                          
094800     MOVE SQLCODE TO SQLCODE-WS                                           
094900     PERFORM DB2-STATUS-CHECK                                             
095000     .                                                                    
095100                                                                          
095200 DB2-FETCH-TZ4REKY-CRS SECTION.                                           
095300                                                                          
095400     MOVE 000100  TO GOOD-SQLCODECODES                                    
095500                                                                          
095600     EXEC SQL                                                             
095700       FETCH TZ4REKY-CRS                                                  
095800                                                                          
095900       INTO :REKY-IDOUTDEST                                               
096000          , :REKY-TIREGDAT                                                
096100          , :REKY-TIKLOCK                                                 
096200          , :REKY-IDLOPNR                                                 
096300          , :REKY-IDOUTREC                                                
096400          , :REKY-KDOUTMETH                                               
096500          , :REKY-IDOUTTYPE                                               
096600          , :REKY-IDLIST                                                  
096700          , :REKY-KVANTEX-PRINTAD                                         
096800          , :REKY-TIAAMMDD-RENS                                           
096900     END-EXEC                                                             
097000                                                                          
097100     MOVE SQLCODE TO SQLCODE-WS                                           
097200     PERFORM DB2-STATUS-CHECK                                             
097300     .                                                                    
097400                                                                          
097500 DB2-CLOSE-TZ4REKY-CRS SECTION.                                           
097600                                                                          
097700     EXEC SQL                                                             
097800        CLOSE TZ4REKY-CRS                                                 
097900     END-EXEC                                                             
098000     .                                                                    
098100                                                                          
098200 DB2-COUNT-CRS2 SECTION.                                                  
098300                                                                          
098400     EXEC SQL                                                             
098500       SELECT COUNT(*)                                                    
098600                                                                          
098700       INTO  :WS-COUNTER-TZ4REKY                                          
098800                                                                          
098900       FROM   TZ4REKY                                                     
099000                                                                          
099100       WHERE  IDOUTDEST  BETWEEN :IDOUTDEST-LOW-KY                        
099200                             AND :IDOUTDEST-HIGH-KY                       
099300        AND   TIMESTAMP_FORMAT (SUBSTR(DIGITS(TIREGDAT),2,6) ||           
099400                                ' ' ||                                    
099500                                SUBSTR(DIGITS(TIKLOCK),2,8),              
099600                                'YYMMDD HH24MISSFF2')                     
099700                              >= CURRENT TIMESTAMP                        
099800                                 - :WS-TIKLOCK-N MINUTES                  
099900        AND   IDLIST     BETWEEN :IDLIST-LOW-KY                           
100000                             AND :IDLIST-HIGH-KY                          
100100        AND   IDOUTREC   BETWEEN :IDOUTREC-LOW-KY                         
100200                             AND :IDOUTREC-HIGH-KY                        
100300        AND   FLRULEMISS BETWEEN :FLRULEMISS-LOW-KY                       
100400                             AND :FLRULEMISS-HIGH-KY                      
100500        AND   IDOUTTYPE  BETWEEN :IDOUTTYPE-LOW-KY                        
100600                             AND :IDOUTTYPE-HIGH-KY                       
100700        AND   KDOUTMETH  BETWEEN :KDOUTMETH-LOW-KY                        
100800                             AND :KDOUTMETH-HIGH-KY                       
100900     END-EXEC                                                             
101000                                                                          
101100     MOVE 000100  TO GOOD-SQLCODECODES                                    
101200                                                                          
101300     MOVE SQLCODE TO SQLCODE-WS                                           
101400     PERFORM DB2-STATUS-CHECK                                             
101500     .                                                                    
101600                                                                          
101700 DB2-DCL-OPN-TZ4REKY-CRS2 SECTION.                                        
101800                                                                          
101900     MOVE 000100 TO GOOD-SQLCODECODES                                     
102000                                                                          
102100     EXEC SQL                                                             
102200       DECLARE TZ4REKY-CRS2 CURSOR WITH HOLD FOR                          
102300                                                                          
102400       SELECT  IDOUTDEST                                                  
102500             , TIREGDAT                                                   
102600             , TIKLOCK                                                    
102700             , IDLOPNR                                                    
102800             , IDOUTREC                                                   
102900             , KDOUTMETH                                                  
103000             , IDOUTTYPE                                                  
103100             , IDLIST                                                     
103200             , KVANTEX_PRINTAD                                            
103300             , TIAAMMDD_RENS                                              
103400                                                                          
103500       FROM    TZ4REKY                                                    
103600                                                                          
103700       WHERE  IDOUTDEST  BETWEEN :IDOUTDEST-LOW-KY                        
103800                             AND :IDOUTDEST-HIGH-KY                       
103900        AND   TIMESTAMP_FORMAT (SUBSTR(DIGITS(TIREGDAT),2,6) ||           
104000                                ' ' ||                                    
104100                                SUBSTR(DIGITS(TIKLOCK),2,8),              
104200                                'YYMMDD HH24MISSFF2')                     
104300                              >= CURRENT TIMESTAMP                        
104400                                 - :WS-TIKLOCK-N MINUTES                  
104500        AND   IDLIST     BETWEEN :IDLIST-LOW-KY                           
104600                             AND :IDLIST-HIGH-KY                          
104700        AND   IDOUTREC   BETWEEN :IDOUTREC-LOW-KY                         
104800                             AND :IDOUTREC-HIGH-KY                        
104900        AND   FLRULEMISS BETWEEN :FLRULEMISS-LOW-KY                       
105000                             AND :FLRULEMISS-HIGH-KY                      
105100        AND   IDOUTTYPE  BETWEEN :IDOUTTYPE-LOW-KY                        
105200                             AND :IDOUTTYPE-HIGH-KY                       
105300        AND   KDOUTMETH  BETWEEN :KDOUTMETH-LOW-KY                        
105400                             AND :KDOUTMETH-HIGH-KY                       
105500                                                                          
105600       ORDER BY IDOUTDEST                                                 
105700              , TIREGDAT DESC                                             
105800              , TIKLOCK DESC                                              
105900              , IDOUTREC                                                  
106000     END-EXEC                                                             
106100                                                                          
106200     MOVE 000100  TO GOOD-SQLCODECODES                                    
106300                                                                          
106400     EXEC SQL                                                             
106500       OPEN TZ4REKY-CRS2                                                  
106600     END-EXEC                                                             
106700                                                                          
106800     MOVE SQLCODE TO SQLCODE-WS                                           
106900     PERFORM DB2-STATUS-CHECK                                             
107000     .                                                                    
107100                                                                          
107200 DB2-FETCH-TZ4REKY-CRS2 SECTION.                                          
107300                                                                          
107400     MOVE 000100  TO GOOD-SQLCODECODES                                    
107500                                                                          
107600     EXEC SQL                                                             
107700       FETCH TZ4REKY-CRS2                                                 
107800                                                                          
107900       INTO :REKY-IDOUTDEST                                               
108000          , :REKY-TIREGDAT                                                
108100          , :REKY-TIKLOCK                                                 
108200          , :REKY-IDLOPNR                                                 
108300          , :REKY-IDOUTREC                                                
108400          , :REKY-KDOUTMETH                                               
108500          , :REKY-IDOUTTYPE                                               
108600          , :REKY-IDLIST                                                  
108700          , :REKY-KVANTEX-PRINTAD                                         
108800          , :REKY-TIAAMMDD-RENS                                           
108900     END-EXEC                                                             
109000                                                                          
109100     MOVE SQLCODE TO SQLCODE-WS                                           
109200     PERFORM DB2-STATUS-CHECK                                             
109300     .                                                                    
109400                                                                          
109500 DB2-CLOSE-TZ4REKY-CRS2 SECTION.                                          
109600                                                                          
109700     EXEC SQL                                                             
109800        CLOSE TZ4REKY-CRS2                                                
109900     END-EXEC                                                             
110000     .                                                                    
110100                                                                          
110200 DB2-DCL-OPN-TZ4REKY-CRS-GET SECTION.                                     
110300                                                                          
110400     MOVE 000100 TO GOOD-SQLCODECODES                                     
110500                                                                          
110600     EXEC SQL                                                             
110700       DECLARE TZ4REKY-CRS-GET CURSOR WITH HOLD FOR                       
110800                                                                          
110900       SELECT  IDOUTDEST                                                  
111000             , TIREGDAT                                                   
111100             , TIKLOCK                                                    
111200             , IDLOPNR                                                    
111300             , IDOUTREC                                                   
111400             , KDOUTMETH                                                  
111500             , IDOUTTYPE                                                  
111600             , IDLIST                                                     
111700             , KVANTEX_PRINTAD                                            
111800             , TIAAMMDD_RENS                                              
111900                                                                          
112000       FROM    TZ4REKY                                                    
112100                                                                          
112200       WHERE  IDLIST     = :IDLIST-GET                                    
112300                                                                          
112400     END-EXEC                                                             
112500                                                                          
112600     MOVE 000100  TO GOOD-SQLCODECODES                                    
112700                                                                          
112800     EXEC SQL                                                             
112900       OPEN TZ4REKY-CRS-GET                                               
113000     END-EXEC                                                             
113100                                                                          
113200     MOVE SQLCODE TO SQLCODE-WS                                           
113300     PERFORM DB2-STATUS-CHECK                                             
113400     .                                                                    
113500                                                                          
113600 DB2-FETCH-TZ4REKY-CRS-GET SECTION.                                       
113700                                                                          
113800     MOVE 000100  TO GOOD-SQLCODECODES                                    
113900                                                                          
114000     EXEC SQL                                                             
114100       FETCH TZ4REKY-CRS-GET                                              
114200                                                                          
114300       INTO :REKY-IDOUTDEST                                               
114400          , :REKY-TIREGDAT                                                
114500          , :REKY-TIKLOCK                                                 
114600          , :REKY-IDLOPNR                                                 
114700          , :REKY-IDOUTREC                                                
114800          , :REKY-KDOUTMETH                                               
114900          , :REKY-IDOUTTYPE                                               
115000          , :REKY-IDLIST                                                  
115100          , :REKY-KVANTEX-PRINTAD                                         
115200          , :REKY-TIAAMMDD-RENS                                           
115300     END-EXEC                                                             
115400                                                                          
115500     MOVE SQLCODE TO SQLCODE-WS                                           
115600     PERFORM DB2-STATUS-CHECK                                             
115700     .                                                                    
115800                                                                          
115900 DB2-CLOSE-TZ4REKY-CRS-GET SECTION.                                       
116000                                                                          
116100     EXEC SQL                                                             
116200        CLOSE TZ4REKY-CRS-GET                                             
116300     END-EXEC                                                             
116400     .                                                                    
116500                                                                          
116600 DB2-STATUS-CHECK  SECTION.                                               
116700                                                                          
116800     SET SQLCODE-IX TO 1                                                  
116900     SEARCH GOOD-SQLCODE                                                  
117000       AT END                                                             
117100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
117200          DELIMITED BY SIZE INTO ERROR-TEXT                               
117300          CALL ABEND USING RKOD-ABEND-DB2                                 
117400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
117500          CONTINUE                                                        
117600     END-SEARCH                                                           
117700     .                                                                    
