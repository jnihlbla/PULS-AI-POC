000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ041400.                                                
000400 AUTHOR.         KJELL ANDRÉ.                                             
000500 DATE-WRITTEN.   04/01/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.DAP.DISTRGETRESTARTDATA                                 
001000*    FUNCTION:                                                            
001100*        RETRIEVE DATA FROM RESTART DATABASE FOR ONLINE VIEWING           
001200*        (THE DATA MAY BE FORMATTED AS AN JSP/HTML REPORT)                
001300*                                                                         
001400*        THE PROGRAM UPDATE TABLE TZ4REKY                                 
001500*        THE PROGRAM READS  TABLE TZ4REDA                                 
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WZ0414U                                             
001900*        REQUEST:     WZ0414I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WZ0414O1                                            
002300                                                                          
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'WZ041400'.            
002900 77  IDSYSTEM                    PIC X(4)    VALUE 'WZ04'.                
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003200 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
003300 77  KDRC-DISPLAY                PIC Z(5).                                
003400                                                                          
003500*    --- CONSTANT WORK FIELDS                                             
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900*77  WS-GET-IT                   PIC X(4)    VALUE 'GETI'.                
004000 77  WS-ADRESS                   PIC X(50)                                
004100                    VALUE 'CARPARTS.DAP.DISTRGETRESTARTDATA'.             
004200                                                                          
004300 77  MAX-KVANTEX-PRINTAD         PIC S9(1)   VALUE +9  COMP-3.            
004400 77  MAX-KVRADER                 PIC S9(3)   VALUE +20 COMP-3.            
004500                                                                          
004600 77  KEYS-SW                     PIC X       VALUE SPACE.                 
004700     88  KEYS-OK                             VALUE 'Y'.                   
004800     88  KEYS-WRONG                          VALUE 'N'.                   
004900                                                                          
005000 77  ACTION-CODE-SW              PIC X   VALUE SPACE.                     
005100     88  ACT-CODE-VALID                  VALUE SPACE.                     
005200     88  ACT-CODE-RETRIEVE               VALUE SPACE.                     
005300                                                                          
005400                                                                          
005500*    --- WORK-FIELDS                                                      
005600 01  IX                          PIC 9       VALUE ZERO.                  
005700 01  TZ4DIRU-COUNTER             PIC S9(1)   VALUE ZERO COMP-3.           
005800 01  IDOUTREC-KY-X.                                                       
005900     03 IDOUTREC-LOW-KY          PIC X(30)  VALUE LOW-VALUE.              
006000     03 IDOUTREC-HIGH-KY         PIC X(30)  VALUE HIGH-VALUE.             
006100 01  WS-TIREGDAT-KEY             PIC S9(7)   VALUE ZERO COMP-3.           
006200 01  WS-TIKLOCK-KEY              PIC S9(9)   VALUE ZERO COMP-3.           
006300 01  WS-REKY-IDLOPNR-KEY         PIC S9(3)   VALUE ZERO COMP-3.           
006400 01  WS-REDA-IDLOPNR-KEY         PIC S9(3)   VALUE ZERO COMP-3.           
006500 01  WS-KVPOST-LAST-KEY          PIC S9(7)   VALUE ZERO COMP-3.           
006600 01  WS-ERROR-TYPE               PIC X(3)    VALUE SPACE.                 
006700 01  WS-KVRADER                  PIC S9(5)   VALUE ZERO COMP-3.           
006800                                                                          
006900 01  WS-IDCALL                   PIC S9(9)   COMP VALUE +0.               
007000 01  WS-KDFUNC                   PIC X(10)   VALUE SPACE.                 
007100 01  WS-KDRC                     PIC S9(9)   COMP VALUE +0.               
007200 01  WS-TEOUTDATA-L              PIC S9(9)   COMP VALUE +0.               
007300 01  WS-TEOUTDATA-D              PIC X(3000) VALUE SPACE.                 
007400                                                                          
007500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007600 01  GENERAL-SUBPROGRAMS.                                                 
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007900     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
008000     SKIP3                                                                
008100                                                                          
008200*    --- PARAMETERS TO ABEND                                              
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
008700                                                                          
008800 01  MESSAGE-CODES.                                                       
008900     03  ERROR-CODES.                                                     
009000         05  ERR-AUTHORIZATION-MISSING PIC X(3)    VALUE '00A'.           
009100         05  ERR-INVALID-KEY           PIC X(3)    VALUE '022'.           
009200         05  ERR-SYSTEM-ERROR          PIC X(3)    VALUE '099'.           
009300     03  INFO-CODES.                                                      
009400         05  INF-MORE-LINES-EXIST      PIC X(3)    VALUE '011'.           
009500     EJECT                                                                
009600                                                                          
009700*     --- PARAMETRAR TILL SUBPROGRAM                                      
009800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009900     SKIP3                                                                
010000*01  -COPY WZ01SUB                                                        
010100     EJECT                                                                
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'SEC-AREA'.            
010400     SKIP3                                                                
010500*01 -COPY WSECAREA                                                        
010600     EJECT                                                                
010700                                                                          
010800*     --- PARAMETRAR TILL OLIKA AREOR                                     
010900                                                                          
011000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011100     SKIP3                                                                
011200 01  REQU-AREA.                                                           
011300*    03  -COPY WZ01REQU                                                   
011400*    03  -COPY WZ0414I1                                                   
011500     EJECT                                                                
011600                                                                          
011700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011800     SKIP3                                                                
011900 01  RESP-AREA.                                                           
012000*    03  -COPY WZ01RESP                                                   
012100*    03  -COPY WZ0414O1                                                   
012200     EJECT                                                                
012300                                                                          
012400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
012500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012600                                                                          
012700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
012800 01  DB2-WS.                                                              
012900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
013000         88  LINES-FOUND                     VALUE 000.                   
013100         88  LINES-MISSING                   VALUE 100.                   
013200         88  RESOURCE-WRONG                  VALUE 904.                   
013300     03  GOOD-SQLCODECODES.                                               
013400         05  GOOD-SQLCODE OCCURS 5                                        
013500             INDEXED BY SQLCODE-IX PIC 9(3).                              
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'TZ4REKY-AREA'.        
013800*01  -COPY TZ4REKY -PRE REKY-                                             
013900                                                                          
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'TZ4REDA-AREA'.        
014200*01  -COPY TZ4REDA -PRE REDA-                                             
014300                                                                          
014400     EXEC SQL INCLUDE TZ4REKY END-EXEC.                                   
014500     EXEC SQL INCLUDE TZ4REDA END-EXEC.                                   
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800     EJECT                                                                
014900 PROCEDURE DIVISION.                                                      
015000 MAIN SECTION.                                                            
015100                                                                          
015200     PERFORM S80-FETCH-REQUEST-ARGUMENT                                   
015300     IF SUB-KDRC = 0                                                      
015400       PERFORM A-INIT                                                     
015500       PERFORM B-CHECK-KEYS-AND-SECURITY                                  
015600       IF RESP-IDMSG-ERROR = SPACE                                        
015700         PERFORM F-READ-SHOW-INFO                                         
015800       END-IF                                                             
015900       PERFORM S80-RETURN-RESPONSE                                        
016000     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     INITIALIZE GOOD-SQLCODECODES                                         
016900     MOVE ALL '+' TO RESP-AREA                                            
017000     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
017100                     RESP-IDELMT-ERROR                                    
017200                     RESP-IDMSG-INFO                                      
017300     MOVE ZERO    TO RESP-KVRADER                                         
017400     .                                                                    
017500     EJECT                                                                
017600*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
017700 B-CHECK-KEYS-AND-SECURITY SECTION.                                       
017800                                                                          
017900     MOVE YES TO KEYS-SW                                                  
018000                                                                          
018100     IF REQU-IDMSGVER NUMERIC AND REQU-IDMSGVER = 001                     
018200       CONTINUE                                                           
018300     ELSE                                                                 
018400       MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
018500       MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                         
018600     END-IF                                                               
018700                                                                          
018800     IF RESP-IDMSG-ERROR = SPACE                                          
018900       MOVE REQU-KDPGMACT TO ACTION-CODE-SW                               
019000       IF ACT-CODE-VALID                                                  
019100         CONTINUE                                                         
019200       ELSE                                                               
019300         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
019400         MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                       
019500       END-IF                                                             
019600     END-IF                                                               
019700                                                                          
019800     IF RESP-IDMSG-ERROR = SPACE                                          
019900       IF  REQU-IDUSER > SPACE  AND  NOT = ALL '+'                        
020000         CONTINUE                                                         
020100       ELSE                                                               
020200         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
020300         MOVE 'IDUSER' TO RESP-IDELMT-ERROR                               
020400       END-IF                                                             
020500     END-IF                                                               
020600                                                                          
020700     IF RESP-IDMSG-ERROR = SPACE                                          
020800       IF REQU-IDOUTTYPE-KEY > SPACE  AND  NOT = ALL '+'                  
020900         CONTINUE                                                         
021000       ELSE                                                               
021100         MOVE NOO             TO KEYS-SW                                  
021200         MOVE 'IDOUTTYPE'     TO RESP-IDELMT-ERROR                        
021300       END-IF                                                             
021400     END-IF                                                               
021500                                                                          
021600     IF RESP-IDMSG-ERROR = SPACE                                          
021700       IF REQU-IDOUTREC-KEY = ALL '+'                                     
021800         MOVE SPACE TO REQU-IDOUTREC-KEY                                  
021900       END-IF                                                             
022000       IF REQU-IDOUTREC-KEY >= SPACE                                      
022100         CONTINUE                                                         
022200       ELSE                                                               
022300         MOVE NOO             TO KEYS-SW                                  
022400         MOVE 'IDOUTREC'      TO RESP-IDELMT-ERROR                        
022500       END-IF                                                             
022600     END-IF                                                               
022700                                                                          
022800     IF RESP-IDMSG-ERROR = SPACE                                          
022900       IF REQU-IDLIST-KEY = ALL '+'                                       
023000         MOVE SPACE TO REQU-IDLIST-KEY                                    
023100       END-IF                                                             
023200       IF REQU-IDLIST-KEY   >= SPACE                                      
023300         CONTINUE                                                         
023400       ELSE                                                               
023500         MOVE NOO             TO KEYS-SW                                  
023600         MOVE 'IDLIST'        TO RESP-IDELMT-ERROR                        
023700       END-IF                                                             
023800     END-IF                                                               
023900                                                                          
024000     IF RESP-IDMSG-ERROR = SPACE                                          
024100       IF  REQU-TIREGDAT-KEY NUMERIC                                      
024200         CONTINUE                                                         
024300       ELSE                                                               
024400         MOVE NOO             TO KEYS-SW                                  
024500         MOVE 'TIREGDAT'      TO RESP-IDELMT-ERROR                        
024600       END-IF                                                             
024700     END-IF                                                               
024800                                                                          
024900     IF RESP-IDMSG-ERROR = SPACE                                          
025000       IF  REQU-TIKLOCK-KEY NUMERIC                                       
025100         CONTINUE                                                         
025200       ELSE                                                               
025300         MOVE NOO             TO KEYS-SW                                  
025400         MOVE 'TIKLOCK'       TO RESP-IDELMT-ERROR                        
025500       END-IF                                                             
025600     END-IF                                                               
025700                                                                          
025800     IF RESP-IDMSG-ERROR = SPACE                                          
025900       IF  REQU-IDLOPNR-KEY NUMERIC                                       
026000         CONTINUE                                                         
026100       ELSE                                                               
026200         MOVE NOO             TO KEYS-SW                                  
026300         MOVE 'IDLOPNR'       TO RESP-IDELMT-ERROR                        
026400       END-IF                                                             
026500     END-IF                                                               
026600                                                                          
026700     IF RESP-IDMSG-ERROR = SPACE                                          
026800       IF  REQU-KVPOST-LAST-KEY NUMERIC                                   
026900         CONTINUE                                                         
027000       ELSE                                                               
027100         MOVE NOO             TO KEYS-SW                                  
027200         MOVE 'KVPOST'        TO RESP-IDELMT-ERROR                        
027300       END-IF                                                             
027400     END-IF                                                               
027500                                                                          
027600     IF KEYS-WRONG                                                        
027700       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
027800     END-IF                                                               
027900                                                                          
028000     PERFORM S01-CHECK-SECURITY                                           
028100     .                                                                    
028200     EJECT                                                                
028300*** - MOVE SEARCHING KEYS TO RESPONSE                                     
028400 F-READ-SHOW-INFO SECTION.                                                
028500                                                                          
028600     MOVE REQU-IDOUTTYPE-KEY TO RESP-IDOUTTYPE-KEY                        
028700     MOVE REQU-IDOUTREC-KEY  TO RESP-IDOUTREC-KEY                         
028800                                                                          
028900*    MOVE REQU-IDOUTREC-KEY    TO IDOUTREC-LOW-KY                         
029000*                                 IDOUTREC-HIGH-KY                        
029100*    INSPECT IDOUTREC-LOW-KY CONVERTING SPACE TO LOW-VALUE                
029200*    INSPECT IDOUTREC-HIGH-KY CONVERTING SPACE TO HIGH-VALUE              
029300                                                                          
029400     MOVE REQU-IDLIST-KEY    TO RESP-IDLIST-KEY                           
029500     MOVE REQU-TIREGDAT-KEY  TO RESP-TIREGDAT-KEY                         
029600                                WS-TIREGDAT-KEY                           
029700     MOVE REQU-TIKLOCK-KEY   TO RESP-TIKLOCK-KEY                          
029800                                WS-TIKLOCK-KEY                            
029900     MOVE REQU-IDLOPNR-KEY   TO RESP-IDLOPNR-KEY                          
030000*    -- REQU-IDLOPNR ORIGINATES FROM REKY TABLE, BUT                      
030100*    -- REKY AND REDA USE DIFFERENT VALUES OF IDLOPNR.                    
030200*    -- REKY USE THE SPECIFIELD VALUE, WHICH MAY BE > 100                 
030300*    -- BUT REDA ALWAYS USE A VALUE < 100.                                
030400*    -- IF REKY-IDLOPNR IS 000, 100, 200 ETC, REDA-IDLOPNR = 000          
030500*    -- IF REKY-IDLOPNR IS 001, 101, 201 ETC, REDA-IDLOPNR = 001          
030600*    -- AND SO ON.                                                        
030700     MOVE REQU-IDLOPNR-KEY   TO WS-REKY-IDLOPNR-KEY                       
030800     IF WS-TIREGDAT-KEY < 151012                                          
030900       COMPUTE WS-REDA-IDLOPNR-KEY =                                      
031000             FUNCTION REM (WS-REKY-IDLOPNR-KEY, 100)                      
031100     ELSE                                                                 
031200       COMPUTE WS-REDA-IDLOPNR-KEY =                                      
031300             FUNCTION REM (WS-REKY-IDLOPNR-KEY, 50)                       
031400     END-IF                                                               
031500                                                                          
031600     MOVE REQU-KVPOST-LAST-KEY TO WS-KVPOST-LAST-KEY                      
031700                                                                          
031800     PERFORM FA-RETRIEVE-OUTPUT                                           
031900                                                                          
032000*    -- UPDATE NBR OF TIMES PRINTED ONLY WHEN PRINT WAS REQUESTED         
032100*    -- AND ONLY WHEN NO MORE DATA REMAINS                                
032200     IF RESP-IDMSG-ERROR = SPACE AND                                      
032300        RESP-IDMSG-INFO  = SPACE AND REQU-KDPRTVIEW = 'P'                 
032400       PERFORM FB-UPDATE-TZ4REKY                                          
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800*** - RESTART DISTRIBUTION                                                
032900 FA-RETRIEVE-OUTPUT      SECTION.                                         
033000                                                                          
033100     MOVE ZERO TO WS-KVRADER                                              
033200     PERFORM DB2-DCL-OPN-TZ4REDA-CRS                                      
033300                                                                          
033400     PERFORM DB2-FETCH-TZ4REDA-CRS                                        
033500     PERFORM UNTIL LINES-MISSING OR WS-KVRADER = MAX-KVRADER              
033600       ADD 1 TO WS-KVRADER                                                
033700       MOVE REDA-KVPOST       TO RESP-KVPOST-LAST-KEY                     
033800       MOVE REDA-TEOUTDATA-L  TO RESP-TEOUTDATA-L (WS-KVRADER)            
033900       MOVE REDA-TEOUTDATA-D (1:REDA-TEOUTDATA-L)                         
034000                              TO RESP-TEOUTDATA-D (WS-KVRADER)            
034100                                                                          
034200       PERFORM DB2-FETCH-TZ4REDA-CRS                                      
034300     END-PERFORM                                                          
034400     IF NOT LINES-MISSING                                                 
034500       MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                      
034600     ELSE                                                                 
034700       MOVE SPACE                 TO RESP-IDMSG-INFO                      
034800     END-IF                                                               
034900                                                                          
035000     PERFORM DB2-CLOSE-TZ4REDA-CRS                                        
035100     MOVE WS-KVRADER TO RESP-KVRADER                                      
035200     .                                                                    
035300     EJECT                                                                
035400 FB-UPDATE-TZ4REKY SECTION.                                               
035500                                                                          
035600     PERFORM DB2-SELECT-TZ4REKY-TAB                                       
035700                                                                          
035800     IF LINES-FOUND                                                       
035900       IF REKY-KVANTEX-PRINTAD < MAX-KVANTEX-PRINTAD                      
036000         ADD 1 TO REKY-KVANTEX-PRINTAD                                    
036100         PERFORM DB2-UPDATE-TZ4REKY-TAB                                   
036200       END-IF                                                             
036300     END-IF                                                               
036400     .                                                                    
036500                                                                          
036600     EJECT                                                                
036700*    --- DISPATCHER SECTIONS                                              
036800 S80-FETCH-REQUEST-ARGUMENT SECTION.                                      
036900                                                                          
037000     MOVE 'GETARG'                   TO SUB-KDFUNC                        
037100     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
037200     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
037300                                                                          
037400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
037500                                                                          
037600     IF SUB-KDRC > 0                                                      
037700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
037800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
037900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
038000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038100     END-IF                                                               
038200     .                                                                    
038300     SKIP3                                                                
038400 S80-RETURN-RESPONSE SECTION.                                             
038500                                                                          
038600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
038700     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
038800     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
038900                              - (MAX-KVRADER - WS-KVRADER)                
039000                              * LENGTH OF RESP-OUTDATA-GROUP              
039100                                                                          
039200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
039300                                                                          
039400     IF SUB-KDRC > 0                                                      
039500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
039600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
039700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 S01-CHECK-SECURITY SECTION.                                              
040300                                                                          
040400     MOVE REQU-IDUSER       TO SEC-IDUSER                                 
040500     MOVE IDSYSTEM          TO SEC-IDTRANS                                
040600     MOVE REQU-IDOUTREC-KEY TO SEC-IDKEY                                  
040700                                                                          
040800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
040900                         SEC-IDKEY  SEC-KDSVAR                            
041000                                                                          
041100     IF SEC-KDSVAR > SPACE                                                
041200       MOVE ERR-AUTHORIZATION-MISSING TO RESP-IDMSG-ERROR                 
041300     END-IF                                                               
041400     .                                                                    
041500                                                                          
041600     EJECT                                                                
041700*    --- DB2 SECTIONS                                                     
041800                                                                          
041900 DB2-DCL-OPN-TZ4REDA-CRS SECTION.                                         
042000                                                                          
042100                                                                          
042200     MOVE 000100 TO GOOD-SQLCODECODES                                     
042300                                                                          
042400     EXEC SQL                                                             
042500       DECLARE TZ4REDA-CRS CURSOR WITH HOLD FOR                           
042600                                                                          
042700       SELECT KVPOST                                                      
042800            , TEOUTDATA                                                   
042900                                                                          
043000       FROM    TZ4REDA                                                    
043100                                                                          
043200       WHERE  IDOUTTYPE = :REQU-IDOUTTYPE-KEY                             
043300        AND   IDOUTREC  = :REQU-IDOUTREC-KEY                              
043400        AND   IDLIST    = :REQU-IDLIST-KEY                                
043500        AND   TIREGDAT  = :WS-TIREGDAT-KEY                                
043600        AND   TIKLOCK   = :WS-TIKLOCK-KEY                                 
043700        AND   IDLOPNR   = :WS-REDA-IDLOPNR-KEY                            
043800        AND   KVPOST    > :WS-KVPOST-LAST-KEY                             
043900                                                                          
044000       ORDER BY IDOUTTYPE                                                 
044100              , IDOUTREC                                                  
044200              , IDLIST                                                    
044300              , TIREGDAT                                                  
044400              , TIKLOCK                                                   
044500              , IDLOPNR                                                   
044600              , KVPOST                                                    
044700     END-EXEC                                                             
044800*       AND   IDOUTREC   BETWEEN :IDOUTREC-LOW-KY                         
044900*                            AND :IDOUTREC-HIGH-KY                        
045000                                                                          
045100     MOVE 000100  TO GOOD-SQLCODECODES                                    
045200                                                                          
045300     EXEC SQL                                                             
045400       OPEN TZ4REDA-CRS                                                   
045500     END-EXEC                                                             
045600                                                                          
045700     MOVE SQLCODE TO SQLCODE-WS                                           
045800     PERFORM DB2-STATUS-CHECK                                             
045900     .                                                                    
046000                                                                          
046100     SKIP3                                                                
046200 DB2-FETCH-TZ4REDA-CRS SECTION.                                           
046300                                                                          
046400     MOVE 000100  TO GOOD-SQLCODECODES                                    
046500                                                                          
046600     EXEC SQL                                                             
046700                                                                          
046800       FETCH TZ4REDA-CRS                                                  
046900                                                                          
047000       INTO :REDA-KVPOST                                                  
047100          , :REDA-TEOUTDATA                                               
047200                                                                          
047300     END-EXEC                                                             
047400                                                                          
047500     MOVE SQLCODE TO SQLCODE-WS                                           
047600     PERFORM DB2-STATUS-CHECK                                             
047700     .                                                                    
047800                                                                          
047900     SKIP3                                                                
048000 DB2-CLOSE-TZ4REDA-CRS SECTION.                                           
048100                                                                          
048200     EXEC SQL                                                             
048300        CLOSE TZ4REDA-CRS                                                 
048400     END-EXEC                                                             
048500     .                                                                    
048600                                                                          
048700     EJECT                                                                
048800 DB2-SELECT-TZ4REKY-TAB SECTION.                                          
048900                                                                          
049000     MOVE 000100  TO GOOD-SQLCODECODES                                    
049100     EXEC SQL                                                             
049200          SELECT KVANTEX_PRINTAD                                          
049300                                                                          
049400          INTO  :REKY-KVANTEX-PRINTAD                                     
049500                                                                          
049600          FROM   TZ4REKY                                                  
049700                                                                          
049800          WHERE  IDOUTTYPE = :REQU-IDOUTTYPE-KEY                          
049900           AND   IDOUTREC  = :REQU-IDOUTREC-KEY                           
050000           AND   IDLIST    = :REQU-IDLIST-KEY                             
050100           AND   TIREGDAT  = :WS-TIREGDAT-KEY                             
050200           AND   TIKLOCK   = :WS-TIKLOCK-KEY                              
050300           AND   IDLOPNR   = :WS-REKY-IDLOPNR-KEY                         
050400     END-EXEC                                                             
050500                                                                          
050600     MOVE SQLCODE TO SQLCODE-WS                                           
050700     PERFORM DB2-STATUS-CHECK                                             
050800     .                                                                    
050900                                                                          
051000     SKIP3                                                                
051100 DB2-UPDATE-TZ4REKY-TAB  SECTION.                                         
051200                                                                          
051300     MOVE 000     TO GOOD-SQLCODECODES                                    
051400     EXEC SQL                                                             
051500         UPDATE TZ4REKY                                                   
051600           SET   KVANTEX_PRINTAD = :REKY-KVANTEX-PRINTAD                  
051700                                                                          
051800         WHERE   IDOUTTYPE = :REQU-IDOUTTYPE-KEY                          
051900          AND    IDOUTREC  = :REQU-IDOUTREC-KEY                           
052000          AND    IDLIST    = :REQU-IDLIST-KEY                             
052100          AND    TIREGDAT  = :WS-TIREGDAT-KEY                             
052200          AND    TIKLOCK   = :WS-TIKLOCK-KEY                              
052300          AND    IDLOPNR   = :WS-REKY-IDLOPNR-KEY                         
052400     END-EXEC                                                             
052500                                                                          
052600     MOVE SQLCODE TO SQLCODE-WS                                           
052700     PERFORM DB2-STATUS-CHECK                                             
052800     .                                                                    
052900                                                                          
053000     EJECT                                                                
053100 DB2-STATUS-CHECK  SECTION.                                               
053200                                                                          
053300     SET SQLCODE-IX TO 1                                                  
053400     SEARCH GOOD-SQLCODE                                                  
053500       AT END                                                             
053600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
053700          DELIMITED BY SIZE INTO ERROR-TEXT                               
053800          CALL ABEND USING RKOD-ABEND-DB2                                 
053900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
054000     END-SEARCH                                                           
054100     .                                                                    
