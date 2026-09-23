000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF027800.                                                
000400 AUTHOR.         ANDERS HENRIKSSON.                                       
000500 DATE-WRITTEN.   02/04/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ERRBUNDDETAIL                                    
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01TRAW                                
001600*        THE PROGRAM UPDATES TABLE T01TBUN                                
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: WF0278T                                             
002000*        REQUEST:     WZ01REQU                                            
002100*                     WF0278I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    WZ01RESP                                            
002500*                     WF0278O1                                            
002600*                                                                         
002700*        TRANSAKTION: WFT202X  (VALID WHEN RESTART WF0202)                
002800*        REQUEST:     HEADER ONLY                                         
002810*                                                                         
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)  VALUE 'WF027800'.             
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900*    --- CONSTANTS                                                        
004000 77  YES                         PIC X      VALUE 'J'.                    
004100 77  NOO                         PIC X      VALUE 'N'.                    
004200                                                                          
004300 77  ERROR-SW                    PIC X      VALUE SPACE.                  
004400     88  ERROR-REQUEST                      VALUE 'T'.                    
004500     88  ERROR-NEXT                         VALUE 'N'.                    
004600     88  ERROR-BEFORE                       VALUE 'P'.                    
004700                                                                          
004800 77  KDPGMACT-SW                 PIC X      VALUE SPACE.                  
004900     88  KDPGMACT-VALID                     VALUE 'S', 'U'.               
005000     88  KDPGMACT-SEARCH                    VALUE 'S'.                    
005100     88  KDPGMACT-UPDATE                    VALUE 'U'.                    
005200                                                                          
005300 77  WS-ADRESS                   PIC X(50)                                
005400                    VALUE 'CARPARTS.BILLIT.ERRBUNDDETAIL'.                
005500                                                                          
005600 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005700     88  KEYS-OK                            VALUE 'J'.                    
005800     88  KEYS-WRONG                         VALUE 'N'.                    
005900                                                                          
006000*    --- WORK FIELDS                                                      
006100 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
006200 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
006300 01  WS-COUNTER-FELIBUNT         PIC S9(7)  VALUE ZERO    COMP-3.         
006400 01  WS-BELEGRAD-1               PIC X(35)  VALUE SPACE.                  
006500 01  WS-IDLEGSEL                 PIC X(4)   VALUE SPACE.                  
006600 01  WS-TIREGTID-KEY             PIC S9(10)  VALUE ZERO COMP-3.           
006700 01  WS-IDREFRAD-KEY             PIC S9(5)  VALUE ZERO COMP-3.            
006800 01  WS-CURRENT-DATE             PIC X(8)   VALUE SPACE.                  
006900 01  WS-DATUM                    PIC X(8)   VALUE SPACE.                  
007000                                                                          
007100*    --- MAPPING FIELDS                                                   
007200 01  MAP-IDLEGSEL                PIC X(4)   VALUE SPACE.                  
007300 01  MAP-IDBUNDLE                PIC X(15)  VALUE SPACE.                  
007400 01  MAP-DAREGDAT                PIC X(8)   VALUE SPACE.                  
007500 01  MAP-TIREGTID                PIC S9(10)  VALUE ZERO COMP-3.           
007600 01  MAP-IDREF                   PIC X(15)  VALUE SPACE.                  
007700 01  MAP-DAREFDAT                PIC X(8)   VALUE SPACE.                  
007800 01  MAP-IDREFRAD                PIC S9(5)  VALUE ZERO COMP-3.            
007900 01  MAP-BEVOLREF                PIC X(10)  VALUE SPACE.                  
008000 01  MAP-IDLANDX3-SEND           PIC X(3)   VALUE SPACE.                  
008100 01  MAP-IDLANDX3-REC            PIC X(3)   VALUE SPACE.                  
008200 01  MAP-IDPARTNR                PIC X(9)   VALUE SPACE.                  
008300 01  MAP-IDEXCUST-1              PIC X(15)  VALUE SPACE.                  
008400 01  MAP-IDEXCUST-2              PIC X(15)  VALUE SPACE.                  
008500 01  MAP-IDEXCUST-3              PIC X(15)  VALUE SPACE.                  
008600 01  MAP-IDOPTION-1              PIC X(15)  VALUE SPACE.                  
008700 01  MAP-IDOPTION-2              PIC X(15)  VALUE SPACE.                  
008800 01  MAP-IDOPTION-3              PIC X(15)  VALUE SPACE.                  
008900 01  MAP-IDOPTION-4              PIC X(15)  VALUE SPACE.                  
009000 01  MAP-IDOPTION-5              PIC X(15)  VALUE SPACE.                  
009100 01  MAP-IDARTNR-FINANCE         PIC X(50)  VALUE SPACE.                  
009200 01  MAP-IDSTATNR                PIC S9(9)  VALUE ZERO COMP-3.            
009300 01  MAP-VKARTNTO                PIC S9(4)V9(3) VALUE ZERO COMP-3.        
009400 01  MAP-PRARTBTO                PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009500 01  MAP-PRARTNTO                PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009600 01  MAP-REARTRAB                PIC S9(2)V9(2) VALUE ZERO COMP-3.        
009700 01  MAP-KVBEART                 PIC S9(7)  VALUE ZERO COMP-3.            
009800 01  MAP-KVLEVART                PIC S9(7)  VALUE ZERO COMP-3.            
009900 01  MAP-BEART                   PIC X(25)  VALUE SPACE.                  
010000 01  MAP-FLSOFT                  PIC X(1)   VALUE SPACE.                  
010100 01  MAP-FLSPECPR                PIC X(1)   VALUE SPACE.                  
010200 01  MAP-FLFREE                  PIC X(1)   VALUE SPACE.                  
010300 01  MAP-KDVAT                   PIC X(2)   VALUE SPACE.                  
010400 01  MAP-KDVALISO                PIC X(3)   VALUE SPACE.                  
010500 01  MAP-KDINVFRQ                PIC X(4)   VALUE SPACE.                  
010600 01  MAP-KDFINDOC                PIC X(4)   VALUE SPACE.                  
010700 01  MAP-IDBREAK-1               PIC X(8)   VALUE SPACE.                  
010800 01  MAP-IDBREAK-2               PIC X(8)   VALUE SPACE.                  
010900 01  MAP-IDSEQ-1                 PIC X(8)   VALUE SPACE.                  
011000 01  MAP-IDSEQ-2                 PIC X(8)   VALUE SPACE.                  
011100 01  MAP-IDSEQ-3                 PIC X(8)   VALUE SPACE.                  
011200 01  MAP-KDARTURS                PIC X(2)   VALUE SPACE.                  
011300 01  MAP-KDANMORS                PIC X(2)   VALUE SPACE.                  
011400 01  MAP-IDFAKREF                PIC S9(9)  VALUE ZERO COMP-3.            
011500 01  MAP-DAFAKREF                PIC X(8)   VALUE SPACE.                  
011600 01  MAP-IDDC                    PIC X(2)   VALUE SPACE.                  
011700 01  MAP-IDACCNT-1               PIC X(15)  VALUE SPACE.                  
011800 01  MAP-IDACCNT-2               PIC X(15)  VALUE SPACE.                  
011900 01  MAP-IDACCNT-3               PIC X(15)  VALUE SPACE.                  
012000 01  MAP-IDACCNT-4               PIC X(15)  VALUE SPACE.                  
012100 01  MAP-VKORDBTO-KOLLI          PIC S9(6)V9 VALUE ZERO COMP-3.           
012200 01  MAP-IDLEVNR                 PIC X(5)   VALUE SPACE.                  
012300 01  MAP-KDFRAKT                 PIC S9(2)  VALUE ZERO COMP-3.            
012400 01  MAP-BELEVVIL                PIC X(35)  VALUE SPACE.                  
012500 01  MAP-IDSYSTEM-SEND           PIC X(4)   VALUE SPACE.                  
012600 01  MAP-IDSYSTEM-REC            PIC X(4)   VALUE SPACE.                  
012700 01  MAP-FILLER                  PIC X(100) VALUE SPACE.                  
012800 01  MAP-IDFELKOD                PIC X(3)   VALUE SPACE.                  
012900 01  MAP-BEFEL                   PIC X(50)  VALUE SPACE.                  
013000 01  MAP-IDAPPEND                PIC X(8)   VALUE SPACE.                  
013100 01  MAP-BEANST                  PIC X(25)  VALUE SPACE.                  
013200 01  MAP-IDUSER                  PIC X(8)   VALUE SPACE.                  
013300                                                                          
013400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013500 01  GENERAL-SUBPROGRAMS.                                                 
013600     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
013700     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
013800     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
013900     03  WORKDAY                 PIC X(8)   VALUE 'WORKDAY '.             
014000                                                                          
014100*    --- PARAMETERS TO ABEND                                              
014200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
014600                                                                          
014700 01  MESSAGE-CODES.                                                       
014800     03  ERROR-CODES.                                                     
014900         05  ERR-FIRST-ROW-SHOWN     PIC X(3)   VALUE '010'.              
015000         05  ERR-LAST-ROW-SHOWN      PIC X(3)   VALUE '012'.              
015100         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
015200         05  ERR-INVALID-FIELDS      PIC X(3)   VALUE '023'.              
015300         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
015400         05  ERR-MUST-BE-ENTERED     PIC X(3)   VALUE '026'.              
015500         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
015600         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
015700     03  INFO-CODES.                                                      
015800         05  INF-NO-ERR-FOUND        PIC X(3)   VALUE '108'.              
015900         05  INF-MORE-LINES-EXIST    PIC X(3)   VALUE '104'.              
016000         05  INF-RESTART-DONE        PIC X(3)   VALUE '105'.              
016100     EJECT                                                                
016200                                                                          
016300*01  -COPY WZ01SUB                                                        
016400     EJECT                                                                
016500                                                                          
016600*01  -COPY WORKAREA                                                       
016700     EJECT                                                                
016800*                                                                         
016900 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
017000 01  REQU-AREA.                                                           
017100*    03 -COPY WZ01REQU                                                    
017200*    03 -COPY WF0278I1                                                    
017300     EJECT                                                                
017400                                                                          
017500 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
017600 01  RESP-AREA.                                                           
017700*    03 -COPY WZ01RESP                                                    
017800*    03 -COPY WF0278O1                                                    
017900     EJECT                                                                
018000                                                                          
018100 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
018200 01  -COPY WZ01SEND                                                       
018300     EJECT                                                                
018400                                                                          
018500 01  SEND-AREA.                                                           
018600*    03  -COPY WZ01REQU -PRE SEND-                                        
018700                                                                          
018800 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
018900     SKIP3                                                                
019000*    -COPY WZ20DATE                                                       
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
019400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
019500                                                                          
019600 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
019700 01  DB2-WS.                                                              
019800     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
019900         88  CURSOR-OK                      VALUE 000.                    
020000         88  LINES-FOUND                    VALUE 000.                    
020100         88  LINES-MISSING                  VALUE 100.                    
020200         88  RESOURCE-WRONG                 VALUE 904.                    
020300                                                                          
020400     03  GOOD-SQLCODECODES.                                               
020500         05  GOOD-SQLCODE OCCURS 5                                        
020600             INDEXED BY SQLCODE-IX PIC 9(3).                              
020700                                                                          
020800 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
020900*01  -COPY T01LSEL -PRE T01LSEL-                                          
021000     EJECT                                                                
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'T01TRAW-AREA'.        
021300*01  -COPY T01TRAW -PRE T01TRAW-                                          
021400     EJECT                                                                
021500                                                                          
021600 01  FILLER                      PIC X(16)   VALUE 'T01TBUN-AREA'.        
021700*01  -COPY T01TBUN -PRE T01TBUN-                                          
021800     EJECT                                                                
021900                                                                          
022000     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
022100     EJECT                                                                
022200     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
022300     EJECT                                                                
022400     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
022500     EJECT                                                                
022600                                                                          
022700 LINKAGE SECTION.                                                         
022800 PROCEDURE DIVISION.                                                      
022900 MAIN SECTION.                                                            
023000                                                                          
023100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
023200     IF SUB-KDRC = ZERO                                                   
023300       PERFORM A-INIT                                                     
023400       PERFORM B-CHECK-KEYS                                               
023500       IF KEYS-OK                                                         
023600         PERFORM BB-CHECK-KEY-RELATION                                    
023700       END-IF                                                             
023800       IF KEYS-OK                                                         
023900         PERFORM F-READ-SHOW-INFO                                         
024000       END-IF                                                             
024100       PERFORM S02-RETURN-RESPONSE                                        
024200     END-IF                                                               
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700                                                                          
024800 A-INIT SECTION.                                                          
024900     INITIALIZE GOOD-SQLCODECODES                                         
025000     MOVE ALL '+' TO RESP-AREA                                            
025100     MOVE SPACE TO RESP-IDMSG-ERROR                                       
025200     MOVE SPACE TO RESP-IDMSG-INFO                                        
025300     MOVE SPACE TO RESP-IDELMT-ERROR                                      
025400     MOVE ZERO                  TO WS-COUNTER-FELIBUNT                    
025500                                                                          
025600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
025700     .                                                                    
025800     EJECT                                                                
025900                                                                          
026000*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
026100 B-CHECK-KEYS SECTION.                                                    
026200     MOVE YES TO KEYS-SW                                                  
026300     MOVE REQU-KDPGMACT TO KDPGMACT-SW                                    
026400     IF  KDPGMACT-VALID                                                   
026500     AND REQU-IDMSGVER NUMERIC                                            
026600     AND REQU-IDLEGSEL-KEY > SPACE                                        
026700     AND REQU-IDBUNDLE-KEY > SPACE                                        
026800     AND REQU-DAREGDAT-KEY > SPACE                                        
026900     AND REQU-TIREGTID-KEY NUMERIC                                        
027000     AND REQU-IDREF-KEY > SPACE                                           
027100     AND REQU-DAREFDAT-KEY > SPACE                                        
027200     AND REQU-IDREFRAD-KEY NUMERIC                                        
027300       CONTINUE                                                           
027400     ELSE                                                                 
027500       MOVE NOO TO KEYS-SW                                                
027600     END-IF                                                               
027700                                                                          
027800     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
027900       MOVE NOO TO KEYS-SW                                                
028000     END-IF                                                               
028100                                                                          
028200     IF KEYS-WRONG                                                        
028300       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
028400       IF REQU-KDPGMACT = 'S' OR 'U'                                      
028500         CONTINUE                                                         
028600       ELSE                                                               
028700         MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                      
028800         MOVE 'KDPGMACT' TO RESP-IDELMT-ERROR                             
028900       END-IF                                                             
029000       IF REQU-IDMSGVER NUMERIC                                           
029100         CONTINUE                                                         
029200       ELSE                                                               
029300         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
029400         MOVE 'IDMSGVER' TO RESP-IDELMT-ERROR                             
029500       END-IF                                                             
029600       IF REQU-IDUSER = SPACE OR = ALL '+'                                
029700         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
029800         MOVE 'IDUSER' TO RESP-IDELMT-ERROR                               
029900       ELSE                                                               
030000         CONTINUE                                                         
030100       END-IF                                                             
030200     END-IF                                                               
030300                                                                          
030400     IF KEYS-WRONG                                                        
030500       CONTINUE                                                           
030600     ELSE                                                                 
030700       PERFORM DB2-SELECT-T01LSEL-TAB                                     
030800       IF LINES-FOUND                                                     
030900         CONTINUE                                                         
031000       ELSE                                                               
031100         MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                      
031200         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
031300         MOVE NOO TO KEYS-SW                                              
031400       END-IF                                                             
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800                                                                          
031900*** - CHECK RELATION BETWEEN REQUSTED KEYS                                
032000 BB-CHECK-KEY-RELATION SECTION.                                           
032100     IF REQU-DAREGDAT-KEY = SPACE OR = ALL '+'                            
032200       CONTINUE                                                           
032300     ELSE                                                                 
032400       IF REQU-DAREGDAT-KEY NUMERIC                                       
032500         IF REQU-DAREGDAT-KEY < WS-CURRENT-DATE                           
032600         OR = WS-CURRENT-DATE                                             
032700         AND REQU-DAREGDAT-KEY NUMERIC                                    
032800           MOVE REQU-DAREGDAT-KEY  TO DATE-TIDATE                         
032900           MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                              
033000           CALL WZ20DATE USING DATE-WZ20DATE                              
033100           IF DATE-KDRC > ZERO                                            
033200             MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                  
033300             MOVE 'DAREGDAT'       TO RESP-IDELMT-ERROR                   
033400           ELSE                                                           
033500             MOVE REQU-DAREGDAT-KEY TO WS-DATUM                           
033600           END-IF                                                         
033700         ELSE                                                             
033800           MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                    
033900           MOVE 'DAREGDAT'        TO RESP-IDELMT-ERROR                    
034000         END-IF                                                           
034100       ELSE                                                               
034200         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
034300         MOVE 'DAREGDAT'          TO RESP-IDELMT-ERROR                    
034400       END-IF                                                             
034500     END-IF                                                               
034600                                                                          
034700     IF REQU-TIREGTID-KEY = ZERO OR = ALL '+' OR = SPACE                  
034800       MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
034900       MOVE 'TIREGTID' TO RESP-IDELMT-ERROR                               
035000     ELSE                                                                 
035100       IF REQU-TIREGTID-KEY NOT NUMERIC                                   
035200         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
035300         MOVE 'TIREGTID' TO RESP-IDELMT-ERROR                             
035400       ELSE                                                               
035500         MOVE REQU-TIREGTID-KEY TO WS-TIREGTID-KEY                        
035600       END-IF                                                             
035700     END-IF                                                               
035800                                                                          
035900     IF REQU-PAGE-REQUEST = 'T' OR 'N' OR 'P'                             
036000       CONTINUE                                                           
036100     ELSE                                                                 
036200       MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                        
036300       MOVE 'PAGE-REQUEST' TO RESP-IDELMT-ERROR                           
036400     END-IF                                                               
036500     IF REQU-PAGE-REQUEST = 'T'                                           
036600       MOVE 'T' TO ERROR-SW                                               
036700     END-IF                                                               
036800     IF REQU-PAGE-REQUEST = 'N'                                           
036900       MOVE 'N' TO ERROR-SW                                               
037000     END-IF                                                               
037100     IF REQU-PAGE-REQUEST = 'P'                                           
037200       MOVE 'P' TO ERROR-SW                                               
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
037800 F-READ-SHOW-INFO SECTION.                                                
037900     MOVE REQU-IDLEGSEL-KEY      TO RESP-IDLEGSEL-KEY                     
038000     MOVE REQU-IDBUNDLE-KEY      TO RESP-IDBUNDLE-KEY                     
038100     MOVE REQU-DAREGDAT-KEY      TO RESP-DAREGDAT-KEY                     
038200     MOVE REQU-TIREGTID-KEY      TO RESP-TIREGTID-KEY                     
038300     MOVE REQU-IDREF-KEY         TO RESP-IDREF-KEY                        
038400     MOVE REQU-DAREFDAT-KEY      TO RESP-DAREFDAT-KEY                     
038500     MOVE REQU-IDREFRAD-KEY      TO RESP-IDREFRAD-KEY                     
038600     MOVE REQU-IDMSGVER          TO RESP-IDMSGVER                         
038700     MOVE WS-BELEGRAD-1          TO RESP-BELEGRAD-1                       
038800                                                                          
038900     IF KDPGMACT-SEARCH                                                   
039000       PERFORM FA-READ-BASICDATA                                          
039100     END-IF                                                               
039200     IF KDPGMACT-UPDATE                                                   
039300       PERFORM FB-UPDATE-T01TBUN                                          
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700                                                                          
039800*** - CHECK WHICH REQUESTED KEY                                           
039900 FA-READ-BASICDATA SECTION.                                               
040000     IF ERROR-REQUEST                                                     
040100       PERFORM FAA-REQUEST-SOKNING                                        
040200     END-IF                                                               
040300                                                                          
040400     IF ERROR-NEXT                                                        
040500       PERFORM FAB-NEXT-SOKNING                                           
040600     END-IF                                                               
040700                                                                          
040800     IF ERROR-BEFORE                                                      
040900       PERFORM FAC-BEFORE-SOKNING                                         
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300                                                                          
041400*** - HANDLE SOKNING OF ERROR IN REQUEST                                  
041500 FAA-REQUEST-SOKNING SECTION.                                             
041600     PERFORM DB2-COUNT-FELIBUNT                                           
041700     IF WS-COUNTER-FELIBUNT = ZERO                                        
041800       MOVE INF-NO-ERR-FOUND TO RESP-IDMSG-INFO                           
041900     END-IF                                                               
042000                                                                          
042100     IF RESP-IDMSG-ERROR = SPACE                                          
042200       PERFORM DB2-DCL-OPN-T01TRAW-CRS-1                                  
042300       PERFORM DB2-FETCH-T01TRAW-CRS-1                                    
042400       MOVE 1 TO WS-IX                                                    
042500       PERFORM UNTIL MAP-IDREF       = REQU-IDREF-KEY                     
042600       AND           MAP-DAREFDAT    = REQU-DAREFDAT-KEY                  
042700       AND           MAP-IDREFRAD    = REQU-IDREFRAD-KEY                  
042800         PERFORM DB2-FETCH-T01TRAW-CRS-1                                  
042900         ADD 1 TO WS-IX                                                   
043000       END-PERFORM                                                        
043100       PERFORM S03-MOVE-TO-RESPOND                                        
043200       PERFORM DB2-CLOSE-T01TRAW-CRS-1                                    
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600                                                                          
043700*** - HANDLE SOKNING AFTER NEXT REQUESTED ERROR                           
043800 FAB-NEXT-SOKNING SECTION.                                                
043900     PERFORM DB2-COUNT-FELIBUNT                                           
044000     IF WS-COUNTER-FELIBUNT = ZERO                                        
044100       MOVE INF-NO-ERR-FOUND TO RESP-IDMSG-INFO                           
044200     END-IF                                                               
044300                                                                          
044400     IF RESP-IDMSG-ERROR = SPACE                                          
044500       PERFORM DB2-DCL-OPN-T01TRAW-CRS-2                                  
044600       PERFORM DB2-FETCH-T01TRAW-CRS-2                                    
044700       MOVE 1 TO WS-IX                                                    
044800       PERFORM UNTIL MAP-IDREF       = REQU-IDREF-KEY                     
044900       AND           MAP-DAREFDAT    = REQU-DAREFDAT-KEY                  
045000       AND           MAP-IDREFRAD    = REQU-IDREFRAD-KEY                  
045100         PERFORM DB2-FETCH-T01TRAW-CRS-2                                  
045200         ADD 1 TO WS-IX                                                   
045300       END-PERFORM                                                        
045400       IF WS-COUNTER-FELIBUNT > WS-IX                                     
045500         PERFORM DB2-FETCH-T01TRAW-CRS-2                                  
045600         ADD 1 TO WS-IX                                                   
045700         PERFORM S03-MOVE-TO-RESPOND                                      
045800       ELSE                                                               
045900         PERFORM S03-MOVE-TO-RESPOND                                      
046000         MOVE ERR-LAST-ROW-SHOWN TO RESP-IDMSG-ERROR                      
046100       END-IF                                                             
046200       PERFORM DB2-CLOSE-T01TRAW-CRS-2                                    
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
046700*** - HANDLE SOKNING AFTER PREVIUS REQUESTED ERROR                        
046800 FAC-BEFORE-SOKNING SECTION.                                              
046900     PERFORM DB2-COUNT-FELIBUNT                                           
047000     IF WS-COUNTER-FELIBUNT = ZERO                                        
047100       MOVE INF-NO-ERR-FOUND TO RESP-IDMSG-INFO                           
047200     END-IF                                                               
047300                                                                          
047400     IF RESP-IDMSG-ERROR = SPACE                                          
047500       PERFORM DB2-DCL-OPN-T01TRAW-CRS-3                                  
047600       PERFORM DB2-FETCH-T01TRAW-CRS-3                                    
047700       MOVE 1 TO WS-IX                                                    
047800       PERFORM UNTIL MAP-IDREF        = REQU-IDREF-KEY                    
047900       AND           MAP-DAREFDAT     = REQU-DAREFDAT-KEY                 
048000       AND           MAP-IDREFRAD     = REQU-IDREFRAD-KEY                 
048100         PERFORM DB2-FETCH-T01TRAW-CRS-3                                  
048200         ADD 1 TO WS-IX                                                   
048300       END-PERFORM                                                        
048400       IF WS-COUNTER-FELIBUNT > WS-IX                                     
048500         PERFORM DB2-FETCH-T01TRAW-CRS-3                                  
048600         ADD 1 TO WS-IX                                                   
048700         COMPUTE WS-IX = WS-COUNTER-FELIBUNT - WS-IX                      
048800         PERFORM S03-MOVE-TO-RESPOND                                      
048900       ELSE                                                               
049000         MOVE 1 TO WS-IX                                                  
049100         PERFORM S03-MOVE-TO-RESPOND                                      
049200         MOVE ERR-FIRST-ROW-SHOWN TO RESP-IDMSG-ERROR                     
049300       END-IF                                                             
049400       PERFORM DB2-CLOSE-T01TRAW-CRS-3                                    
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800                                                                          
049900 FB-UPDATE-T01TBUN SECTION.                                               
050000*    IF REQU-FLRESTBUN  = 'Y'                                             
050100       PERFORM DB2-UPDATE-T01TBUN                                         
050200       MOVE INF-RESTART-DONE TO RESP-IDMSG-INFO                           
050300       PERFORM S04-MOVE-TO-RESPOND                                        
050400* WHEN STARTING WF020200 A RISK FOR DEADLOCK                              
050500*      PERFORM G-START-PGM-WF020200                                       
050600*    ELSE                                                                 
050700*      PERFORM FAA-REQUEST-SOKNING                                        
050800*      MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                        
050900*      MOVE 'FLRESTBUN' TO RESP-IDELMT-ERROR                              
051000*    END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300                                                                          
051400*G-START-PGM-WF020200 SECTION.                                            
051500*    PERFORM S05-SEND-TO-WF0202-OPEN                                      
051600*    PERFORM S06-SEND-TO-WF0202-PUT                                       
051700*    PERFORM S07-SEND-TO-WF0202-CLOSE                                     
051800*    .                                                                    
051900*    EJECT                                                                
052000*                                                                         
052100*   --- DISPATCHER SECTION START                                          
052200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
052300     MOVE 'GETARG'             TO SUB-KDFUNC                              
052400     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
052500     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
052600                                                                          
052700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
052800                                                                          
052900     IF SUB-KDRC > 0                                                      
053000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
053100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
053200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
053300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 S02-RETURN-RESPONSE SECTION.                                             
053900     MOVE 'RETURN'             TO SUB-KDFUNC                              
054000                                                                          
054100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
054200                                                                          
054300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
054400                                                                          
054500     IF SUB-KDRC > 0                                                      
054600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
054700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
054800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
055400*   --- MOVE TO OUTPUT SECTION START                                      
055500*** - MOVE DATA TO RESPOND WHEN CURRENT LINE,                             
055600***   WHEN COMING LINE MODIFY CURRENT LINE.                               
055700 S03-MOVE-TO-RESPOND SECTION.                                             
055800     MOVE MAP-IDREF              TO RESP-IDREF-KEY                        
055900     MOVE MAP-DAREFDAT           TO RESP-DAREFDAT-KEY                     
056000     MOVE MAP-IDREFRAD           TO RESP-IDREFRAD-KEY                     
056100     MOVE MAP-BEVOLREF           TO RESP-BEVOLREF                         
056200     MOVE MAP-IDLANDX3-SEND      TO RESP-IDLANDX3-SEND                    
056300     MOVE MAP-IDLANDX3-REC       TO RESP-IDLANDX3-REC                     
056400     MOVE MAP-IDPARTNR           TO RESP-IDPARTNR                         
056500     MOVE MAP-IDEXCUST-1         TO RESP-IDEXCUST(1)                      
056600     MOVE MAP-IDEXCUST-2         TO RESP-IDEXCUST(2)                      
056700     MOVE MAP-IDEXCUST-3         TO RESP-IDEXCUST(3)                      
056800     MOVE MAP-IDOPTION-1         TO RESP-IDOPTION(1)                      
056900     MOVE MAP-IDOPTION-2         TO RESP-IDOPTION(2)                      
057000     MOVE MAP-IDOPTION-3         TO RESP-IDOPTION(3)                      
057100     MOVE MAP-IDOPTION-4         TO RESP-IDOPTION(4)                      
057200     MOVE MAP-IDOPTION-5         TO RESP-IDOPTION(5)                      
057300     MOVE MAP-IDARTNR-FINANCE    TO RESP-IDARTNR-FINANCE                  
057400     MOVE MAP-IDSTATNR           TO RESP-IDSTATNR                         
057500     MOVE MAP-VKARTNTO           TO RESP-VKARTNTO                         
057600     MOVE MAP-PRARTBTO           TO RESP-PRARTBTO                         
057700     MOVE MAP-PRARTNTO           TO RESP-PRARTNTO                         
057800     MOVE MAP-REARTRAB           TO RESP-REARTRAB                         
057900     MOVE MAP-KVBEART            TO RESP-KVBEART                          
058000     MOVE MAP-KVLEVART           TO RESP-KVLEVART                         
058100     MOVE MAP-BEART              TO RESP-BEART                            
058200     IF MAP-FLSOFT = 'J'                                                  
058300       MOVE 'Y' TO MAP-FLSOFT                                             
058400     END-IF                                                               
058500     IF MAP-FLSPECPR = 'J'                                                
058600       MOVE 'Y' TO MAP-FLSPECPR                                           
058700     END-IF                                                               
058800     IF MAP-FLFREE = 'J'                                                  
058900       MOVE 'Y' TO MAP-FLFREE                                             
059000     END-IF                                                               
059100     MOVE MAP-FLSOFT             TO RESP-FLSOFT                           
059200     MOVE MAP-FLSPECPR           TO RESP-FLSPECPR                         
059300     MOVE MAP-FLFREE             TO RESP-FLFREE                           
059400     MOVE MAP-KDVAT              TO RESP-KDVAT                            
059500     MOVE MAP-KDVALISO           TO RESP-KDVALISO                         
059600     MOVE MAP-KDINVFRQ           TO RESP-KDINVFRQ                         
059700     MOVE MAP-KDFINDOC           TO RESP-KDFINDOC                         
059800     MOVE MAP-IDBREAK-1          TO RESP-IDBREAK(1)                       
059900     MOVE MAP-IDBREAK-2          TO RESP-IDBREAK(2)                       
060000     MOVE MAP-IDSEQ-1            TO RESP-IDSEQ(1)                         
060100     MOVE MAP-IDSEQ-2            TO RESP-IDSEQ(2)                         
060200     MOVE MAP-IDSEQ-3            TO RESP-IDSEQ(3)                         
060300     MOVE MAP-KDARTURS           TO RESP-KDARTURS                         
060400     MOVE MAP-KDANMORS           TO RESP-KDANMORS                         
060500     MOVE MAP-IDFAKREF           TO RESP-IDFAKREF                         
060600     MOVE MAP-DAFAKREF           TO RESP-DAFAKREF                         
060700     MOVE MAP-IDDC               TO RESP-IDDC                             
060800     MOVE MAP-IDACCNT-1          TO RESP-IDACCNT(1)                       
060900     MOVE MAP-IDACCNT-2          TO RESP-IDACCNT(2)                       
061000     MOVE MAP-IDACCNT-3          TO RESP-IDACCNT(3)                       
061100     MOVE MAP-IDACCNT-4          TO RESP-IDACCNT(4)                       
061200     MOVE MAP-VKORDBTO-KOLLI     TO RESP-VKORDBTO-KOLLI                   
061300     MOVE MAP-IDLEVNR            TO RESP-IDLEVNR                          
061400     MOVE MAP-KDFRAKT            TO RESP-KDFRAKT                          
061500     MOVE MAP-BELEVVIL           TO RESP-BELEVVIL                         
061600     MOVE MAP-IDSYSTEM-SEND      TO RESP-IDSYSTEM-SEND                    
061700     MOVE MAP-IDSYSTEM-REC       TO RESP-IDSYSTEM-REC                     
061800     MOVE MAP-IDFELKOD           TO RESP-IDFELKOD                         
061900     MOVE MAP-FILLER             TO RESP-BETEXT                           
062000     MOVE MAP-IDFELKOD           TO RESP-IDFELKOD                         
062100     MOVE MAP-BEFEL              TO RESP-BEFEL                            
062200     MOVE MAP-IDAPPEND           TO RESP-IDAPPEND                         
062300     MOVE MAP-BEANST             TO RESP-BEANST                           
062400     MOVE MAP-IDUSER             TO RESP-IDUSER                           
062500     MOVE WS-COUNTER-FELIBUNT    TO RESP-KVFELBUN                         
062600     MOVE WS-IX                  TO RESP-IDFELBUN                         
062700     MOVE 'N'                    TO RESP-FLRESTBUN                        
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 S04-MOVE-TO-RESPOND SECTION.                                             
063200     MOVE SPACE                  TO RESP-BEVOLREF                         
063300     MOVE SPACE                  TO RESP-IDLANDX3-SEND                    
063400     MOVE SPACE                  TO RESP-IDLANDX3-REC                     
063500     MOVE ZERO                   TO RESP-IDPARTNR                         
063600     MOVE SPACE                  TO RESP-IDEXCUST(1)                      
063700     MOVE SPACE                  TO RESP-IDEXCUST(2)                      
063800     MOVE SPACE                  TO RESP-IDEXCUST(3)                      
063900     MOVE SPACE                  TO RESP-IDOPTION(1)                      
064000     MOVE SPACE                  TO RESP-IDOPTION(2)                      
064100     MOVE SPACE                  TO RESP-IDOPTION(3)                      
064200     MOVE SPACE                  TO RESP-IDOPTION(4)                      
064300     MOVE SPACE                  TO RESP-IDOPTION(5)                      
064400     MOVE SPACE                  TO RESP-IDARTNR-FINANCE                  
064500     MOVE ZERO                   TO RESP-IDSTATNR                         
064600     MOVE ZERO                   TO RESP-VKARTNTO                         
064700     MOVE ZERO                   TO RESP-PRARTBTO                         
064800     MOVE ZERO                   TO RESP-PRARTNTO                         
064900     MOVE ZERO                   TO RESP-REARTRAB                         
065000     MOVE ZERO                   TO RESP-KVBEART                          
065100     MOVE ZERO                   TO RESP-KVLEVART                         
065200     MOVE SPACE                  TO RESP-BEART                            
065300     MOVE SPACE                  TO RESP-FLSOFT                           
065400     MOVE SPACE                  TO RESP-FLSPECPR                         
065500     MOVE SPACE                  TO RESP-FLFREE                           
065600     MOVE SPACE                  TO RESP-KDVAT                            
065700     MOVE SPACE                  TO RESP-KDVALISO                         
065800     MOVE SPACE                  TO RESP-KDINVFRQ                         
065900     MOVE SPACE                  TO RESP-KDFINDOC                         
066000     MOVE SPACE                  TO RESP-IDBREAK(1)                       
066100     MOVE SPACE                  TO RESP-IDBREAK(2)                       
066200     MOVE SPACE                  TO RESP-IDSEQ(1)                         
066300     MOVE SPACE                  TO RESP-IDSEQ(2)                         
066400     MOVE SPACE                  TO RESP-IDSEQ(3)                         
066500     MOVE SPACE                  TO RESP-KDARTURS                         
066600     MOVE SPACE                  TO RESP-KDANMORS                         
066700     MOVE ZERO                   TO RESP-IDFAKREF                         
066800     MOVE ZERO                   TO RESP-DAFAKREF                         
066900     MOVE SPACE                  TO RESP-IDDC                             
067000     MOVE SPACE                  TO RESP-IDACCNT(1)                       
067100     MOVE SPACE                  TO RESP-IDACCNT(2)                       
067200     MOVE SPACE                  TO RESP-IDACCNT(3)                       
067300     MOVE SPACE                  TO RESP-IDACCNT(4)                       
067400     MOVE ZERO                   TO RESP-VKORDBTO-KOLLI                   
067500     MOVE SPACE                  TO RESP-IDLEVNR                          
067600     MOVE ZERO                   TO RESP-KDFRAKT                          
067700     MOVE SPACE                  TO RESP-BELEVVIL                         
067800     MOVE SPACE                  TO RESP-IDSYSTEM-SEND                    
067900     MOVE SPACE                  TO RESP-IDSYSTEM-REC                     
068000     MOVE SPACE                  TO RESP-IDFELKOD                         
068100     MOVE SPACE                  TO RESP-BETEXT                           
068200     MOVE SPACE                  TO RESP-IDFELKOD                         
068300     MOVE SPACE                  TO RESP-BEFEL                            
068400     MOVE SPACE                  TO RESP-IDAPPEND                         
068500     MOVE SPACE                  TO RESP-BEANST                           
068600     MOVE SPACE                  TO RESP-IDUSER                           
068700     MOVE ZERO                   TO RESP-IDFELBUN                         
068800     MOVE ZERO                   TO RESP-KVFELBUN                         
068900     MOVE REQU-FLRESTBUN         TO RESP-FLRESTBUN                        
069000     .                                                                    
069100     EJECT                                                                
069200                                                                          
069300*S05-SEND-TO-WF0202-OPEN SECTION.                                         
069400*    MOVE 'CARPARTS.BILLIT.VALIDATE' TO SEND-ADDISPABS                    
069500*    MOVE 'OPEN'                     TO SEND-KDFUNC                       
069600*    CALL WZ01SEND USING SEND-CONTROL-AREA                                
069700*                        SEND-OPEN-AREA                                   
069800*    IF SEND-KDRC > ZERO                                                  
069900*      MOVE SEND-KDRC TO KDRC-DISPLAY                                     
070000*      STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
070100*      DELIMITED BY SIZE INTO ERROR-TEXT                                  
070200*      CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
070300*    END-IF                                                               
070400*    .                                                                    
070500*    EJECT                                                                
070600*                                                                         
070700*S06-SEND-TO-WF0202-PUT SECTION.                                          
070800*    MOVE 'PUT'                           TO SEND-KDFUNC                  
070900*    MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
071000*    CALL WZ01SEND USING SEND-CONTROL-AREA                                
071100*                        SEND-KVDLEN                                      
071200*                        SEND-AREA                                        
071300*    IF SEND-KDRC > ZERO                                                  
071400*      MOVE SEND-KDRC TO KDRC-DISPLAY                                     
071500*      STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
071600*      DELIMITED BY SIZE INTO ERROR-TEXT                                  
071700*      CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
071800*    END-IF                                                               
071900*    .                                                                    
072000*    EJECT                                                                
072100*                                                                         
072200*S07-SEND-TO-WF0202-CLOSE SECTION.                                        
072300*    MOVE 'CLOSE'                    TO SEND-KDFUNC                       
072400*    CALL WZ01SEND USING SEND-CONTROL-AREA                                
072500*                                                                         
072600*    IF SEND-KDRC > 0                                                     
072700*      MOVE SEND-KDRC TO KDRC-DISPLAY                                     
072800*      STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
072900*      DELIMITED BY SIZE INTO ERROR-TEXT                                  
073000*      CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
073100*    END-IF                                                               
073200*    .                                                                    
073300*    EJECT                                                                
073400*                                                                         
073500*   --- DB2 SECTIONS                                                      
073600*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
073700 DB2-SELECT-T01LSEL-TAB SECTION.                                          
073800     MOVE 000100 TO GOOD-SQLCODECODES                                     
073900                                                                          
074000     EXEC SQL                                                             
074100           SELECT  IDLEGSEL                                               
074200                 , BELEGRAD_1                                             
074300                                                                          
074400           INTO   :WS-IDLEGSEL                                            
074500                , :WS-BELEGRAD-1                                          
074600                                                                          
074700           FROM    T01LSEL                                                
074800                                                                          
074900           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
075000               AND KDSTATUS = 001                                         
075100     END-EXEC                                                             
075200                                                                          
075300     MOVE SQLCODE TO SQLCODE-WS                                           
075400     PERFORM DB2-STATUS-CHECK                                             
075500     .                                                                    
075600     EJECT                                                                
075700                                                                          
075800 DB2-DCL-OPN-T01TRAW-CRS-1 SECTION.                                       
075900     MOVE 000100 TO GOOD-SQLCODECODES                                     
076000                                                                          
076100     EXEC SQL                                                             
076200         DECLARE T01TRAW-CRS-1 CURSOR WITH HOLD FOR                       
076300                                                                          
076400           SELECT  A.IDLEGSEL                                             
076500                 , A.IDBUNDLE                                             
076600                 , A.DAREGDAT                                             
076700                 , A.TIREGTID                                             
076800                 , A.IDREF                                                
076900                 , A.DAREFDAT                                             
077000                 , A.IDREFRAD                                             
077100                 , A.BEVOLREF                                             
077200                 , A.IDLANDX3_SEND                                        
077300                 , A.IDLANDX3_REC                                         
077400                 , A.IDPARTNR                                             
077500                 , A.IDEXCUST_1                                           
077600                 , A.IDEXCUST_2                                           
077700                 , A.IDEXCUST_3                                           
077800                 , A.IDOPTION_1                                           
077900                 , A.IDOPTION_2                                           
078000                 , A.IDOPTION_3                                           
078100                 , A.IDOPTION_4                                           
078200                 , A.IDOPTION_5                                           
078300                 , A.IDARTNR_FINANCE                                      
078400                 , A.IDSTATNR                                             
078500                 , A.VKARTNTO                                             
078600                 , A.PRARTBTO                                             
078700                 , A.PRARTNTO                                             
078800                 , A.REARTRAB                                             
078900                 , A.KVBEART                                              
079000                 , A.KVLEVART                                             
079100                 , A.BEART                                                
079200                 , A.FLSOFT                                               
079300                 , A.FLSPECPR                                             
079400                 , A.FLFREE                                               
079500                 , A.KDVAT                                                
079600                 , A.KDVALISO                                             
079700                 , A.KDINVFRQ                                             
079800                 , A.KDFINDOC                                             
079900                 , A.IDBREAK_1                                            
080000                 , A.IDBREAK_2                                            
080100                 , A.IDSEQ_1                                              
080200                 , A.IDSEQ_2                                              
080300                 , A.IDSEQ_3                                              
080400                 , A.KDARTURS                                             
080500                 , A.KDANMORS                                             
080600                 , A.IDFAKREF                                             
080700                 , A.DAFAKREF                                             
080800                 , A.IDDC                                                 
080900                 , A.IDACCNT_1                                            
081000                 , A.IDACCNT_2                                            
081100                 , A.IDACCNT_3                                            
081200                 , A.IDACCNT_4                                            
081300                 , A.VKORDBTO_KOLLI                                       
081400                 , A.IDLEVNR                                              
081500                 , A.KDFRAKT                                              
081600                 , A.BELEVVIL                                             
081700                 , A.IDSYSTEM_SEND                                        
081800                 , A.IDSYSTEM_REC                                         
081900                 , A.FILLER                                               
082000                 , A.IDFELKOD                                             
082100                 , A.BEFEL                                                
082200                 , A.IDAPPEND                                             
082300                 , A.BEANST                                               
082400                 , A.IDUSER                                               
082500                                                                          
082600           FROM    T01TRAW A                                              
082700           ,       T01TBUN B                                              
082800                                                                          
082900           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
083000           AND    A.IDLEGSEL = B.IDLEGSEL                                 
083100           AND    A.IDBUNDLE = B.IDBUNDLE                                 
083200           AND    A.DAREGDAT = B.DAREGDAT                                 
083300           AND    A.TIREGTID = B.TIREGTID                                 
083400           AND    A.IDBUNDLE = :REQU-IDBUNDLE-KEY                         
083500           AND    A.DAREGDAT = :REQU-DAREGDAT-KEY                         
083600           AND    A.TIREGTID = :WS-TIREGTID-KEY                           
083700           AND    B.FLFEL    = 'J'                                        
083800                                                                          
083900           ORDER BY A.IDLEGSEL                                            
084000                  , A.DAREGDAT DESC                                       
084100                  , A.TIREGTID DESC                                       
084200                  , A.IDBUNDLE                                            
084300                  , A.IDREF                                               
084400                  , A.DAREFDAT                                            
084500                  , A.IDREFRAD                                            
084600     END-EXEC                                                             
084700                                                                          
084800     MOVE 000100  TO GOOD-SQLCODECODES                                    
084900                                                                          
085000     EXEC SQL                                                             
085100        OPEN T01TRAW-CRS-1                                                
085200     END-EXEC                                                             
085300                                                                          
085400     MOVE SQLCODE TO SQLCODE-WS                                           
085500     PERFORM DB2-STATUS-CHECK                                             
085600     .                                                                    
085700     EJECT                                                                
085800                                                                          
085900 DB2-FETCH-T01TRAW-CRS-1 SECTION.                                         
086000     MOVE 000100  TO GOOD-SQLCODECODES                                    
086100                                                                          
086200     EXEC SQL                                                             
086300                                                                          
086400         FETCH T01TRAW-CRS-1                                              
086500                                                                          
086600         INTO :MAP-IDLEGSEL                                               
086700            , :MAP-IDBUNDLE                                               
086800            , :MAP-DAREGDAT                                               
086900            , :MAP-TIREGTID                                               
087000            , :MAP-IDREF                                                  
087100            , :MAP-DAREFDAT                                               
087200            , :MAP-IDREFRAD                                               
087300            , :MAP-BEVOLREF                                               
087400            , :MAP-IDLANDX3-SEND                                          
087500            , :MAP-IDLANDX3-REC                                           
087600            , :MAP-IDPARTNR                                               
087700            , :MAP-IDEXCUST-1                                             
087800            , :MAP-IDEXCUST-2                                             
087900            , :MAP-IDEXCUST-3                                             
088000            , :MAP-IDOPTION-1                                             
088100            , :MAP-IDOPTION-2                                             
088200            , :MAP-IDOPTION-3                                             
088300            , :MAP-IDOPTION-4                                             
088400            , :MAP-IDOPTION-5                                             
088500            , :MAP-IDARTNR-FINANCE                                        
088600            , :MAP-IDSTATNR                                               
088700            , :MAP-VKARTNTO                                               
088800            , :MAP-PRARTBTO                                               
088900            , :MAP-PRARTNTO                                               
089000            , :MAP-REARTRAB                                               
089100            , :MAP-KVBEART                                                
089200            , :MAP-KVLEVART                                               
089300            , :MAP-BEART                                                  
089400            , :MAP-FLSOFT                                                 
089500            , :MAP-FLSPECPR                                               
089600            , :MAP-FLFREE                                                 
089700            , :MAP-KDVAT                                                  
089800            , :MAP-KDVALISO                                               
089900            , :MAP-KDINVFRQ                                               
090000            , :MAP-KDFINDOC                                               
090100            , :MAP-IDBREAK-1                                              
090200            , :MAP-IDBREAK-2                                              
090300            , :MAP-IDSEQ-1                                                
090400            , :MAP-IDSEQ-2                                                
090500            , :MAP-IDSEQ-3                                                
090600            , :MAP-KDARTURS                                               
090700            , :MAP-KDANMORS                                               
090800            , :MAP-IDFAKREF                                               
090900            , :MAP-DAFAKREF                                               
091000            , :MAP-IDDC                                                   
091100            , :MAP-IDACCNT-1                                              
091200            , :MAP-IDACCNT-2                                              
091300            , :MAP-IDACCNT-3                                              
091400            , :MAP-IDACCNT-4                                              
091500            , :MAP-VKORDBTO-KOLLI                                         
091600            , :MAP-IDLEVNR                                                
091700            , :MAP-KDFRAKT                                                
091800            , :MAP-BELEVVIL                                               
091900            , :MAP-IDSYSTEM-SEND                                          
092000            , :MAP-IDSYSTEM-REC                                           
092100            , :MAP-FILLER                                                 
092200            , :MAP-IDFELKOD                                               
092300            , :MAP-BEFEL                                                  
092400            , :MAP-IDAPPEND                                               
092500            , :MAP-BEANST                                                 
092600            , :MAP-IDUSER                                                 
092700     END-EXEC                                                             
092800                                                                          
092900     MOVE SQLCODE TO SQLCODE-WS                                           
093000     PERFORM DB2-STATUS-CHECK                                             
093100     .                                                                    
093200     EJECT                                                                
093300                                                                          
093400 DB2-CLOSE-T01TRAW-CRS-1 SECTION.                                         
093500     EXEC SQL                                                             
093600        CLOSE T01TRAW-CRS-1                                               
093700     END-EXEC                                                             
093800     .                                                                    
093900     EJECT                                                                
094000                                                                          
094100 DB2-DCL-OPN-T01TRAW-CRS-2 SECTION.                                       
094200     MOVE 000100 TO GOOD-SQLCODECODES                                     
094300                                                                          
094400     EXEC SQL                                                             
094500         DECLARE T01TRAW-CRS-2 CURSOR WITH HOLD FOR                       
094600                                                                          
094700           SELECT  A.IDLEGSEL                                             
094800                 , A.IDBUNDLE                                             
094900                 , A.DAREGDAT                                             
095000                 , A.TIREGTID                                             
095100                 , A.IDREF                                                
095200                 , A.DAREFDAT                                             
095300                 , A.IDREFRAD                                             
095400                 , A.BEVOLREF                                             
095500                 , A.IDLANDX3_SEND                                        
095600                 , A.IDLANDX3_REC                                         
095700                 , A.IDPARTNR                                             
095800                 , A.IDEXCUST_1                                           
095900                 , A.IDEXCUST_2                                           
096000                 , A.IDEXCUST_3                                           
096100                 , A.IDOPTION_1                                           
096200                 , A.IDOPTION_2                                           
096300                 , A.IDOPTION_3                                           
096400                 , A.IDOPTION_4                                           
096500                 , A.IDOPTION_5                                           
096600                 , A.IDARTNR_FINANCE                                      
096700                 , A.IDSTATNR                                             
096800                 , A.VKARTNTO                                             
096900                 , A.PRARTBTO                                             
097000                 , A.PRARTNTO                                             
097100                 , A.REARTRAB                                             
097200                 , A.KVBEART                                              
097300                 , A.KVLEVART                                             
097400                 , A.BEART                                                
097500                 , A.FLSOFT                                               
097600                 , A.FLSPECPR                                             
097700                 , A.FLFREE                                               
097800                 , A.KDVAT                                                
097900                 , A.KDVALISO                                             
098000                 , A.KDINVFRQ                                             
098100                 , A.KDFINDOC                                             
098200                 , A.IDBREAK_1                                            
098300                 , A.IDBREAK_2                                            
098400                 , A.IDSEQ_1                                              
098500                 , A.IDSEQ_2                                              
098600                 , A.IDSEQ_3                                              
098700                 , A.KDARTURS                                             
098800                 , A.KDANMORS                                             
098900                 , A.IDFAKREF                                             
099000                 , A.DAFAKREF                                             
099100                 , A.IDDC                                                 
099200                 , A.IDACCNT_1                                            
099300                 , A.IDACCNT_2                                            
099400                 , A.IDACCNT_3                                            
099500                 , A.IDACCNT_4                                            
099600                 , A.VKORDBTO_KOLLI                                       
099700                 , A.IDLEVNR                                              
099800                 , A.KDFRAKT                                              
099900                 , A.BELEVVIL                                             
100000                 , A.IDSYSTEM_SEND                                        
100100                 , A.IDSYSTEM_REC                                         
100200                 , A.FILLER                                               
100300                 , A.IDFELKOD                                             
100400                 , A.BEFEL                                                
100500                 , A.IDAPPEND                                             
100600                 , A.BEANST                                               
100700                 , A.IDUSER                                               
100800                                                                          
100900           FROM    T01TRAW A                                              
101000           ,       T01TBUN B                                              
101100                                                                          
101200           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
101300           AND    A.IDLEGSEL = B.IDLEGSEL                                 
101400           AND    A.IDBUNDLE = B.IDBUNDLE                                 
101500           AND    A.DAREGDAT = B.DAREGDAT                                 
101600           AND    A.TIREGTID = B.TIREGTID                                 
101700           AND    A.IDBUNDLE = :REQU-IDBUNDLE-KEY                         
101800           AND    A.DAREGDAT = :REQU-DAREGDAT-KEY                         
101900           AND    A.TIREGTID = :WS-TIREGTID-KEY                           
102000           AND    B.FLFEL    = 'J'                                        
102100                                                                          
102200           ORDER BY A.IDLEGSEL                                            
102300                  , A.DAREGDAT DESC                                       
102400                  , A.TIREGTID DESC                                       
102500                  , A.IDBUNDLE                                            
102600                  , A.IDREF                                               
102700                  , A.DAREFDAT                                            
102800                  , A.IDREFRAD                                            
102900     END-EXEC                                                             
103000                                                                          
103100     MOVE 000100  TO GOOD-SQLCODECODES                                    
103200                                                                          
103300     EXEC SQL                                                             
103400        OPEN T01TRAW-CRS-2                                                
103500     END-EXEC                                                             
103600                                                                          
103700     MOVE SQLCODE TO SQLCODE-WS                                           
103800     PERFORM DB2-STATUS-CHECK                                             
103900     .                                                                    
104000     EJECT                                                                
104100                                                                          
104200 DB2-FETCH-T01TRAW-CRS-2 SECTION.                                         
104300     MOVE 000100  TO GOOD-SQLCODECODES                                    
104400                                                                          
104500     EXEC SQL                                                             
104600                                                                          
104700         FETCH T01TRAW-CRS-2                                              
104800                                                                          
104900         INTO :MAP-IDLEGSEL                                               
105000            , :MAP-IDBUNDLE                                               
105100            , :MAP-DAREGDAT                                               
105200            , :MAP-TIREGTID                                               
105300            , :MAP-IDREF                                                  
105400            , :MAP-DAREFDAT                                               
105500            , :MAP-IDREFRAD                                               
105600            , :MAP-BEVOLREF                                               
105700            , :MAP-IDLANDX3-SEND                                          
105800            , :MAP-IDLANDX3-REC                                           
105900            , :MAP-IDPARTNR                                               
106000            , :MAP-IDEXCUST-1                                             
106100            , :MAP-IDEXCUST-2                                             
106200            , :MAP-IDEXCUST-3                                             
106300            , :MAP-IDOPTION-1                                             
106400            , :MAP-IDOPTION-2                                             
106500            , :MAP-IDOPTION-3                                             
106600            , :MAP-IDOPTION-4                                             
106700            , :MAP-IDOPTION-5                                             
106800            , :MAP-IDARTNR-FINANCE                                        
106900            , :MAP-IDSTATNR                                               
107000            , :MAP-VKARTNTO                                               
107100            , :MAP-PRARTBTO                                               
107200            , :MAP-PRARTNTO                                               
107300            , :MAP-REARTRAB                                               
107400            , :MAP-KVBEART                                                
107500            , :MAP-KVLEVART                                               
107600            , :MAP-BEART                                                  
107700            , :MAP-FLSOFT                                                 
107800            , :MAP-FLSPECPR                                               
107900            , :MAP-FLFREE                                                 
108000            , :MAP-KDVAT                                                  
108100            , :MAP-KDVALISO                                               
108200            , :MAP-KDINVFRQ                                               
108300            , :MAP-KDFINDOC                                               
108400            , :MAP-IDBREAK-1                                              
108500            , :MAP-IDBREAK-2                                              
108600            , :MAP-IDSEQ-1                                                
108700            , :MAP-IDSEQ-2                                                
108800            , :MAP-IDSEQ-3                                                
108900            , :MAP-KDARTURS                                               
109000            , :MAP-KDANMORS                                               
109100            , :MAP-IDFAKREF                                               
109200            , :MAP-DAFAKREF                                               
109300            , :MAP-IDDC                                                   
109400            , :MAP-IDACCNT-1                                              
109500            , :MAP-IDACCNT-2                                              
109600            , :MAP-IDACCNT-3                                              
109700            , :MAP-IDACCNT-4                                              
109800            , :MAP-VKORDBTO-KOLLI                                         
109900            , :MAP-IDLEVNR                                                
110000            , :MAP-KDFRAKT                                                
110100            , :MAP-BELEVVIL                                               
110200            , :MAP-IDSYSTEM-SEND                                          
110300            , :MAP-IDSYSTEM-REC                                           
110400            , :MAP-FILLER                                                 
110500            , :MAP-IDFELKOD                                               
110600            , :MAP-BEFEL                                                  
110700            , :MAP-IDAPPEND                                               
110800            , :MAP-BEANST                                                 
110900            , :MAP-IDUSER                                                 
111000     END-EXEC                                                             
111100                                                                          
111200     MOVE SQLCODE TO SQLCODE-WS                                           
111300     PERFORM DB2-STATUS-CHECK                                             
111400     .                                                                    
111500     EJECT                                                                
111600                                                                          
111700 DB2-CLOSE-T01TRAW-CRS-2 SECTION.                                         
111800     EXEC SQL                                                             
111900        CLOSE T01TRAW-CRS-2                                               
112000     END-EXEC                                                             
112100     .                                                                    
112200     EJECT                                                                
112300                                                                          
112400 DB2-DCL-OPN-T01TRAW-CRS-3 SECTION.                                       
112500     MOVE 000100 TO GOOD-SQLCODECODES                                     
112600                                                                          
112700     EXEC SQL                                                             
112800         DECLARE T01TRAW-CRS-3 CURSOR WITH HOLD FOR                       
112900                                                                          
113000           SELECT  A.IDLEGSEL                                             
113100                 , A.IDBUNDLE                                             
113200                 , A.DAREGDAT                                             
113300                 , A.TIREGTID                                             
113400                 , A.IDREF                                                
113500                 , A.DAREFDAT                                             
113600                 , A.IDREFRAD                                             
113700                 , A.BEVOLREF                                             
113800                 , A.IDLANDX3_SEND                                        
113900                 , A.IDLANDX3_REC                                         
114000                 , A.IDPARTNR                                             
114100                 , A.IDEXCUST_1                                           
114200                 , A.IDEXCUST_2                                           
114300                 , A.IDEXCUST_3                                           
114400                 , A.IDOPTION_1                                           
114500                 , A.IDOPTION_2                                           
114600                 , A.IDOPTION_3                                           
114700                 , A.IDOPTION_4                                           
114800                 , A.IDOPTION_5                                           
114900                 , A.IDARTNR_FINANCE                                      
115000                 , A.IDSTATNR                                             
115100                 , A.VKARTNTO                                             
115200                 , A.PRARTBTO                                             
115300                 , A.PRARTNTO                                             
115400                 , A.REARTRAB                                             
115500                 , A.KVBEART                                              
115600                 , A.KVLEVART                                             
115700                 , A.BEART                                                
115800                 , A.FLSOFT                                               
115900                 , A.FLSPECPR                                             
116000                 , A.FLFREE                                               
116100                 , A.KDVAT                                                
116200                 , A.KDVALISO                                             
116300                 , A.KDINVFRQ                                             
116400                 , A.KDFINDOC                                             
116500                 , A.IDBREAK_1                                            
116600                 , A.IDBREAK_2                                            
116700                 , A.IDSEQ_1                                              
116800                 , A.IDSEQ_2                                              
116900                 , A.IDSEQ_3                                              
117000                 , A.KDARTURS                                             
117100                 , A.KDANMORS                                             
117200                 , A.IDFAKREF                                             
117300                 , A.DAFAKREF                                             
117400                 , A.IDDC                                                 
117500                 , A.IDACCNT_1                                            
117600                 , A.IDACCNT_2                                            
117700                 , A.IDACCNT_3                                            
117800                 , A.IDACCNT_4                                            
117900                 , A.VKORDBTO_KOLLI                                       
118000                 , A.IDLEVNR                                              
118100                 , A.KDFRAKT                                              
118200                 , A.BELEVVIL                                             
118300                 , A.IDSYSTEM_SEND                                        
118400                 , A.IDSYSTEM_REC                                         
118500                 , A.FILLER                                               
118600                 , A.IDFELKOD                                             
118700                 , A.BEFEL                                                
118800                 , A.IDAPPEND                                             
118900                 , A.BEANST                                               
119000                 , A.IDUSER                                               
119100                                                                          
119200           FROM    T01TRAW A                                              
119300           ,       T01TBUN B                                              
119400                                                                          
119500           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
119600           AND    A.IDLEGSEL = B.IDLEGSEL                                 
119700           AND    A.IDBUNDLE = B.IDBUNDLE                                 
119800           AND    A.DAREGDAT = B.DAREGDAT                                 
119900           AND    A.TIREGTID = B.TIREGTID                                 
120000           AND    A.IDBUNDLE = :REQU-IDBUNDLE-KEY                         
120100           AND    A.DAREGDAT = :REQU-DAREGDAT-KEY                         
120200           AND    A.TIREGTID = :WS-TIREGTID-KEY                           
120300           AND    B.FLFEL    = 'J'                                        
120400                                                                          
120500           ORDER BY A.IDLEGSEL                                            
120600                  , A.DAREGDAT                                            
120700                  , A.TIREGTID                                            
120800                  , A.IDBUNDLE                                            
120900                  , A.IDREF                                               
121000                  , A.DAREFDAT                                            
121100                  , A.IDREFRAD DESC                                       
121200     END-EXEC                                                             
121300                                                                          
121400     MOVE 000100  TO GOOD-SQLCODECODES                                    
121500                                                                          
121600     EXEC SQL                                                             
121700        OPEN T01TRAW-CRS-3                                                
121800     END-EXEC                                                             
121900                                                                          
122000     MOVE SQLCODE TO SQLCODE-WS                                           
122100     PERFORM DB2-STATUS-CHECK                                             
122200     .                                                                    
122300     EJECT                                                                
122400                                                                          
122500 DB2-FETCH-T01TRAW-CRS-3 SECTION.                                         
122600     MOVE 000100  TO GOOD-SQLCODECODES                                    
122700                                                                          
122800     EXEC SQL                                                             
122900                                                                          
123000         FETCH T01TRAW-CRS-3                                              
123100                                                                          
123200         INTO :MAP-IDLEGSEL                                               
123300            , :MAP-IDBUNDLE                                               
123400            , :MAP-DAREGDAT                                               
123500            , :MAP-TIREGTID                                               
123600            , :MAP-IDREF                                                  
123700            , :MAP-DAREFDAT                                               
123800            , :MAP-IDREFRAD                                               
123900            , :MAP-BEVOLREF                                               
124000            , :MAP-IDLANDX3-SEND                                          
124100            , :MAP-IDLANDX3-REC                                           
124200            , :MAP-IDPARTNR                                               
124300            , :MAP-IDEXCUST-1                                             
124400            , :MAP-IDEXCUST-2                                             
124500            , :MAP-IDEXCUST-3                                             
124600            , :MAP-IDOPTION-1                                             
124700            , :MAP-IDOPTION-2                                             
124800            , :MAP-IDOPTION-3                                             
124900            , :MAP-IDOPTION-4                                             
125000            , :MAP-IDOPTION-5                                             
125100            , :MAP-IDARTNR-FINANCE                                        
125200            , :MAP-IDSTATNR                                               
125300            , :MAP-VKARTNTO                                               
125400            , :MAP-PRARTBTO                                               
125500            , :MAP-PRARTNTO                                               
125600            , :MAP-REARTRAB                                               
125700            , :MAP-KVBEART                                                
125800            , :MAP-KVLEVART                                               
125900            , :MAP-BEART                                                  
126000            , :MAP-FLSOFT                                                 
126100            , :MAP-FLSPECPR                                               
126200            , :MAP-FLFREE                                                 
126300            , :MAP-KDVAT                                                  
126400            , :MAP-KDVALISO                                               
126500            , :MAP-KDINVFRQ                                               
126600            , :MAP-KDFINDOC                                               
126700            , :MAP-IDBREAK-1                                              
126800            , :MAP-IDBREAK-2                                              
126900            , :MAP-IDSEQ-1                                                
127000            , :MAP-IDSEQ-2                                                
127100            , :MAP-IDSEQ-3                                                
127200            , :MAP-KDARTURS                                               
127300            , :MAP-KDANMORS                                               
127400            , :MAP-IDFAKREF                                               
127500            , :MAP-DAFAKREF                                               
127600            , :MAP-IDDC                                                   
127700            , :MAP-IDACCNT-1                                              
127800            , :MAP-IDACCNT-2                                              
127900            , :MAP-IDACCNT-3                                              
128000            , :MAP-IDACCNT-4                                              
128100            , :MAP-VKORDBTO-KOLLI                                         
128200            , :MAP-IDLEVNR                                                
128300            , :MAP-KDFRAKT                                                
128400            , :MAP-BELEVVIL                                               
128500            , :MAP-IDSYSTEM-SEND                                          
128600            , :MAP-IDSYSTEM-REC                                           
128700            , :MAP-FILLER                                                 
128800            , :MAP-IDFELKOD                                               
128900            , :MAP-BEFEL                                                  
129000            , :MAP-IDAPPEND                                               
129100            , :MAP-BEANST                                                 
129200            , :MAP-IDUSER                                                 
129300     END-EXEC                                                             
129400                                                                          
129500     MOVE SQLCODE TO SQLCODE-WS                                           
129600     PERFORM DB2-STATUS-CHECK                                             
129700     .                                                                    
129800     EJECT                                                                
129900                                                                          
130000 DB2-CLOSE-T01TRAW-CRS-3 SECTION.                                         
130100     EXEC SQL                                                             
130200        CLOSE T01TRAW-CRS-3                                               
130300     END-EXEC                                                             
130400     .                                                                    
130500     EJECT                                                                
130600                                                                          
130700******* COUNT ROWS OF ERROR IN BUNDLE *********                           
130800 DB2-COUNT-FELIBUNT SECTION.                                              
130900     EXEC SQL                                                             
131000                                                                          
131100        SELECT COUNT(*)                                                   
131200                                                                          
131300        INTO  :WS-COUNTER-FELIBUNT                                        
131400                                                                          
131500        FROM   T01TRAW A                                                  
131600        ,      T01TBUN B                                                  
131700                                                                          
131800        WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                            
131900        AND    B.IDLEGSEL = A.IDLEGSEL                                    
132000        AND    A.IDBUNDLE = :REQU-IDBUNDLE-KEY                            
132100        AND    B.IDBUNDLE = A.IDBUNDLE                                    
132200        AND    A.DAREGDAT = :REQU-DAREGDAT-KEY                            
132300        AND    B.DAREGDAT = A.DAREGDAT                                    
132400        AND    A.TIREGTID = :WS-TIREGTID-KEY                              
132500        AND    B.TIREGTID = A.TIREGTID                                    
132600        AND    B.FLFEL    = 'J'                                           
132700                                                                          
132800     END-EXEC                                                             
132900     MOVE 000100  TO GOOD-SQLCODECODES                                    
133000                                                                          
133100     MOVE SQLCODE TO SQLCODE-WS                                           
133200     PERFORM DB2-STATUS-CHECK                                             
133300     .                                                                    
133400     EJECT                                                                
133500                                                                          
133600 DB2-UPDATE-T01TBUN SECTION.                                              
133700     MOVE 000100  TO GOOD-SQLCODECODES                                    
133800     EXEC SQL UPDATE T01TBUN                                              
133900       SET FLFEL    = :NOO                                                
134000       ,   FLKNTRL  = :NOO                                                
134100                                                                          
134200       WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                                
134300       AND   IDBUNDLE = :REQU-IDBUNDLE-KEY                                
134400       AND   DAREGDAT = :REQU-DAREGDAT-KEY                                
134500       AND   TIREGTID = :WS-TIREGTID-KEY                                  
134600     END-EXEC                                                             
134700                                                                          
134800     MOVE SQLCODE TO SQLCODE-WS                                           
134900     PERFORM DB2-STATUS-CHECK                                             
135000     .                                                                    
135100     EJECT                                                                
135200                                                                          
135300 DB2-STATUS-CHECK  SECTION.                                               
135400     SET SQLCODE-IX TO 1                                                  
135500     SEARCH GOOD-SQLCODE                                                  
135600       AT END                                                             
135700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
135800          DELIMITED BY SIZE INTO ERROR-TEXT                               
135900          CALL ABEND USING RKOD-ABEND-DB2                                 
136000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
136100          CONTINUE                                                        
136200     END-SEARCH                                                           
136300     .                                                                    
