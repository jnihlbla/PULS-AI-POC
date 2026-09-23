000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF025200.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/02/12.                                                
000600 DATE-COMPILED.                                                           
000800*    O.B.S.   O.B.S.   O.B.S.   O.B.S.  O.B.S.                            
000900*        PRESS "CAPS ON" BEFORE STARTING PROGRAMMING                      
001000*                                                                         
001100*    NAME:                                                                
001200*        CARPARTS.BILLIT.FINCUSTMAINTENANCE                               
001300*    FUNCTION:                                                            
001400*        READ/UPDATE/INSERT CUSTOMER TABLE T01FCUS DEPENDING ON           
001500*        REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                        
001600*        KDPGMACT = 'S' READ                                              
001700*        KDPGMACT = 'U' UPDATE                                            
001800*        KDPGMACT = 'I' INSERT                                            
001900*                                                                         
002000*        THE PROGRAM READS   TABLE T01LSEL                                
002100*        THE PROGRAM READS   TABLE T01COCO                                
002200*        THE PROGRAM READS   TABLE T01CUGR                                
002300*        THE PROGRAM READS   TABLE T01CURR                                
002400*        THE PROGRAM UPDATES TABLE T01FCUS                                
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSACTION: WF0252U                                             
002900*        REQUEST:     WF0252I1                                            
003000*                                                                         
003100*    OUTDATA.                                                             
003200*        RESPONSE:    WF0252O1                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF025200'.            
004700                                                                          
004800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004900 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100                                                                          
005200*    --- CONSTANT WORK FIELDS                                             
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500 77  WS-ADRESS                   PIC X(50)                                
005600                    VALUE 'CARPARTS.BILLIT.FINCUSTMAINTENANCE'.           
005700 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005800 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
005900 77  WS-CURRENCY-SEK             PIC X(3)    VALUE 'SEK'.                 
006000 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
006100 77  WS-ENGLISH                  PIC X(2)    VALUE 'EN'.                  
006200 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
006300 77  WS-KDBETALV                 PIC X(4).                                
006400 77  WS-ALPHA-NUM                PIC X(36)                                
006500                    VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'.         
006600 77  WS-ALPHA-NUM-COMPARE        PIC X(36)                                
006700                    VALUE '                                    '.         
006800 77  UPPER-ALPHA                 PIC X(29)                                
006900                            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.        
007000 77  LOWER-ALPHA                 PIC X(29)                                
007100                            VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.        
007200                                                                          
007300 77  KEYS-SW                     PIC X       VALUE SPACE.                 
007400     88  KEYS-OK                             VALUE 'Y'.                   
007500     88  KEYS-WRONG                          VALUE 'N'.                   
007600                                                                          
007700 77  CUSTOMER-TYPE-SW            PIC X(3)    VALUE SPACE.                 
007800     88  CUSTOMER-VALID               VALUE 'INT', 'EXT'.                 
007900     88  INTERN-CUSTOMER                     VALUE 'INT'.                 
008000     88  EXTERN-CUSTOMER                     VALUE 'EXT'.                 
008100                                                                          
008200 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
008300     88  ACT-CODE-VALID                      VALUE 'S', 'U', 'I'.         
008400     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
008500     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
008600     88  ACT-CODE-INSERT                     VALUE 'I'.                   
008700                                                                          
008800 77  INSERT-01PATE-SW            PIC X       VALUE SPACE.                 
008900     88  INSERT-01PATE-OK                    VALUE 'Y'.                   
009000                                                                          
009100*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0252O1                      
009200 01  MAP-KDSTATUS                PIC S9(3)   VALUE ZERO COMP-3.           
009300 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
009400 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
009500 01  MAP-DADELDAT                PIC X(8)    VALUE SPACE.                 
009600                                                                          
009700*    --- WORK-FIELDS                                                      
009800 01  WS-FLCONTROL                PIC X       VALUE SPACE.                 
009900 01  WS-IDALPHA                  PIC X(10)   VALUE SPACE.                 
010000 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
010100 01  WS-DASTADAT                 PIC X(8)    VALUE SPACE.                 
010200                                                                          
010300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010400 01  GENERAL-SUBPROGRAMS.                                                 
010500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010700     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
010800     SKIP3                                                                
010900                                                                          
011000*    --- PARAMETERS TO ABEND                                              
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
011500                                                                          
011600 01  MESSAGE-CODES.                                                       
011700     03  ERROR-CODES.                                                     
011800         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
011900         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
012000         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
012100         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
012200         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
012300         05  ERR-FIELD-NOT-FOUND     PIC X(3)    VALUE '025'.             
012400         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
012500         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
012600         05  ERR-NOT-CHANGEABLE      PIC X(3)    VALUE '031'.             
012700         05  ERR-SYSTEM-ERROR        PIC X(3)    VALUE '099'.             
012800     03  INFO-CODES.                                                      
012900         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
013000         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
013100         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
013200         05  INF-UPDATE-OK-UPD-PATE  PIC X(3)    VALUE '106'.             
013300         05  INF-OHTER-VERSION-EXIST-PATE                                 
013400                                     PIC X(3)    VALUE '107'.             
013500     EJECT                                                                
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013800     SKIP3                                                                
013900 01  -COPY WZ01SUB                                                        
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
014200     SKIP3                                                                
014300 01  -COPY WZ20DATE                                                       
014400     EJECT                                                                
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
014700     SKIP3                                                                
014800*01  -COPY WF0252O1  -PRE MAP-                                            
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015100     SKIP3                                                                
015200 01  REQU-AREA.                                                           
015300*    03  -COPY WZ01REQU                                                   
015400*    03  -COPY WF0252I1                                                   
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015700     SKIP3                                                                
015800 01  RESP-AREA.                                                           
015900*    03  -COPY WZ01RESP                                                   
016000*    03  -COPY WF0252O1                                                   
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
016300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
016400                                                                          
016500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
016600 01  DB2-WS.                                                              
016700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
016800         88  CURSOR-OK                       VALUE 000.                   
016900         88  LINES-FOUND                     VALUE 000.                   
017000         88  LINES-MISSING                   VALUE 100.                   
017100         88  RESOURCE-WRONG                  VALUE 904.                   
017200     03  GOOD-SQLCODECODES.                                               
017300         05  GOOD-SQLCODE OCCURS 5                                        
017400             INDEXED BY SQLCODE-IX PIC 9(3).                              
017500     EJECT                                                                
017600                                                                          
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
017900                                                                          
018000*01  -COPY T01LSEL -PRE T01LSEL-                                          
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
018300                                                                          
018400*01  -COPY T01COCO -PRE T01COCO-                                          
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
018700                                                                          
018800*01  -COPY T01CUGR -PRE T01CUGR-                                          
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'T01FCUS-AREA'.        
019100                                                                          
019200*01  -COPY T01FCUS -PRE T01FCUS-                                          
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'T01CURR-AREA'.        
019500                                                                          
019600*01  -COPY T01CURR -PRE T01CURR-                                          
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'T01PATE-AREA'.        
019900                                                                          
020000*01  -COPY T01PATE -PRE T01PATE-                                          
020100                                                                          
020200     EJECT                                                                
020300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
020400     EJECT                                                                
020500     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
020600     EJECT                                                                
020700     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
020800     EJECT                                                                
020900     EXEC SQL INCLUDE T01FCUS END-EXEC.                                   
021000     EJECT                                                                
021100     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
021200     EJECT                                                                
021300     EXEC SQL INCLUDE T01PATE END-EXEC.                                   
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600     EJECT                                                                
021700                                                                          
021800 PROCEDURE DIVISION.                                                      
021900 MAIN SECTION.                                                            
022000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
022100     IF SUB-KDRC = 0                                                      
022200       PERFORM A-INIT                                                     
022300       PERFORM B-CHECK-KEYS                                               
022400       IF KEYS-OK                                                         
022500         PERFORM F-READ-SHOW-INFO                                         
022600       END-IF                                                             
022700       IF KEYS-WRONG                                                      
022800         PERFORM S05-MOVE-MISSING-TO-RESPOND                              
022900       END-IF                                                             
023000       PERFORM S02-RETURN-RESPONSE                                        
023100     END-IF                                                               
023200                                                                          
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
023800 A-INIT SECTION.                                                          
023900     INITIALIZE GOOD-SQLCODECODES                                         
024000     MOVE ALL '+' TO RESP-AREA                                            
024100     MOVE SPACE TO RESP-IDMSG-ERROR                                       
024200                   RESP-IDMSG-INFO                                        
024300                   RESP-IDELMT-ERROR                                      
024400     INITIALIZE MAP-RESP-WF0252O1                                         
024500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
024600     INSPECT REQU-KDPARTTY                                                
024700        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
024800     INSPECT REQU-KDPARTGR                                                
024900        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
025000     INSPECT REQU-IDLANDX3                                                
025100        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
025200     INSPECT REQU-KDVALISO                                                
025300        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
025400     INSPECT REQU-FLRATE                                                  
025500        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
025600     INSPECT REQU-FLLOCCUR                                                
025700        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
025800     INSPECT REQU-FLFINFIL                                                
025900        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026000     INSPECT REQU-FLSAPBLK                                                
026010        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026100     INSPECT REQU-KDBETALV                                                
026200        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026210     INSPECT REQU-FLDECIMAL                                               
026220        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026230     INSPECT REQU-FLCURINF                                                
026240        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026250     INSPECT REQU-FLCURRND                                                
026260        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026270     INSPECT REQU-KDVALTYP                                                
026280        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026290     INSPECT REQU-FLDIRVAT                                                
026291        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
026300     MOVE SPACE                       TO INSERT-01PATE-SW                 
026400     .                                                                    
026500                                                                          
026600*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
026700 B-CHECK-KEYS SECTION.                                                    
026800     MOVE YES           TO KEYS-SW                                        
026900     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
027000                                                                          
027100     IF REQU-KDSTATUS-KEY NUMERIC                                         
027200     AND REQU-IDMSGVER NUMERIC                                            
027300       IF (REQU-IDLEGSEL-KEY > SPACE AND NOT = ALL '+')                   
027400       AND (REQU-IDPARTNR-KEY > SPACE AND NOT = ALL '+')                  
027500       AND ACT-CODE-VALID                                                 
027600       AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                  
027700         CONTINUE                                                         
027800       ELSE                                                               
027900         MOVE NOO       TO KEYS-SW                                        
028000       END-IF                                                             
028100     ELSE                                                                 
028200       MOVE NOO         TO KEYS-SW                                        
028300     END-IF                                                               
028400                                                                          
028500     IF KEYS-WRONG                                                        
028600       MOVE ERR-INVALID-KEY    TO RESP-IDMSG-ERROR                        
028700       IF REQU-IDMSGVER NUMERIC                                           
028800         CONTINUE                                                         
028900       ELSE                                                               
029000         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
029100         MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                       
029200       END-IF                                                             
029300       IF ACT-CODE-VALID                                                  
029400         CONTINUE                                                         
029500       ELSE                                                               
029600         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
029700         MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                       
029800       END-IF                                                             
029900     END-IF                                                               
030000     .                                                                    
030100                                                                          
030200*** - MOVE KEYS AND COMPULSORY FIELDS TO RESPOND                          
030300 F-READ-SHOW-INFO SECTION.                                                
030400     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
030500     MOVE REQU-IDPARTNR-KEY  TO RESP-IDPARTNR-KEY                         
030600     MOVE REQU-KDSTATUS-KEY  TO RESP-KDSTATUS-KEY                         
030700     MOVE SPACE              TO WS-IDALPHA                                
030800                                                                          
030900     PERFORM DB2-SELECT-T01LSEL-TAB                                       
031000     IF LINES-FOUND                                                       
031100       MOVE T01LSEL-BELEGRAD-1  TO RESP-BELEGRAD-1                        
031200       IF (REQU-IDUSER > SPACE AND NOT = ALL '+')                         
031300         PERFORM FA-READ-BASICDATA                                        
031400       ELSE                                                               
031500         MOVE ERR-SYSTEM-ERROR  TO RESP-IDMSG-ERROR                       
031600         MOVE 'IDUSER'          TO RESP-IDELMT-ERROR                      
031700       END-IF                                                             
031800     ELSE                                                                 
031900       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
032000       MOVE 'IDLEGSEL'          TO RESP-IDELMT-ERROR                      
032100       MOVE NOO                 TO KEYS-SW                                
032200     END-IF                                                               
032300     .                                                                    
032400                                                                          
032500*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
032600 FA-READ-BASICDATA SECTION.                                               
032700     MOVE REQU-KDPARTTY TO CUSTOMER-TYPE-SW                               
032800                                                                          
032900     IF ACT-CODE-SEARCH                                                   
033000       PERFORM FAA-SEARCH-T01FCUS                                         
033100     ELSE                                                                 
033200       PERFORM FAB-CHECK-REQU-DATA                                        
033300       IF RESP-IDMSG-ERROR = SPACE                                        
033400         IF ACT-CODE-UPDATE                                               
033500           PERFORM FAC-UPDATE-T01FCUS                                     
033600         ELSE                                                             
033700           IF ACT-CODE-INSERT                                             
033800             PERFORM FAD-INSERT-T01FCUS                                   
033900           END-IF                                                         
034000         END-IF                                                           
034100       END-IF                                                             
034200     END-IF                                                               
034300     .                                                                    
034400                                                                          
034500*** - SEARCH FOR RIGHT CUSTOMER AND MARK CURRENT LINE IF COMING           
034600*** - LINE EXIST.                                                         
034700 FAA-SEARCH-T01FCUS SECTION.                                              
034800     MOVE NOO TO WS-FLCONTROL                                             
034900                                                                          
035000     PERFORM DB2-DCL-OPN-T01FCUS-CRS-1                                    
035100     PERFORM DB2-FETCH-T01FCUS-CRS-1                                      
035200                                                                          
035300     IF LINES-FOUND                                                       
035400       PERFORM UNTIL LINES-MISSING                                        
035500         IF REQU-KDSTATUS-KEY = WS-CURRENT                                
035600           IF MAP-KDSTATUS = WS-CURRENT                                   
035700             PERFORM S03-MOVE-SEARCH-TO-RESPOND                           
035800             MOVE YES                   TO WS-FLCONTROL                   
035900           ELSE                                                           
036000             IF MAP-KDSTATUS = WS-COMING                                  
036100               IF WS-FLCONTROL = YES                                      
036200                 MOVE YES               TO RESP-FLCOMING                  
036300                 MOVE NOO               TO WS-FLCONTROL                   
036400               END-IF                                                     
036500             END-IF                                                       
036600           END-IF                                                         
036700         ELSE                                                             
036800           IF REQU-KDSTATUS-KEY = WS-COMING                               
036900             IF MAP-KDSTATUS = WS-COMING                                  
037000               PERFORM S03-MOVE-SEARCH-TO-RESPOND                         
037100               MOVE SPACE               TO RESP-IDMSG-ERROR               
037200             ELSE                                                         
037300               MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR               
037400               MOVE 'COMMING VERSION'   TO RESP-IDELMT-ERROR              
037500               PERFORM S05-MOVE-MISSING-TO-RESPOND                        
037600             END-IF                                                       
037700           END-IF                                                         
037800         END-IF                                                           
037900         PERFORM DB2-FETCH-T01FCUS-CRS-1                                  
038000       END-PERFORM                                                        
038100     ELSE                                                                 
038200       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
038300       MOVE 'IDPARTNR'          TO RESP-IDELMT-ERROR                      
038400       MOVE NOO                 TO KEYS-SW                                
038500     END-IF                                                               
038600                                                                          
038700     PERFORM DB2-CLOSE-T01FCUS-CRS-1                                      
038800     .                                                                    
038900                                                                          
039000*** - LOGICAL CONTROL                                                     
039100 FAB-CHECK-REQU-DATA SECTION.                                             
039200     IF RESP-IDMSG-ERROR = SPACE AND                                      
039300        ACT-CODE-UPDATE                                                   
039400       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
039500       OR REQU-KDSTATUS-KEY = WS-COMING                                   
039600         PERFORM DB2-SELECT-T01FCUS-TAB-STA1                              
039700       END-IF                                                             
039800       IF LINES-FOUND                                                     
039900         CONTINUE                                                         
040000       ELSE                                                               
040100         MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                     
040200         MOVE 'IDPARTNR'          TO RESP-IDELMT-ERROR                    
040300         MOVE NOO                 TO KEYS-SW                              
040400       END-IF                                                             
040500     END-IF                                                               
040600                                                                          
040700     IF RESP-IDMSG-ERROR = SPACE AND                                      
040800        ACT-CODE-INSERT                                                   
040900       IF (EXTERN-CUSTOMER AND                                            
041000           T01LSEL-FLCUSUPD = 'N') OR                                     
041100          REQU-KDSTATUS-KEY = WS-COMING                                   
041200         MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
041300       END-IF                                                             
041400     END-IF                                                               
041500                                                                          
041600     IF RESP-IDMSG-ERROR = SPACE                                          
041700       IF CUSTOMER-VALID                                                  
041800         IF (REQU-KDPARTGR > SPACE AND NOT = ALL '+')                     
041900             PERFORM DB2-SELECT-T01CUGR-TAB                               
042000           IF LINES-FOUND                                                 
042100             CONTINUE                                                     
042200           ELSE                                                           
042300             MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                 
042400             MOVE 'KDPARTTYKDPARTGR'  TO RESP-IDELMT-ERROR                
042500           END-IF                                                         
042600         ELSE                                                             
042700           MOVE ERR-MUST-BE-ENTERED   TO RESP-IDMSG-ERROR                 
042800           MOVE 'KDPARTGR'            TO RESP-IDELMT-ERROR                
042900         END-IF                                                           
043000       ELSE                                                               
043100         MOVE ERR-INVALID-FIELD       TO RESP-IDMSG-ERROR                 
043200         MOVE 'KDPARTTY'              TO RESP-IDELMT-ERROR                
043300       END-IF                                                             
043400     END-IF                                                               
043500                                                                          
043600     IF RESP-IDMSG-ERROR = SPACE                                          
043700       IF REQU-IDSPRAK = WS-ENGLISH                                       
043800         CONTINUE                                                         
043900       ELSE                                                               
044000         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
044100         MOVE 'IDSPRAK'         TO RESP-IDELMT-ERROR                      
044200       END-IF                                                             
044300     END-IF                                                               
044400                                                                          
044500*** COMMENT: DADELDAT CAN NEVER BE SOMETHING ELSE THAN "ALL '+'"          
044600***          TODAY, BUT YOU NEVER NOW WHAT HAPPENS IN FUTURE.....         
044700     IF RESP-IDMSG-ERROR = SPACE                                          
044800       IF (REQU-DADELDAT = '00000000' OR                                  
044900           REQU-DADELDAT = SPACE      OR                                  
045000           REQU-DADELDAT = ALL '+')                                       
045100         MOVE '00000000'        TO REQU-DADELDAT                          
045200       ELSE                                                               
045300         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
045400         MOVE 'DADELDAT'        TO RESP-IDELMT-ERROR                      
045500       END-IF                                                             
045600     END-IF                                                               
045700                                                                          
045800     IF RESP-IDMSG-ERROR = SPACE                                          
045900       IF INTERN-CUSTOMER                                                 
046000         PERFORM DB2-SELECT-T01CURR-MAX                                   
046100         IF LINES-FOUND                                                   
046200           PERFORM FABA-VALIDATE-INT-CUST                                 
046300         ELSE                                                             
046400           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
046500           MOVE 'KDVALISO'        TO RESP-IDELMT-ERROR                    
046600         END-IF                                                           
046700       ELSE                                                               
046800         PERFORM DB2-SELECT-T01CURR-MAX                                   
046900         IF LINES-FOUND                                                   
047000*          IF ACT-CODE-UPDATE                                             
047100             PERFORM FABB-VALIDATE-EXT-CUST                               
047200*          END-IF                                                         
047300         ELSE                                                             
047400           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
047500           MOVE 'KDVALISO'        TO RESP-IDELMT-ERROR                    
047600         END-IF                                                           
047700       END-IF                                                             
047800     END-IF                                                               
047900                                                                          
048000     IF RESP-IDMSG-ERROR = SPACE                                          
048100       IF REQU-FLRATE NOT = 'Y' AND 'N'                                   
048200         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
048300         MOVE 'FLRATE'          TO RESP-IDELMT-ERROR                      
048400       ELSE                                                               
048500         IF REQU-FLRATE = 'Y'                                             
048600           MOVE 'J'             TO REQU-FLRATE                            
048700         END-IF                                                           
048800       END-IF                                                             
048900     END-IF                                                               
049000     IF RESP-IDMSG-ERROR = SPACE                                          
049100       IF REQU-FLLOCCUR NOT = 'Y' AND 'N'                                 
049200         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
049300         MOVE 'FLLOCCUR'        TO RESP-IDELMT-ERROR                      
049400       ELSE                                                               
049500         IF REQU-FLLOCCUR = 'Y'                                           
049600           IF REQU-FLRATE = 'J'                                           
049700             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
049800             MOVE 'FLLOCCUR'        TO RESP-IDELMT-ERROR                  
049900           ELSE                                                           
050000             MOVE 'J'             TO REQU-FLLOCCUR                        
050100           END-IF                                                         
050200         END-IF                                                           
050300       END-IF                                                             
050400     END-IF                                                               
050500     IF RESP-IDMSG-ERROR = SPACE                                          
050600       IF REQU-FLFINFIL NOT = 'Y' AND 'N'                                 
050700         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
050800         MOVE 'FLFINFIL'        TO RESP-IDELMT-ERROR                      
050900       ELSE                                                               
051000         IF REQU-FLFINFIL = 'Y'                                           
051100           MOVE 'J'             TO REQU-FLFINFIL                          
051200         END-IF                                                           
051300       END-IF                                                             
051400     END-IF                                                               
051410     IF RESP-IDMSG-ERROR = SPACE                                          
051420       IF REQU-FLSAPBLK NOT = 'Y' AND 'N'                                 
051430         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051440         MOVE 'FLSAPBLK'        TO RESP-IDELMT-ERROR                      
051450       ELSE                                                               
051460         IF REQU-FLSAPBLK = 'Y'                                           
051470           MOVE 'J'             TO REQU-FLSAPBLK                          
051480         END-IF                                                           
051490       END-IF                                                             
051491     END-IF                                                               
051492     IF RESP-IDMSG-ERROR = SPACE                                          
051493       IF REQU-FLDECIMAL NOT = 'Y' AND 'N'                                
051494         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051495         MOVE 'FLDECIMAL'       TO RESP-IDELMT-ERROR                      
051496       ELSE                                                               
051497         IF REQU-FLDECIMAL = 'Y'                                          
051498           mOVE 'J'             TO REQU-FLDECIMAL                         
051499         END-IF                                                           
051500       END-IF                                                             
051501     END-IF                                                               
051502     IF RESP-IDMSG-ERROR = SPACE                                          
051503       IF REQU-FLCURINF NOT = 'Y' AND 'N'                                 
051504         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051505         MOVE 'FLCURINF'        TO RESP-IDELMT-ERROR                      
051506       ELSE                                                               
051507         IF REQU-FLCURINF = 'Y'                                           
051508           MOVE 'J'             TO REQU-FLCURINF                          
051509         END-IF                                                           
051510       END-IF                                                             
051511     END-IF                                                               
051512                                                                          
051513     IF RESP-IDMSG-ERROR = SPACE                                          
051514       IF REQU-FLLOCCUR = 'N'                                             
051515         MOVE 'N'                 TO REQU-FLCURRND                        
051516       ELSE                                                               
051517         IF REQU-FLCURRND NOT = 'Y' AND 'N'                               
051518           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
051519           MOVE 'FLCURRND'        TO RESP-IDELMT-ERROR                    
051520         ELSE                                                             
051528           IF REQU-FLCURRND = 'Y'                                         
051529             MOVE 'J'             TO REQU-FLCURRND                        
051530           END-IF                                                         
051531         END-IF                                                           
051532       END-IF                                                             
051533     END-IF                                                               
051534     IF RESP-IDMSG-ERROR = SPACE                                          
051535       IF REQU-KDVALTYP NOT = 'A' AND 'M' AND 'D'                         
051536         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051537         MOVE 'KDVALTYP'        TO RESP-IDELMT-ERROR                      
051542       END-IF                                                             
051543     END-IF                                                               
051544     IF RESP-IDMSG-ERROR = SPACE                                          
051545       IF REQU-FLDIRVAT = 'N'                                             
051546         MOVE 'N'                 TO REQU-FLDIRVAT                        
051547       ELSE                                                               
051548         IF REQU-FLDIRVAT NOT = 'Y' AND 'N'                               
051549           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
051550           MOVE 'FLDIRVAT'        TO RESP-IDELMT-ERROR                    
051551         ELSE                                                             
051552           IF REQU-FLDIRVAT = 'Y'                                         
051553             MOVE 'J'             TO REQU-FLDIRVAT                        
051554           END-IF                                                         
051555         END-IF                                                           
051556       END-IF                                                             
051557     END-IF                                                               
051560     .                                                                    
051600                                                                          
051700*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT ON INTERNAL             
051800***   CUSTOMER.                                                           
051900 FABA-VALIDATE-INT-CUST SECTION.                                          
052000     IF RESP-IDMSG-ERROR = SPACE                                          
052100       IF (REQU-BEBET-NAME1 > SPACE AND NOT = ALL '+')                    
052200         MOVE REQU-BEBET-NAME1          TO WS-IDALPHA                     
052300         INSPECT WS-IDALPHA                                               
052400                 CONVERTING LOWER-ALPHA TO UPPER-ALPHA                    
052500       ELSE                                                               
052600         MOVE ERR-MUST-BE-ENTERED       TO RESP-IDMSG-ERROR               
052700         MOVE 'BEBET-NAME1'             TO RESP-IDELMT-ERROR              
052800       END-IF                                                             
052900     END-IF                                                               
053000                                                                          
053100     IF RESP-IDMSG-ERROR = SPACE                                          
053200       IF (REQU-ADBET-STREET > SPACE AND NOT = ALL '+')                   
053300         CONTINUE                                                         
053400       ELSE                                                               
053500         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
053600         MOVE 'ADBET-STREET'      TO RESP-IDELMT-ERROR                    
053700       END-IF                                                             
053800     END-IF                                                               
053900                                                                          
054000     IF RESP-IDMSG-ERROR = SPACE                                          
054100       IF (REQU-ADBET-PCODE > SPACE AND NOT = ALL '+')                    
054200         CONTINUE                                                         
054300       ELSE                                                               
054400         MOVE SPACE TO REQU-ADBET-PCODE                                   
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     IF RESP-IDMSG-ERROR = SPACE                                          
054900       IF (REQU-ADBET-CITY > SPACE AND NOT = ALL '+')                     
055000         CONTINUE                                                         
055100       ELSE                                                               
055200         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
055300         MOVE 'ADBET-CITY'        TO RESP-IDELMT-ERROR                    
055400       END-IF                                                             
055500     END-IF                                                               
055600                                                                          
055700     IF RESP-IDMSG-ERROR = SPACE                                          
055800       IF (REQU-IDLANDX3 > SPACE AND NOT = ALL '+')                       
055900         PERFORM DB2-SELECT-T01COCO-TAB                                   
056000         IF LINES-FOUND                                                   
056100           CONTINUE                                                       
056200         ELSE                                                             
056300           MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                   
056400           MOVE 'IDLANDX3'          TO RESP-IDELMT-ERROR                  
056500         END-IF                                                           
056600       ELSE                                                               
056700         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
056800         MOVE 'IDLANDX3'          TO RESP-IDELMT-ERROR                    
056900       END-IF                                                             
057000     END-IF                                                               
057100                                                                          
057200     IF RESP-IDMSG-ERROR = SPACE                                          
057300       IF REQU-KDKREDSP = NOO                                             
057400         CONTINUE                                                         
057500       ELSE                                                               
057600         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
057700         MOVE 'KDKREDSP'        TO RESP-IDELMT-ERROR                      
057800       END-IF                                                             
057900     END-IF                                                               
058000                                                                          
058100     IF RESP-IDMSG-ERROR = SPACE                                          
058200       IF (REQU-IDLEVNR-AP = SPACE OR                                     
058300           REQU-IDLEVNR-AP = ALL '+')                                     
058400         MOVE SPACE TO REQU-IDLEVNR-AP                                    
058500       ELSE                                                               
058600         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
058700         MOVE 'IDLEVNR-AP'      TO RESP-IDELMT-ERROR                      
058800       END-IF                                                             
058900     END-IF                                                               
059000                                                                          
059100     IF RESP-IDMSG-ERROR = SPACE                                          
059200       IF REQU-IDVAT = ALL '+'                                            
059300         MOVE SPACE TO REQU-IDVAT                                         
059400       END-IF                                                             
059500     END-IF                                                               
059600                                                                          
059700     IF RESP-IDMSG-ERROR = SPACE                                          
059810       IF (REQU-KDBETALV = 'NONE' OR                                      
060000           REQU-KDBETALV = SPACE  OR                                      
060100           REQU-KDBETALV = ALL '+')                                       
060200         MOVE 'NONE'            TO REQU-KDBETALV                          
060300       ELSE                                                               
060310         IF INTERN-CUSTOMER                                               
060320           CONTINUE                                                       
060330         ELSE                                                             
060400           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
060500           MOVE 'KDBETALV'        TO RESP-IDELMT-ERROR                    
060600         END-IF                                                           
060610       END-IF                                                             
060700     END-IF                                                               
060800                                                                          
060900     IF RESP-IDMSG-ERROR = SPACE                                          
061000       IF ACT-CODE-UPDATE                                                 
061100         IF REQU-KDSTATUS-KEY = WS-CURRENT                                
061200            MOVE WS-CURRENT-DATE         TO REQU-DAUPPDAT                 
061300         ELSE                                                             
061400           IF REQU-DAUPPDAT NUMERIC                                       
061500             IF REQU-DAUPPDAT > WS-CURRENT-DATE                           
061600                MOVE REQU-DAUPPDAT       TO DATE-TIDATE                   
061700                MOVE WS-DATE-FORMAT      TO DATE-KDDATFMT                 
061800                CALL WZ20DATE USING DATE-WZ20DATE                         
061900                IF DATE-KDRC > ZERO                                       
062000                  MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR              
062100                  MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR             
062200                END-IF                                                    
062300              ELSE                                                        
062400                MOVE ERR-INVALID-FIELD   TO RESP-IDMSG-ERROR              
062500                MOVE 'DAUPPDAT'          TO RESP-IDELMT-ERROR             
062600              END-IF                                                      
062700            ELSE                                                          
062800              MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR              
062900              MOVE 'DAUPPDAT'            TO RESP-IDELMT-ERROR             
063000            END-IF                                                        
063100          END-IF                                                          
063200        ELSE                                                              
063300          IF ACT-CODE-INSERT                                              
063400            IF REQU-KDSTATUS-KEY = WS-COMING                              
063500              MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR             
063600            ELSE                                                          
063700              IF REQU-DAUPPDAT = '00000000' OR                            
063800                 REQU-DAUPPDAT = SPACE      OR                            
063900                 REQU-DAUPPDAT = ALL '+'                                  
064000                MOVE '00000000'           TO REQU-DAUPPDAT                
064100              ELSE                                                        
064200                MOVE ERR-INVALID-FIELD    TO RESP-IDMSG-ERROR             
064300                MOVE 'DAUPPDAT'           TO RESP-IDELMT-ERROR            
064400              END-IF                                                      
064500            END-IF                                                        
064600          END-IF                                                          
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     IF REQU-BEBET-NAME2 = ALL '+'                                        
065100       MOVE SPACE TO REQU-BEBET-NAME2                                     
065200     END-IF                                                               
065300                                                                          
065400     IF REQU-BEBET-NAME3 = ALL '+'                                        
065500       MOVE SPACE TO REQU-BEBET-NAME3                                     
065600     END-IF                                                               
065700                                                                          
065800     IF REQU-BEBET-NAME4 = ALL '+'                                        
065900       MOVE SPACE TO REQU-BEBET-NAME4                                     
066000     END-IF                                                               
066100                                                                          
066200     IF REQU-ADBET-BOX = ALL '+'                                          
066300       MOVE SPACE TO REQU-ADBET-BOX                                       
066400     END-IF                                                               
066500                                                                          
066600     IF REQU-ADBET-PCODE = ALL '+'                                        
066700       MOVE SPACE TO REQU-ADBET-PCODE                                     
066800     END-IF                                                               
066900                                                                          
067000     IF REQU-IDTFN = ALL '+'                                              
067100       MOVE SPACE TO REQU-IDTFN                                           
067200     END-IF                                                               
067300                                                                          
067400     IF REQU-IDTFX = ALL '+'                                              
067500       MOVE SPACE TO REQU-IDTFX                                           
067600     END-IF                                                               
067700                                                                          
067800     IF REQU-IDMAIL = ALL '+'                                             
067900       MOVE SPACE TO REQU-IDMAIL                                          
068000     END-IF                                                               
068100                                                                          
068200     IF REQU-KDTRADP = ALL '+'                                            
068300       MOVE SPACE TO REQU-KDTRADP                                         
068400     END-IF                                                               
068500     .                                                                    
068600                                                                          
068700*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON EXTERNAL                    
068800***   CUSTOMER.                                                           
068900 FABB-VALIDATE-EXT-CUST SECTION.                                          
069000     IF RESP-IDMSG-ERROR = SPACE                                          
069100       PERFORM FABBA-CHECK-NOT-CHANGEABLE                                 
069200     END-IF                                                               
069300                                                                          
069400     IF RESP-IDMSG-ERROR = SPACE                                          
069500       PERFORM FABBB-CHECK-CHANGEABLE                                     
069600     END-IF                                                               
069700                                                                          
069800     IF RESP-IDMSG-ERROR = SPACE                                          
069900       IF REQU-KDVALISO = ALL '+'                                         
070000         MOVE SPACE                   TO REQU-KDVALISO                    
070100       END-IF                                                             
070200       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
070300         MOVE WS-CURRENT-DATE         TO REQU-DAUPPDAT                    
070400       ELSE                                                               
070500         IF REQU-DAUPPDAT NUMERIC                                         
070600           IF REQU-DAUPPDAT > WS-CURRENT-DATE                             
070700             MOVE REQU-DAUPPDAT       TO DATE-TIDATE                      
070800             MOVE WS-DATE-FORMAT      TO DATE-KDDATFMT                    
070900             CALL WZ20DATE USING DATE-WZ20DATE                            
071000             IF DATE-KDRC > ZERO                                          
071100               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
071200               MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR                
071300             END-IF                                                       
071400           ELSE                                                           
071500             MOVE ERR-INVALID-FIELD   TO RESP-IDMSG-ERROR                 
071600             MOVE 'DAUPPDAT'          TO RESP-IDELMT-ERROR                
071700           END-IF                                                         
071800         ELSE                                                             
071900           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
072000           MOVE 'DAUPPDAT'            TO RESP-IDELMT-ERROR                
072100         END-IF                                                           
072200       END-IF                                                             
072300     END-IF                                                               
072400     .                                                                    
072500                                                                          
072600*** - CHECK WHICH FIELDS THAT CANT BE CHANGED WHEN UPDATE ON              
072700***   EXTERNAL CUSTOMER.                                                  
072800 FABBA-CHECK-NOT-CHANGEABLE SECTION.                                      
072900     IF RESP-IDMSG-ERROR = SPACE                                          
073000       IF REQU-BEBET-NAME1 = ALL '+'                                      
073100         MOVE SPACE                   TO REQU-BEBET-NAME1                 
073200       END-IF                                                             
073300*      MOVE    REQU-BEBET-NAME1 TO RESP-BEBET-NAME2                       
073400*      MOVE    MAP-RESP-BEBET-NAME1  TO RESP-BEBET-NAME3                  
073500       IF REQU-BEBET-NAME1 = MAP-RESP-BEBET-NAME1                         
073600         CONTINUE                                                         
073700       ELSE                                                               
073800         IF T01LSEL-FLCUSUPD = 'N'                                        
073900           MOVE ERR-NOT-CHANGEABLE    TO RESP-IDMSG-ERROR                 
074000           MOVE 'BEBET-NAME1'         TO RESP-IDELMT-ERROR                
074100         ELSE                                                             
074200           IF REQU-BEBET-NAME1 = SPACE                                    
074300             MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
074400             MOVE 'BEBET-NAME1'       TO RESP-IDELMT-ERROR                
074500           END-IF                                                         
074600         END-IF                                                           
074700       END-IF                                                             
074800     END-IF                                                               
074900                                                                          
075000     IF RESP-IDMSG-ERROR = SPACE                                          
075100       IF REQU-BEBET-NAME2 = ALL '+'                                      
075200         MOVE SPACE                TO REQU-BEBET-NAME2                    
075300       END-IF                                                             
075400       IF REQU-BEBET-NAME2 = MAP-RESP-BEBET-NAME2                         
075500         CONTINUE                                                         
075600       ELSE                                                               
075700         IF T01LSEL-FLCUSUPD = 'N'                                        
075800           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
075900           MOVE 'BEBET-NAME2'      TO RESP-IDELMT-ERROR                   
076000         END-IF                                                           
076100       END-IF                                                             
076200     END-IF                                                               
076300                                                                          
076400     IF RESP-IDMSG-ERROR = SPACE                                          
076500       IF REQU-BEBET-NAME3 = ALL '+'                                      
076600         MOVE SPACE                TO REQU-BEBET-NAME3                    
076700       END-IF                                                             
076800       IF REQU-BEBET-NAME3 = MAP-RESP-BEBET-NAME3                         
076900         CONTINUE                                                         
077000       ELSE                                                               
077100         IF T01LSEL-FLCUSUPD = 'N'                                        
077200           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
077300           MOVE 'BEBET-NAME3'      TO RESP-IDELMT-ERROR                   
077400         END-IF                                                           
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800     IF RESP-IDMSG-ERROR = SPACE                                          
077900       IF REQU-BEBET-NAME4 = ALL '+'                                      
078000         MOVE SPACE                TO REQU-BEBET-NAME4                    
078100       END-IF                                                             
078200       IF REQU-BEBET-NAME4 = MAP-RESP-BEBET-NAME4                         
078300         CONTINUE                                                         
078400       ELSE                                                               
078500         IF T01LSEL-FLCUSUPD = 'N'                                        
078600           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
078700           MOVE 'BEBET-NAME4'      TO RESP-IDELMT-ERROR                   
078800         END-IF                                                           
078900       END-IF                                                             
079000     END-IF                                                               
079100                                                                          
079200     IF RESP-IDMSG-ERROR = SPACE                                          
079300       IF REQU-ADBET-STREET = ALL '+'                                     
079400         MOVE SPACE                TO REQU-ADBET-STREET                   
079500       END-IF                                                             
079600       IF REQU-ADBET-STREET = MAP-RESP-ADBET-STREET                       
079700         CONTINUE                                                         
079800       ELSE                                                               
079900         IF T01LSEL-FLCUSUPD = 'N'                                        
080000           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
080100           MOVE 'ADBET-STREET'     TO RESP-IDELMT-ERROR                   
080200         END-IF                                                           
080300       END-IF                                                             
080400     END-IF                                                               
080500                                                                          
080600     IF RESP-IDMSG-ERROR = SPACE                                          
080700       IF REQU-ADBET-BOX = ALL '+'                                        
080800         MOVE SPACE                TO REQU-ADBET-BOX                      
080900       END-IF                                                             
081000       IF REQU-ADBET-BOX = MAP-RESP-ADBET-BOX                             
081100         CONTINUE                                                         
081200       ELSE                                                               
081300         IF T01LSEL-FLCUSUPD = 'N'                                        
081400           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
081500           MOVE 'ADBET-BOX'        TO RESP-IDELMT-ERROR                   
081600         END-IF                                                           
081700       END-IF                                                             
081800     END-IF                                                               
081900                                                                          
082000     IF RESP-IDMSG-ERROR = SPACE                                          
082100       IF REQU-ADBET-PCODE = ALL '+'                                      
082200         MOVE SPACE                TO REQU-ADBET-PCODE                    
082300       END-IF                                                             
082400       IF REQU-ADBET-PCODE = MAP-RESP-ADBET-PCODE                         
082500         CONTINUE                                                         
082600       ELSE                                                               
082700         IF T01LSEL-FLCUSUPD = 'N'                                        
082800           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
082900           MOVE 'ADBET-PCODE'      TO RESP-IDELMT-ERROR                   
083000         END-IF                                                           
083100       END-IF                                                             
083200     END-IF                                                               
083300                                                                          
083400     IF RESP-IDMSG-ERROR = SPACE                                          
083500       IF REQU-ADBET-CITY = ALL '+'                                       
083600         MOVE SPACE                TO REQU-ADBET-CITY                     
083700       END-IF                                                             
083800       IF REQU-ADBET-CITY = MAP-RESP-ADBET-CITY                           
083900         CONTINUE                                                         
084000       ELSE                                                               
084100         IF T01LSEL-FLCUSUPD = 'N'                                        
084200           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
084300           MOVE 'ADBET-CITY'       TO RESP-IDELMT-ERROR                   
084400         END-IF                                                           
084500       END-IF                                                             
084600     END-IF                                                               
084700                                                                          
084800     IF RESP-IDMSG-ERROR = SPACE                                          
084900       IF REQU-IDLANDX3 = ALL '+'                                         
085000         MOVE SPACE                   TO REQU-IDLANDX3                    
085100       END-IF                                                             
085200       IF REQU-IDLANDX3 = MAP-RESP-IDLANDX3                               
085300         CONTINUE                                                         
085400       ELSE                                                               
085500         IF T01LSEL-FLCUSUPD = 'N'                                        
085600           MOVE ERR-NOT-CHANGEABLE    TO RESP-IDMSG-ERROR                 
085700           MOVE 'IDLANDX3   '         TO RESP-IDELMT-ERROR                
085800         ELSE                                                             
085900           IF REQU-IDLANDX3    = SPACE                                    
086000             MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
086100             MOVE 'IDLANDX3   '       TO RESP-IDELMT-ERROR                
086200           END-IF                                                         
086300         END-IF                                                           
086400       END-IF                                                             
086500     END-IF                                                               
086600                                                                          
086700     IF RESP-IDMSG-ERROR = SPACE                                          
086800       IF REQU-IDTFN = ALL '+'                                            
086900         MOVE SPACE                TO REQU-IDTFN                          
087000       END-IF                                                             
087100       IF REQU-IDTFN = MAP-RESP-IDTFN                                     
087200         CONTINUE                                                         
087300       ELSE                                                               
087400         IF T01LSEL-FLCUSUPD = 'N'                                        
087500           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
087600           MOVE 'IDTFN'            TO RESP-IDELMT-ERROR                   
087700         END-IF                                                           
087800       END-IF                                                             
087900     END-IF                                                               
088000                                                                          
088100     IF RESP-IDMSG-ERROR = SPACE                                          
088200       IF REQU-IDTFX = ALL '+'                                            
088300         MOVE SPACE                TO REQU-IDTFX                          
088400       END-IF                                                             
088500       IF REQU-IDTFX = MAP-RESP-IDTFX                                     
088600         CONTINUE                                                         
088700       ELSE                                                               
088800         IF T01LSEL-FLCUSUPD = 'N'                                        
088900           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
089000           MOVE 'IDTFX'            TO RESP-IDELMT-ERROR                   
089100         END-IF                                                           
089200       END-IF                                                             
089300     END-IF                                                               
089400                                                                          
089500     IF RESP-IDMSG-ERROR = SPACE                                          
089600       IF REQU-IDMAIL = ALL '+'                                           
089700         MOVE SPACE TO REQU-IDMAIL                                        
089800       END-IF                                                             
089900     END-IF                                                               
090000                                                                          
090100     IF RESP-IDMSG-ERROR = SPACE                                          
090200       IF REQU-IDLEVNR-AP = ALL '+'                                       
090300         MOVE SPACE                TO REQU-IDLEVNR-AP                     
090400       END-IF                                                             
090500       IF REQU-IDLEVNR-AP = MAP-RESP-IDLEVNR-AP                           
090600         CONTINUE                                                         
090700       ELSE                                                               
090800         IF T01LSEL-FLCUSUPD = 'N'                                        
090900           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
091000           MOVE 'IDLEVNR-AP'       TO RESP-IDELMT-ERROR                   
091100         END-IF                                                           
091200       END-IF                                                             
091300     END-IF                                                               
091400                                                                          
091500     IF RESP-IDMSG-ERROR = SPACE                                          
091600       IF REQU-IDVAT = ALL '+'                                            
091700         MOVE SPACE                   TO REQU-IDVAT                       
091800       END-IF                                                             
091900       IF REQU-IDVAT = MAP-RESP-IDVAT                                     
092000         CONTINUE                                                         
092100       ELSE                                                               
092200         IF T01LSEL-FLCUSUPD = 'N'                                        
092300           MOVE ERR-NOT-CHANGEABLE    TO RESP-IDMSG-ERROR                 
092400           MOVE 'IDVAT      '         TO RESP-IDELMT-ERROR                
092500         ELSE                                                             
092600           IF REQU-IDVAT       = SPACE                                    
092700             MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
092800             MOVE 'IDVAT      '       TO RESP-IDELMT-ERROR                
092900           END-IF                                                         
093000         END-IF                                                           
093100       END-IF                                                             
093200     END-IF                                                               
093300                                                                          
093400     IF RESP-IDMSG-ERROR = SPACE                                          
093500       IF REQU-KDTRADP = ALL '+'                                          
093600         MOVE SPACE                   TO REQU-KDTRADP                     
093700       END-IF                                                             
093800       IF REQU-KDTRADP = MAP-RESP-KDTRADP                                 
093900         CONTINUE                                                         
094000       ELSE                                                               
094100         IF T01LSEL-FLCUSUPD = 'N'                                        
094200           MOVE ERR-NOT-CHANGEABLE    TO RESP-IDMSG-ERROR                 
094300           MOVE 'KDTRADP    '         TO RESP-IDELMT-ERROR                
094400         ELSE                                                             
094500           IF REQU-KDTRADP     = SPACE                                    
094600             MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
094700             MOVE 'KDTRADP    '       TO RESP-IDELMT-ERROR                
094800           END-IF                                                         
094900         END-IF                                                           
095000       END-IF                                                             
095100     END-IF                                                               
095200                                                                          
095300     IF RESP-IDMSG-ERROR = SPACE                                          
095400       IF REQU-KDKREDSP = ALL '+'                                         
095500         MOVE SPACE                TO REQU-KDKREDSP                       
095600       END-IF                                                             
095700       IF REQU-KDKREDSP = MAP-RESP-KDKREDSP                               
095800         CONTINUE                                                         
095900       ELSE                                                               
096000         IF T01LSEL-FLCUSUPD = 'N'                                        
096100           MOVE ERR-NOT-CHANGEABLE TO RESP-IDMSG-ERROR                    
096200           MOVE 'KDKREDSP'         TO RESP-IDELMT-ERROR                   
096300         END-IF                                                           
096400       END-IF                                                             
096500     END-IF                                                               
096600     .                                                                    
096700                                                                          
096800 FABBB-CHECK-CHANGEABLE SECTION.                                          
096900     IF RESP-IDMSG-ERROR = SPACE                                          
097000       IF (REQU-KDBETALV > SPACE AND NOT = ALL '+')                       
097100         IF REQU-KDBETALV = MAP-RESP-KDBETALV                             
097200           CONTINUE                                                       
097300         ELSE                                                             
097400           PERFORM DB2-SELECT-T01PATE                                     
097500           IF LINES-MISSING                                               
097600             MOVE REQU-KDBETALV       TO WS-KDBETALV                      
097700                                                                          
097800***************** A-Z,0-9 ARE OK                                          
097900             INSPECT WS-KDBETALV                                          
098000             CONVERTING WS-ALPHA-NUM  TO WS-ALPHA-NUM-COMPARE             
098100             IF WS-KDBETALV = SPACE                                       
098200               MOVE YES               TO INSERT-01PATE-SW                 
098300             ELSE                                                         
098400               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
098500               MOVE 'KDBETALV'        TO RESP-IDELMT-ERROR                
098600             END-IF                                                       
098700           END-IF                                                         
098800         END-IF                                                           
098900       ELSE                                                               
099000         MOVE ERR-MUST-BE-ENTERED     TO RESP-IDMSG-ERROR                 
099100         MOVE 'KDBETALV'              TO RESP-IDELMT-ERROR                
099200       END-IF                                                             
099300     END-IF                                                               
099400     .                                                                    
099500                                                                          
099600*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
099700 FAC-UPDATE-T01FCUS SECTION.                                              
099800     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
099900       PERFORM FACA-UPD-CURRENT                                           
100000     ELSE                                                                 
100100       PERFORM FACB-UPD-COMING                                            
100200     END-IF                                                               
100300                                                                          
100400     IF RESP-IDMSG-ERROR = SPACE AND                                      
100500        INSERT-01PATE-OK         AND                                      
100600       (RESP-IDMSG-INFO  = INF-OTHER-VERSION-EXIST OR                     
100700                           INF-UPDATE-OK)                                 
100800       PERFORM DB2-INSERT-T01PATE                                         
100900       IF RESP-IDMSG-INFO =   INF-UPDATE-OK                               
101000         MOVE INF-UPDATE-OK-UPD-PATE  TO RESP-IDMSG-INFO                  
101100       ELSE                                                               
101200         MOVE INF-OHTER-VERSION-EXIST-PATE                                
101300                                      TO RESP-IDMSG-INFO                  
101400       END-IF                                                             
101500     END-IF                                                               
101600     .                                                                    
101700                                                                          
101800*** - UPDATE CURRENT LINE ON T01FCUS.                                     
101900 FACA-UPD-CURRENT SECTION.                                                
102000     PERFORM DB2-SELECT-T01FCUS-TAB-STA1                                  
102100                                                                          
102200     IF LINES-FOUND                                                       
102300       IF INTERN-CUSTOMER OR                                              
102400          T01LSEL-FLCUSUPD = 'J'                                          
102500         PERFORM DB2-UPDATE-T01FCUS-INT-STA1                              
102600       ELSE                                                               
102700         PERFORM DB2-UPDATE-T01FCUS-EXT-STA1                              
102800       END-IF                                                             
102900       PERFORM S04-MOVE-UPD-INS-TO-RESPOND                                
103000       IF REQU-FLCOMING = YES                                             
103100         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
103200       ELSE                                                               
103300         MOVE INF-UPDATE-OK           TO RESP-IDMSG-INFO                  
103400       END-IF                                                             
103500     ELSE                                                                 
103600       MOVE ERR-FIELD-NOT-FOUND       TO RESP-IDMSG-ERROR                 
103700       MOVE 'IDPARTNR'                TO RESP-IDELMT-ERROR                
103800       MOVE NOO                       TO KEYS-SW                          
103900     END-IF                                                               
104000     .                                                                    
104100                                                                          
104200*** - UPDATE COMING LINE ON T01FCUS. IF NOO COMING LINE EXIST BUT         
104300***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
104400***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
104500***   THIS AS AN UPDATE.                                                  
104600 FACB-UPD-COMING SECTION.                                                 
104700     PERFORM DB2-SELECT-T01FCUS-TAB-STA2                                  
104800                                                                          
104900     IF LINES-FOUND                                                       
105000       IF INTERN-CUSTOMER OR                                              
105100          T01LSEL-FLCUSUPD = 'J'                                          
105200         PERFORM DB2-UPDATE-T01FCUS-INT-STA2                              
105300       ELSE                                                               
105400         PERFORM DB2-UPDATE-T01FCUS-EXT-STA2                              
105500       END-IF                                                             
105600       PERFORM S04-MOVE-UPD-INS-TO-RESPOND                                
105700       MOVE INF-OTHER-VERSION-EXIST   TO RESP-IDMSG-INFO                  
105800     ELSE                                                                 
105900       PERFORM DB2-SELECT-T01FCUS-TAB-STA1                                
106000       IF LINES-FOUND                                                     
106100         PERFORM DB2-INSERT-T01FCUS-TAB-STA2                              
106200         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
106300         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
106400       ELSE                                                               
106500         MOVE ERR-UPDATE-NOT-ALLOWED  TO RESP-IDMSG-ERROR                 
106600       END-IF                                                             
106700     END-IF                                                               
106800     .                                                                    
106900                                                                          
107000*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
107100***   IF THERE IS A LINE WITH DADELDAT>0 THIS LINE FIRST WILL BE          
107200***   DELETED AND AFTER THAT A NEW LINE WILL BE INSERTED WITH             
107300***   ENTERED VALUES.                                                     
107400 FAD-INSERT-T01FCUS SECTION.                                              
107500     PERFORM DB2-SELECT-T01FCUS-TAB-DEL                                   
107600                                                                          
107700     IF LINES-FOUND                                                       
107800       IF T01FCUS-DADELDAT = WS-ACTIVE                                    
107900         MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                       
108000         MOVE 'IDPARTNR'    TO RESP-IDELMT-ERROR                          
108100       ELSE                                                               
108200         PERFORM DB2-DELETE-T01FCUS-TAB                                   
108300         PERFORM DB2-INSERT-T01FCUS-TAB-STA1                              
108400         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
108500         MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                            
108600       END-IF                                                             
108700     ELSE                                                                 
108800       PERFORM DB2-INSERT-T01FCUS-TAB-STA1                                
108900       PERFORM S04-MOVE-UPD-INS-TO-RESPOND                                
109000       MOVE INF-INSERT-OK   TO RESP-IDMSG-INFO                            
109100     END-IF                                                               
109200     .                                                                    
109300                                                                          
109400*    --- DISPATCHER SECTIONS                                              
109500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
109600     MOVE 'GETARG'                   TO SUB-KDFUNC                        
109700     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
109800     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
109900                                                                          
110000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
110100                                                                          
110200     IF SUB-KDRC > 0                                                      
110300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
110400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
110500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
110600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
110700     END-IF                                                               
110800     .                                                                    
110900                                                                          
111000 S02-RETURN-RESPONSE SECTION.                                             
111100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
111200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
111300                                                                          
111400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
111500                                                                          
111600     IF SUB-KDRC > 0                                                      
111700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
111800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
111900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
112000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400                                                                          
112500*    --- MOVE TO OUTPUT SECTIONS                                          
112600 S03-MOVE-SEARCH-TO-RESPOND SECTION.                                      
112700     MOVE MAP-RESP-BEBET-NAME1  TO RESP-BEBET-NAME1                       
112800     MOVE MAP-RESP-BEBET-NAME2  TO RESP-BEBET-NAME2                       
112900     MOVE MAP-RESP-BEBET-NAME3  TO RESP-BEBET-NAME3                       
113000     MOVE MAP-RESP-BEBET-NAME4  TO RESP-BEBET-NAME4                       
113100     MOVE MAP-RESP-ADBET-STREET TO RESP-ADBET-STREET                      
113200     MOVE MAP-RESP-ADBET-BOX    TO RESP-ADBET-BOX                         
113300     MOVE MAP-RESP-ADBET-PCODE  TO RESP-ADBET-PCODE                       
113400     MOVE MAP-RESP-ADBET-CITY   TO RESP-ADBET-CITY                        
113500     MOVE MAP-RESP-IDLANDX3     TO RESP-IDLANDX3                          
113600     MOVE MAP-RESP-IDSPRAK      TO RESP-IDSPRAK                           
113700     MOVE MAP-RESP-IDTFN        TO RESP-IDTFN                             
113800     MOVE MAP-RESP-IDTFX        TO RESP-IDTFX                             
113900     MOVE MAP-RESP-IDMAIL       TO RESP-IDMAIL                            
114000     MOVE MAP-RESP-IDLEVNR-AP   TO RESP-IDLEVNR-AP                        
114100     MOVE MAP-RESP-IDVAT        TO RESP-IDVAT                             
114200     MOVE MAP-RESP-KDVALISO     TO RESP-KDVALISO                          
114300     IF MAP-RESP-FLRATE = 'J'                                             
114400       MOVE 'Y'                 TO RESP-FLRATE                            
114500     ELSE                                                                 
114600       MOVE MAP-RESP-FLRATE     TO RESP-FLRATE                            
114700     END-IF                                                               
114800     IF MAP-RESP-FLFINFIL = 'J'                                           
114900       MOVE 'Y'                 TO RESP-FLFINFIL                          
115000     ELSE                                                                 
115100       MOVE MAP-RESP-FLFINFIL   TO RESP-FLFINFIL                          
115200     END-IF                                                               
115210     IF MAP-RESP-FLSAPBLK = 'J'                                           
115220       MOVE 'Y'                 TO RESP-FLSAPBLK                          
115230     ELSE                                                                 
115240       MOVE MAP-RESP-FLSAPBLK   TO RESP-FLSAPBLK                          
115250     END-IF                                                               
115300     IF MAP-RESP-FLLOCCUR = 'J'                                           
115400       IF RESP-FLRATE = 'Y'                                               
115500         MOVE 'N'               TO RESP-FLLOCCUR                          
115600       ELSE                                                               
115700         MOVE 'Y'               TO RESP-FLLOCCUR                          
115800       END-IF                                                             
115900     ELSE                                                                 
116000       MOVE MAP-RESP-FLLOCCUR   TO RESP-FLLOCCUR                          
116100     END-IF                                                               
116110     IF MAP-RESP-FLDECIMAL = 'J'                                          
116120       MOVE 'Y'                 TO RESP-FLDECIMAL                         
116130     ELSE                                                                 
116140       MOVE MAP-RESP-FLDECIMAL  TO RESP-FLDECIMAL                         
116150     END-IF                                                               
116160     IF MAP-RESP-FLCURINF = 'J'                                           
116170       MOVE 'Y'                 TO RESP-FLCURINF                          
116180     ELSE                                                                 
116190       MOVE MAP-RESP-FLCURINF   TO RESP-FLCURINF                          
116191     END-IF                                                               
116197     IF MAP-RESP-FLCURRND = 'J'                                           
116198       IF RESP-FLLOCCUR = 'N'                                             
116199         MOVE 'N'               TO RESP-FLCURRND                          
116200       ELSE                                                               
116201         MOVE 'Y'               TO RESP-FLCURRND                          
116202       END-IF                                                             
116203     ELSE                                                                 
116204       MOVE MAP-RESP-FLCURRND   TO RESP-FLCURRND                          
116205     END-IF                                                               
116206     IF MAP-RESP-FLDIRVAT = 'J'                                           
116207       MOVE 'Y'                 TO RESP-FLDIRVAT                          
116208     ELSE                                                                 
116209       MOVE MAP-RESP-FLDIRVAT   TO RESP-FLDIRVAT                          
116210     END-IF                                                               
116220     MOVE MAP-RESP-KDTRADP      TO RESP-KDTRADP                           
116300     MOVE MAP-RESP-KDBETALV     TO RESP-KDBETALV                          
116400     MOVE MAP-RESP-KDKREDSP     TO RESP-KDKREDSP                          
116500     MOVE MAP-RESP-KDPARTTY     TO RESP-KDPARTTY                          
116600     MOVE MAP-RESP-KDPARTGR     TO RESP-KDPARTGR                          
116610     MOVE MAP-RESP-KDVALTYP     TO RESP-KDVALTYP                          
116700     MOVE MAP-DAREGDAT          TO RESP-DAREGDAT                          
116800     MOVE MAP-DAUPPDAT          TO RESP-DAUPPDAT                          
116900     MOVE MAP-DADELDAT          TO RESP-DADELDAT                          
117000     MOVE MAP-RESP-IDUSER       TO RESP-IDUSER                            
117100     MOVE NOO                   TO RESP-FLCOMING                          
117200     .                                                                    
117300     EJECT                                                                
117400                                                                          
117500 S04-MOVE-UPD-INS-TO-RESPOND SECTION.                                     
117600     MOVE REQU-BEBET-NAME1  TO RESP-BEBET-NAME1                           
117700     MOVE REQU-BEBET-NAME2  TO RESP-BEBET-NAME2                           
117800     MOVE REQU-BEBET-NAME3  TO RESP-BEBET-NAME3                           
117900     MOVE REQU-BEBET-NAME4  TO RESP-BEBET-NAME4                           
118000     MOVE REQU-ADBET-STREET TO RESP-ADBET-STREET                          
118100     MOVE REQU-ADBET-BOX    TO RESP-ADBET-BOX                             
118200     MOVE REQU-ADBET-PCODE  TO RESP-ADBET-PCODE                           
118300     MOVE REQU-ADBET-CITY   TO RESP-ADBET-CITY                            
118400     MOVE REQU-IDLANDX3     TO RESP-IDLANDX3                              
118500     MOVE REQU-IDSPRAK      TO RESP-IDSPRAK                               
118600     MOVE REQU-IDTFN        TO RESP-IDTFN                                 
118700     MOVE REQU-IDTFX        TO RESP-IDTFX                                 
118800     MOVE REQU-IDMAIL       TO RESP-IDMAIL                                
118900     MOVE REQU-IDLEVNR-AP   TO RESP-IDLEVNR-AP                            
119000     MOVE REQU-IDVAT        TO RESP-IDVAT                                 
119100     MOVE REQU-KDVALISO     TO RESP-KDVALISO                              
119200     IF REQU-FLRATE = 'J'                                                 
119300       MOVE 'Y'             TO RESP-FLRATE                                
119400     ELSE                                                                 
119500       MOVE REQU-FLRATE     TO RESP-FLRATE                                
119600     END-IF                                                               
119700     IF REQU-FLFINFIL = 'J'                                               
119800       MOVE 'Y'             TO RESP-FLFINFIL                              
119900     ELSE                                                                 
120000       MOVE REQU-FLFINFIL   TO RESP-FLFINFIL                              
120100     END-IF                                                               
120110     IF REQU-FLSAPBLK = 'J'                                               
120120       MOVE 'Y'             TO RESP-FLSAPBLK                              
120130     ELSE                                                                 
120140       MOVE REQU-FLSAPBLK   TO RESP-FLSAPBLK                              
120150     END-IF                                                               
120200     IF REQU-FLLOCCUR = 'J'                                               
120300       IF RESP-FLRATE = 'Y'                                               
120400         MOVE 'N'             TO RESP-FLLOCCUR                            
120500       ELSE                                                               
120600         MOVE 'Y'             TO RESP-FLLOCCUR                            
120700       END-IF                                                             
120800     ELSE                                                                 
120900       MOVE REQU-FLLOCCUR   TO RESP-FLLOCCUR                              
121000     END-IF                                                               
121010     IF REQU-FLDECIMAL = 'J'                                              
121020       MOVE 'Y'             TO RESP-FLDECIMAL                             
121030     ELSE                                                                 
121040       MOVE REQU-FLDECIMAL  TO RESP-FLDECIMAL                             
121050     END-IF                                                               
121060     IF REQU-FLCURINF = 'J'                                               
121070       MOVE 'Y'             TO RESP-FLCURINF                              
121080     ELSE                                                                 
121090       MOVE REQU-FLCURINF   TO RESP-FLCURINF                              
121091     END-IF                                                               
121097     IF REQU-FLCURRND = 'J'                                               
121098       IF RESP-FLLOCCUR = 'N'                                             
121099         MOVE 'N'             TO RESP-FLCURRND                            
121100       ELSE                                                               
121101         MOVE 'Y'             TO RESP-FLCURRND                            
121102       END-IF                                                             
121103     ELSE                                                                 
121104       MOVE REQU-FLCURRND   TO RESP-FLCURRND                              
121105     END-IF                                                               
121106     IF REQU-FLDIRVAT = 'J'                                               
121107       MOVE 'Y'             TO RESP-FLDIRVAT                              
121108     ELSE                                                                 
121109       MOVE REQU-FLDIRVAT   TO RESP-FLDIRVAT                              
121110     END-IF                                                               
121120     MOVE REQU-KDTRADP      TO RESP-KDTRADP                               
121200     MOVE REQU-KDBETALV     TO RESP-KDBETALV                              
121300     MOVE REQU-KDKREDSP     TO RESP-KDKREDSP                              
121400     MOVE REQU-KDPARTTY     TO RESP-KDPARTTY                              
121500     MOVE REQU-KDPARTGR     TO RESP-KDPARTGR                              
121510     MOVE REQU-KDVALTYP     TO RESP-KDVALTYP                              
121600     MOVE MAP-DAREGDAT      TO RESP-DAREGDAT                              
121700     MOVE '00000000'        TO RESP-DAUPPDAT                              
121800     MOVE '00000000'        TO RESP-DADELDAT                              
121900*    MOVE REQU-DAUPPDAT     TO RESP-DAUPPDAT                              
122000*    MOVE REQU-DADELDAT     TO RESP-DADELDAT                              
122100     MOVE REQU-IDUSER       TO RESP-IDUSER                                
122200     MOVE REQU-FLCOMING     TO RESP-FLCOMING                              
122300                                                                          
122400* DONT SHOW DAREGDAT WHEN UPDATING ON COMING VERSION                      
122500     IF ACT-CODE-UPDATE                                                   
122600     AND REQU-KDSTATUS-KEY = WS-COMING                                    
122700       MOVE ZERO TO RESP-DAREGDAT                                         
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100                                                                          
123200 S05-MOVE-MISSING-TO-RESPOND SECTION.                                     
123300     MOVE SPACE             TO RESP-BEBET-NAME1                           
123400                               RESP-BEBET-NAME2                           
123500                               RESP-BEBET-NAME3                           
123600                               RESP-BEBET-NAME4                           
123700                               RESP-ADBET-STREET                          
123800                               RESP-ADBET-BOX                             
123900                               RESP-ADBET-PCODE                           
124000                               RESP-ADBET-CITY                            
124100                               RESP-IDLANDX3                              
124200                               RESP-IDSPRAK                               
124300                               RESP-IDTFN                                 
124400                               RESP-IDTFX                                 
124500                               RESP-IDMAIL                                
124600                               RESP-IDLEVNR-AP                            
124700                               RESP-IDVAT                                 
124800                               RESP-KDVALISO                              
124900                               RESP-FLRATE                                
125000                               RESP-FLFINFIL                              
125010                               RESP-FLSAPBLK                              
125100                               RESP-FLLOCCUR                              
125110                               RESP-FLDECIMAL                             
125120                               RESP-FLCURINF                              
125130                               RESP-FLCURRND                              
125200                               RESP-KDTRADP                               
125300                               RESP-KDBETALV                              
125400                               RESP-KDKREDSP                              
125500                               RESP-KDPARTTY                              
125600                               RESP-KDPARTGR                              
125610                               RESP-KDVALTYP                              
125620                               RESP-FLDIRVAT                              
125700                               RESP-IDUSER                                
125800                               RESP-FLCOMING                              
125900     MOVE ZERO              TO RESP-DAREGDAT                              
126000                               RESP-DAUPPDAT                              
126100                               RESP-DADELDAT                              
126200     .                                                                    
126300                                                                          
126400*    --- DB2 SECTIONS                                                     
126500 DB2-SELECT-T01PATE     SECTION.                                          
126600     MOVE 000100 TO GOOD-SQLCODECODES                                     
126700                                                                          
126800     EXEC SQL                                                             
126900           SELECT  KDBETALV                                               
127000                                                                          
127100           INTO   :T01PATE-KDBETALV                                       
127200                                                                          
127300           FROM    T01PATE                                                
127400                                                                          
127500           WHERE   IDLEGSEL  = :REQU-IDLEGSEL-KEY                         
127600           AND     IDSPRAK   = :REQU-IDSPRAK                              
127700           AND     KDBETALV  = :REQU-KDBETALV                             
127800     END-EXEC                                                             
127900                                                                          
128000     MOVE SQLCODE TO SQLCODE-WS                                           
128100     PERFORM DB2-STATUS-CHECK                                             
128200     .                                                                    
128300                                                                          
128400 DB2-SELECT-T01LSEL-TAB  SECTION.                                         
128500     MOVE 000100 TO GOOD-SQLCODECODES                                     
128600                                                                          
128700     EXEC SQL                                                             
128800           SELECT  BELEGRAD_1                                             
128900                  ,FLCUSUPD                                               
129000                                                                          
129100           INTO    :T01LSEL-BELEGRAD-1                                    
129200                  ,:T01LSEL-FLCUSUPD                                      
129300                                                                          
129400           FROM    T01LSEL                                                
129500                                                                          
129600           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
129700           AND     KDSTATUS = :WS-CURRENT                                 
129800     END-EXEC                                                             
129900                                                                          
130000     MOVE SQLCODE TO SQLCODE-WS                                           
130100     PERFORM DB2-STATUS-CHECK                                             
130200     .                                                                    
130300                                                                          
130400 DB2-SELECT-T01COCO-TAB SECTION.                                          
130500     MOVE 000100  TO GOOD-SQLCODECODES                                    
130600     EXEC SQL                                                             
130700         SELECT  IDLANDX3                                                 
130800                                                                          
130900         INTO    :T01COCO-IDLANDX3                                        
131000                                                                          
131100         FROM    T01COCO                                                  
131200                                                                          
131300         WHERE   IDLANDX3 = :REQU-IDLANDX3                                
131400     END-EXEC                                                             
131500                                                                          
131600     MOVE SQLCODE TO SQLCODE-WS                                           
131700     PERFORM DB2-STATUS-CHECK                                             
131800     .                                                                    
131900                                                                          
132000 DB2-SELECT-T01CUGR-TAB SECTION.                                          
132100     MOVE 000100  TO GOOD-SQLCODECODES                                    
132200     EXEC SQL                                                             
132300         SELECT  KDPARTGR                                                 
132400                                                                          
132500         INTO    :T01CUGR-KDPARTGR                                        
132600                                                                          
132700         FROM    T01CUGR                                                  
132800                                                                          
132900         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
133000         AND     KDPARTTY = :REQU-KDPARTTY                                
133100         AND     KDPARTGR = :REQU-KDPARTGR                                
133200         AND     KDSTATUS = :WS-CURRENT                                   
133300         AND     DADELDAT = :WS-ACTIVE                                    
133400     END-EXEC                                                             
133500                                                                          
133600     MOVE SQLCODE TO SQLCODE-WS                                           
133700     PERFORM DB2-STATUS-CHECK                                             
133800     .                                                                    
133900                                                                          
134000 DB2-DCL-OPN-T01FCUS-CRS-1 SECTION.                                       
134100     MOVE 000100 TO GOOD-SQLCODECODES                                     
134200                                                                          
134300     EXEC SQL                                                             
134400         DECLARE T01FCUS-CRS-1 CURSOR WITH HOLD FOR                       
134500                                                                          
134600           SELECT  KDSTATUS                                               
134700                 , BEBET_NAME1                                            
134800                 , BEBET_NAME2                                            
134900                 , BEBET_NAME3                                            
135000                 , BEBET_NAME4                                            
135100                 , ADBET_STREET                                           
135200                 , ADBET_BOX                                              
135300                 , ADBET_PCODE                                            
135400                 , ADBET_CITY                                             
135500                 , IDLANDX3                                               
135600                 , IDSPRAK                                                
135700                 , IDTFN                                                  
135800                 , IDTFX                                                  
135900                 , IDMAIL                                                 
136000                 , IDLEVNR_AP                                             
136100                 , IDVAT                                                  
136200                 , KDVALISO                                               
136300                 , FLRATE                                                 
136400                 , FLFINFIL                                               
136410                 , FLSAPBLK                                               
136500                 , FLLOCCUR                                               
136510                 , FLDECIMAL                                              
136520                 , FLCURINF                                               
136530                 , FLCURRND                                               
136600                 , KDTRADP                                                
136700                 , KDBETALV                                               
136800                 , KDKREDSP                                               
136900                 , KDPARTTY                                               
137000                 , KDPARTGR                                               
137010                 , KDVALTYP                                               
137020                 , FLDIRVAT                                               
137100                 , DAREGDAT                                               
137200                 , DAUPPDAT                                               
137300                 , DADELDAT                                               
137400                 , IDUSER                                                 
137500                                                                          
137600           FROM     T01FCUS                                               
137700                                                                          
137800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
137900           AND      IDPARTNR = :REQU-IDPARTNR-KEY                         
138000           AND      KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
138100                                                                          
138200           ORDER BY IDLEGSEL                                              
138300                  , IDPARTNR                                              
138400                  , KDSTATUS                                              
138500     END-EXEC                                                             
138600                                                                          
138700     MOVE 000100  TO GOOD-SQLCODECODES                                    
138800                                                                          
138900     EXEC SQL                                                             
139000        OPEN T01FCUS-CRS-1                                                
139100     END-EXEC                                                             
139200                                                                          
139300     MOVE SQLCODE TO SQLCODE-WS                                           
139400     PERFORM DB2-STATUS-CHECK                                             
139500     .                                                                    
139600                                                                          
139700 DB2-FETCH-T01FCUS-CRS-1 SECTION.                                         
139800     MOVE 000100  TO GOOD-SQLCODECODES                                    
139900                                                                          
140000     EXEC SQL                                                             
140100                                                                          
140200         FETCH T01FCUS-CRS-1                                              
140300                                                                          
140400         INTO :MAP-KDSTATUS                                               
140500            , :MAP-RESP-BEBET-NAME1                                       
140600            , :MAP-RESP-BEBET-NAME2                                       
140700            , :MAP-RESP-BEBET-NAME3                                       
140800            , :MAP-RESP-BEBET-NAME4                                       
140900            , :MAP-RESP-ADBET-STREET                                      
141000            , :MAP-RESP-ADBET-BOX                                         
141100            , :MAP-RESP-ADBET-PCODE                                       
141200            , :MAP-RESP-ADBET-CITY                                        
141300            , :MAP-RESP-IDLANDX3                                          
141400            , :MAP-RESP-IDSPRAK                                           
141500            , :MAP-RESP-IDTFN                                             
141600            , :MAP-RESP-IDTFX                                             
141700            , :MAP-RESP-IDMAIL                                            
141800            , :MAP-RESP-IDLEVNR-AP                                        
141900            , :MAP-RESP-IDVAT                                             
142000            , :MAP-RESP-KDVALISO                                          
142100            , :MAP-RESP-FLRATE                                            
142200            , :MAP-RESP-FLFINFIL                                          
142210            , :MAP-RESP-FLSAPBLK                                          
142300            , :MAP-RESP-FLLOCCUR                                          
142310            , :MAP-RESP-FLDECIMAL                                         
142320            , :MAP-RESP-FLCURINF                                          
142330            , :MAP-RESP-FLCURRND                                          
142400            , :MAP-RESP-KDTRADP                                           
142500            , :MAP-RESP-KDBETALV                                          
142600            , :MAP-RESP-KDKREDSP                                          
142700            , :MAP-RESP-KDPARTTY                                          
142800            , :MAP-RESP-KDPARTGR                                          
142810            , :MAP-RESP-KDVALTYP                                          
142820            , :MAP-RESP-FLDIRVAT                                          
142900            , :MAP-DAREGDAT                                               
143000            , :MAP-DAUPPDAT                                               
143100            , :MAP-DADELDAT                                               
143200            , :MAP-RESP-IDUSER                                            
143300     END-EXEC                                                             
143400                                                                          
143500     MOVE SQLCODE TO SQLCODE-WS                                           
143600     PERFORM DB2-STATUS-CHECK                                             
143700     .                                                                    
143800                                                                          
143900 DB2-CLOSE-T01FCUS-CRS-1  SECTION.                                        
144000     EXEC SQL                                                             
144100        CLOSE T01FCUS-CRS-1                                               
144200     END-EXEC                                                             
144300     .                                                                    
144400                                                                          
144500 DB2-SELECT-T01FCUS-TAB-STA1 SECTION.                                     
144600     MOVE 000100  TO GOOD-SQLCODECODES                                    
144700     EXEC SQL                                                             
144800                                                                          
144900         SELECT  BEBET_NAME1                                              
145000               , BEBET_NAME2                                              
145100               , BEBET_NAME3                                              
145200               , BEBET_NAME4                                              
145300               , ADBET_STREET                                             
145400               , ADBET_BOX                                                
145500               , ADBET_PCODE                                              
145600               , ADBET_CITY                                               
145700               , IDLANDX3                                                 
145800               , IDSPRAK                                                  
145900               , IDTFN                                                    
146000               , IDTFX                                                    
146100               , IDMAIL                                                   
146200               , IDLEVNR_AP                                               
146300               , IDVAT                                                    
146400               , KDVALISO                                                 
146500               , FLRATE                                                   
146600               , FLFINFIL                                                 
146610               , FLSAPBLK                                                 
146700               , FLLOCCUR                                                 
146711               , FLDECIMAL                                                
146712               , FLCURINF                                                 
146713               , FLCURRND                                                 
146800               , KDTRADP                                                  
146900               , KDBETALV                                                 
147000               , KDKREDSP                                                 
147100               , KDPARTTY                                                 
147200               , KDPARTGR                                                 
147210               , KDVALTYP                                                 
147220               , FLDIRVAT                                                 
147300               , DAREGDAT                                                 
147400               , DAUPPDAT                                                 
147500               , DADELDAT                                                 
147600               , IDUSER                                                   
147700                                                                          
147800         INTO   :MAP-RESP-BEBET-NAME1                                     
147900              , :MAP-RESP-BEBET-NAME2                                     
148000              , :MAP-RESP-BEBET-NAME3                                     
148100              , :MAP-RESP-BEBET-NAME4                                     
148200              , :MAP-RESP-ADBET-STREET                                    
148300              , :MAP-RESP-ADBET-BOX                                       
148400              , :MAP-RESP-ADBET-PCODE                                     
148500              , :MAP-RESP-ADBET-CITY                                      
148600              , :MAP-RESP-IDLANDX3                                        
148700              , :MAP-RESP-IDSPRAK                                         
148800              , :MAP-RESP-IDTFN                                           
148900              , :MAP-RESP-IDTFX                                           
149000              , :MAP-RESP-IDMAIL                                          
149100              , :MAP-RESP-IDLEVNR-AP                                      
149200              , :MAP-RESP-IDVAT                                           
149300              , :MAP-RESP-KDVALISO                                        
149400              , :MAP-RESP-FLRATE                                          
149500              , :MAP-RESP-FLFINFIL                                        
149510              , :MAP-RESP-FLSAPBLK                                        
149600              , :MAP-RESP-FLLOCCUR                                        
149610              , :MAP-RESP-FLDECIMAL                                       
149620              , :MAP-RESP-FLCURINF                                        
149630              , :MAP-RESP-FLCURRND                                        
149700              , :MAP-RESP-KDTRADP                                         
149800              , :MAP-RESP-KDBETALV                                        
149900              , :MAP-RESP-KDKREDSP                                        
150000              , :MAP-RESP-KDPARTTY                                        
150100              , :MAP-RESP-KDPARTGR                                        
150110              , :MAP-RESP-KDVALTYP                                        
150120              , :MAP-RESP-FLDIRVAT                                        
150200              , :MAP-DAREGDAT                                             
150300              , :MAP-DAUPPDAT                                             
150400              , :MAP-DADELDAT                                             
150500              , :MAP-RESP-IDUSER                                          
150600                                                                          
150700         FROM    T01FCUS                                                  
150800                                                                          
150900         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
151000         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
151100         AND     KDSTATUS = :WS-CURRENT                                   
151200         AND     DADELDAT = :WS-ACTIVE                                    
151300     END-EXEC                                                             
151400                                                                          
151500     MOVE SQLCODE TO SQLCODE-WS                                           
151600     PERFORM DB2-STATUS-CHECK                                             
151700     .                                                                    
151800                                                                          
151900 DB2-UPDATE-T01FCUS-INT-STA1  SECTION.                                    
152000     MOVE 000     TO GOOD-SQLCODECODES                                    
152100     EXEC SQL                                                             
152200         UPDATE T01FCUS                                                   
152300             SET IDALPHA      = :WS-IDALPHA                               
152400               , BEBET_NAME1  = :REQU-BEBET-NAME1                         
152500               , BEBET_NAME2  = :REQU-BEBET-NAME2                         
152600               , BEBET_NAME3  = :REQU-BEBET-NAME3                         
152700               , BEBET_NAME4  = :REQU-BEBET-NAME4                         
152800               , ADBET_STREET = :REQU-ADBET-STREET                        
152900               , ADBET_BOX    = :REQU-ADBET-BOX                           
153000               , ADBET_PCODE  = :REQU-ADBET-PCODE                         
153100               , ADBET_CITY   = :REQU-ADBET-CITY                          
153200               , IDLANDX3     = :REQU-IDLANDX3                            
153300               , IDSPRAK      = :REQU-IDSPRAK                             
153400               , IDTFN        = :REQU-IDTFN                               
153500               , IDTFX        = :REQU-IDTFX                               
153600               , IDMAIL       = :REQU-IDMAIL                              
153700               , IDLEVNR_AP   = :REQU-IDLEVNR-AP                          
153800               , IDVAT        = :REQU-IDVAT                               
153900               , KDVALISO     = :REQU-KDVALISO                            
154000               , FLRATE       = :REQU-FLRATE                              
154100               , FLFINFIL     = :REQU-FLFINFIL                            
154110               , FLSAPBLK     = :REQU-FLSAPBLK                            
154200               , FLLOCCUR     = :REQU-FLLOCCUR                            
154210               , FLDECIMAL    = :REQU-FLDECIMAL                           
154220               , FLCURINF     = :REQU-FLCURINF                            
154230               , FLCURRND     = :REQU-FLCURRND                            
154300               , KDTRADP      = :REQU-KDTRADP                             
154400               , KDBETALV     = :REQU-KDBETALV                            
154500               , KDKREDSP     = :REQU-KDKREDSP                            
154600               , KDPARTTY     = :REQU-KDPARTTY                            
154700               , KDPARTGR     = :REQU-KDPARTGR                            
154710               , KDVALTYP     = :REQU-KDVALTYP                            
154720               , FLDIRVAT     = :REQU-FLDIRVAT                            
154800               , DAREGDAT     = :MAP-DAREGDAT                             
154900               , DAUPPDAT     = :REQU-DAUPPDAT                            
155000               , DADELDAT     = :REQU-DADELDAT                            
155100               , IDUSER       = :REQU-IDUSER                              
155200                                                                          
155300         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
155400         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
155500         AND     KDSTATUS = :WS-CURRENT                                   
155600         AND     DADELDAT = :WS-ACTIVE                                    
155700     END-EXEC                                                             
155800                                                                          
155900     MOVE SQLCODE TO SQLCODE-WS                                           
156000     PERFORM DB2-STATUS-CHECK                                             
156100     .                                                                    
156200                                                                          
156300 DB2-UPDATE-T01FCUS-EXT-STA1  SECTION.                                    
156400     MOVE 000     TO GOOD-SQLCODECODES                                    
156500     EXEC SQL                                                             
156600         UPDATE T01FCUS                                                   
156700             SET KDPARTGR     = :REQU-KDPARTGR                            
156800               , DAUPPDAT     = :REQU-DAUPPDAT                            
156900               , KDVALISO     = :REQU-KDVALISO                            
157000               , FLRATE       = :REQU-FLRATE                              
157100               , FLFINFIL     = :REQU-FLFINFIL                            
157110               , FLSAPBLK     = :REQU-FLSAPBLK                            
157200               , FLLOCCUR     = :REQU-FLLOCCUR                            
157210               , FLDECIMAL    = :REQU-FLDECIMAL                           
157220               , FLCURINF     = :REQU-FLCURINF                            
157230               , FLCURRND     = :REQU-FLCURRND                            
157300               , KDBETALV     = :REQU-KDBETALV                            
157310               , KDVALTYP     = :REQU-KDVALTYP                            
157320               , FLDIRVAT     = :REQU-FLDIRVAT                            
157400               , IDUSER       = :REQU-IDUSER                              
157500               , IDMAIL       = :REQU-IDMAIL                              
157600                                                                          
157700         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
157800         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
157900         AND     KDSTATUS = :WS-CURRENT                                   
158000         AND     DADELDAT = :WS-ACTIVE                                    
158100     END-EXEC                                                             
158200                                                                          
158300     MOVE SQLCODE TO SQLCODE-WS                                           
158400     PERFORM DB2-STATUS-CHECK                                             
158500     .                                                                    
158600                                                                          
158700 DB2-SELECT-T01FCUS-TAB-STA2 SECTION.                                     
158800     MOVE 000100  TO GOOD-SQLCODECODES                                    
158900     EXEC SQL                                                             
159000                                                                          
159100         SELECT  BEBET_NAME1                                              
159200               , BEBET_NAME2                                              
159300               , BEBET_NAME3                                              
159400               , BEBET_NAME4                                              
159500               , ADBET_STREET                                             
159600               , ADBET_BOX                                                
159700               , ADBET_PCODE                                              
159800               , ADBET_CITY                                               
159900               , IDLANDX3                                                 
160000               , IDSPRAK                                                  
160100               , IDTFN                                                    
160200               , IDTFX                                                    
160300               , IDMAIL                                                   
160400               , IDLEVNR_AP                                               
160500               , IDVAT                                                    
160600               , KDVALISO                                                 
160700               , FLRATE                                                   
160800               , FLFINFIL                                                 
160810               , FLSAPBLK                                                 
160900               , FLLOCCUR                                                 
160910               , FLDECIMAL                                                
160920               , FLCURINF                                                 
160930               , FLCURRND                                                 
161000               , KDTRADP                                                  
161100               , KDBETALV                                                 
161200               , KDKREDSP                                                 
161300               , KDPARTTY                                                 
161400               , KDPARTGR                                                 
161410               , KDVALTYP                                                 
161420               , FLDIRVAT                                                 
161500               , DAREGDAT                                                 
161600               , DAUPPDAT                                                 
161700               , DADELDAT                                                 
161800               , IDUSER                                                   
161900                                                                          
162000         INTO   :MAP-RESP-BEBET-NAME1                                     
162100              , :MAP-RESP-BEBET-NAME2                                     
162200              , :MAP-RESP-BEBET-NAME3                                     
162300              , :MAP-RESP-BEBET-NAME4                                     
162400              , :MAP-RESP-ADBET-STREET                                    
162500              , :MAP-RESP-ADBET-BOX                                       
162600              , :MAP-RESP-ADBET-PCODE                                     
162700              , :MAP-RESP-ADBET-CITY                                      
162800              , :MAP-RESP-IDLANDX3                                        
162900              , :MAP-RESP-IDSPRAK                                         
163000              , :MAP-RESP-IDTFN                                           
163100              , :MAP-RESP-IDTFX                                           
163200              , :MAP-RESP-IDMAIL                                          
163300              , :MAP-RESP-IDLEVNR-AP                                      
163400              , :MAP-RESP-IDVAT                                           
163500              , :MAP-RESP-KDVALISO                                        
163600              , :MAP-RESP-FLRATE                                          
163700              , :MAP-RESP-FLFINFIL                                        
163710              , :MAP-RESP-FLSAPBLK                                        
163800              , :MAP-RESP-FLLOCCUR                                        
163810              , :MAP-RESP-FLDECIMAL                                       
163820              , :MAP-RESP-FLCURINF                                        
163830              , :MAP-RESP-FLCURRND                                        
163900              , :MAP-RESP-KDTRADP                                         
164000              , :MAP-RESP-KDBETALV                                        
164100              , :MAP-RESP-KDKREDSP                                        
164200              , :MAP-RESP-KDPARTTY                                        
164300              , :MAP-RESP-KDPARTGR                                        
164310              , :MAP-RESP-KDVALTYP                                        
164320              , :MAP-RESP-FLDIRVAT                                        
164400              , :MAP-DAREGDAT                                             
164500              , :MAP-DAUPPDAT                                             
164600              , :MAP-DADELDAT                                             
164700              , :MAP-RESP-IDUSER                                          
164800                                                                          
164900         FROM    T01FCUS                                                  
165000                                                                          
165100         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
165200         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
165300         AND     KDSTATUS = :WS-COMING                                    
165400         AND     DADELDAT = :WS-ACTIVE                                    
165500     END-EXEC                                                             
165600                                                                          
165700     MOVE SQLCODE TO SQLCODE-WS                                           
165800     PERFORM DB2-STATUS-CHECK                                             
165900     .                                                                    
166000                                                                          
166100 DB2-UPDATE-T01FCUS-INT-STA2  SECTION.                                    
166200     MOVE 000     TO GOOD-SQLCODECODES                                    
166300     EXEC SQL                                                             
166400         UPDATE T01FCUS                                                   
166500             SET IDALPHA      = :WS-IDALPHA                               
166600               , BEBET_NAME1  = :REQU-BEBET-NAME1                         
166700               , BEBET_NAME2  = :REQU-BEBET-NAME2                         
166800               , BEBET_NAME3  = :REQU-BEBET-NAME3                         
166900               , BEBET_NAME4  = :REQU-BEBET-NAME4                         
167000               , ADBET_STREET = :REQU-ADBET-STREET                        
167100               , ADBET_BOX    = :REQU-ADBET-BOX                           
167200               , ADBET_PCODE  = :REQU-ADBET-PCODE                         
167300               , ADBET_CITY   = :REQU-ADBET-CITY                          
167400               , IDLANDX3     = :REQU-IDLANDX3                            
167500               , IDSPRAK      = :REQU-IDSPRAK                             
167600               , IDTFN        = :REQU-IDTFN                               
167700               , IDTFX        = :REQU-IDTFX                               
167800               , IDMAIL       = :REQU-IDMAIL                              
167900               , IDLEVNR_AP   = :REQU-IDLEVNR-AP                          
168000               , IDVAT        = :REQU-IDVAT                               
168100               , KDVALISO     = :REQU-KDVALISO                            
168200               , FLRATE       = :REQU-FLRATE                              
168300               , FLFINFIL     = :REQU-FLFINFIL                            
168310               , FLSAPBLK     = :REQU-FLSAPBLK                            
168400               , FLLOCCUR     = :REQU-FLLOCCUR                            
168410               , FLDECIMAL    = :REQU-FLDECIMAL                           
168420               , FLCURINF     = :REQU-FLCURINF                            
168430               , FLCURRND     = :REQU-FLCURRND                            
168500               , KDTRADP      = :REQU-KDTRADP                             
168600               , KDBETALV     = :REQU-KDBETALV                            
168700               , KDKREDSP     = :REQU-KDKREDSP                            
168800               , KDPARTTY     = :REQU-KDPARTTY                            
168900               , KDPARTGR     = :REQU-KDPARTGR                            
168910               , KDVALTYP     = :REQU-KDVALTYP                            
168920               , FLDIRVAT     = :REQU-FLDIRVAT                            
169000               , DAREGDAT     = :MAP-DAREGDAT                             
169100               , DAUPPDAT     = :REQU-DAUPPDAT                            
169200               , DADELDAT     = :REQU-DADELDAT                            
169300               , IDUSER       = :REQU-IDUSER                              
169400                                                                          
169500         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
169600         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
169700         AND     KDSTATUS = :WS-COMING                                    
169800         AND     DADELDAT = :WS-ACTIVE                                    
169900     END-EXEC                                                             
170000                                                                          
170100     MOVE SQLCODE TO SQLCODE-WS                                           
170200     PERFORM DB2-STATUS-CHECK                                             
170300     .                                                                    
170400                                                                          
170500 DB2-UPDATE-T01FCUS-EXT-STA2  SECTION.                                    
170600     MOVE 000     TO GOOD-SQLCODECODES                                    
170700     EXEC SQL                                                             
170800         UPDATE T01FCUS                                                   
170900             SET KDPARTGR     = :REQU-KDPARTGR                            
171000               , DAUPPDAT     = :REQU-DAUPPDAT                            
171100               , KDBETALV     = :REQU-KDBETALV                            
171110               , KDVALTYP     = :REQU-KDVALTYP                            
171120               , FLDIRVAT     = :REQU-FLDIRVAT                            
171200               , IDUSER       = :REQU-IDUSER                              
171300               , IDMAIL       = :REQU-IDMAIL                              
171400                                                                          
171500         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
171600         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
171700         AND     KDSTATUS = :WS-COMING                                    
171800         AND     DADELDAT = :WS-ACTIVE                                    
171900     END-EXEC                                                             
172000                                                                          
172100     MOVE SQLCODE TO SQLCODE-WS                                           
172200     PERFORM DB2-STATUS-CHECK                                             
172300     .                                                                    
172400                                                                          
172500 DB2-SELECT-T01FCUS-TAB-DEL SECTION.                                      
172600     MOVE 000100  TO GOOD-SQLCODECODES                                    
172700     EXEC SQL                                                             
172800                                                                          
172900         SELECT  DADELDAT                                                 
173000                                                                          
173100         INTO   :T01FCUS-DADELDAT                                         
173200                                                                          
173300         FROM    T01FCUS                                                  
173400                                                                          
173500         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
173600         AND     IDPARTNR = :REQU-IDPARTNR-KEY                            
173700         AND     KDSTATUS = :WS-CURRENT                                   
173800     END-EXEC                                                             
173900                                                                          
174000     MOVE SQLCODE TO SQLCODE-WS                                           
174100     PERFORM DB2-STATUS-CHECK                                             
174200     .                                                                    
174300                                                                          
174400 DB2-INSERT-T01FCUS-TAB-STA1  SECTION.                                    
174500     MOVE 000   TO GOOD-SQLCODECODES                                      
174600     EXEC SQL                                                             
174700         INSERT INTO T01FCUS                                              
174800            (IDLEGSEL,IDPARTNR,KDSTATUS,IDALPHA,BEBET_NAME1               
174900            ,BEBET_NAME2,BEBET_NAME3,BEBET_NAME4,ADBET_STREET             
175000            ,ADBET_BOX,ADBET_PCODE,ADBET_CITY,IDLANDX3,IDSPRAK            
175100            ,IDTFN,IDTFX,IDMAIL,IDLEVNR_AP,IDVAT,KDVALISO,FLRATE          
175200            ,KDTRADP,FLLOCCUR,FLFINFIL,FLSAPBLK                           
175210            ,FLDECIMAL,FLCURINF,FLCURRND,KDVALTYP,FLDIRVAT                
175300            ,KDBETALV,KDKREDSP,KDPARTTY,KDPARTGR,DAREGDAT,DAUPPDAT        
175400            ,DADELDAT,IDUSER)                                             
175500         VALUES                                                           
175600            (:REQU-IDLEGSEL-KEY,:REQU-IDPARTNR-KEY                        
175700            ,:WS-CURRENT,:WS-IDALPHA                                      
175800            ,:REQU-BEBET-NAME1,:REQU-BEBET-NAME2                          
175900            ,:REQU-BEBET-NAME3,:REQU-BEBET-NAME4                          
176000            ,:REQU-ADBET-STREET,:REQU-ADBET-BOX,:REQU-ADBET-PCODE         
176100            ,:REQU-ADBET-CITY,:REQU-IDLANDX3,:REQU-IDSPRAK                
176200            ,:REQU-IDTFN,:REQU-IDTFX,:REQU-IDMAIL,:REQU-IDLEVNR-AP        
176300            ,:REQU-IDVAT,:REQU-KDVALISO,:REQU-FLRATE                      
176400            ,:REQU-KDTRADP,:REQU-FLLOCCUR,:REQU-FLFINFIL                  
176410            ,:REQU-FLSAPBLK,:REQU-FLDECIMAL,:REQU-FLCURINF                
176420            ,:REQU-FLCURRND,:REQU-KDVALTYP,:REQU-FLDIRVAT                 
176500            ,:REQU-KDBETALV,:REQU-KDKREDSP,:REQU-KDPARTTY                 
176600            ,:REQU-KDPARTGR,:WS-CURRENT-DATE,:REQU-DAUPPDAT               
176700            ,:REQU-DADELDAT,:REQU-IDUSER)                                 
176800     END-EXEC                                                             
176900                                                                          
177000     MOVE SQLCODE TO SQLCODE-WS                                           
177100     PERFORM DB2-STATUS-CHECK                                             
177200     .                                                                    
177300     EJECT                                                                
177400                                                                          
177500 DB2-INSERT-T01FCUS-TAB-STA2  SECTION.                                    
177600     MOVE 000   TO GOOD-SQLCODECODES                                      
177700     EXEC SQL                                                             
177800         INSERT INTO T01FCUS                                              
177900            (IDLEGSEL,IDPARTNR,KDSTATUS,IDALPHA,BEBET_NAME1               
178000            ,BEBET_NAME2,BEBET_NAME3,BEBET_NAME4,ADBET_STREET             
178100            ,ADBET_BOX,ADBET_PCODE,ADBET_CITY,IDLANDX3,IDSPRAK            
178200            ,IDTFN,IDTFX,IDMAIL,IDLEVNR_AP,IDVAT,KDVALISO,FLRATE          
178300            ,KDTRADP,FLLOCCUR,FLFINFIL,FLSAPBLK                           
178310            ,FLDECIMAL,FLCURINF,FLCURRND,KDVALTYP,FLDIRVAT                
178400            ,KDBETALV,KDKREDSP,KDPARTTY,KDPARTGR,DAREGDAT,DAUPPDAT        
178500            ,DADELDAT,IDUSER)                                             
178600         VALUES                                                           
178700            (:REQU-IDLEGSEL-KEY,:REQU-IDPARTNR-KEY                        
178800            ,:WS-COMING,:WS-IDALPHA                                       
178900            ,:REQU-BEBET-NAME1,:REQU-BEBET-NAME2                          
179000            ,:REQU-BEBET-NAME3,:REQU-BEBET-NAME4                          
179100            ,:REQU-ADBET-STREET,:REQU-ADBET-BOX,:REQU-ADBET-PCODE         
179200            ,:REQU-ADBET-CITY,:REQU-IDLANDX3,:REQU-IDSPRAK                
179300            ,:REQU-IDTFN,:REQU-IDTFX,:REQU-IDMAIL,:REQU-IDLEVNR-AP        
179400            ,:REQU-IDVAT,:REQU-KDVALISO,:REQU-FLRATE                      
179500            ,:REQU-KDTRADP,:REQU-FLLOCCUR,:REQU-FLFINFIL                  
179510            ,:REQU-FLSAPBLK,:REQU-FLDECIMAL,:REQU-FLCURINF                
179520            ,:REQU-FLCURRND,:REQU-KDVALTYP,:REQU-FLDIRVAT                 
179600            ,:REQU-KDBETALV,:REQU-KDKREDSP,:REQU-KDPARTTY                 
179700            ,:REQU-KDPARTGR,:MAP-DAREGDAT,:REQU-DAUPPDAT                  
179800            ,:REQU-DADELDAT,:REQU-IDUSER)                                 
179900     END-EXEC                                                             
180000                                                                          
180100     MOVE SQLCODE TO SQLCODE-WS                                           
180200     PERFORM DB2-STATUS-CHECK                                             
180300     .                                                                    
180400                                                                          
180500 DB2-INSERT-T01PATE           SECTION.                                    
180600     MOVE 000   TO GOOD-SQLCODECODES                                      
180700     EXEC SQL                                                             
180800         INSERT INTO T01PATE                                              
180900            (IDLEGSEL                                                     
181000            ,IDSPRAK                                                      
181100            ,KDBETALV                                                     
181200            ,BEBETVIL                                                     
181300            ,DAREGDAT                                                     
181400            ,DAUPPDAT                                                     
181500            ,IDUSER)                                                      
181600         VALUES                                                           
181700            (:REQU-IDLEGSEL-KEY                                           
181800            ,:REQU-IDSPRAK                                                
181900            ,:REQU-KDBETALV                                               
182000            ,' '                                                          
182100            ,:WS-CURRENT-DATE                                             
182200            ,'00000000'                                                   
182300            ,:REQU-IDUSER)                                                
182400     END-EXEC                                                             
182500                                                                          
182600     MOVE SQLCODE TO SQLCODE-WS                                           
182700     PERFORM DB2-STATUS-CHECK                                             
182800     .                                                                    
182900     EJECT                                                                
183000                                                                          
183100 DB2-DELETE-T01FCUS-TAB SECTION.                                          
183200     MOVE 000   TO GOOD-SQLCODECODES                                      
183300                                                                          
183400     EXEC SQL                                                             
183500         DELETE FROM T01FCUS                                              
183600                                                                          
183700         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
183800         AND    IDPARTNR = :REQU-IDPARTNR-KEY                             
183900         AND    KDSTATUS = :WS-CURRENT                                    
184000         AND    DADELDAT <> :WS-ACTIVE                                    
184100     END-EXEC                                                             
184200                                                                          
184300     MOVE SQLCODE TO SQLCODE-WS                                           
184400     PERFORM DB2-STATUS-CHECK                                             
184500     .                                                                    
184600     EJECT                                                                
184700                                                                          
184800 DB2-SELECT-T01CURR-MAX SECTION.                                          
184900     MOVE 000100305  TO GOOD-SQLCODECODES                                 
185000     EXEC SQL                                                             
185100         SELECT   MAX(KDVALISO)                                           
185200                                                                          
185300         INTO    :T01CURR-KDVALISO                                        
185400                                                                          
185500         FROM     T01CURR                                                 
185600                                                                          
185700         WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                           
185800         AND      KDVALISO = :REQU-KDVALISO                               
185900     END-EXEC                                                             
186000                                                                          
186100     MOVE SQLCODE TO SQLCODE-WS                                           
186200     PERFORM DB2-STATUS-CHECK                                             
186300     .                                                                    
186400     EJECT                                                                
186500                                                                          
186600 DB2-STATUS-CHECK  SECTION.                                               
186700     SET SQLCODE-IX TO 1                                                  
186800     SEARCH GOOD-SQLCODE                                                  
186900       AT END                                                             
187000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
187100          DELIMITED BY SIZE INTO ERROR-TEXT                               
187200          CALL ABEND USING RKOD-ABEND-DB2                                 
187300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
187400     END-SEARCH                                                           
187500     .                                                                    
