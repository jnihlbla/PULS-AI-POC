000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF028500.                                                
000400 AUTHOR.         BARSHARANI BISHOYE.                                      
000500 DATE-WRITTEN.   22/06/2020.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ROLLINGDOCNUMSERMAINTENANCE                      
001000*    FUNCTION:                                                            
001100*        READ/UPDATE/INSERT/DELETE TABLE T01NSDO DEPENDING ON             
001200*        REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                        
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001500*        KDPGMACT = 'I' INSERT                                            
001600*        KDPGMACT = 'D' DELETE                                            
001700*                                                                         
001800*        THE PROGRAM READS   TABLE T01LSEL                                
001900*        THE PROGRAM READS   TABLE T01ASNS                                
002000*        THE PROGRAM UPDATES TABLE T01NSDO                                
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION: WF0285U                                             
002400*        REQUEST:     WF0285I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:    WF0285O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'WF028500'.            
004200                                                                          
004300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004400 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600                                                                          
004700*    --- CONSTANT WORK FIELDS                                             
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000 77  WS-ADRESS                   PIC X(50)                                
005100             VALUE 'CARPARTS.BILLIT.ROLLINGDOCNUMSERMAINTENANCE'.         
005200 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005300                                                                          
005400 77  KEYS-SW                     PIC X       VALUE SPACE.                 
005500     88  KEYS-OK                             VALUE 'Y'.                   
005600     88  KEYS-WRONG                          VALUE 'N'.                   
005700                                                                          
005800 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
005900     88  ACT-CODE-VALID                  VALUE 'S', 'U', 'I', 'D'.        
006000     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006100     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
006200     88  ACT-CODE-INSERT                     VALUE 'I'.                   
006300     88  ACT-CODE-DELETE                     VALUE 'D'.                   
006400                                                                          
006500*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0285O1                      
006600 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
006700 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
006800 01  MAP-DADELDAT                PIC X(8)    VALUE SPACE.                 
006900 01  MAP-IDFINDOC-START          PIC S9(9)   VALUE ZERO COMP-3.           
007000 01  MAP-IDFINDOC-NEXT           PIC S9(9)   VALUE ZERO COMP-3.           
007100 01  MAP-IDFINDOC-STOP           PIC S9(9)   VALUE ZERO COMP-3.           
007200                                                                          
007300*    --- WORK-FIELDS SEARCHING-KEYS                                       
007500 01  WS-IDLOPNR-KEY              PIC S9(3)   VALUE ZERO COMP-3.           
007600                                                                          
007700*    --- WORK-FIELDS                                                      
007800 01  WS-IDLEGSEL                 PIC X(4)    VALUE SPACE.                 
007900 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
008000 01  WS-CURRENT-YEARX2           PIC X(2)    VALUE SPACE.                 
008100 01  WS-IDLEGSEL-DUMMY           PIC X(4)    VALUE SPACE.                 
008200 01  WS-IDLOPNR-DUMMY            PIC S9(3)   VALUE ZERO COMP-3.           
008300 01  T01NSDO-COUNTER             PIC S9(3)   VALUE ZERO COMP-3.           
008400                                                                          
008500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008600 01  GENERAL-SUBPROGRAMS.                                                 
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008900     SKIP3                                                                
009000                                                                          
009100*    --- PARAMETERS TO ABEND                                              
009200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
009600                                                                          
009700 01  MESSAGE-CODES.                                                       
009800     03  ERROR-CODES.                                                     
009900         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
010000         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
010100         05  ERR-DELETE-NOT-ALLOWED  PIC X(3)    VALUE '009'.             
010200         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
010300         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
010400         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
010500         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
010600         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
010700         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
010800         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
010900         05  NOT-CHANGEABLE          PIC X(3)    VALUE '031'.             
011000         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
011100         05  ERR-OVERLAP             PIC X(3)    VALUE '103'.             
011200     03  INFO-CODES.                                                      
011300         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
011400         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
011500         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
011600*                                                                         
011700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011800     SKIP3                                                                
011900 01  -COPY WZ01SUB                                                        
012000     EJECT                                                                
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
012300     SKIP3                                                                
012400 01  -COPY WF0285O1  -PRE MAP-                                            
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012700     SKIP3                                                                
012800 01  REQU-AREA.                                                           
012900*    03  -COPY WZ01REQU                                                   
013000*    03  -COPY WF0285I1                                                   
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013300     SKIP3                                                                
013400 01  RESP-AREA.                                                           
013500*    03  -COPY WZ01RESP                                                   
013600*    03  -COPY WF0285O1                                                   
013700     EJECT                                                                
013800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
013900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
014200 01  DB2-WS.                                                              
014300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
014400         88  CURSOR-OK                       VALUE 000.                   
014500         88  LINES-FOUND                     VALUE 000.                   
014600         88  UPDATE-OK                       VALUE 000.                   
014700         88  INSERT-OK                       VALUE 000.                   
014800         88  DELETE-OK                       VALUE 000.                   
014900         88  LINES-MISSING                   VALUE 100.                   
015000         88  RESOURCE-WRONG                  VALUE 904.                   
015100     03  GOOD-SQLCODECODES.                                               
015200         05  GOOD-SQLCODE OCCURS 5                                        
015300             INDEXED BY SQLCODE-IX PIC 9(3).                              
015400     EJECT                                                                
015500                                                                          
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
015800*01  -COPY T01LSEL -PRE T01LSEL-                                          
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'T01ASNS-AREA'.        
016100*01  -COPY T01ASNS -PRE T01ASNS-                                          
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'T01NSDO-AREA'.        
016400*01  -COPY T01NSDO -PRE T01NSDO-                                          
016500                                                                          
016600     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
016700     EJECT                                                                
016800     EXEC SQL INCLUDE T01ASNS END-EXEC.                                   
016900     EJECT                                                                
017000     EXEC SQL INCLUDE T01NSDO END-EXEC.                                   
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300     EJECT                                                                
017400 PROCEDURE DIVISION.                                                      
017500 MAIN SECTION.                                                            
017600                                                                          
017700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017800     IF SUB-KDRC = 0                                                      
017900       PERFORM A-INIT                                                     
018000       PERFORM B-CHECK-KEYS                                               
018100       IF KEYS-OK                                                         
018200         PERFORM F-READ-SHOW-INFO                                         
018300       END-IF                                                             
018400       IF KEYS-WRONG                                                      
018500         PERFORM S08-MOVE-MISSING-TO-RESPOND                              
018600       END-IF                                                             
018700       PERFORM S02-RETURN-RESPONSE                                        
018800     END-IF                                                               
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     INITIALIZE GOOD-SQLCODECODES                                         
019700     MOVE ALL '+' TO RESP-AREA                                            
019800     MOVE SPACE TO RESP-IDMSG-ERROR                                       
019900     MOVE SPACE TO RESP-IDMSG-INFO                                        
020000     MOVE SPACE TO RESP-IDELMT-ERROR                                      
020100     INITIALIZE MAP-RESP-WF0285O1                                         
020200     MOVE ZERO TO T01NSDO-COUNTER                                         
020300                                                                          
020400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
020500     MOVE FUNCTION CURRENT-DATE (3:2) TO WS-CURRENT-YEARX2                
020600     .                                                                    
020700*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
020800 B-CHECK-KEYS SECTION.                                                    
020900                                                                          
021000     MOVE YES TO KEYS-SW                                                  
021100     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
021200                                                                          
021300     IF REQU-IDMSGVER NUMERIC                                             
021400       IF REQU-IDLEGSEL-KEY > SPACE                                       
021500       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
021800       AND REQU-IDLOPNR-KEY NUMERIC                                       
021900       AND REQU-IDLOPNR-KEY > ZERO                                        
022000       AND ACT-CODE-VALID                                                 
022100         CONTINUE                                                         
022200       ELSE                                                               
022300         MOVE NOO TO KEYS-SW                                              
022400       END-IF                                                             
022500     ELSE                                                                 
022600       MOVE NOO TO KEYS-SW                                                
022700     END-IF                                                               
022800                                                                          
022900     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
023000       MOVE NOO TO KEYS-SW                                                
023100     END-IF                                                               
023200                                                                          
023300     IF KEYS-WRONG                                                        
023400       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
023500       IF REQU-IDMSGVER NUMERIC                                           
023600         CONTINUE                                                         
023700       ELSE                                                               
023800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
023900         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
024000       END-IF                                                             
024100       IF ACT-CODE-VALID                                                  
024200         CONTINUE                                                         
024300       ELSE                                                               
024400         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
024500         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
024600       END-IF                                                             
024700       IF REQU-IDUSER = SPACE OR = ALL '+'                                
024800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
024900         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
025000       END-IF                                                             
025100     END-IF                                                               
025200     IF KEYS-OK                                                           
025300       PERFORM DB2-SELECT-T01LSEL-TAB                                     
025400       IF LINES-FOUND                                                     
025500         CONTINUE                                                         
025600       ELSE                                                               
025700         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
025800         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
025900         MOVE NOO TO KEYS-SW                                              
026000       END-IF                                                             
026100     END-IF                                                               
026200     .                                                                    
026300*** - MOVE KEYS AND COMPULSORY FIELDS TO RESPOND                          
026400 F-READ-SHOW-INFO SECTION.                                                
026500                                                                          
026600     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
026900     MOVE REQU-IDLOPNR-KEY   TO RESP-IDLOPNR-KEY                          
027000                                WS-IDLOPNR-KEY                            
027100     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
027200                                                                          
027300     PERFORM FA-READ-BASICDATA                                            
027400     .                                                                    
027500*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
027600 FA-READ-BASICDATA SECTION.                                               
027700                                                                          
027800     IF ACT-CODE-SEARCH                                                   
027900        PERFORM FAA-HANDLE-SEARCH                                         
028000     ELSE                                                                 
028100        IF ACT-CODE-UPDATE                                                
028200           PERFORM FAB-UPDATE-T01NSDO                                     
028300        ELSE                                                              
028400           IF ACT-CODE-INSERT                                             
028500              PERFORM FAC-INSERT-T01NSDO                                  
028600           ELSE                                                           
028700              IF ACT-CODE-DELETE                                          
028800                 PERFORM FAD-DELETE-T01NSDO                               
028900              END-IF                                                      
029000           END-IF                                                         
029100        END-IF                                                            
029200     END-IF                                                               
029300     .                                                                    
029400*** - SEARCH DOCUMENT NUMBER SERIE ID.                                    
029500 FAA-HANDLE-SEARCH SECTION.                                               
029600                                                                          
029700     PERFORM DB2-SELECT-T01NSDO-TAB-1                                     
029800                                                                          
029900     IF LINES-FOUND                                                       
030000       MOVE '00000000' TO MAP-DADELDAT                                    
030100       PERFORM S03-MOVE-SEARCH-TO-RESPOND                                 
030200     ELSE                                                                 
030300       MOVE NOT-FOUND TO RESP-IDMSG-ERROR                                 
030400       MOVE 'IDLOPNR' TO RESP-IDELMT-ERROR                                
030500       MOVE NOO TO KEYS-SW                                                
030600     END-IF                                                               
030700     .                                                                    
030800*** - UPDATE ONE LINE ON TABLE T01NSDO.                                   
030900 FAB-UPDATE-T01NSDO SECTION.                                              
031000                                                                          
031100     PERFORM DB2-SELECT-T01NSDO-TAB-1                                     
031200                                                                          
031300     IF LINES-FOUND                                                       
031400       PERFORM FABA-CHECK-UPD-DATA                                        
031500       IF RESP-IDMSG-ERROR = SPACE                                        
031600         PERFORM DB2-UPDATE-T01NSDO-TAB                                   
031700         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
031800         PERFORM S04-MOVE-UPD-TO-RESPOND                                  
031900       END-IF                                                             
032000     ELSE                                                                 
032100       MOVE NOT-FOUND TO RESP-IDMSG-ERROR                                 
032200       MOVE 'IDLOPNR' TO RESP-IDELMT-ERROR                                
032300       MOVE NOO TO KEYS-SW                                                
032400     END-IF                                                               
032500     .                                                                    
032600*** - VALIDATE DATA FOR UPDATE ON TABLE T01NSDO.                          
032700 FABA-CHECK-UPD-DATA SECTION.                                             
032800                                                                          
032900*** COMPULSORY FIELDS                                                     
033000     IF  REQU-BETEXT > SPACE                                              
033100     AND REQU-BETEXT NOT = ALL '+'                                        
033200        CONTINUE                                                          
033300     ELSE                                                                 
033400        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
033500        MOVE 'BETEXT' TO RESP-IDELMT-ERROR                                
033600     END-IF                                                               
033700                                                                          
033800     IF RESP-IDMSG-ERROR = SPACE                                          
033900        IF REQU-IDFINDOC-START NUMERIC                                    
034000           IF REQU-IDFINDOC-START > ZERO                                  
034100             CONTINUE                                                     
034200           ELSE                                                           
034300             MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
034400             MOVE 'IDFINDOC-START' TO RESP-IDELMT-ERROR                   
034500           END-IF                                                         
034600        ELSE                                                              
034700           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
034800           MOVE 'IDFINDOC-START' TO RESP-IDELMT-ERROR                     
034900        END-IF                                                            
035000     END-IF                                                               
035100                                                                          
035200     IF RESP-IDMSG-ERROR = SPACE                                          
035300        IF REQU-IDFINDOC-STOP NUMERIC                                     
035400          IF REQU-IDFINDOC-STOP > ZERO                                    
035500            CONTINUE                                                      
035600          ELSE                                                            
035700            MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                  
035800            MOVE 'IDFINDOC-STOP' TO RESP-IDELMT-ERROR                     
035900          END-IF                                                          
036000       ELSE                                                               
036100          MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                    
036200          MOVE 'IDFINDOC-STOP' TO RESP-IDELMT-ERROR                       
036300       END-IF                                                             
036400     END-IF                                                               
036500                                                                          
036600*** CONTROL OF FIELDS                                                     
037300     IF RESP-IDMSG-ERROR = SPACE                                          
037400     AND REQU-IDFINDOC-START NOT = MAP-IDFINDOC-START                     
037500        IF REQU-IDFINDOC-START > REQU-IDFINDOC-NEXT                       
037600           MOVE REQU-IDFINDOC-START TO REQU-IDFINDOC-NEXT                 
037700        ELSE                                                              
037900             IF MAP-IDFINDOC-START > MAP-IDFINDOC-NEXT                    
038000               MOVE REQU-IDFINDOC-START TO REQU-IDFINDOC-NEXT             
038100             ELSE                                                         
038200               MOVE NOT-CHANGEABLE   TO RESP-IDMSG-ERROR                  
038300               MOVE 'IDFINDOC-START' TO RESP-IDELMT-ERROR                 
038400             END-IF                                                       
038600        END-IF                                                            
038700     END-IF                                                               
038800                                                                          
038900     IF RESP-IDMSG-ERROR = SPACE                                          
039000     AND REQU-IDFINDOC-STOP NOT = MAP-IDFINDOC-STOP                       
039110        IF REQU-IDFINDOC-START > REQU-IDFINDOC-NEXT                       
039200           CONTINUE                                                       
039300        ELSE                                                              
039500             IF MAP-IDFINDOC-START NOT = MAP-IDFINDOC-NEXT                
039600               CONTINUE                                                   
039700             ELSE                                                         
039710               IF  REQU-IDFINDOC-STOP > MAP-IDFINDOC-START                
039720               AND REQU-IDFINDOC-STOP > MAP-IDFINDOC-NEXT                 
039721                 CONTINUE                                                 
039722               ELSE                                                       
039723                 MOVE NOT-CHANGEABLE   TO RESP-IDMSG-ERROR                
039724                 MOVE 'IDFINDOC-STOP'  TO RESP-IDELMT-ERROR               
039910               END-IF                                                     
040000             END-IF                                                       
040200        END-IF                                                            
040300     END-IF                                                               
040400                                                                          
040500     IF RESP-IDMSG-ERROR = SPACE                                          
040600        MOVE REQU-IDFINDOC-START TO MAP-IDFINDOC-START                    
040700        MOVE REQU-IDFINDOC-NEXT  TO MAP-IDFINDOC-NEXT                     
040800        MOVE REQU-IDFINDOC-STOP  TO MAP-IDFINDOC-STOP                     
040900        PERFORM S07-CHECK-RELATIONS                                       
041000     END-IF                                                               
041100     .                                                                    
041200*** - INSERT NEW LINE INTO TABLE T01NSDO                                  
041300 FAC-INSERT-T01NSDO SECTION.                                              
041400                                                                          
041500     PERFORM DB2-SELECT-T01NSDO-TAB                                       
041600                                                                          
041700     IF LINES-FOUND                                                       
041800        MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                        
041900        MOVE 'IDLOPNR' TO RESP-IDELMT-ERROR                               
042000     ELSE                                                                 
042100        PERFORM FACA-CHECK-INS-DATA                                       
042200        IF RESP-IDMSG-ERROR = SPACE                                       
042300           PERFORM DB2-INSERT-T01NSDO-TAB                                 
042400           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
042500           PERFORM S05-MOVE-INS-TO-RESPOND                                
042600        END-IF                                                            
042700     END-IF                                                               
042800     .                                                                    
042900*** - VALIDATE DATA FOR INSERT ON TABLE T01NSDO.                          
043000 FACA-CHECK-INS-DATA SECTION.                                             
043100                                                                          
043200*** COMPULSORY FIELDS                                                     
043300     IF  REQU-BETEXT > SPACE                                              
043400     AND REQU-BETEXT NOT = ALL '+'                                        
043500        CONTINUE                                                          
043600     ELSE                                                                 
043700        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
043800        MOVE 'BETEXT' TO RESP-IDELMT-ERROR                                
043900     END-IF                                                               
044000                                                                          
044100     IF RESP-IDMSG-ERROR = SPACE                                          
044200        IF REQU-IDFINDOC-START NUMERIC                                    
044300          IF REQU-IDFINDOC-START > ZERO                                   
044400            MOVE REQU-IDFINDOC-START TO REQU-IDFINDOC-NEXT                
044500          ELSE                                                            
044600            MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                  
044700            MOVE 'IDFINDOC-START' TO RESP-IDELMT-ERROR                    
044800          END-IF                                                          
044900        ELSE                                                              
045000          MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                    
045100          MOVE 'IDFINDOC-START' TO RESP-IDELMT-ERROR                      
045200        END-IF                                                            
045300     END-IF                                                               
045400                                                                          
045500     IF RESP-IDMSG-ERROR = SPACE                                          
045600        IF REQU-IDFINDOC-STOP NUMERIC                                     
045700          IF REQU-IDFINDOC-STOP > ZERO                                    
045800            CONTINUE                                                      
045900          ELSE                                                            
046000            MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                  
046100            MOVE 'IDFINDOC-STOP' TO RESP-IDELMT-ERROR                     
046200          END-IF                                                          
046300        ELSE                                                              
046400          MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                    
046500          MOVE 'IDFINDOC-STOP' TO RESP-IDELMT-ERROR                       
046600        END-IF                                                            
046700     END-IF                                                               
046800                                                                          
046900     IF RESP-IDMSG-ERROR = SPACE                                          
047000        IF REQU-IDUSER > SPACE                                            
047100        AND REQU-IDUSER NOT = ALL '+'                                     
047200          CONTINUE                                                        
047300        ELSE                                                              
047400          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
047500          MOVE 'IDUSER' TO RESP-IDELMT-ERROR                              
047600        END-IF                                                            
047700     END-IF                                                               
047800                                                                          
047900*** CONTROL OF FIELDS                                                     
048600     IF RESP-IDMSG-ERROR = SPACE                                          
048700        MOVE REQU-IDFINDOC-START TO MAP-IDFINDOC-START                    
048800        MOVE REQU-IDFINDOC-NEXT  TO MAP-IDFINDOC-NEXT                     
048900        MOVE REQU-IDFINDOC-STOP  TO MAP-IDFINDOC-STOP                     
049000        PERFORM S07-CHECK-RELATIONS                                       
049100     END-IF                                                               
049200     .                                                                    
049300*** - DELETE ONE LINE FROM TABLE T01NSDO                                  
049400 FAD-DELETE-T01NSDO SECTION.                                              
049500                                                                          
049600     PERFORM DB2-SELECT-T01NSDO-TAB                                       
049700                                                                          
049800     IF LINES-FOUND                                                       
049900        PERFORM FADA-CHECK-DEL-DATA                                       
050000        IF RESP-IDMSG-ERROR = SPACE                                       
050100           PERFORM DB2-DELETE-T01NSDO-TAB                                 
050200           MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                          
050300           PERFORM S06-MOVE-DEL-TO-RESPOND                                
050400        END-IF                                                            
050500     ELSE                                                                 
050600       MOVE NOT-FOUND TO RESP-IDMSG-ERROR                                 
050700       MOVE 'IDLOPNR' TO RESP-IDELMT-ERROR                                
050800       MOVE NOO TO KEYS-SW                                                
050900     END-IF                                                               
051000     .                                                                    
051100*** - VALIDATE DATA FOR DELETE ON TABLE T01NSDO.                          
051200 FADA-CHECK-DEL-DATA SECTION.                                             
051300                                                                          
051400     IF RESP-IDMSG-ERROR = SPACE                                          
051500        IF REQU-IDFINDOC-START < REQU-IDFINDOC-NEXT                       
051600          MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
051700        ELSE                                                              
051900           IF MAP-IDFINDOC-START = MAP-IDFINDOC-NEXT                      
052000              CONTINUE                                                    
052100           ELSE                                                           
052200             MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR              
052300           END-IF                                                         
052400        END-IF                                                            
052500     END-IF                                                               
052600                                                                          
052700* CHECK THAT NUMBER SERIE NOT IS CONNECTED IN TABLE T01ASNS.              
052800     IF RESP-IDMSG-ERROR = SPACE                                          
052900        PERFORM DB2-SELECT-T01ASNS-TAB                                    
053000        IF LINES-FOUND                                                    
053100           MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                
053200        END-IF                                                            
053300     END-IF                                                               
053400     .                                                                    
053500*    --- DISPATCHER SECTIONS                                              
053600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
053700                                                                          
053800     MOVE 'GETARG'                   TO SUB-KDFUNC                        
053900     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
054000     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
054100                                                                          
054200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
054300                                                                          
054400     IF SUB-KDRC > 0                                                      
054500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
054600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
054700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054900     END-IF                                                               
055000     .                                                                    
055100     SKIP3                                                                
055200 S02-RETURN-RESPONSE SECTION.                                             
055300                                                                          
055400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
055500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
055600                                                                          
055700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
055800                                                                          
055900     IF SUB-KDRC > 0                                                      
056000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
056100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
056200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056400     END-IF                                                               
056500     .                                                                    
056600*    --- MOVE TO OUTPUT SECTIONS                                          
056700 S03-MOVE-SEARCH-TO-RESPOND SECTION.                                      
056800                                                                          
056900     MOVE MAP-RESP-BETEXT    TO RESP-BETEXT                               
057000     MOVE MAP-IDFINDOC-START TO RESP-IDFINDOC-START                       
057100     MOVE MAP-IDFINDOC-NEXT  TO RESP-IDFINDOC-NEXT                        
057200     MOVE MAP-IDFINDOC-STOP  TO RESP-IDFINDOC-STOP                        
057300     MOVE MAP-DAREGDAT       TO RESP-DAREGDAT                             
057400     MOVE MAP-DAUPPDAT       TO RESP-DAUPPDAT                             
057500     MOVE MAP-DADELDAT       TO RESP-DADELDAT                             
057600     MOVE MAP-RESP-IDUSER    TO RESP-IDUSER                               
057700     .                                                                    
057800 S04-MOVE-UPD-TO-RESPOND SECTION.                                         
057900                                                                          
058000     MOVE REQU-BETEXT         TO RESP-BETEXT                              
058100     MOVE REQU-IDFINDOC-START TO RESP-IDFINDOC-START                      
058200     MOVE REQU-IDFINDOC-NEXT  TO RESP-IDFINDOC-NEXT                       
058300     MOVE REQU-IDFINDOC-STOP  TO RESP-IDFINDOC-STOP                       
058400     MOVE MAP-DAREGDAT        TO RESP-DAREGDAT                            
058500     MOVE WS-CURRENT-DATE     TO RESP-DAUPPDAT                            
058600     MOVE MAP-DADELDAT        TO RESP-DADELDAT                            
058700     MOVE REQU-IDUSER         TO RESP-IDUSER                              
058800     .                                                                    
058900 S05-MOVE-INS-TO-RESPOND SECTION.                                         
059000                                                                          
059100     MOVE REQU-BETEXT         TO RESP-BETEXT                              
059200     MOVE REQU-IDFINDOC-START TO RESP-IDFINDOC-START                      
059300     MOVE REQU-IDFINDOC-NEXT  TO RESP-IDFINDOC-NEXT                       
059400     MOVE REQU-IDFINDOC-STOP  TO RESP-IDFINDOC-STOP                       
059500     MOVE WS-CURRENT-DATE     TO RESP-DAREGDAT                            
059600     MOVE ZERO                TO RESP-DAUPPDAT                            
059700     MOVE ZERO                TO RESP-DADELDAT                            
059800     MOVE REQU-IDUSER         TO RESP-IDUSER                              
059900     .                                                                    
060000 S06-MOVE-DEL-TO-RESPOND SECTION.                                         
060100                                                                          
060200     MOVE SPACE           TO RESP-BETEXT                                  
060300     MOVE ZERO            TO RESP-IDFINDOC-START                          
060400     MOVE ZERO            TO RESP-IDFINDOC-NEXT                           
060500     MOVE ZERO            TO RESP-IDFINDOC-STOP                           
060600     MOVE ZERO            TO RESP-DAREGDAT                                
060700     MOVE ZERO            TO RESP-DAUPPDAT                                
060800     MOVE WS-CURRENT-DATE TO RESP-DADELDAT                                
060900     MOVE REQU-IDUSER     TO RESP-IDUSER                                  
061000     .                                                                    
061100*** - CHECK THAT UPDATED/INSERTED NUMBERSERIES NOT IS CROSSING            
061200***   ANOTHER NUMBERSERIE.                                                
061300 S07-CHECK-RELATIONS SECTION.                                             
061400     PERFORM DB2-COUNT-T01NSDO-CROSS                                      
061500     IF T01NSDO-COUNTER = ZERO                                            
061600       CONTINUE                                                           
061700     ELSE                                                                 
061800       IF T01NSDO-COUNTER = 1                                             
061900         PERFORM DB2-SELECT-T01NSDO-CROSS                                 
062000         IF LINES-MISSING                                                 
062100           MOVE ERR-OVERLAP TO RESP-IDMSG-ERROR                           
062200         ELSE                                                             
062300           CONTINUE                                                       
062400         END-IF                                                           
062500       ELSE                                                               
062600         MOVE ERR-OVERLAP   TO RESP-IDMSG-ERROR                           
062700       END-IF                                                             
062800     END-IF                                                               
062900     .                                                                    
063000                                                                          
063100 S08-MOVE-MISSING-TO-RESPOND SECTION.                                     
063200     MOVE SPACE             TO RESP-BETEXT                                
063300                               RESP-IDUSER                                
063400     MOVE ZERO              TO RESP-DAREGDAT                              
063500                               RESP-DAUPPDAT                              
063600                               RESP-DADELDAT                              
063700                               RESP-IDFINDOC-START                        
063800                               RESP-IDFINDOC-NEXT                         
063900                               RESP-IDFINDOC-STOP                         
064000     .                                                                    
064100     EJECT                                                                
064200                                                                          
064300*    --- DB2 SECTIONS                                                     
064400 DB2-SELECT-T01LSEL-TAB  SECTION.                                         
064500                                                                          
064600     MOVE 000100 TO GOOD-SQLCODECODES                                     
064700                                                                          
064800     EXEC SQL                                                             
064900           SELECT  BELEGRAD_1                                             
065000                                                                          
065100           INTO   :T01LSEL-BELEGRAD-1                                     
065200                                                                          
065300           FROM    T01LSEL                                                
065400                                                                          
065500           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
065600               AND KDSTATUS = :WS-CURRENT                                 
065700     END-EXEC                                                             
065800                                                                          
065900     MOVE SQLCODE TO SQLCODE-WS                                           
066000     PERFORM DB2-STATUS-CHECK                                             
066100     .                                                                    
066200 DB2-SELECT-T01ASNS-TAB SECTION.                                          
066300                                                                          
066400     MOVE 000100  TO GOOD-SQLCODECODES                                    
066500     EXEC SQL                                                             
066600         SELECT  IDLEGSEL                                                 
066700                                                                          
066800         INTO    :WS-IDLEGSEL-DUMMY                                       
066900                                                                          
067000         FROM    T01ASNS                                                  
067100                                                                          
067200         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
067400             AND IDLOPNR  = :WS-IDLOPNR-KEY                               
067500     END-EXEC                                                             
067600                                                                          
067700     MOVE SQLCODE TO SQLCODE-WS                                           
067800     PERFORM DB2-STATUS-CHECK                                             
067900     .                                                                    
068000 DB2-SELECT-T01NSDO-TAB SECTION.                                          
068100                                                                          
068200     MOVE 000100 TO GOOD-SQLCODECODES                                     
068300                                                                          
068400     EXEC SQL                                                             
068500                                                                          
068600           SELECT  IDLEGSEL                                               
068700                                                                          
068800           INTO   :WS-IDLEGSEL                                            
068900                                                                          
069000           FROM    T01NSDO                                                
069100                                                                          
069200           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
069400               AND IDLOPNR  = :WS-IDLOPNR-KEY                             
069500     END-EXEC                                                             
069600                                                                          
069700     MOVE SQLCODE TO SQLCODE-WS                                           
069800     PERFORM DB2-STATUS-CHECK                                             
069900     .                                                                    
070000 DB2-SELECT-T01NSDO-TAB-1 SECTION.                                        
070100                                                                          
070200     MOVE 000100 TO GOOD-SQLCODECODES                                     
070300                                                                          
070400     EXEC SQL                                                             
070500                                                                          
070600           SELECT  IDLEGSEL                                               
070700                 , BETEXT                                                 
070800                 , IDFINDOC_START                                         
070900                 , IDFINDOC_NEXT                                          
071000                 , IDFINDOC_STOP                                          
071100                 , DAREGDAT                                               
071200                 , DAUPPDAT                                               
071300                 , IDUSER                                                 
071400                                                                          
071500           INTO   :WS-IDLEGSEL                                            
071600                , :MAP-RESP-BETEXT                                        
071700                , :MAP-IDFINDOC-START                                     
071800                , :MAP-IDFINDOC-NEXT                                      
071900                , :MAP-IDFINDOC-STOP                                      
072000                , :MAP-DAREGDAT                                           
072100                , :MAP-DAUPPDAT                                           
072200                , :MAP-RESP-IDUSER                                        
072300                                                                          
072400           FROM    T01NSDO                                                
072500                                                                          
072600           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
072800               AND IDLOPNR  = :WS-IDLOPNR-KEY                             
072900     END-EXEC                                                             
073000                                                                          
073100     MOVE SQLCODE TO SQLCODE-WS                                           
073200     PERFORM DB2-STATUS-CHECK                                             
073300     .                                                                    
073400 DB2-UPDATE-T01NSDO-TAB  SECTION.                                         
073500                                                                          
073600     MOVE 000     TO GOOD-SQLCODECODES                                    
073700     EXEC SQL                                                             
073800         UPDATE T01NSDO                                                   
073900             SET BETEXT         = :REQU-BETEXT                            
074000               , IDFINDOC_START = :MAP-IDFINDOC-START                     
074100               , IDFINDOC_NEXT  = :MAP-IDFINDOC-NEXT                      
074200               , IDFINDOC_STOP  = :MAP-IDFINDOC-STOP                      
074300               , DAUPPDAT       = :WS-CURRENT-DATE                        
074400               , IDUSER         = :REQU-IDUSER                            
074500                                                                          
074600         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
074800             AND IDLOPNR  = :WS-IDLOPNR-KEY                               
074900     END-EXEC                                                             
075000                                                                          
075100     MOVE SQLCODE TO SQLCODE-WS                                           
075200     PERFORM DB2-STATUS-CHECK                                             
075300     .                                                                    
075400 DB2-INSERT-T01NSDO-TAB  SECTION.                                         
075500                                                                          
075600     MOVE 000   TO GOOD-SQLCODECODES                                      
075700                                                                          
075800     EXEC SQL                                                             
075900         INSERT INTO T01NSDO                                              
076000            (IDLEGSEL,IDLOPNR,BETEXT,IDFINDOC_START                       
076100            ,IDFINDOC_NEXT,IDFINDOC_STOP,DAREGDAT,DAUPPDAT                
076200            ,IDUSER)                                                      
076300         VALUES                                                           
076400            (:REQU-IDLEGSEL-KEY,:WS-IDLOPNR-KEY                           
076500            ,:REQU-BETEXT,:MAP-IDFINDOC-START,:MAP-IDFINDOC-NEXT          
076600            ,:MAP-IDFINDOC-STOP,:WS-CURRENT-DATE,:MAP-DAUPPDAT            
076700            ,:REQU-IDUSER)                                                
076800     END-EXEC                                                             
076900                                                                          
077000     MOVE SQLCODE TO SQLCODE-WS                                           
077100     PERFORM DB2-STATUS-CHECK                                             
077200     .                                                                    
077300 DB2-DELETE-T01NSDO-TAB SECTION.                                          
077400                                                                          
077500     MOVE 000   TO GOOD-SQLCODECODES                                      
077600                                                                          
077700     EXEC SQL                                                             
077800         DELETE FROM T01NSDO                                              
077900                                                                          
078000         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
078200            AND IDLOPNR  = :WS-IDLOPNR-KEY                                
078300     END-EXEC                                                             
078400                                                                          
078500     MOVE SQLCODE TO SQLCODE-WS                                           
078600     PERFORM DB2-STATUS-CHECK                                             
078700     .                                                                    
078800 DB2-COUNT-T01NSDO-CROSS SECTION.                                         
078900                                                                          
079000     MOVE 000100  TO GOOD-SQLCODECODES                                    
079100     EXEC SQL                                                             
079200        SELECT COUNT(*)                                                   
079300                                                                          
079400        INTO :T01NSDO-COUNTER                                             
079500                                                                          
079600        FROM T01NSDO                                                      
079700                                                                          
079800        WHERE IDLEGSEL        = :REQU-IDLEGSEL-KEY                        
080000        AND   IDFINDOC_START <= :MAP-IDFINDOC-STOP                        
080100        AND   IDFINDOC_STOP  >= :MAP-IDFINDOC-START                       
080200     END-EXEC                                                             
080300                                                                          
080400     MOVE SQLCODE TO SQLCODE-WS                                           
080500     PERFORM DB2-STATUS-CHECK                                             
080600     .                                                                    
080700 DB2-SELECT-T01NSDO-CROSS SECTION.                                        
080800     MOVE 000100 TO GOOD-SQLCODECODES                                     
080900                                                                          
081000     EXEC SQL                                                             
081100                                                                          
081200           SELECT  IDLEGSEL                                               
081300                                                                          
081400           INTO   :WS-IDLEGSEL                                            
081500                                                                          
081600           FROM    T01NSDO                                                
081700                                                                          
081800           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
082000               AND IDLOPNR  = :WS-IDLOPNR-KEY                             
082100     END-EXEC                                                             
082200                                                                          
082300     MOVE SQLCODE TO SQLCODE-WS                                           
082400     PERFORM DB2-STATUS-CHECK                                             
082500     .                                                                    
082600 DB2-STATUS-CHECK  SECTION.                                               
082700                                                                          
082800     SET SQLCODE-IX TO 1                                                  
082900     SEARCH GOOD-SQLCODE                                                  
083000       AT END                                                             
083100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
083200          DELIMITED BY SIZE INTO ERROR-TEXT                               
083300          CALL ABEND USING RKOD-ABEND-DB2                                 
083400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
083500     END-SEARCH                                                           
083600     .                                                                    
